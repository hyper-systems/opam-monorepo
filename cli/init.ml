open Duniverse_lib

let build_config ~local_packages ~pins ~pull_mode ~opam_repo =
  let open Rresult.R.Infix in
  Opam_cmd.choose_root_packages ~local_packages >>= fun root_packages ->
  let ocaml_compilers =
    match Dune_file.Project.supported_ocaml_compilers () with
    | Ok l -> List.map Ocaml_version.to_string l
    | Error (`Msg msg) ->
        Logs.warn (fun l -> l "%s" msg);
        []
  in
  let version = "1" in
  let root_packages = List.map Opam_cmd.package_of_filename root_packages in
  Ok { Duniverse.Config.version; root_packages; pins; pull_mode; ocaml_compilers; opam_repo }

let compute_deps ~opam_entries =
  Dune_cmd.log_invalid_packages opam_entries;
  let get_default_branch remote = Exec.git_default_branch ~remote () in
  Duniverse.Deps.from_opam_entries ~get_default_branch opam_entries

let compute_depexts ~local_opam_repo pkgs =
  let open Rresult.R in
  Exec.map (Opam_cmd.get_opam_depexts ~local_opam_repo) pkgs >>| fun depexts ->
  List.flatten depexts |> List.sort_uniq Stdlib.compare

let resolve_ref deps =
  let resolve_ref ~upstream ~ref = Exec.git_resolve ~remote:upstream ~ref in
  Duniverse.Deps.resolve ~resolve_ref deps



module Pins = struct
  open Rresult.R

  let report_commit_is_gone_repos repos =
    let sep fmt () =
      Format.pp_print_newline fmt ();
      Styled_pp.header_indent fmt ();
      Fmt.(const string "  - ") fmt ()
    in
    let fmt_repos = Fmt.(list ~sep Styled_pp.package_name) in
    Common.Logs.app (fun l ->
        l "The following repos could not be pulled as the commit we want is gone:%a%a" sep ()
          fmt_repos repos);
    Common.Logs.app (fun l ->
        l "You should run 'duniverse update' to fix the commits associated with the tracked refs")

  let read_from_config duniverse_file =
    Bos.OS.File.exists duniverse_file >>= fun exists ->
    if not exists then Ok [] else
    Duniverse.load ~file:duniverse_file >>= fun duniverse ->
    Ok duniverse.config.pins

  let to_package (pin : Types.Opam.pin) : Types.Opam.package =
    let name = pin.pin in
    let version = None in
    let path = Fpath.(Config.pins_dir / (name ^ ".opam") |> to_string) in
    {name; version; path}

  let to_opam_entry (pin : Types.Opam.pin) : Types.Opam.entry =
    let name = pin.pin in
    let package = to_package pin in
    let dev_repo = Opam_cmd.classify_from_dev_repo ~name pin.url in
    { package; dev_repo; tag = pin.tag; is_dune = true }

  let clone_source_dep ~output_dir dep =
    let { Duniverse.Deps.Source.dir; upstream; ref = { Git.Ref.t = ref; commit }; _ } = dep in
    let output_dir = Fpath.(output_dir / dir) in
    Bos.OS.Dir.delete ~must_exist:false ~recurse:true output_dir >>= fun () ->
    Cloner.get_cache () >>= fun cache ->
    Cloner.clone_to ~output_dir ~remote:upstream ~ref ~commit cache
    |> Rresult.R.reword_error (fun (`Msg _) -> `Commit_is_gone dir)
    >>= fun cached ->
    Common.Logs.app (fun l ->
        l "Pulled sources for %a.%a into %a" Styled_pp.path output_dir Styled_pp.cached cached
            Fpath.pp output_dir);
    Ok (output_dir, dep)

  (* Pull pins and return a list of output paths with provided packages. *)
  let pull ~repo src_deps =
    let duniverse_dir = Fpath.(repo // Config.vendor_dir) in
    List.map (clone_source_dep ~output_dir:duniverse_dir) src_deps
    |> Stdune.Result.List.fold_left ~init:([], []) ~f:(fun (err_acc, ok_acc) res ->
          match res with
          | Ok dir -> Ok (err_acc, dir :: ok_acc)
          | Error (`Commit_is_gone dir) -> Ok (dir :: err_acc, ok_acc)
          | Error (`Msg _ as err) -> Error (err :> [> `Msg of string ]))
    >>= function
    | ([], ok_acc) ->
        let total = List.length src_deps in
        let pp_count = Styled_pp.good Fmt.int in
        Common.Logs.app (fun l ->
            l "Successfully pulled %a/%a pinned repositories" pp_count total pp_count total);
        Ok ok_acc
    | (commit_is_gone_repos, _) ->
        report_commit_is_gone_repos commit_is_gone_repos;
        Error (`Msg "Could not pull all the source dependencies")

  let find_changes ~config_pins ~duniverse_pin_paths =
    List.iter (fun pin -> Fmt.pr "find_changes: config_pin=%s@." pin.Types.Opam.pin) config_pins;
    List.iter (Fmt.pr "find_changes: duniverse_pin_paths=%s@.") duniverse_pin_paths;
    let path_to_name path = Filename.(remove_extension (basename path)) in
    let to_remove =
      (* All duniverse_pin_paths that do not exist in config_pins. *)
      match config_pins with
      | [] -> duniverse_pin_paths
      | config_pins ->
        List.filter (fun dpin ->
          not (List.exists (fun cpin -> path_to_name dpin = cpin.Types.Opam.pin) config_pins))
        duniverse_pin_paths
    in
    let to_pull =
      (* All config_pins that do not exist in duniverse_pin_paths. *)
      match duniverse_pin_paths with
      | [] -> config_pins
      | duniverse_pin_paths ->
        List.filter (fun (cpin : Types.Opam.pin) ->
          not (List.exists (fun dpin -> path_to_name dpin = cpin.pin) duniverse_pin_paths))
        config_pins
    in
    (to_remove, to_pull)

  let remove_files to_remove =
    (* TODO: remove repo  *)
    Fmt.pr "Removing %d files...@." (List.length to_remove);
    Stdune.Result.List.iter to_remove ~f:(fun path ->
      Fmt.pr "Removing file: %s@." path;
      Bos.OS.File.delete ~must_exist:true (Fpath.v path))

  let update_duniverse_pins fetched_sources =
    Fmt.pr "Copying pinned opam files to %a@." Fpath.pp Config.pins_dir;
    Bos.OS.Dir.create Config.pins_dir >>= fun _created ->
    Stdune.Result.List.iter fetched_sources
      ~f:(fun (dir, dep) ->
        Stdune.Result.List.iter dep.Duniverse.Deps.Source.provided_packages
          ~f:(fun pkg ->
            let opam = pkg.Duniverse.Deps.Opam.name ^ ".opam" in
            let src = Fpath.(dir / opam |> to_string) in
            let dst = Fpath.(Config.pins_dir / opam |> to_string) in
            let cmd = Bos.Cmd.(v "cp" % src % dst) in
            Bos.OS.Cmd.run cmd))

  let init ~repo pins =
    Opam_cmd.find_local_opam_files Config.pins_dir >>= fun duniverse_pin_paths ->
    let (paths_to_remove, pins_to_pull) = find_changes ~config_pins:pins ~duniverse_pin_paths in
    remove_files paths_to_remove >>= fun () ->
    let opam_entries = List.map to_opam_entry pins_to_pull in
    Fmt.pr "Fetching %d pin sources...@." (List.length opam_entries);
    compute_deps ~opam_entries >>= fun deps ->
    resolve_ref deps >>= fun deps ->
    let src_deps = deps.Duniverse.Deps.duniverse in
    pull ~repo src_deps >>= fun fetched_sources ->
    update_duniverse_pins fetched_sources >>= fun () ->
    Ok src_deps
end


let run (`Repo repo)
    (`Opam_repo opam_repo) (`Pull_mode pull_mode) () =
  let open Rresult.R.Infix in
  (match Cloner.get_cache_dir () with None -> Ok (Fpath.v ".") | Some t -> t) >>= fun cache_dir ->
  let local_opam_repo = Fpath.(cache_dir / "opam-repository.git") in
  let opam_repo_url = Uri.with_fragment opam_repo None |> Uri.to_string in
  let opam_repo_branch = match Uri.fragment opam_repo with None -> "master" | Some b -> b in
  Exec.git_clone_or_pull ~remote:opam_repo_url ~branch:opam_repo_branch ~output_dir:local_opam_repo
  >>= fun () ->
  Opam_cmd.find_local_opam_files repo >>= fun local_packages ->
  Pins.read_from_config Fpath.(repo // Config.duniverse_file) >>= fun pins ->
  build_config ~local_packages ~pins ~pull_mode ~opam_repo >>= fun config ->
  Pins.init ~repo pins >>= fun pin_deps ->
  (* TODO: make sure that all pins are used here, not just the fetched ones? *)
  if pin_deps <> [] then
    Common.Logs.app (fun l ->
        l "Added %a pins to duniverse."
        Fmt.(styled `Green int) (List.length pin_deps));
  let root_packages = config.root_packages @ List.map Pins.to_package config.pins in
  Opam_cmd.calculate_opam ~root_packages ~local_opam_repo >>= fun packages ->
  Opam_cmd.report_packages_stats packages;
  compute_depexts ~local_opam_repo packages
  >>= fun depexts ->
  Common.Logs.app (fun l ->
      l "Recording %a depext formulae for %a packages."
        Fmt.(styled `Green int)
        (List.length depexts)
        Fmt.(styled `Green int)
        (List.length packages));
  List.iter (fun (k, v) -> Logs.info (fun l -> l "depext %s %s" (String.concat "," k) v)) depexts;
  Common.Logs.app (fun l -> l "Calculating Git repositories to vendor source code.");
  compute_deps ~opam_entries:packages >>= fun unresolved_deps ->
  resolve_ref unresolved_deps >>= fun deps ->
  let deps = { deps with duniverse = deps.duniverse @ pin_deps } in
  let duniverse = { Duniverse.config; deps; depexts } in
  let file = Fpath.(repo // Config.duniverse_file) in
  Duniverse.save ~file duniverse >>= fun () ->
  Common.Logs.app (fun l ->
      l "Wrote duniverse file with %a entries to %a. You can now run %a to fetch the sources."
        Fmt.(styled `Green int)
        (Duniverse.Deps.count duniverse.deps)
        Styled_pp.path (Fpath.normalize file)
        Fmt.(styled `Blue string)
        "duniverse pull");
  Ok ()

open Cmdliner

let opam_repo =
  let doc =
    "URL or path to the Duniverse opam-repository that has overrides for packages that have not \
     yet been ported to Dune upstream."
  in
  Common.Arg.named
    (fun x -> `Opam_repo (Uri.of_string x))
    Arg.(
      value & opt string Config.duniverse_opam_repo & info [ "opam-repo" ] ~docv:"OPAM_REPO" ~doc)

let pull_mode =
  let doc =
    "How to pull the sources. If $(i,submodules), the pull command will initialise them as git \
     submodules.  If $(i,source) then the source code will directly be cloned to the source tree."
  in
  Common.Arg.named
    (fun x -> `Pull_mode x)
    Arg.(
      value
      & opt
          (enum [ ("submodule", Duniverse.Config.Submodules); ("source", Duniverse.Config.Source) ])
          Duniverse.Config.Source
      & info [ "pull-mode" ] ~docv:"PULL_MODE" ~doc)

let info =
  let exits = Term.default_exits in
  let doc =
    Fmt.strf "analyse opam files to generate an initial $(b,%a)" Fpath.pp Config.duniverse_file
  in
  let man = [] in
  Term.info "init" ~doc ~exits ~man ~envs:Common.Arg.caches

let term =
  let open Term in
  term_result
    ( const run $ Common.Arg.repo $ opam_repo $ pull_mode
    $ Common.Arg.setup_logs () )

let cmd = (term, info)
