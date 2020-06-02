

## Changes

- Add `path` to `Types.Opam.package`. There're multiple places in the code where the path for opam files is inferred from the context and manually constructed. With an explicit path it should always be clear where the opam package is coming from (i.e. the opam repository, project root or the pins directory).


## How to pin a package?


```sh
$ duniverse pin coap hypercollective/ocaml-coap
$ cat dune-get
((config
  ((version 1)
   (root_packages
    ((./hyper-edge-server.opam ((name hyper-edge-server) (version (opam))))
     (./duniverse/_pins/reason.opam ((name reason) (version (opam))))
     (./duniverse/_pins/coap-server-lwt.opam
      ((name coap-server-lwt) (version (opam))))
     (./duniverse/_pins/options.opam ((name options) (version (opam))))
     (./duniverse/_pins/coap-core.opam ((name coap-core) (version (opam))))))
   (pull_mode Source)
   (ocaml_compilers
    (4.02.0 4.03.0 4.04.0 4.05.0 4.06.0 4.07.0 4.08.0 4.09.0 4.10.0 4.11.0))))

```

- If there is no `dune-get` file, it will be created.


```sh
$ rm dune-get
$ cp ../pkg/pkg.opam ./duniverse/_pins/pkg.opam
$ duniverse init
```

- Will read add _pins/*.opam to root packages
- Will update the dune-get file with pins



## Tests

```sh
$ rm dune-get
$ duniverse pin ...
Error: no dune-get file found, please run duniverse init
```

```sh
$ duniverse init
$ ls dune-get
./dune-get
$ duniverse pin coap hypercollective/ocaml-coap#0.1
$ cat dune-get
(config ... (pins (pin coap) (url hypercollective/ocaml-coap) (tag 0.1)))
$ ls ./duniverse/_pins
coap.opam
```

```
duniverse pin $package $git_url

- adds an entry to dune-get with the pinned package
- fetches the source code of the package into ./duniverse
- copies ./duniverse/$package/$package.opam to ./duniverse/_pins/$package.opam
- performs an `init` to recompute dependencies for dune-get
```



* * *



Conditions:

- dune-get is present
- dune-get contains a non-empty list of pins
- ./duniverse/_pins contains opam files
Init:
- if config.pins <> []
  - remove all duniverse/.pins files that are not in config.pins
  - remove all download duniverse/$pkg repos
  - fetch all config.pins that are not in duniverse/.pins
    - copy opam files for fetched config.pins to duniverse/.pins
- use all config.pins for dependency resolution
- all config.pins must be classified as virtual
- update deps with non-virtual pins to track exact hashes


Flow:

- `dune-get` was extended to include a new `config` field for `pins`.
- the `pins` field represents the list of opam packages that should be:
  - present in the ./duniverse/_pins folder
  - used as root packages for dependency resolution

This config `pins` field contains the list of git urls to be:
  - fetched during `init` if not already present in ./duniverse/_pins
  -



Checklist:

- [ ] Make sure to delete git refs if source mode is used;.
- [ ] If multiple pins belong to the same repository they should be aggregated.
- [ ] If an opam file for a package being pinned is found pins_dir, do not fetch the source.
- [ ] If an opam file exists in pins_dir, but no code is present in duniverse, ensure that pull fetches it.
- [ ] Implement the duniverse pin command that updates the dune-get file.
- [ ] If a dune-get file has a pin after running init, it should also have a
      dep entry that points to a resolved hash. This ways pins are reproducible
      in the presence of dune-get.
- [ ] Init/solve must recompute the hashes of the pins.



