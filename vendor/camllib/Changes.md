## Release 1.4.0

- now use dune as build system
- newer compiler support (previous version allowed only <=4.08)

## Release 1.3.0

- Updated Hashhe, DHashhe, Sette and Mappe according to changes
  brought by release 4.00 of OCaml, minor one thing: new seeded
  hash functions not provided, in order to remain compatible with
  older OCaml runtimes.

- New Ilist.t type (sublists are now give an attribute/exponent),
  induces non compatible changes in graphs modules
