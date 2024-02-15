# Camllib

Various abstract datatypes inspired by the standard OCaml library.

## To install the library
`opam install camllib`

## Development
- install VSCode and opam
- install Ocaml Platform extension
- make local switch `opam switch create ./`
- install minimal requirement environment: dune, LSP and format with `opam install dune ocaml-lsp-server ocamlformat user-setup`
- run `opam exec -- dune build`,  `opam install . --deps-only`, `opam exec -- dune build` again.
- documentation is available by running `opam exec -- dune build @doc`