(lang dune 3.6)

(generate_opam_files true)

(license ISC)

(package
 (name reaver)
 (version 0.9.4)
 (license "GPL-3")
 (authors "Peter Shrammel")
 (maintainers "Pavlo Tokariev <pavlo.tokariev@inria.fr>")
 (synopsis "ReaVer is a tool framework for safety verification of discrete and hybrid systems")
 (source
  (github PaulRaUnite/reaver))
 (depends
  (ocaml
   (>= 4.08))
  dune
  camllib
  bddapron
  ocamlyices
  ocamlqsopt_ex
  fixpoint
  (odoc :with-doc)
  (alcotest :with-test)))
