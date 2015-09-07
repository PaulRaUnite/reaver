
OCAMLFLAGS=-annot,-g
PKGS=apron.polkaMPQ,fixpoint,camllib

.PHONY: main.native main.byte

main.native:
	ocamlbuild -classic-display -use-ocamlfind -pkgs $(PKGS) -cflags $(OCAMLFLAGS) $@
main.byte:
	ocamlbuild -classic-display -use-ocamlfind -pkgs $(PKGS) -cflags $(OCAMLFLAGS) $@

.PHONY: tags TAG
tags: TAGS
TAGS: $(MLMODULES:%=%.mli) $(MLMODULES:%=%.ml)
	ocamltags $^

-include Makefile.depend
