include Makefile.config
PKGNAME = bddapron
PKGVERS = 2.3.3
VERSION_DATE = 07/02/19

#---------------------------------------
# Directories
#---------------------------------------

SRCDIR = $(shell pwd)
#
# Installation directory prefix
#
PKG-NAME = bddapron

#---------------------------------------
# CAML part
#---------------------------------------

OCAMLINC =

BDDMOD = output normalform reg env int enum cond decompose expr0 expr1 domain0 domain1
BDDMOD := $(BDDMOD:%=bdd/%)
BDDAPRONMOD = \
	apronexpr env cond apronexprDD common apronDD \
	expr0 expr1 expr2 \
	descend mtbdddomain0 bddleaf bdddomain0 domain0 \
	domainlevel1 mtbdddomain1 bdddomain1 domain1 formula \
	policy \
	syntax yacc lex parser
BDDAPRONMOD := $(BDDAPRONMOD:%=bddapron/%)

MLMODULES = $(BDDMOD) $(BDDAPRONMOD)

FILES_TOINSTALL = META $(addprefix _build/,bdd.cmi bdd.cma bdd.d.cma	\
	bddapron.cmi bddapron.cma bddapron.d.cma bdd.cmx bdd.cmxa	\
	bdd.a bddapron.cmx bddapron.cmxa bddapron.a bdd.p.cmx		\
	bdd.p.cmxa bdd.p.a bddapron.p.cmx bddapron.p.cmxa		\
	bddapron.p.a bdd.mli bddapron.mli)

ifneq ($(HAS_TYPEREX),)
FILES_TOINSTALL += bdd.cmt bddapron.cmt
endif

#---------------------------------------
# Rules
#---------------------------------------

OCAMLBUILD_JOBS = $(if $(JOBS),-j $(JOBS),)

# Global rules
all:
	ocamlbuild $(OCAMLBUILD_JOBS) -use-ocamlfind all.otarget

ifneq ($(DOCDIR),)
.PHONY: install-doc
install: install-doc
install-doc: uninstall-doc html
	mkdir -p "$(DOCDIR)/$(PKGNAME)";
	cp -r html "$(DOCDIR)/$(PKGNAME)/$(PKGVERS)"; #XXX rights?

.PHONY: uninstall-doc
uninstall: uninstall-doc
uninstall-doc: force
	rm -rf "$(DOCDIR)/$(PKGNAME)/$(PKGVERS)";
endif

install: install-findlib
install-findlib: all META
	$(OCAMLFIND) remove $(PKG-NAME)
	$(OCAMLFIND) install $(PKG-NAME) $(FILES_TOINSTALL)

uninstall: uninstall-findlib
uninstall-findlib:
	$(OCAMLFIND) remove $(PKG-NAME)

distclean: clean
	/bin/rm -f Makefile.depend TAGS

META: META.in force
	sed -e "s!@VERSION@!$(PKGVERS)!g;" $< > $@;

clean:
	-ocamlbuild -clean
	/bin/rm -f *.log *.aux *.bbl *.blg *.toc **.idx *.ilg *.ind ocamldoc*.tex ocamldoc.sty *.dvi *.pdf *.out META
	/bin/rm -fr tmp _build html index.html
	/bin/rm -f bddtop bddaprontop *.byte *.opt bdd_ocamldoc.* bddapron_ocamldoc.* bddapron.dot
	for i in . bdd bddapron; do \
		cd $(SRCDIR)/$$i; \
		/bin/rm -f *.[aoc] *.cm[tioxa] *.cmti *.cmxa *.annot; \
	done
	(cd bddapron; /bin/rm -f yacc.ml yacc.mli lex.ml)

# TEX rules
.PHONY: bddapron.dvi bddapron.pdf bdd.dvi bdd.pdf html html_bdd html_bddapron depend
.PRECIOUS: %.cma %.cmo %.cmi %.cmx
.PRECIOUS: bdd.cma bdd.cmi bdd.cmo bdd.cmx bdd/%.cmi bdd/%.cmo bdd/%.cmx
.PRECIOUS: bddapron.cma bddapron.cmi bddapron.cmo bddapron.cmx bddapron/%.cmi bddapron/%.cmo bddapron/%.cmx
.PRECIOUS: bddapron/yacc.ml bddapron/yacc.mli bddapron/lex.ml

DOCLATEX =\
-latextitle 1,part\
-latextitle 2,chapter\
-latextitle 3,section\
-latextitle 4,subsection\
-latextitle 5,subsubsection\
-latextitle 6,paragraph\
-latextitle 7,subparagraph\
-noheader -notrailer

BDDDOCOCAMLINC =\
-I $(shell $(OCAMLFIND) query camllib) \
-I $(shell $(OCAMLFIND) query cudd)
BDDAPRONDOCOCAMLINC = $(BDDDOCOCAMLINC) \
-I $(shell $(OCAMLFIND) query gmp) \
-I $(shell $(OCAMLFIND) query apron)

bddapron.pdf: bddapron.dvi
	$(DVIPDF) bddapron.dvi bddapron.pdf

bddapron.dvi: bdd.mli bddapron.mli bdd.cmi bddapron.cmi
	mkdir -p tmp
	cp bdd.mli tmp/bdd.mli
	cp bddapron.mli tmp/bddapron.mli
	-$(OCAMLDOC) $(OCAMLINC) \
-I $(shell $(OCAMLFIND) query gmp) \
-I $(shell $(OCAMLFIND) query cudd) \
-I $(shell $(OCAMLFIND) query apron) \
-I $(shell $(OCAMLFIND) query camllib) \
-t "BDDAPRON, version $(PKGVERS), $(VERSION_DATE)" \
-latextitle 1,part -latextitle 2,chapter -latextitle 3,section -latextitle 4,subsection -latextitle 5,subsubsection -latextitle 6,paragraph -latextitle 7,subparagraph \
-noheader -notrailer -latex -o bddapron_ocamldoc.tex tmp/bdd.mli tmp/bddapron.mli
	$(LATEX) bddapron
	$(MAKEINDEX) bddapron
	$(LATEX) bddapron
	$(LATEX) bddapron

html: all
	mkdir -p html
	mkdir -p tmp
	cp _build/bdd.mli tmp/bdd.mli
	cp _build/bddapron.mli tmp/bddapron.mli
	cp $(shell $(OCAMLFIND) query cudd)/cudd_ocamldoc.mli tmp/cudd.mli
	cp $(shell $(OCAMLFIND) query apron)/apron_ocamldoc.mli tmp/apron.mli
	-$(OCAMLDOC) -html -d html -colorize-code -intro bddapron.odoci \
-I _build \
-I $(shell $(OCAMLFIND) query gmp) \
-I $(shell $(OCAMLFIND) query cudd) \
-I $(shell $(OCAMLFIND) query apron) \
-I $(shell $(OCAMLFIND) query camllib) \
$(shell $(OCAMLFIND) query gmp)/*.mli \
tmp/cudd.mli tmp/apron.mli \
$(patsubst %,$(shell $(OCAMLFIND) query apron)/%, box.mli oct.mli polka.mli ppl.mli polkaGrid.mli t1p.mli) \
$(shell $(OCAMLFIND) query camllib)/*.mli \
tmp/bdd.mli tmp/bddapron.mli

dot: bdd.mli bddapron.mli bdd.cmi bddapron.cmi bddapron.odoci
	mkdir -p html
	mkdir -p tmp
	cp bdd.mli tmp/bdd.mli
	cp bddapron.mli tmp/bddapron.mli
	cp $(shell $(OCAMLFIND) query cudd)/cudd_ocamldoc.mli tmp/cudd.mli
	cp $(shell $(OCAMLFIND) query apron)/apron_ocamldoc.mli tmp/apron.mli
	-$(OCAMLDOC) -dot -dot-reduce -o bddapron.dot \
-I $(shell $(OCAMLFIND) query gmp) \
-I $(shell $(OCAMLFIND) query cudd) \
-I $(shell $(OCAMLFIND) query apron) \
-I $(shell $(OCAMLFIND) query camllib) \
$(shell $(OCAMLFIND) query gmp)/*.mli \
tmp/cudd.mli tmp/apron.mli \
$(patsubst %,$(shell $(OCAMLFIND) query apron)/%, box.mli oct.mli polka.mli ppl.mli polkaGrid.mli t1p.mli) \
$(shell $(OCAMLFIND) query camllib)/*.mli \
tmp/bdd.mli tmp/bddapron.mli

homepage: html bddapron.pdf
	hyperlatex index
	scp -r index.html html bddapron.pdf presentation-bddapron.pdf Changes salgado:/home/wwwpop-art/people/bjeannet/bjeannet-forge/bddapron
	ssh salgado chmod -R ugoa+rx /home/wwwpop-art/people/bjeannet/bjeannet-forge/bddapron

#---------------------------------------
# Test
#---------------------------------------

# example1.byte: bdd/example1.ml
# 	$(OCAMLFIND) ocamlc -g $(OCAMLFLAGS) $(OCAMLINC) -o $@ $< -package bddapron.bdd -linkpkg

# example2.byte: bddapron/example2.ml bddapron.cma
# 	$(OCAMLFIND) ocamlc -g $(OCAMLFLAGS) $(OCAMLINC) -o $@ $< -package "bddapron.bddapron apron.boxMPQ apron.polkaMPQ" -linkpkg

# test_random.byte: bddapron/test_random.ml bddapron.cma
# 	$(OCAMLFIND) ocamlc -g $(OCAMLFLAGS) $(OCAMLINC) -o $@ $< -package "bddapron.bddapron apron.boxMPQ apron.polkaMPQ" -linkpkg

# example1.opt: bdd/example1.ml bdd.cmxa
# 	$(OCAMLFIND) ocamlopt -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ $< -package bddapron.bdd -linkpkg

# example2.opt: bddapron/example2.ml bddapron.cmxa
# 	$(OCAMLFIND) ocamlopt -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ $< -package "bddapron.bddapron apron.boxMPQ apron.polkaMPQ" -linkpkg

# test2.opt: bddapron/test2.ml
# 	$(OCAMLFIND) ocamlopt -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ $< -package "bddapron.bddapron apron.boxMPQ apron.polkaMPQ" -linkpkg -predicates debug
# test2.byte: bddapron/test2.ml bdd.cma bddapron.cma
# 	$(OCAMLFIND) ocamlc -verbose -g $(OCAMLFLAGS) $(OCAMLINC) -o $@ bdd.cma bddapron.cma $< -package "apron.boxMPQ apron.polkaMPQ camllib cudd" -linkpkg -predicates debug

# test_random.opt: bddapron/test_random.ml bddapron.cmxa
# 	$(OCAMLFIND) ocamlopt -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ $< -package "bddapron.bddapron apron.boxMPQ apron.polkaMPQ" -linkpkg

#--------------------------------------------------------------
# IMPLICIT RULES AND DEPENDENCIES
#--------------------------------------------------------------

.SUFFIXES: .ml .mli .cmi .cmo .cmx .tex
.PRECIOUS: $(BDDMOD:%=%.cmi) $(BDDMOD:%=%.cmo) $(BDDMOD:%=%.cmx) $(BDDAPRONMOD:%=%.cmi) $(BDDAPRONMOD:%=%.cmo) $(BDDAPRONMOD:%=%.cmx)

#-----------------------------------
# CAML
#-----------------------------------

.PHONY: tags TAGS
tags: TAGS
TAGS: $(MLMODULES:%=%.mli) $(MLMODULES:%=%.ml)
	ocamltags $^

#-----------------------------------
# OPAM Packaging
#-----------------------------------

# see `https://github.com/nberth/opam-dist'
ifneq ($(OPAM_DIST_DIR),)

  OPAM_DIR = opam
  OPAM_FILES = descr opam

  DIST_DEPS = clean
  DIST_FILES = bdd bddapron $(wildcard *.ml *.mli *.itarget *.odoci	\
    *.odocl *.mlpack *.mlpacki *.mllib *.tex) META.in Changes README	\
    COPYING TODO Makefile Makefile.config.* configure ocamlpack _tags

  -include $(OPAM_DIST_DIR)/opam-dist.mk

endif

# ---

.PHONY: force
force:

.NOTPARALLEL:
