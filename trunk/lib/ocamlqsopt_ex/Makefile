include Makefile.config

##################################################

#---------------------------------------
# Directories
#---------------------------------------

SRCDIR = $(shell pwd)
#
# Installation directory
#
SITE-LIB = $(shell $(OCAMLFIND) printconf destdir)
PKG-NAME = ocamlqsopt_ex
SITE-LIB-PKG = $(SITE-LIB)/$(PKG-NAME)

#---------------------------------------
# C part
#---------------------------------------

ICFLAGS = -I$(INC-DIR)/include \
-I$(CAML_PREFIX)/lib/ocaml -I$(CAMLIDL_PREFIX)/lib/ocaml

#---------------------------------------
# OCaml part
#---------------------------------------

OCAMLCCOPT = \
-ccopt -L$(SITE-LIB)/stublibs \
-ccopt -L$(SITE-LIB-PKG) \
-ccopt -L$(CAMLIDL_PREFIX)/lib/ocaml \
-ccopt -L$(CAML_PREFIX)/lib/ocaml

REQ_PKG = "num gmp"

#---------------------------------------
# Files
#---------------------------------------

IDLMODULES = 

MLMODULES = numutil tools qsopt_ex lp

CCMODULES = qsopt_ex_if ocamlqsopt_ex

CCLIB = libocamlqsopt_ex.a

FILES_TOINSTALL = META \
	ocamlqsopt_ex.cmi ocamlqsopt_ex.cma \
	ocamlqsopt_ex.cmxa ocamlqsopt_ex.a \
	$(CCLIB) dllocamlqsopt_ex.so

#ocamlqsopt_ex.cmx ocamlqsopt_ex.cmo
# 

#---------------------------------------
# Rules
#---------------------------------------

# Global rules
all: $(FILES_TOINSTALL)

META: Makefile
	/bin/rm -f META
	 echo "description = \"OCaml Interface QSopt_ex library\"" > META
	 echo "version = \"1.0\"" >> META
	 echo "archive(byte) = \"ocamlqsopt_ex.cma\"" >> META
	 echo "archive(native) = \"ocamlqsopt_ex.cmxa\"" >> META
	 echo "archive(native,gprof) = \"ocamlqsopt_ex.p.cmxa\"" >> META

install: $(FILES_TOINSTALL)
	$(OCAMLFIND) remove $(PKG-NAME)
	$(OCAMLFIND) install $(PKG-NAME) $^

uninstall:
	$(OCAMLFIND) remove $(PKG-NAME)

clean:
	/bin/rm -f *.[ao] *.so *.cm[ioxa] *.cmxa *.opt *.annot 

# CAML rules

ocamlqsopt_ex.cma: qsopt_ex.cmo $(CCLIB)
	$(OCAMLFIND) ocamlc -verbose -a	-o $@ $< \
	-dllib -locamlqsopt_ex \
	-cclib -locamlqsopt_ex -cclib -lcamlidl $(OCAMLCCOPT)

ocamlqsopt_ex.cmxa ocamlqsopt_ex.a: ocamlqsopt_ex.cmx 
	$(OCAMLFIND) ocamlopt -verbose -a -o $@ $< \
	-cclib -locamlqsopt_ex -cclib -lcamlidl $(OCAMLCCOPT)

ocamlqsopt_ex.cmo ocamlqsopt_ex.cmi: $(MLMODULES:%=%.cmo)
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -pack -o $@ $^
ocamlqsopt_ex.cmx ocamlqsopt_ex.o: $(MLMODULES:%=%.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -pack -o $@ $^


# C rules
libocamlqsopt_ex.a: $(CCMODULES:%=%.o) $(LIB-DIR)/libQSopt_ex.a $(LIB-DIR)/libEGlib.a ocamlqsopt_ex.a
	mkdir -p tmp
	cd tmp; cp $(LIB-DIR)/libQSopt_ex.a $(LIB-DIR)/libEGlib.a $(SITE-LIB)/gmp/gmp.a $(CAML_PREFIX)/lib/ocaml/nums.a .; /bin/rm -fr *.o; $(AR) x libQSopt_ex.a; $(AR) x libEGlib.a; $(AR) x gmp.a; $(AR) x nums.a
	rm -f $@
	$(AR) r $@ $(CCMODULES:%=%.o) tmp/*.o
	$(RANLIB) $@
	/bin/rm -f tmp/*.o

dllocamlqsopt_ex.so: libocamlqsopt_ex.a 
	mkdir -p tmp
	(cd tmp; /bin/rm -f *.o; $(AR) x ../libocamlqsopt_ex.a)
	$(CC) $(CFLAGS) $(XCFLAGS) -shared -o $@ tmp/*.o 
	/bin/rm -fR tmp

#-----------------------------------
# test
#-----------------------------------

#test: test.cmo
#	ocamlfind ocamlc -verbose -linkpkg -package "num gmp ocamlqsopt_ex"  $^ -o $@

test.opt: test.cmx
	ocamlfind ocamlopt -verbose -linkpkg \
	-package "num gmp ocamlqsopt_ex" $^ -o $@

#--------------------------------------------------------------
# IMPLICIT RULES AND DEPENDENCIES
#--------------------------------------------------------------

.SUFFIXES: .c .h .o .ml .mli .cmi .cmo .cmx .idl .d.o _caml.c

#-----------------------------------
# C
#-----------------------------------

%.o: %.c 
	$(CC) $(CFLAGS) $(ICFLAGS) $(XCFLAGS) -c -o $@ $<

#-----------------------------------
# CAML
#-----------------------------------

%.cmi: %.mli
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) $(OCAMLINC) -package $(REQ_PKG) -c $<

%.cmo: %.ml %.cmi
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) $(OCAMLINC) -package $(REQ_PKG) -c $<

$(MLMODULES:%=%.cmx): %.cmx: %.ml %.cmi
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(REQ_PKG) -for-pack Ocamlqsopt_ex -c $<

test.cmx: %.cmx: %.ml %.cmi
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(REQ_PKG) -c $<

#-----------------------------------
# Dependencies
#-----------------------------------

depend: $(IDLMODULES:%=%.ml) $(IDLMODULES:%=%.mli)
	$(OCAMLDEP) $(MLMODULES:%=%.mli) $(MLMODULES:%=%.ml) >Makefile.depend

Makefile.depend: $(IDLMODULES:%=%.ml) $(IDLMODULES:%=%.mli)
	$(OCAMLDEP) $(MLMODULES:%=%.mli) $(MLMODULES:%=%.ml) >Makefile.depend

-include Makefile.depend
