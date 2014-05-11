include Makefile.config

################# installation #################################################
SRCDIR = $(shell pwd)
SITE-LIB = $(shell $(OCAMLFIND) printconf destdir)
PKG-NAME = reaver
SITE-LIB-PKG = $(SITE-LIB)/$(PKG-NAME)
PROG-NAME = reaver

####################### framework modules ######################################
# ---------- util --------------------------------------------------------------
UTILDIR = util
UTILMOD = log util timeMeas

APRONDIR = apron
APRONMOD = apronUtil apronAccel apronHybrid 

LPDIR = lp
LPMOD = 
ifeq ($(MAXSTRAT),1)
  LPMOD += apronLp
endif

BDDAPRONDIR = bddapron
BDDAPRONMOD = mtbddUtil bddapronUtil bddapronAnalysis bddapronAccel bddapronHybrid 

SMTDIR = smt
SMTMOD = 
ifeq ($(MAXSTRAT),1)
  SMTMOD +=  yicesSmt bddapronSmt
endif

# ---------- base  -------------------------------------------------------------
BASEDIR = base
BASE1MOD = env arc loc cfg domain 
BASE2MOD = program analysis verifUtil

####################### instantiation modules ##################################
# ---------- impl ----------------------------------------------------------
DOMAINDIR = domain
DOMAINMOD = domainStd finDisPart template

DFTOCFDIR = dftocf
DFTOCFMOD = df2cfDiscrete zeroTrans df2cfHybrid

ANALYSISDIR = analysis
ANALYSISMOD = analysisStd fixpointAcc accel
ifeq ($(MAXSTRAT),1)
  ANALYSISMOD +=  stratiter2 stratiter3
endif
#stratiter 

TRANSDIR = trans
TRANSMOD = partitionUtil transGen transDiscrete transHybrid relAbstr transAcc

# ---------- frontends ---------------------------------------------------------
NBACDIR = nbac
NBACMOD = nbacExpr nbacSyntaxCheck nbacParser nbacLexer nbacTrans

FRONTENDDIR = frontend
FRONTENDDIRS = $(NBACDIR) 
FRONTENDMOD = $(NBACMOD:%=$(NBACDIR)/%) 

# ---------- mainutil ----------------------------------------------------------
MAINUTILDIR = mainutil
MAINUTILMOD = parserParam lexerParam parseParams

# ---------- main --------------------------------------------------------------
MAINDIR = main
MAINMOD = df2cf verif option main

# ======= select ===============================================================
ifeq ($(MAXSTRAT),0)
  PREPARE = cp $(MAINDIR)/verif.no-maxstrat.ml $(MAINDIR)/verif.ml 
else
  PREPARE = cp $(MAINDIR)/verif.full.ml $(MAINDIR)/verif.ml
endif


################# required packages ############################################
ifeq ($(MAXSTRAT),1)
  REQ_PKG = "num unix str bigarray camllib cudd gmp apron.apron apron.polkaMPQ apron.octMPQ apron.boxMPQ bddapron.bddapron bddapron.bdd fixpoint ocamlyices ocamlqsopt_ex"
else
  REQ_PKG = "num unix str bigarray camllib cudd gmp apron.apron apron.polkaMPQ apron.octMPQ apron.boxMPQ bddapron.bddapron bddapron.bdd fixpoint"
endif

# zelus

LIBS =
ifeq ($(MAXSTRAT),1)
  LIBS +=  ocamlyices yices
endif
#ocamlyices yices QSopt_ex EGlib ocamlqsopt_ex


LIB-DIR-OPT = $(LIB-DIR:%=-ccopt -L%)
LIBS-OPT = $(LIBS:%=-cclib -l%)

################# all modules ##################################################
MLMOD = $(UTILMOD:%=$(UTILDIR)/%) $(APRONMOD:%=$(APRONDIR)/%) \
 $(LPMOD:%=$(LPDIR)/%) $(BDDAPRONMOD:%=$(BDDAPRONDIR)/%)  \
 $(SMTMOD:%=$(SMTDIR)/%) $(BASE1MOD:%=$(BASEDIR)/%) \
 $(DOMAINMOD:%=$(DOMAINDIR)/%)  $(BASE2MOD:%=$(BASEDIR)/%) \
 $(DFTOCFMOD:%=$(DFTOCFDIR)/%) \
 $(ANALYSISMOD:%=$(ANALYSISDIR)/%) $(TRANSMOD:%=$(TRANSDIR)/%) \
 $(FRONTENDMOD:%=$(FRONTENDDIR)/%) $(MAINUTILMOD:%=$(MAINUTILDIR)/%) \
 $(MAINMOD:%=$(MAINDIR)/%)

DIRS = $(UTILDIR) $(APRONDIR) $(BDDAPRONDIR) $(LPDIR) $(SMTDIR) \
 $(BASEDIR) $(DOMAINDIR) \
 $(DFTOCFDIR) $(ANALYSISDIR) $(TRANSDIR) $(FRONTENDDIR) \
 $(FRONTENDDIRS:%=$(FRONTENDDIR)/%) $(MAINUTILDIR) \
 $(MAINDIR)
INCLUDES = $(DIRS:%=-I %)

ADD-DOC-INCLUDES = $(ADD-DOC-DIRS:%=-I %)

################# executable ###################################################
byte: $(PROG-NAME).byte
opt: $(PROG-NAME).opt

$(PROG-NAME).byte: $(MLMOD:%=%.cmo)
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) $(LIB-DIR-OPT) $(LIBS-OPT) \
          -verbose -linkpkg -package $(REQ_PKG) -cclib -lapron_debug $^ -o $@  

$(PROG-NAME).opt: $(MLMOD:%=%.cmx)
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(LIB-DIR-OPT) $(LIBS-OPT) \
           -linkpkg -package $(REQ_PKG) $^ -o $@

clean:
	/bin/rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot *.html *.ps *.pdf *.dvi *.opt *.byte
	for dir in $(DIRS) ; do \
	  cd $$dir; /bin/rm -f *.cm[ioxa] *.o *.annot; cd $(SRCDIR) ; \
        done
	rm -f $(MAINDIR)/verif.ml

################# documentation ################################################
doc: $(MLMOD:%=%.mli)
	mkdir -p doc
	ocamldoc -html -d doc \
          $(INCLUDES) $(ADD-DOC-INCLUDES) \
          $(MLMOD:%=%.mli)

cleandoc:
	rm doc -Rf

################# basic rules ##################################################
.SUFFIXES: .ml .mli .cmi .cmo .cmx
.PRECIOUS: %.cma %.cmo %.cmi %.cmx 

%.cmi: %.mli 
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) -package $(REQ_PKG) $(INCLUDES) -c $< -o $@

$(MAINDIR)/verif.ml:  
	$(PREPARE)

%.cmo: %.ml %.cmi
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) -package $(REQ_PKG) $(INCLUDES) -c $< -o $@

%.cmx: %.ml %.cmi
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) -package $(REQ_PKG) $(INCLUDES) -c $< -o $@

################# additional rules #############################################

%.ml: %.mll
	$(OCAMLLEX) $^ -o $@

%.ml: %.mly
	$(OCAMLYACC) $^ #-b $(@:%.ml=%)

depend: 
	$(OCAMLFIND) ocamldep -package $(REQ_PKG) $(MLMOD:%=%.mli) $(MLMOD:%=%.ml) >Makefile.depend

-include Makefile.depend
