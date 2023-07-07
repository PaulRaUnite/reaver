#!/bin/bash
set -e

RUN ./ocaml.sh
RUN ./camlidl.sh
RUN ./findlib.sh
RUN ./gmp.sh
RUN ./mpfr.sh
RUN ./eglib.sh
RUN ./qsopt_ex.sh
RUN ./ocamlyices.sh
RUN ./mlgmpidl.sh
RUN ./mlcuddidl.sh
RUN ./camllib.sh
RUN ./fixpoint.sh
RUN ./apron.sh
RUN ./bddapron.sh