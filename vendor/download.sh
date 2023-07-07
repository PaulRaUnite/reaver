#!/bin/bash
set -e

wget -nc http://caml.inria.fr/pub/distrib/ocaml-4.02/ocaml-4.02.0.tar.gz
wget -nc http://caml.inria.fr/pub/old_caml_site/distrib/bazar-ocaml/camlidl-1.05.tar.gz
wget -nc http://download.camlcity.org/download/findlib-1.9.6.tar.gz
wget -nc https://gmplib.org/download/gmp/gmp-5.1.3.tar.bz2
wget -nc https://www.mpfr.org/mpfr-3.1.6/mpfr-3.1.6.tar.bz2
wget -nc http://www.dii.uchile.cl/~daespino/SOurce/EGlib.tar.bz2
wget -nc http://www.dii.uchile.cl/~daespino/SOurce/QSoptExact.tar.bz2
wget -nc https://yices.csl.sri.com/old/binaries/yices-1.0.40-x86_64-unknown-linux-gnu.tar.gz
[ ! -d 'ocamlyices' ] && git clone https://github.com/polazarus/ocamlyices
[ ! -d 'mlgmpidl' ] && git clone https://github.com/nberth/mlgmpidl
[ ! -d 'mlcuddidl' ] && git clone https://framagit.org/nberth/mlcuddidl/
[ ! -d 'camllib' ] && git clone https://github.com/thizanne/camllib/
wget -nc http://nberth.space/pool/fixpoint/fixpoint-2.3.2.tar.gz
wget -nc http://nberth.space/pool/apron/apron-0.9.11.tar.gz
wget -nc http://nberth.space/pool/bddapron/bddapron-2.2.1.tar.gz
