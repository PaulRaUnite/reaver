#!/bin/bash
set -e

#download tool
DOWNLOAD=lwp-download

cd ~/Downloads

$DOWNLOAD http://caml.inria.fr/pub/distrib/ocaml-4.02/ocaml-4.02.0.tar.gz
$DOWNLOAD http://caml.inria.fr/pub/old_caml_site/distrib/bazar-ocaml/camlidl-1.05.tar.gz
$DOWNLOAD http://download.camlcity.org/download/findlib-1.9.6.tar.gz
$DOWNLOAD https://gmplib.org/download/gmp/gmp-5.1.3.tar.bz2
$DOWNLOAD https://www.mpfr.org/mpfr-3.1.6/mpfr-3.1.6.tar.bz2
$DOWNLOAD http://www.dii.uchile.cl/~daespino/SOurce/EGlib.tar.bz2
$DOWNLOAD http://www.dii.uchile.cl/~daespino/SOurce/QSoptExact.tar.bz2
$DOWNLOAD https://yices.csl.sri.com/old/binaries/yices-1.0.40-x86_64-unknown-linux-gnu.tar.gz
git clone https://github.com/polazarus/ocamlyices
git clone https://github.com/nberth/mlgmpidl
git clone https://framagit.org/nberth/mlcuddidl/
git clone https://github.com/thizanne/camllib/
$DOWNLOAD http://nberth.space/pool/fixpoint/fixpoint-2.3.2.tar.gz
$DOWNLOAD http://nberth.space/pool/apron/apron-0.9.11.tar.gz
$DOWNLOAD http://nberth.space/pool/bddapron/bddapron-2.2.0.tar.gz
