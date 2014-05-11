#!/bin/bash

#################################################################################
# ReaVer - installation script
#################################################################################

# prerequisites:
# - install g++, M4, autoconf, libwww-perl

#location for downloaded files
TMP=~/Downloads

# set to zero if you have already installed appropriate versions for:
INSTALL_OCAML=0
INSTALL_CAMLIDL=0
INSTALL_FINDLIB=0
INSTALL_GMP=0
INSTALL_MPFR=0
INSTALL_MLGMPIDL=0
INSTALL_CAMLLIB=0
INSTALL_FIXPOINT=0
INSTALL_APRON=0
INSTALL_MLCUDDIDL=0
INSTALL_BDDAPRON=1

# for max-strategy iteration support
# - download Yices from http://yices.csl.sri.com/cgi-bin/yices-newlicense.cgi?file=yices-1.0.40-x86_64-unknown-linux-gnu.tar.gz to $TMP
INSTALL_EGLIB=0
INSTALL_QSOPT_EX=0
INSTALL_OCAMLYICES=0

#################################################################################

pushd .
cd $TMP

#Ocaml
if [ $INSTALL_OCAML -ne 0 ]; then
lwp-download http://caml.inria.fr/pub/distrib/ocaml-4.01/ocaml-4.01.0.tar.gz
tar xfz ocaml-4.01.0.tar.gz
cd ocaml-4.01.0
./configure
make world.opt
sudo make install
make clean
cd ..
fi

if [ $INSTALL_CAMLIDL -ne 0 ]; then
#camlidl
lwp-download http://caml.inria.fr/pub/old_caml_site/distrib/bazar-ocaml/camlidl-1.05.tar.gz
tar xfz camlidl-1.05.tar.gz
cd camlidl-1.05
cp config/Makefile.unix config/Makefile
make
sudo make install
cd ..
fi

#findlib
if [ $INSTALL_FINDLIB -ne 0 ]; then
lwp-download http://download.camlcity.org/download/findlib-1.4.1.tar.gz
tar xfz findlib-1.4.1.tar.gz
cd findlib-1.4.1
./configure
make
sudo make install
cd ..
fi

#GMP
if [ $INSTALL_GMP -ne 0 ]; then
lwp-download https://gmplib.org/download/gmp/gmp-5.1.3.tar.bz2
tar xfj gmp-5.1.3.tar.bz2
cd gmp-5.1.3
./configure --enable-cxx --enable-alloca=malloc-reentrant
make
sudo make install
make clean
cd ..
fi

#MPFR
if [ $INSTALL_MPFR -ne 0 ]; then
lwp-download http://www.mpfr.org/mpfr-current/mpfr-3.1.2.tar.bz2
tar xfj mpfr-3.1.2.tar.bz2
cd mpfr-3.1.2
./configure --with-gmp=/usr/local
make
sudo make install
make clean
cd ..
fi

#EGlib
if [ $INSTALL_EGLIB -ne 0 ]; then
lwp-download http://www.dii.uchile.cl/~daespino/SOurce/EGlib.tar.bz2
tar xfj EGlib.tar.bz2
cd EGlib-2.6.21
./configure --with-gmp-lib-dir=/usr/local/lib --with-gmp-include-dir=/usr/local/include --enable-gmp-memslab=no CFLAGS=-fPIC
make
sudo cp lib/* /usr/local/lib/
sudo cp include/* /usr/local/include/
rm -Rf obj/* bin/*
cd ..
fi

#QSopt_ex
if [ $INSTALL_QSOPT_EX -ne 0 ]; then
lwp-download http://www.dii.uchile.cl/~daespino/SOurce/QSoptExact.tar.bz2
tar xfj QSoptExact.tar.bz2
cd QSopt_ex-2.5.10
./configure --with-gmp-lib-dir=/usr/local/lib --with-gmp-include-dir=/usr/local/include --with-eglib-lib-dir=/usr/local/lib --with-eglib-include-dir=/usr/local/include CFLAGS=-fPIC
make
sudo cp lib/* /usr/local/lib/
sudo cp include/* /usr/local/include/
rm -Rf obj/* bin/*
cd ..
fi

#Ocamlyices
if [ $INSTALL_OCAMLYICES -ne 0 ]; then
git clone https://github.com/polazarus/ocamlyices.git
cd ocamlyices
./install-yices.sh ../yices-1.0.40-x86_64-unknown-linux-gnu.tar.gz
./configure
make
sudo make install
cd ..
fi

#mlgmpidl
#-r273
if [ $INSTALL_MLGMPIDL -ne 0 ]; then
svn checkout svn://scm.gforge.inria.fr/svnroot/mlxxxidl/mlgmpidl/trunk mlgmpidl
cd mlgmpidl
cp Makefile.config.model Makefile.config
emacs Makefile.config
make
sudo make install
cd ..
fi

#mlcuddidl
#-r273
if [ $INSTALL_MLCUDDIDL -ne 0 ]; then
svn checkout svn://scm.gforge.inria.fr/svnroot/mlxxxidl/mlcuddidl/trunk mlcuddidl
cd mlcuddidl
cp Makefile.config.model Makefile.config
emacs Makefile.config
make
sudo make install
cd ..
fi

#camllib
#-r983
if [ $INSTALL_CAMLLIB -ne 0 ]; then
svn checkout svn://scm.gforge.inria.fr/svnroot/bjeannet/pkg/camllib/trunk camllib
cd camllib
cp Makefile.config.model Makefile.config
make
sudo make install
cd ..
fi

#fixpoint
#-r983
if [ $INSTALL_FIXPOINT -ne 0 ]; then
svn checkout svn://scm.gforge.inria.fr/svnroot/bjeannet/pkg/fixpoint/trunk fixpoint
cd fixpoint
cp Makefile.config.model Makefile.config
make
sudo make install
cd ..
fi

#apron
#-r1013
if [ $INSTALL_APRON -ne 0 ]; then
svn co svn://scm.gforge.inria.fr/svnroot/apron/apron/trunk apron
cd apron
cp Makefile.config.model Makefile.config
emacs Makefile.config
make rebuild
make
sudo make install
cd ..
fi

#bddapron
#-r923
if [ $INSTALL_BDDAPRON -ne 0 ]; then
svn checkout svn://scm.gforge.inria.fr/svn/bjeannet/pkg/bddapron/trunk bddapron
cd bddapron
cp Makefile.config.model Makefile.config
make 
sudo make install
cd ..
fi

#policy/impl
#cd policy_impl
#cp Makefile.inc.model Makefile.inc
#emacs Makefile.inc
#cd smt_util2
#make
#sudo make install
#cd ../maxstratutil
#make
#sudo make install
#cd ../ocamlqsopt_ex
#make
#sudo make install

#reaver
popd
cp Makefile.config.model Makefile.config
#emacs Makefile.config
make
