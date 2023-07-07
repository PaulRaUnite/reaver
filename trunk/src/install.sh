#!/bin/bash
set -e
#################################################################################
# ReaVer - installation script
#################################################################################

# prerequisites:
# - install g++, M4, autoconf, libwww-perl

#location for downloaded files
TMP=~/Downloads

#editor for configurations
EDIT=emacs


# set to zero if you have already installed appropriate versions for:
INSTALL_OCAML=1
INSTALL_CAMLIDL=1
INSTALL_FINDLIB=1
INSTALL_GMP=1
INSTALL_MPFR=1
INSTALL_MLGMPIDL=1
INSTALL_CAMLLIB=1
INSTALL_FIXPOINT=1
INSTALL_APRON=1
INSTALL_MLCUDDIDL=1
INSTALL_BDDAPRON=1

# for max-strategy iteration support
# - download Yices from http://yices.csl.sri.com/cgi-bin/yices-newlicense.cgi?file=yices-1.0.40-x86_64-unknown-linux-gnu.tar.gz to $TMP
INSTALL_EGLIB=1
INSTALL_QSOPT_EX=1
INSTALL_OCAMLYICES=1
INSTALL_OCAMLQSOPT_EX=1

#################################################################################

pushd .
cd $TMP

#Ocaml
if [ $INSTALL_OCAML -ne 0 ]; then
tar xfz ocaml-4.02.0.tar.gz
cd ocaml-4.02.0
./configure
make world.opt
sudo make install
make clean
cd $TMP
fi

if [ $INSTALL_CAMLIDL -ne 0 ]; then
#camlidl
tar xfz camlidl-1.05.tar.gz
cd camlidl-1.05
cp config/Makefile.unix config/Makefile
make
sudo make install
cd $TMP
fi

#findlib
if [ $INSTALL_FINDLIB -ne 0 ]; then
tar xfz findlib-1.9.6.tar.gz
cd findlib-1.9.6
./configure
make
sudo make install
cd $TMP
fi

#GMP
if [ $INSTALL_GMP -ne 0 ]; then
tar xfj gmp-5.1.3.tar.bz2
cd gmp-5.1.3
./configure --enable-cxx --enable-alloca=malloc-reentrant
make
sudo make install
make clean
cd $TMP
fi

#MPFR
if [ $INSTALL_MPFR -ne 0 ]; then
tar xfj mpfr-3.1.6.tar.bz2
cd mpfr-3.1.6
./configure --with-gmp=/usr/local
make
sudo make install
make clean
cd $TMP
fi

#EGlib
if [ $INSTALL_EGLIB -ne 0 ]; then
tar xfj EGlib.tar.bz2
cd EGlib-2.6.21
./configure --with-gmp-lib-dir=/usr/local/lib --with-gmp-include-dir=/usr/local/include --enable-gmp-memslab=no CFLAGS=-fPIC
make
sudo cp lib/* /usr/local/lib/
sudo cp include/* /usr/local/include/
rm -Rf obj/* bin/*
cd $TMP
fi

#QSopt_ex
if [ $INSTALL_QSOPT_EX -ne 0 ]; then
tar xfj QSoptExact.tar.bz2
cd QSopt_ex-2.5.10
./configure --with-gmp-lib-dir=/usr/local/lib --with-gmp-include-dir=/usr/local/include --with-eglib-lib-dir=/usr/local/lib --with-eglib-include-dir=/usr/local/include CFLAGS=-fPIC
make
sudo cp lib/* /usr/local/lib/
sudo cp include/* /usr/local/include/
rm -Rf obj/* bin/*
cd $TMP
fi

#Ocamlyices
if [ $INSTALL_OCAMLYICES -ne 0 ]; then
cd ocamlyices
git checkout 306ebcfb073b8a841728d796d5dbbe90fe4b77a1
./install-yices.sh ../yices-1.0.40-x86_64-unknown-linux-gnu.tar.gz
./configure
make
sudo make install
cd $TMP
fi

#mlgmpidl
if [ $INSTALL_MLGMPIDL -ne 0 ]; then
cd mlgmpidl
git checkout e9fae5fd9d1895d66732a2abdcd4113e5adce940
cp Makefile.config.model Makefile.config
#$EDIT Makefile.config
#./configure --gmp-prefix /usr/local --mpfr-prefix /usr/local
sed -i '25s#.*#MLGMPIDL_PREFIX = /usr/local#' Makefile.config
sed -i '29s#.*#GMP_PREFIX = /usr/local#' Makefile.config
sed -i '33s#.*#MPFR_PREFIX = /usr/local#' Makefile.config
make
sudo make install
cd $TMP
fi

#mlcuddidl
if [ $INSTALL_MLCUDDIDL -ne 0 ]; then
cd mlcuddidl
git checkout release-2.2.0
cp Makefile.config.model Makefile.config
sed -i '60s/.*//' Makefile.config
sed -i '62s/.*/XCFLAGS = -fPIC -m64 -DHAVE_IEEE_754 -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8 -DSIZEOF_INT=4/' Makefile.config
sed -i.bak -e '69d;78d;86d' Makefile.config
#$EDIT Makefile.config
make
sudo make install
cd $TMP
fi

#camllib
if [ $INSTALL_CAMLLIB -ne 0 ]; then
cd camllib
cp Makefile.config.model Makefile.config
make
sudo make install
cd $TMP
fi

#fixpoint
#-r983
if [ $INSTALL_FIXPOINT -ne 0 ]; then
tar xfz fixpoint-2.3.2.tar.gz
cd fixpoint-2.3.2
cp Makefile.config.model Makefile.config
make
sudo make install
cd $TMP
fi

#apron
#-r1013
if [ $INSTALL_APRON -ne 0 ]; then
tar xfz apron-0.9.11.tar.gz
cd apron-0.9.11
sed -i.bak -e '26,35d' ./apron/ap_config.h
cp Makefile.config.model Makefile.config
#$EDIT Makefile.config
./configure
sed -i '22s#.*#MLGMPIDL_PREFIX = /usr/local#' Makefile.config
sed -i '25s#.*#GMP_PREFIX = /usr/local#' Makefile.config
sed -i '26s#.*#MPFR_PREFIX = /usr/local#' Makefile.config
make
sudo make install
cd $TMP
fi

#bddapron
#-r923
if [ $INSTALL_BDDAPRON -ne 0 ]; then
tar xfz bddapron-2.3.1.tar.gz
cd bddapron-2.3.1
cp Makefile.config.model Makefile.config
make 
sudo make install
cd $TMP
fi


popd

#ocamlqsopt_ex
if [ $INSTALL_OCAMLQSOPT_EX -ne 0 ]; then
cd ../lib/ocamlqsopt_ex 
cp Makefile.config.model Makefile.config
sudo make install
cd ../../src
fi

#reaver
cp Makefile.config.model Makefile.config
$EDIT Makefile.config
make opt
