#!/bin/bash
set -e
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