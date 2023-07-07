#!/bin/bash
set -e
tar xfz apron-0.9.11.tar.gz
cd apron-0.9.11
sed -i.bak -e '26,35d' ./apron/ap_config.h
cp Makefile.config.model Makefile.config
#$EDIT Makefile.config
./configure
sed -i '22s#.*#MLGMPIDL_PREFIX = /usr/local#' Makefile.config
sed -i '25s#.*#GMP_PREFIX = /usr/local#' Makefile.config
sed -i '26s#.*#MPFR_PREFIX = /usr/local#' Makefile.config
sed -i '29s#.*#CAMLIDL_DIR = /usr/local/#' Makefile.config
make rebuild
make
sudo make install