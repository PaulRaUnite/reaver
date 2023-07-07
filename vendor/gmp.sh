#!/bin/bash
set -e
tar xfj gmp-5.1.3.tar.bz2
cd gmp-5.1.3
./configure --enable-cxx --enable-alloca=malloc-reentrant
make
sudo make install
make clean