#!/bin/bash
set -e
tar xfj mpfr-3.1.6.tar.bz2
cd mpfr-3.1.6
./configure --with-gmp=/usr/local
make
sudo make install
make clean