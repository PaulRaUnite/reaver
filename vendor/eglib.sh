#!/bin/bash
set -e
tar xfj EGlib.tar.bz2
cd EGlib-2.6.21
./configure --with-gmp-lib-dir=/usr/local/lib --with-gmp-include-dir=/usr/local/include --enable-gmp-memslab=no CFLAGS=-fPIC
make
sudo cp lib/* /usr/local/lib/
sudo cp include/* /usr/local/include/
rm -Rf obj/* bin/*