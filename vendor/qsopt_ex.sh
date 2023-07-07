#!/bin/bash
set -e
tar xfj QSoptExact.tar.bz2
cd QSopt_ex-2.5.10
./configure --with-gmp-lib-dir=/usr/local/lib --with-gmp-include-dir=/usr/local/include --with-eglib-lib-dir=/usr/local/lib --with-eglib-include-dir=/usr/local/include CFLAGS=-fPIC
make
sudo cp lib/* /usr/local/lib/
sudo cp include/* /usr/local/include/
rm -Rf obj/* bin/*