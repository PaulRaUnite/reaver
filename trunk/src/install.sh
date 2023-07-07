#!/bin/bash
set -e
cd ../lib/ocamlqsopt_ex
cp Makefile.config.model Makefile.config
sudo make install
cd ../../src

cp Makefile.config.model Makefile.config
#$EDIT Makefile.config
make opt