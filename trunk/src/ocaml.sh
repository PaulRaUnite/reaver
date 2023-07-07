#!/bin/bash
set -e
tar xfz ocaml-4.02.0.tar.gz
cd ocaml-4.02.0
./configure
make world.opt
sudo make install
make clean