#!/bin/bash
set -e
tar xfz bddapron-2.2.0.tar.gz
cd bddapron-2.2.0
cp Makefile.config.model Makefile.config
make
sudo make install