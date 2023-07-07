#!/bin/bash
set -e
tar xfz fixpoint-2.3.2.tar.gz
cd fixpoint-2.3.2
cp Makefile.config.model Makefile.config
make
sudo make install