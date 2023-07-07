#!/bin/bash
set -e
tar xfz findlib-1.9.6.tar.gz
cd findlib-1.9.6
./configure
make
sudo make install