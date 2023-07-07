#!/bin/bash
set -e
cd ocamlyices
git checkout 306ebcfb073b8a841728d796d5dbbe90fe4b77a1
./install-yices.sh ../yices-1.0.40-x86_64-unknown-linux-gnu.tar.gz
./configure
make
sudo make install