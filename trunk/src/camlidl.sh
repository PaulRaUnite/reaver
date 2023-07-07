#!/bin/bash
set -e
tar xfz camlidl-1.05.tar.gz
cd camlidl-1.05
cp config/Makefile.unix config/Makefile
make
sudo make install