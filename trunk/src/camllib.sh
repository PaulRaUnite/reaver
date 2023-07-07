#!/bin/bash
set -e
cd camllib
cp Makefile.config.model Makefile.config
make
sudo make install