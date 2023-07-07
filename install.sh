#!/bin/bash
set -e
#################################################################################
# ReaVer - installation script
#################################################################################

# prerequisites:
# - install g++, M4, autoconf, wget, git

cd ./vendor
./dowload.sh
./build.sh
cd ..
cd ./trunk/src/
./install.sh
