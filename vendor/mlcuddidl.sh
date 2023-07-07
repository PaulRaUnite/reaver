#!/bin/bash
set -e
cd mlcuddidl
git checkout release-2.2.0
cp Makefile.config.model Makefile.config
sed -i '60s/.*//' Makefile.config
sed -i '62s/.*/XCFLAGS = -fPIC -m64 -DHAVE_IEEE_754 -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8 -DSIZEOF_INT=4/' Makefile.config
sed -i.bak -e '69d;78d;86d' Makefile.config
#$EDIT Makefile.config
make
sudo make install