#!/bin/bash
set -e

tar xfj EGlib.tar.bz2
cd EGlib-2.6.21
./configure --with-gmp-lib-dir=/usr/local/lib --with-gmp-include-dir=/usr/local/include --enable-gmp-memslab=no CFLAGS=-fPIC
make
cd ..

tar xfj QSoptExact.tar.bz2
cd QSopt_ex-2.5.10
./configure --with-gmp-lib-dir=/usr/local/lib --with-gmp-include-dir=/usr/local/include --with-EGlib-lib-dir="../EGlib-2.6.21/lib" --with-EGlib-include-dir="../EGlib-2.6.21/include" CFLAGS=-fPIC
make
cd ..


CFLAGS="\
-Wcast-qual -Wswitch -Werror-implicit-function-declaration \
-Wall -Wextra -Wundef -Wbad-function-cast -Wcast-align -Wstrict-prototypes \
-Wno-unused \
-fPIC -O3 -DNDEBUG"
XCFLAGS="-O2 -fPIC -m64 -ansi -DBSD -DHAVE_IEEE_754 -DSIZEOF_VOID_P=8 -DSIZEOF_LONG=8"
mkdir -p tmp
(cd tmp; rm -f *.o; ar x ../EGlib-2.6.21/lib/EGlib.a)
cc $CFLAGS $XCFLAGS -shared -o ./EGlib-2.6.21/lib/dllEGlib.so tmp/*.o 
rm -fR tmp
mkdir -p tmp
(cd tmp; rm -f *.o; ar x ../QSopt_ex-2.5.10/lib/QSopt_ex.a)
cc $CFLAGS $XCFLAGS -shared -o ./QSopt_ex-2.5.10/lib/dllQSopt_ex.so tmp/*.o 
rm -fR tmp

cp ./QSopt_ex-2.5.10/lib/QSopt_ex.a ./libQSoptEx.a
cp ./QSopt_ex-2.5.10/lib/dllQSopt_ex.so ./dllQSoptEx.so
cp ./EGlib-2.6.21/lib/EGlib.a ./libEGlib.a
cp ./EGlib-2.6.21/lib/dllEGlib.so ./dllEGlib.so
cp ./EGlib-2.6.21/include/EGlib.h ./
cp ./QSopt_ex-2.5.10/include/QSopt_ex.h ./