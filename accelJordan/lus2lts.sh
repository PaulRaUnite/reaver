#!/bin/sh
if test $# -ne 2
then
   echo "usage: lus2lts <lustre file> <main node>"
   exit 1
fi;
FILE=$1
MAIN=$2
lus2nbac $FILE $MAIN
if test $? -ne 0
then
    echo "error in lus2nnac";
    exit $?
fi
../reaver/reaver.opt -inputformat nbac $MAIN.ba -s "pIF;pE;rT;rB;rS" -jordan $MAIN.lts
../proto/jordan.pl $MAIN.lts

