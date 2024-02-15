#!/bin/bash
set -e -x

dune exec reaver -s "$1" -inputformat nbac -dot "$2.dot" $2