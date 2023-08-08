#!/bin/bash
set -e

args=$(getopt -l "strategy:" -o "s:h" -- "$@")
eval set -- "$args"
strategy="aB;aB:b;pIF;pMD;rT;aS"
while [ $# -ge 1 ]; do
        case "$1" in
                --)
                    # No more options left.
                    shift
                    break
                   ;;
                -s|--strategy)
                        strategy="$2"
                        shift
                        ;;
                -h)
                        echo "Display some help"
                        exit 0
                        ;;
        esac

        shift
done
cat "$1" | docker run -i reaver -s "$strategy" -debug_force DEBUG -inputformat nbac -debug DEBUG