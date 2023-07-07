#!/bin/bash
set -e

cd ./vendor
bash download.sh
cd ..
sudo docker build . -t reaver:latest