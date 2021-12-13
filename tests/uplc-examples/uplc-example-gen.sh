#!/bin/bash

# note: this script assumes that the uplc binary lives in this directory

echo $examples
for name in `./uplc example -a`
do
    ./uplc example -s $name > $name.uplc
    ./uplc evaluate --print-mode Classic -i $name.uplc > $name.uplc.expected 2>&1
done
