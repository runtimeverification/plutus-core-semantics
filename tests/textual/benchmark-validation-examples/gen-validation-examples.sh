#!/bin/bash

#
# This script assumes that
# 1) a copy of the uplc executable is in the current directory and
# 2) the plutus repo is cloned in the home directory
#

for file in `find ~/plutus/ -type f -name *.flat`
do
    filename=`basename -s .flat $file`.uplc
    ./uplc convert -i $file --if flat -o $filename

    # The command below doesn't work because uplc's textual lexer cannot parse all builtin keywords
    # ./uplc evaluate -i $filename > $filename.expected
    # Instead, evaluate the programs using the flat (a.k.a. serialized) format

    ./uplc evaluate --print-mode Classic --if flat -i $file > $filename.expected
done
