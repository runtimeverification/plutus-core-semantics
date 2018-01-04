#!/usr/bin/env bash

for file in `ls *.plcore`;
  do
    printf "Running $file\n"
    krun -d ../src/execution $file
done;
