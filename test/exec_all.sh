#!/usr/bin/env bash

for file in `ls *.plcore`;
  do
    printf "Running $file\n"
    ./run-pretty.sh $file
    echo ""
done;
