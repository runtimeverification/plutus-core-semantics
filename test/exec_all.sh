#!/usr/bin/env bash

for file in `ls *.plcore`;
  do
    printf "Running $file\n"
    krun -d ../src/execution $file > temp.xml
    xmllint --format temp.xml | tail -n +2 | sed -e 's/&gt;/>/g'
    rm temp.xml
    echo ""
done;
