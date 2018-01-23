#!/usr/bin/env bash

cat $1 | sed -e 's/;/\/\//g' > temp.txt
krun -d ../../src/erc20 temp.txt > temp.xml
xmllint --format temp.xml | tail -n +2 | sed -e 's/&gt;/>/g'

rm temp.txt
rm temp.xml
