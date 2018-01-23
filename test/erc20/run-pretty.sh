#!/usr/bin/env bash

# cat $1 | sed -e 's/;/\/\//g' > $1
krun -d ../../src/erc20 $1 > temp.xml
xmllint --format temp.xml | tail -n +2 | sed -e 's/&gt;/>/g'

rm temp.xml
