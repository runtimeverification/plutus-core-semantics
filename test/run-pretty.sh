#!/usr/bin/env bash

krun -d ../src/execution $1 > temp.xml
xmllint --format temp.xml | tail -n +2 | sed -e 's/&gt;/>/g'

rm temp.xml
