#!/usr/bin/env bash

krun -d ../src/execution $1 | xmllint --format - | tail -n +2 | sed -e 's/&gt;/>/g'
