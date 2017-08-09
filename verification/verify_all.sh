#!/bin/sh
krun --debug --directory ../ --prove "int-addition_spec.k" --z3-executable "int-addition.plcore"
