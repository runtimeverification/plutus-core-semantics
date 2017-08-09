#!/bin/sh
krun --debug --directory ../ --prove "int-addition_spec.k" --z3-executable "int-addition.plcore"
# test use of the second argument - "ENV" in the spec is indeed replaced with
# the contents of the env cell resulting from executing int-addition-lib.plcore
krun --debug --directory ../ --prove int-addition-with-import_spec.k --z3-executable int-addition-lib.plcore
