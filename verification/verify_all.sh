#!/bin/sh
krun --debug --directory ../ --prove int-addition_spec.k --z3-executable dummy.plcore
# test use of the second argument - "ENV" in the spec is indeed replaced with
# the contents of the env cell resulting from executing int-addition-lib.plcore
krun --debug --directory ../ --prove int-addition-with-import_spec.k --z3-executable int-addition-lib.plcore
krun --debug --directory ../ --prove id_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove flip_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove flip-no-prelude_spec.k --z3-executable dummy.plcore
krun --debug --directory ../ --prove applyTo-no-prelude_spec.k --z3-executable dummy.plcore
krun --debug --directory ../ --prove compose-no-prelude_spec.k --z3-executable dummy.plcore
krun --debug --directory ../ --prove const_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove fst_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove snd_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove swap_spec.k --z3-executable prelude.plc
