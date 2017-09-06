#!/bin/sh
krun --debug --directory ../ --prove int-addition_spec.k --z3-executable dummy.plcore
# test use of the second argument - "ENV" in the spec is indeed replaced with
# the contents of the env cell resulting from executing int-addition-lib.plcore
krun --debug --directory ../ --prove int-addition-with-import_spec.k --z3-executable int-addition-lib.plcore
krun --debug --directory ../ --prove equality_spec.k --z3-executable dummy.plcore
krun --debug --directory ../ --prove inequality_spec.k --z3-executable dummy.plcore
krun --debug --directory ../ --prove sum_spec.k --z3-executable sum.plcore
krun --debug --directory ../ --prove id_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove const_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove flip_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove flip-no-prelude_spec.k --z3-executable dummy.plcore
krun --debug --directory ../ --prove applyTo_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove applyTo-no-prelude_spec.k --z3-executable dummy.plcore
krun --debug --directory ../ --prove compose-no-prelude_spec.k --z3-executable dummy.plcore
krun --debug --directory ../ --prove compose2-no-prelude_spec.k --z3-executable dummy.plcore
krun --debug --directory ../ --prove fst_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove snd_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove curry_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove curry-no-prelude_spec.k --z3-executable dummy.plcore
krun --debug --directory ../ --prove uncurry_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove swap_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove maybe-nothing_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove maybe-just_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove fromJust_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove fromMaybe-nothing_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove fromMaybe-just_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove mapMaybe-nothing_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove mapMaybe-just_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove either-left_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove either-right_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove eitherToMaybe-left_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove eitherToMaybe-right_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove maybeToEither-nothing_spec.k --z3-executable prelude.plc
krun --debug --directory ../ --prove maybeToEither-just_spec.k --z3-executable prelude.plc
