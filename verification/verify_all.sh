#!/bin/sh
krun --directory ../src/execution --prove int-addition_spec.k --z3-executable dummy.plcore
# # test use of the second argument - "ENV" in the spec is indeed replaced with
# # the contents of the env cell resulting from executing int-addition-lib.plcore
krun --directory ../src/execution --prove int-addition-with-import_spec.k --z3-executable int-addition-lib.plcore
krun --directory ../src/execution --prove equality_spec.k --z3-executable dummy.plcore
krun --directory ../src/execution --prove inequality_spec.k --z3-executable dummy.plcore
krun --directory ../src/execution --prove sum_spec.k --z3-executable sum.plcore
# krun --directory ../src/execution --prove id_spec.k --z3-executable prelude.plc
krun --directory ../src/execution --prove const_spec.k --z3-executable prelude.plc
krun --directory ../src/execution --prove flip_spec.k --z3-executable prelude.plc
krun --directory ../src/execution --prove flip-no-prelude_spec.k --z3-executable dummy.plcore
krun --directory ../src/execution --prove applyTo_spec.k --z3-executable prelude.plc
krun --directory ../src/execution --prove applyTo-no-prelude_spec.k --z3-executable dummy.plcore
krun --directory ../src/execution --prove compose-no-prelude_spec.k --z3-executable dummy.plcore
krun --directory ../src/execution --prove compose2-no-prelude_spec.k --z3-executable dummy.plcore
# krun --directory ../src/execution --prove fst_spec.k --z3-executable prelude.plc
# krun --directory ../src/execution --prove snd_spec.k --z3-executable prelude.plc
krun --directory ../src/execution --prove curry_spec.k --z3-executable prelude.plc
krun --directory ../src/execution --prove curry-no-prelude_spec.k --z3-executable dummy.plcore
krun --directory ../src/execution --prove uncurry_spec.k --z3-executable prelude.plc
krun --directory ../src/execution --prove swap_spec.k --z3-executable prelude.plc
krun --directory ../src/execution --prove maybe-nothing_spec.k --z3-executable prelude.plc
krun --directory ../src/execution --prove maybe-just_spec.k --z3-executable prelude.plc
# krun --directory ../src/execution --prove fromJust_spec.k --z3-executable prelude.plc
# krun --directory ../src/execution --prove fromMaybe-nothing_spec.k --z3-executable prelude.plc
# krun --directory ../src/execution --prove fromMaybe-just_spec.k --z3-executable prelude.plc
# krun --directory ../src/execution --prove mapMaybe-nothing_spec.k --z3-executable prelude.plc
# krun --directory ../src/execution --prove mapMaybe-just_spec.k --z3-executable prelude.plc
# krun --directory ../src/execution --prove either-left_spec.k --z3-executable prelude.plc
# krun --directory ../src/execution --prove either-right_spec.k --z3-executable prelude.plc
# krun --directory ../src/execution --prove eitherToMaybe-left_spec.k --z3-executable prelude.plc
# krun --directory ../src/execution --prove eitherToMaybe-right_spec.k --z3-executable prelude.plc
# krun --directory ../src/execution --prove maybeToEither-nothing_spec.k --z3-executable prelude.plc
# krun --directory ../src/execution --prove maybeToEither-just_spec.k --z3-executable prelude.plc
