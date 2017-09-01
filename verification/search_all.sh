#!/bin/sh
krun --debug --directory ../ --search int-addition.plcore
krun --debug --directory ../ --search equality.plcore
krun --debug --directory ../ --search id.plcore
krun --debug --directory ../ --search const.plcore
# TODO: fst.plcore and snd.plcore generate the same symbolic Int V0 for x and y.
# Need to figure out why and fix this
krun --debug --directory ../ --search fst.plcore
krun --debug --directory ../ --search snd.plcore
krun --debug --directory ../ --search swap.plcore
