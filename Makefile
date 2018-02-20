.build/%/plutus-core-kompiled/kore.txt: src/%/plutus-core.k $(wildcard src/%/*.k)
	kompile -d .build/$*/ --debug --verbose --syntax-module PLUTUS-CORE-SYNTAX src/$*/plutus-core.k

.PHONY: test-exec test-erc test-typing test-translation

test-exec: .build/execution/plutus-core-kompiled/kore.txt
	cd test && ./test_exec.sh

test-erc: .build/erc/plutus-core-kompiled/kore.txt
	cd test/erc20 && ./test_all.sh

test-verify: .build/execution/plutus-core-kompiled/kore.txt
	cd verification && ./verify_all.sh

test-translation: .build/translation/plutus-core-kompiled/kore.txt
	krun -d .build/translation/ test/translation/add2.plc
