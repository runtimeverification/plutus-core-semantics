.build/%/plutus-core-kompiled/kore.txt: src/%/plutus-core.k $(wildcard src/%/*.k)
	kompile -d .build/$*/ --debug --verbose --syntax-module PLUTUS-CORE-SYNTAX src/$*/plutus-core.k

.PHONY: all clean \
        execution translation erc20 typing \
        test-exec test-erc test-typing test-translation

all:    .build/execution/plutus-core-kompiled/kore.txt      \
        .build/erc20/plutus-core-kompiled/kore.txt          \
        .build/translation/plutus-core-kompiled/kore.txt   \
        .build/typing/plutus-core-kompiled/kore.txt

clean:
	rm -rf .build

execution:   .build/execution/plutus-core-kompiled/kore.txt
translation: .build/translation/plutus-core-kompiled/kore.txt
erc:         .build/erc/plutus-core-kompiled/kore.txt
typing:      .build/typing/plutus-core-kompiled/kore.txt

test-exec: .build/execution/plutus-core-kompiled/kore.txt
	cd test && ./test_exec.sh

test-erc: .build/erc/plutus-core-kompiled/kore.txt
	cd test/erc20 && ./test_all.sh

test-verify: .build/execution/plutus-core-kompiled/kore.txt
	cd verification && ./verify_all.sh

test-translation: .build/translation/plutus-core-kompiled/kore.txt \
                  test/translation/add.out  test/translation/add2.out
	git diff --exit-code test/translation/*.out

test/translation/%.out: test/translation/%.plc .build/translation/plutus-core-kompiled/kore.txt
	krun -d .build/translation/ $< | xmllint --format - | tail -n +2 | sed -e 's/&gt;/>/g' > $@
