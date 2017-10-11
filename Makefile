plutus-core-kompiled: src/plutus-core.k src/plutus-core-syntax.k src/plutus-core-typing.k
	kompile -d . --debug --verbose --syntax-module PLUTUS-CORE-SYNTAX src/plutus-core.k
	touch plutus-core-kompiled # workaround for kompile not updating mtime
# reported as https://github.com/kframework/k/issues/2327

test: plutus-core-kompiled
	cd test && ./test_all.sh
verify: plutus-core-kompiled
	cd verification && ./verify_all.sh

clean:
	rm -rf plutus-core-kompiled

.PHONY: test clean
