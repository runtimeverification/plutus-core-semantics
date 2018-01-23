typing: src/typing/plutus-core.k src/typing/plutus-core-syntax.k src/typing/plutus-core-typing.k
	kompile -d . --debug --verbose --syntax-module PLUTUS-CORE-SYNTAX src/typing/plutus-core.k
	touch plutus-core-kompiled
	cp -r plutus-core-kompiled src/typing
	rm -rf plutus-core-kompiled
# workaround for kompile not updating mtime, reported as https://github.com/kframework/k/issues/2327

exec: src/execution/plutus-core.k src/execution/plutus-core-syntax.k src/execution/plutus-core-execution.k
	kompile -d . --debug --verbose --syntax-module PLUTUS-CORE-SYNTAX src/execution/plutus-core.k
	touch plutus-core-kompiled
	cp -r plutus-core-kompiled src/execution
	rm -rf plutus-core-kompiled

erc: src/execution/plutus-core.k src/execution/plutus-core-syntax.k src/execution/plutus-core-execution.k
	kompile -d . --debug --verbose --syntax-module PLUTUS-CORE-SYNTAX src/erc20/plutus-core.k
	touch plutus-core-kompiled
	cp -r plutus-core-kompiled src/erc20
	rm -rf plutus-core-kompiled

test: exec
	cd test && ./test_exec.sh

test_erc: erc
	cd test/erc20 && ./test_all.sh

verify: exec
	cd verification && ./verify_all.sh

clean:
	rm -rf src/typing/plutus-core-kompiled
	rm -rf src/execution/plutus-core-kompiled
	rm -rf src/erc20/plutus-core-kompiled

.PHONY: test clean
