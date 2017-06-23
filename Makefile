plutus-core-kompiled: src/plutus-core.k src/plutus-core-syntax.k src/plutus-core-execution.k
	kompile -d . --debug --verbose --syntax-module PLUTUS-CORE-SYNTAX src/plutus-core.k

test: plutus-core-kompiled
	cd test && ./test_all.sh

clean:
	rm -rf plutus-core-kompiled
