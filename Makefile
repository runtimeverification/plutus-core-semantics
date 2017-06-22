plutus-core-kompiled: src/plutus-core.k src/plutus-core-syntax.k src/plutus-core-execution.k
	kompile -d . --no-prelude --debug --verbose --syntax-module PLUTUS-CORE-SYNTAX src/plutus-core.k

clean:
	rm -rf plutus-core-kompiled
