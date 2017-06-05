all:
	time kompile --no-prelude --debug --verbose --syntax-module PLUTUS-CORE-SYNTAX src/plutus-core.k
	rm -rf plutus-core-kompiled
	mv src/plutus-core-kompiled .

clean:
	rm -rf plutus-core-kompiled
