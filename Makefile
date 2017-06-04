all:
	time kompile --debug --verbose --syntax-module PLUTUS-CORE-SYNTAX src/plutus-core.k

clean:
	rm -rf src/plutus-core-kompiled
