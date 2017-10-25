plutus-core-kompiled: .build/plutus-core-kompiled/extras/timestamp

defn_dir=src
defn_files=${defn_dir}/hex-conversion.k ${defn_dir}/plutus-core-execution.k ${defn_dir}/plutus-core-syntax.k ${defn_dir}/plutus-core.k

.build/plutus-core-kompiled/extras/timestamp: $(defn_files)
	kompile -d .build/ --debug --verbose --syntax-module PLUTUS-CORE-SYNTAX src/plutus-core.k

test: plutus-core-kompiled
	cd test && ./test_all.sh
verify: plutus-core-kompiled
	cd verification && ./verify_all.sh

clean:
	rm -rf plutus-core-kompiled

.PHONY: plutus-core-kompiled test clean
