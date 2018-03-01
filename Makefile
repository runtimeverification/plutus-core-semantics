# Settings
# --------

build_dir:=$(CURDIR)/.build
k_submodule:=$(build_dir)/k
k_bin:=$(k_submodule)/k-distribution/target/release/k/bin

.PHONY: all clean build deps \
        execution translation erc20 typing \
        test-exec test-erc test-typing test-translation

all: build

clean:
	rm -rf .build

# Dependencies
# ------------

deps: $(k_submodule)/make.timestamp

$(k_submodule)/make.timestamp:
	git submodule update --init -- $(k_submodule)
	cd $(k_submodule) \
		&& mvn package -q -DskipTests
	touch $(k_submodule)/make.timestamp

# Build
# -----

.build/%/plutus-core-kompiled/kore.txt: src/%/plutus-core.k $(wildcard src/%/*.k)
	kompile -d .build/$*/ --debug --verbose --syntax-module PLUTUS-CORE-SYNTAX src/$*/plutus-core.k

build: execution translation erc typing

execution:   .build/execution/plutus-core-kompiled/kore.txt
translation: .build/translation/plutus-core-kompiled/kore.txt
erc:         .build/erc/plutus-core-kompiled/kore.txt
typing:      .build/typing/plutus-core-kompiled/kore.txt

# Testing
# -------

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
