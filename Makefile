# Settings
# --------

build_dir:=$(CURDIR)/.build
k_submodule:=$(build_dir)/k
k_bin:=$(k_submodule)/k-distribution/target/release/k/bin

.PHONY: all clean build deps \
        execution translation erc20 typing \
        test test-passing test-failing test-verify
        # Somehow SECONDEXPANSION and PHONY are interacting poorly, meaning these can't be PHONY
        # test-execution test-erc20 test-typing test-translation

all: build

clean:
	rm -rf .build

# Dependencies
# ------------

dep_files:=$(k_submodule)/make.timestamp

deps: $(dep_files)

$(k_submodule)/make.timestamp:
	git submodule update --init -- $(k_submodule)
	cd $(k_submodule) \
		&& mvn package -q -DskipTests
	touch $(k_submodule)/make.timestamp

# Build
# -----

# Allow expansion of $* in wildcard; See https://stackoverflow.com/questions/15948822/directory-wildcard-in-makefile-pattern-rule
.SECONDEXPANSION:
.build/%/plutus-core-kompiled/kore.txt: src/%/plutus-core.k $$(wildcard src/$$*/*.k) $(dep_files)
	$(k_bin)/kompile --debug --verbose --directory .build/$*/ \
					 --syntax-module PLUTUS-CORE-SYNTAX src/$*/plutus-core.k

build: execution translation erc20 typing

execution:   .build/execution/plutus-core-kompiled/kore.txt
translation: .build/translation/plutus-core-kompiled/kore.txt
erc20:       .build/erc20/plutus-core-kompiled/kore.txt
typing:      .build/typing/plutus-core-kompiled/kore.txt

# Testing
# -------

test: test-passing test-failing
test-passing: test-translation test-execution
test-failing: test-erc20 test-verify

test-verify: .build/execution/plutus-core-kompiled/kore.txt
	cd verification && ./verify_all.sh

# TODO: Should wildcard on *.plc and patsubst to change extension, but that isn't quite workin
test-%: .build/%/plutus-core-kompiled/kore.txt  $$(wildcard test/$$*/*.out)
	git diff --exit-code test/$*/*.out

test/%.out: test/%.plc .build/$$(dir $$*)/plutus-core-kompiled/kore.txt
	$(k_bin)/krun --debug --directory .build/$(dir $*)/ $< \
		| xmllint --format - | tail -n +2 | sed -e 's/&gt;/>/g' > $@
