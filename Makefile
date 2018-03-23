# Settings
# --------

build_dir:=$(CURDIR)/.build
iele_submodule:=$(build_dir)/iele
k_submodule:=$(build_dir)/k
k_bin:=$(k_submodule)/k-distribution/target/release/k/bin

.PHONY: all clean build deps ocaml-deps \
        execution translation erc20 typing \
        test test-passing test-failing test-verify test-verify-commented \
        iele
        # Somehow SECONDEXPANSION and PHONY are interacting poorly, meaning these can't be PHONY
        # test-execution test-erc20 test-typing test-translation

all: build

clean:
	rm -rf .build

# Dependencies
# ------------

dep_files:=$(k_submodule)/.git

deps: $(dep_files) ocaml-deps

%/.git: .git/HEAD
	git submodule update --recursive --init -- $(dir $*)

$(k_bin)/krun:
	cd $(k_submodule) \
		&& mvn package -q -DskipTests

ocaml-deps:
	opam init --quiet --no-setup
	opam repository add k "$(k_submodule)/k-distribution/target/release/k/lib/opam" \
	    || opam repository set-url k "$(k_submodule)/k-distribution/target/release/k/lib/opam"
	opam update
	opam switch 4.03.0+k
	eval $$(opam config env) \
	opam install --yes mlgmp zarith uuidm

iele: $(iele_submodule)/.git
	(   cd $(iele_submodule)      && \
	    make deps                 && \
	    eval $$(opam config env)  && \
	    make                         \
	)

# Build
# -----

# Allow expansion of $* in wildcard; See https://stackoverflow.com/questions/15948822/directory-wildcard-in-makefile-pattern-rule
.SECONDEXPANSION:

.build/%/plutus-core-kompiled/interpreter: src/%/plutus-core.k $(wildcard src/*.k) $$(wildcard src/$$*/*.k) $(dep_files)
	eval $$(opam config env) \
	$(k_bin)/kompile --debug --verbose --directory .build/$*/ \
					 --syntax-module PLUTUS-CORE-SYNTAX src/$*/plutus-core.k
# Since the `interpreter` targets aren't explicitly mentioned as targets, it treats
# them as intermediate targets and deletes them when it is done. Marking
# them as PRECIOUS prevents this.
.PRECIOUS: .build/%/plutus-core-kompiled/interpreter

build: build-passing build-failing
build-passing: execution translation
build-failing: erc20 typing

execution:   .build/execution/plutus-core-kompiled/interpreter
translation: .build/translation/plutus-core-kompiled/interpreter
erc20:       .build/erc20/plutus-core-kompiled/interpreter
typing:      .build/typing/plutus-core-kompiled/interpreter

# Testing
# -------

test: test-passing test-failing
test-passing: test-translation test-execution
test-failing: test-erc20 test-verify test-verify-commented

translation_tests:=$(wildcard test/translation/*.plc)
execution_tests:=$(wildcard test/execution/*.plc)
erc20_tests:=$(wildcard test/erc20/*.plc)

test-translation: $(translation_tests:=.test)
test-execution: $(execution_tests:=.test)
test-erc20: $(erc20_tests:=.test)

test/execution/%   : driver = execution
test/translation/% : driver = translation
test/typing/%      : driver = typing
test/erc20/%       : driver = erc20

test/%.plc.test test/%.out: .build/$$(dir $$*)/plutus-core-kompiled/interpreter
	./bin/kplc run $(driver) test/$*.plc > test/$*.out
test/%.iele: test/%.out
	bin/config-to-iele < $^ > $@

test/%.iele.test: test/%.iele
	(cd .build/iele/ && ./blockchaintest ../../test/$*.iele.json)

test-verify: .build/execution/plutus-core-kompiled/interpreter
	./bin/kplc prove execution verification/int-addition_spec.k             verification/dummy.plcore
	./bin/kplc prove execution verification/int-addition-with-import_spec.k verification/int-addition-lib.plcore
	./bin/kplc prove execution verification/equality_spec.k                 verification/dummy.plcore
	./bin/kplc prove execution verification/inequality_spec.k               verification/dummy.plcore
	./bin/kplc prove execution verification/sum_spec.k                      verification/sum.plcore
	./bin/kplc prove execution verification/const_spec.k                    verification/prelude.plc
	./bin/kplc prove execution verification/flip_spec.k                     verification/prelude.plc
	./bin/kplc prove execution verification/flip-no-prelude_spec.k          verification/dummy.plcore
	./bin/kplc prove execution verification/applyTo_spec.k                  verification/prelude.plc
	./bin/kplc prove execution verification/applyTo-no-prelude_spec.k       verification/dummy.plcore
	./bin/kplc prove execution verification/compose-no-prelude_spec.k       verification/dummy.plcore
	./bin/kplc prove execution verification/compose2-no-prelude_spec.k      verification/dummy.plcore
	./bin/kplc prove execution verification/curry_spec.k                    verification/prelude.plc
	./bin/kplc prove execution verification/curry-no-prelude_spec.k         verification/dummy.plcore
	./bin/kplc prove execution verification/uncurry_spec.k                  verification/prelude.plc
	./bin/kplc prove execution verification/swap_spec.k                     verification/prelude.plc
	./bin/kplc prove execution verification/maybe-nothing_spec.k            verification/prelude.plc
	./bin/kplc prove execution verification/maybe-just_spec.k               verification/prelude.plc

test-verify-commented: .build/execution/plutus-core-kompiled/timestamp
	./bin/kplc prove execution verification/id_spec.k                       verification/prelude.plc
	./bin/kplc prove execution verification/fst_spec.k                      verification/prelude.plc
	./bin/kplc prove execution verification/snd_spec.k                      verification/prelude.plc
	./bin/kplc prove execution verification/fromJust_spec.k                 verification/prelude.plc
	./bin/kplc prove execution verification/fromMaybe-nothing_spec.k        verification/prelude.plc
	./bin/kplc prove execution verification/fromMaybe-just_spec.k           verification/prelude.plc
	./bin/kplc prove execution verification/mapMaybe-nothing_spec.k         verification/prelude.plc
	./bin/kplc prove execution verification/mapMaybe-just_spec.k            verification/prelude.plc
	./bin/kplc prove execution verification/either-left_spec.k              verification/prelude.plc
	./bin/kplc prove execution verification/either-right_spec.k             verification/prelude.plc
	./bin/kplc prove execution verification/eitherToMaybe-left_spec.k       verification/prelude.plc
	./bin/kplc prove execution verification/eitherToMaybe-right_spec.k      verification/prelude.plc
	./bin/kplc prove execution verification/maybeToEither-nothing_spec.k    verification/prelude.plc
	./bin/kplc prove execution verification/maybeToEither-just_spec.k       verification/prelude.plc
