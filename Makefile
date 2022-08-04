# Define standard colors
# ----------------------

# Reset
Color_Off='\033[0m'       # Text Reset

# Regular Colors
Black='\033[0;30m'        # Black
Red='\033[0;31m'          # Red
Green='\033[0;32m'        # Green
Yellow='\033[0;33m'       # Yellow
Blue='\033[0;34m'         # Blue
Purple='\033[0;35m'       # Purple
Cyan='\033[0;36m'         # Cyan
White='\033[0;37m'        # White

# Bold
BBlack='\033[1;30m'       # Black
BRed='\033[1;31m'         # Red
BGreen='\033[1;32m'       # Green
BYellow='\033[1;33m'      # Yellow
BBlue='\033[1;34m'        # Blue
BPurple='\033[1;35m'      # Purple
BCyan='\033[1;36m'        # Cyan
BWhite='\033[1;37m'       # White

# Underline
UBlack='\033[4;30m'       # Black
URed='\033[4;31m'         # Red
UGreen='\033[4;32m'       # Green
UYellow='\033[4;33m'      # Yellow
UBlue='\033[4;34m'        # Blue
UPurple='\033[4;35m'      # Purple
UCyan='\033[4;36m'        # Cyan
UWhite='\033[4;37m'       # White

# Background
On_Black='\033[40m'       # Black
On_Red='\033[41m'         # Red
On_Green='\033[42m'       # Green
On_Yellow='\033[43m'      # Yellow
On_Blue='\033[44m'        # Blue
On_Purple='\033[45m'      # Purple
On_Cyan='\033[46m'        # Cyan
On_White='\033[47m'       # White

# High Intensity
IBlack='\033[0;90m'       # Black
IRed='\033[0;91m'         # Red
IGreen='\033[0;92m'       # Green
IYellow='\033[0;93m'      # Yellow
IBlue='\033[0;94m'        # Blue
IPurple='\033[0;95m'      # Purple
ICyan='\033[0;96m'        # Cyan
IWhite='\033[0;97m'       # White

# Bold High Intensity
BIBlack='\033[1;90m'      # Black
BIRed='\033[1;91m'        # Red
BIGreen='\033[1;92m'      # Green
BIYellow='\033[1;93m'     # Yellow
BIBlue='\033[1;94m'       # Blue
BIPurple='\033[1;95m'     # Purple
BICyan='\033[1;96m'       # Cyan
BIWhite='\033[1;97m'      # White

# High Intensity backgrounds
On_IBlack='\033[0;100m'   # Black
On_IRed='\033[0;101m'     # Red
On_IGreen='\033[0;102m'   # Green
On_IYellow='\033[0;103m'  # Yellow
On_IBlue='\033[0;104m'    # Blue
On_IPurple='\033[0;105m'  # Purple
On_ICyan='\033[0;106m'    # Cyan
On_IWhite='\033[0;107m'   # White

# Settings
# --------

UNAME_S := $(shell uname -s)

DEPS_DIR      := deps
BUILD_DIR     := .build
BUILD_LOCAL   := $(abspath $(BUILD_DIR)/local)
LOCAL_BIN     := $(BUILD_LOCAL)/bin

INSTALL_PREFIX  := /usr
INSTALL_BIN     ?= $(INSTALL_PREFIX)/bin
INSTALL_LIB     ?= $(INSTALL_PREFIX)/lib/kplutus
INSTALL_INCLUDE ?= $(INSTALL_LIB)/include

KPLUTUS_BIN     := $(BUILD_DIR)$(INSTALL_BIN)
KPLUTUS_LIB     := $(BUILD_DIR)$(INSTALL_LIB)
KPLUTUS_INCLUDE := $(KPLUTUS_LIB)/include
KPLUTUS_K_BIN   := $(KPLUTUS_LIB)/kframework/bin
KPLUTUS         := kplc

KPLUTUS_VERSION     ?= 0.1.0
KPLUTUS_RELEASE_TAG := $(shell git describe --tags --dirty --long)

K_SUBMODULE := $(DEPS_DIR)/k

PATH := $(abspath $(KPLUTUS_BIN)):$(abspath $(KPLUTUS_K_BIN)):$(LOCAL_BIN):$(PATH)
export PATH

PLUGIN_SUBMODULE := $(abspath $(DEPS_DIR)/blockchain-k-plugin)
PLUGIN_SOURCE    := $(KPLUTUS_INCLUDE)/kframework/blockchain-k-plugin/krypto.md
export PLUGIN_SUBMODULE

.PHONY: all clean distclean                \
        deps k-deps plugin-deps libff      \
        build build-coverage build-kplutus \
        build-haskell build-llvm           \
        install uninstall                  \
        test-new-syntax test-simple        \
        test-uplc-examples                 \
        test-benchmark-validation-examples \
        test-nofib-exe-examples            \
        conformance-test update-results    \
        test-prove test-unit-tests         \
        fresh-test-coverage

.SECONDARY:

all: build

clean:
	rm -rf $(KPLUTUS_BIN) $(KPLUTUS_LIB)

distclean:
	rm -rf $(BUILD_DIR)
	git clean -dffx -- tests/

# Non-K Dependencies
# ------------------

libff_out := $(KPLUTUS_LIB)/libff/lib/libff.a
libcryptopp_out  := $(KPLUTUS_LIB)/cryptopp/lib/libcryptopp.a

ifneq (,$(wildcard $(KPLUTUS_K_BIN)/../lib/cmake/kframework/LLVMKompilePrelude.cmake))
	llvm_kompile_prelude := $(realpath $(KPLUTUS_K_BIN)/../lib/cmake/kframework/LLVMKompilePrelude.cmake)
else
	llvm_kompile_prelude := $(dir $(shell which kompile))../lib/cmake/kframework/LLVMKompilePrelude.cmake
endif

LIBFF_CMAKE_FLAGS = -DCMAKE_TOOLCHAIN_FILE=$(llvm_kompile_prelude)

ifeq ($(UNAME_S),Linux)
    LIBFF_CMAKE_FLAGS +=
else ifeq ($(UNAME_S),Darwin)
    LIBFF_CMAKE_FLAGS += -DWITH_PROCPS=OFF -DOPENSSL_ROOT_DIR=$(shell brew --prefix openssl)
else
    LIBFF_CMAKE_FLAGS += -DWITH_PROCPS=OFF
endif

ifneq ($(APPLE_SILICON),)
    LIBFF_CMAKE_FLAGS += -DCURVE=ALT_BN128 -DUSE_ASM=Off
endif

libff: $(libff_out)

$(libff_out): $(PLUGIN_SUBMODULE)/deps/libff/CMakeLists.txt
	@mkdir -p $(PLUGIN_SUBMODULE)/deps/libff/build
	cd $(PLUGIN_SUBMODULE)/deps/libff/build                                                                     \
	    && cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$(INSTALL_LIB)/libff $(LIBFF_CMAKE_FLAGS) \
	    && make -s -j4                                                                                          \
	    && make install DESTDIR=$(CURDIR)/$(BUILD_DIR)

$(libcryptopp_out): $(PLUGIN_SUBMODULE)/deps/cryptopp/GNUmakefile
	cd $(PLUGIN_SUBMODULE)/deps/cryptopp                                                     \
	    && $(MAKE) static                                                                    \
	    && $(MAKE) install-lib DESTDIR=$(CURDIR)/$(BUILD_DIR) PREFIX=$(INSTALL_LIB)/cryptopp

# K Dependencies
# --------------

deps: k-deps

K_MVN_ARGS :=
ifneq ($(APPLE_SILICON),)
    K_MVN_ARGS += -Dstack.extra-opts='--compiler ghc-8.10.7 --system-ghc'
endif

ifneq ($(RELEASE),)
    K_BUILD_TYPE := FastBuild
else
    K_BUILD_TYPE := Debug
endif

k-deps:
	cd $(K_SUBMODULE)                                                                                                                                                                            \
	    && mvn --batch-mode package -DskipTests -Dllvm.backend.prefix=$(INSTALL_LIB)/kframework -Dllvm.backend.destdir=$(CURDIR)/$(BUILD_DIR) -Dproject.build.type=$(K_BUILD_TYPE) $(K_MVN_ARGS) \
	    && DESTDIR=$(CURDIR)/$(BUILD_DIR) PREFIX=$(INSTALL_LIB)/kframework package/package

k-deps-profiling: K_MVN_ARGS += -Dhaskell.backend.skip=true
k-deps-profiling: K_BUILD_TYPE := FastBuild
k-deps-profiling: export CMAKE_CXX_FLAGS='-O2 -DNDEBUG -fno-omit-frame-pointer'
k-deps-profiling: k-deps

plugin_include    := $(KPLUTUS_LIB)/blockchain-k-plugin/include
plugin_k          := krypto.md
plugin_c          := plugin_util.cpp crypto.cpp hash_ext.cpp blake2.cpp plugin_util.h blake2.h
plugin_includes   := $(patsubst %, $(plugin_include)/kframework/%, $(plugin_k))
plugin_c_includes := $(patsubst %, $(plugin_include)/c/%,          $(plugin_c))

$(plugin_include)/c/%: $(PLUGIN_SUBMODULE)/plugin-c/%
	@mkdir -p $(dir $@)
	install $< $@

$(plugin_include)/kframework/%: $(PLUGIN_SUBMODULE)/plugin/%
	@mkdir -p $(dir $@)
	install $< $@

plugin-deps: $(plugin_includes) $(plugin_c_includes)

# Semantics Build
# ---------------

KOMPILE := $(KPLUTUS) kompile

kplutus_files := uplc.md \
                 bitstream.md \
                 uplc-builtins.md \
                 uplc-bytestring-builtins.md \
                 uplc-cbor-parser.md \
                 uplc-crypto-builtins.md  \
                 uplc-integer-builtins.md \
                 uplc-semantics.md \
                 uplc-syntax.md \
                 uplc-bytestring.md \
                 uplc-environment.md \
                 uplc-genvironment.md \
				 uplc-genvironment-instance.md \
                 uplc-string-builtins.md \
                 uplc-configuration.md \
                 uplc-flat-parser.md \
                 uplc-polymorphic-builtins.md \
                 uplc-string.md \
                 uplc-data-builtins.md \
                 uplc-discharge.md

kplutus_includes := $(patsubst %, $(KPLUTUS_INCLUDE)/kframework/%, $(kplutus_files))

$(KPLUTUS_INCLUDE)/kframework/%.md: %.md
	@mkdir -p $(dir $@)
	install $< $@

llvm_dir           := llvm
llvm_main_module   := UPLC
llvm_syntax_module := UPLC-SYNTAX
llvm_main_file     := uplc.md
llvm_main_filename := $(basename $(notdir $(llvm_main_file)))
llvm_kompiled_dir  := $(llvm_dir)/$(llvm_main_filename)-kompiled/
llvm_kompiled      := $(llvm_kompiled_dir)/interpreter

haskell_dir            := haskell
haskell_main_module    := UPLC
haskell_syntax_module  := $(haskell_main_module)
haskell_main_file      := uplc.md
haskell_main_filename  := $(basename $(notdir $(haskell_main_file)))
haskell_kompiled       := $(haskell_dir)/$(haskell_main_filename)-kompiled/definition.kore

KOMPILE_OPTS += --no-exc-wrap

ifndef NOBUILD_CRYPTOPP
  $(KPLUTUS_LIB)/$(llvm_kompiled): $(libcryptopp_out)
endif

build-llvm-profiling: KOMPILE_OPTS += -O3 -ccopt -fno-omit-frame-pointer
build-llvm-profiling: $(KPLUTUS_LIB)/$(llvm_kompiled)

$(KPLUTUS_LIB)/$(llvm_kompiled): $(kplutus_includes) $(plugin_includes) $(plugin_c_includes) $(libff_out) $(KPLUTUS_BIN)/kplc
	$(KOMPILE) --backend llvm                 \
	    $(llvm_main_file)                     \
	    --main-module $(llvm_main_module)     \
	    --syntax-module $(llvm_syntax_module) \
	    $(KOMPILE_OPTS)

$(KPLUTUS_LIB)/$(haskell_kompiled): $(kplutus_includes) $(plugin_includes) $(KPLUTUS_BIN)/kplc
	$(KOMPILE) --backend haskell                     \
	    $(haskell_main_file) $(HASKELL_KOMPILE_OPTS) \
	    --main-module $(haskell_main_module)         \
	    --syntax-module $(haskell_syntax_module)     \
	    $(KOMPILE_OPTS)

# Coverage Processing
# -------------------

covr_ignore_files := uplc-bytestring.md \
                     uplc-flat-parser.md \
		     uplc-discharge.md

covr_ignore_includes := $(patsubst %, $(KPLUTUS_INCLUDE)/kframework/%, $(covr_ignore_files))

K_BUILTINS_INCLUDE := $(KPLUTUS_LIB)/kframework/include/kframework/builtin

k_builtin_ignore_files := domains.md

k_builtin_ignore_includes := $(patsubst %, $(K_BUILTINS_INCLUDE)/%, $(k_builtin_ignore_files))

coverage:
	$(KPLUTUS_BIN)/kplutus-covr $(KPLUTUS_LIB)/$(llvm_kompiled_dir) \
        -- $(kplutus_includes) \
        -ig $(covr_ignore_includes) $(k_builtin_ignore_includes) > $(BUILD_DIR)/coverage.xml

# Installing
# ----------

install_bins := kplc kplutus-covr

install_libs := $(llvm_kompiled)    \
                $(haskell_kompiled) \
                release.md          \
                version

build_bins := $(install_bins)

build_libs := $(install_libs)

$(KPLUTUS_BIN)/kplc: kplc
	@mkdir -p $(dir $@)
	install $< $@

$(KPLUTUS_BIN)/kplutus-covr: kplutus-covr
	@mkdir -p $(dir $@)
	install $< $@

$(KPLUTUS_LIB)/version:
	@mkdir -p $(dir $@)
	echo $(KPLUTUS_RELEASE_TAG) > $@

$(KPLUTUS_LIB)/release.md: INSTALL.md
	@mkdir -p $(dir $@)
	echo "KPLUTUS Release $(KPLUTUS_RELEASE_TAG)"  > $@
	echo                                          >> $@
	cat INSTALL.md                                >> $@

build: $(patsubst %, $(KPLUTUS_BIN)/%, $(install_bins)) $(patsubst %, $(KPLUTUS_LIB)/%, $(install_libs))

build-coverage: KOMPILE_OPTS += --coverage
build-coverage: $(KPLUTUS_LIB)/$(llvm_kompiled)

build-kplutus: $(KPLUTUS_BIN)/kplc $(plugin_includes) $(kplutus_includes)
build-llvm:    $(KPLUTUS_LIB)/$(llvm_kompiled)
build-haskell: $(KPLUTUS_LIB)/$(haskell_kompiled)

all_bin_sources := $(shell find $(KPLUTUS_BIN) -type f | sed 's|^$(KPLUTUS_BIN)/||')
all_lib_sources := $(shell find $(KPLUTUS_LIB) -type f                                            \
                            -not -path "$(KPLUTUS_LIB)/**/dt/*"                                   \
                            -not -path "$(KPLUTUS_LIB)/kframework/share/kframework/pl-tutorial/*" \
                            -not -path "$(KPLUTUS_LIB)/kframework/share/kframework/k-tutorial/*"  \
                            | sed 's|^$(KPLUTUS_LIB)/||')

install: $(patsubst %, $(DESTDIR)$(INSTALL_BIN)/%, $(all_bin_sources)) \
         $(patsubst %, $(DESTDIR)$(INSTALL_LIB)/%, $(all_lib_sources))

$(DESTDIR)$(INSTALL_BIN)/%: $(KPLUTUS_BIN)/%
	@mkdir -p $(dir $@)
	install $< $@

$(DESTDIR)$(INSTALL_LIB)/%: $(KPLUTUS_LIB)/%
	@mkdir -p $(dir $@)
	install $< $@

uninstall:
	rm -rf $(DESTDIR)$(INSTALL_BIN)/kplutus
	rm -rf $(DESTDIR)$(INSTALL_LIB)/kplutus

procs := $(shell nproc)

fresh-test-coverage:
	[ -d $(KPLUTUS_LIB)/$(llvm_kompiled_dir) ] && rm -r $(KPLUTUS_LIB)/$(llvm_kompiled_dir)
	make build-coverage
	make conformance-test -j$(procs)
	make coverage

# Prove tests
#------------
KPROVE_OPTS :=

unit_tests := $(wildcard unit-tests/*.md)
test-unit-tests: $(unit_tests:=.prove)

prove_tests := $(wildcard simple-proofs/[!verification]*.md)

test-prove: $(prove_tests:=.prove)

unit-tests/%.md.prove: unit-tests/%.md unit-tests/verification/haskell/verification-kompiled/timestamp
	$(KPLUTUS) prove --directory unit-tests/verification/haskell $< $(KPROVE_OPTS)

unit-tests/verification/haskell/verification-kompiled/timestamp: unit-tests/verification.k $(kplutus_includes)
	$(KOMPILE) --backend haskell $< --directory unit-tests/verification/haskell

simple-proofs/%.md.prove: simple-proofs/%.md simple-proofs/verification/haskell/verification-kompiled/timestamp
	$(KPLUTUS) prove --directory simple-proofs/verification/haskell $< $(KPROVE_OPTS)

simple-proofs/verification/haskell/verification-kompiled/timestamp: simple-proofs/verification.md $(kplutus_includes)
	$(KOMPILE) --symbolic --backend haskell $< --directory simple-proofs/verification/haskell


# Testing
# -------

TEST_MSG      := "\n>>> Testing "

# Option result-only prints the contents of the k cell.
# The sed command below removes .TermList constants generated by K
# for application.
# The sed command is a temporary solution that removes empty (e.g. .List)
# constants generated by K and also `[]` from the final state.

TEST_OPTS  := --result-only | \
              sed -e 's/\[\]//g' | \
              sed -e 's/\(.TermList\s*\|.ConstantList\s*\|.DataList\s*\|.Env\s*\)//g'

CHECK          := diff --ignore-all-space
TEST           := $(KPLUTUS) run
UPLC           := uplc
EXPECTED       :=.expected

# KRUN_TEST and KRUN_TEST_OPTS are used for tests that can only be run by kplc at the moment
# Once the new syntax is adopted by uplc, we will move these tests to the simple tests.

KRUN_TEST      := $(KPLUTUS) run
KRUN_TEST_OPTS := $(TEST_OPTS)


# Failing
# -------

# File tests/failing lists tests that are not passing.
# Lines starting with '#' are considered comments in tests/failing.
COMMRE = "^\#"
failing_tests := $(shell grep -v $(COMMRE) tests/failing)


# Running the test
# ----------------

tests/%.uplc.run: tests/%.uplc
	@echo $(BWhite)$(TEST_MSG)$(Color_off)$(Green)$<$(Color_Off)"\n"
	$(TEST)  $< $(TEST_OPTS) > $<.out
	$(CHECK) $<.out $<$(EXPECTED)

TEST_FLAT          := $(KPLUTUS) run
TEST_FLAT_OPTS  := --flat-format $(TEST_OPTS)

tests/%.flat.run: tests/%.flat
	@echo $(BWhite)$(TEST_MSG)$(Color_off)$(Green)$<$(Color_Off)"\n"
	$(TEST_FLAT) $< $(TEST_FLAT_OPTS) > $<.out
	$(CHECK) $<.out $<$(EXPECTED)

# This target runs tests that contains syntax that is yet to be supported on uplc.
# Once uplc adopts this new syntax, remove this rule and compare the kplc output to uplc output.
tests/%.uplc.krun: tests/%.uplc
	@echo $(BWhite)$(TEST_MSG)$(Color_off)$(Green)$<$(Color_Off)"\n"
	$(KRUN_TEST)  $< $(KRUN_TEST_OPTS) > $<.out
	$(CHECK) $<.out $<$(EXPECTED)

update-results: conformance-test
update-results: TEST=$(UPLC) evaluate --print-mode Classic -i
update-results: TEST_FLAT=$(UPLC) evaluate --if flat -i
# The trail command below removes break lines that litters
# the .EXPECTED file and breaks testing.
update-results: TEST_OPTS= | tr -d '\012\015'
update-results: TEST_FLAT_OPTS= | tr -d '\012\015'
update-results: CHECK=cp
update-results: TEST_MSG="\n>>> Updating results for "

# Conformance tests
#
# Any test that is executed by `kplc run` invokes krun. Since the semantics is meant to
# conform the execution of the reference implementation, all output from `kplc run` needs to be
# compared to tests that use krun needs to the output of the reference implementation. The
# reference interpreter is a program maintained by IOG called uplc.
#
# The tests below are run using kplc and the outputs are compared against expected output in files
# ending in .uplc.expected. The expected output has been generated by running `uplc evaluate` on the
# input test case.

conformance-test: test-simple test-new-syntax        \
                  test-uplc-examples                 \
                  test-benchmark-validation-examples \
                  test-nofib-exe-examples            \
                  test-flat test-error

# Simple Tests

all_simple_tests := $(wildcard tests/textual/simple/*.uplc)
simple_tests     := $(filter-out $(failing_tests), $(all_simple_tests))

test-simple: $(simple_tests:=.run)

# uplc-example Tests

all_uplc-examples_tests := $(wildcard tests/textual/uplc-examples/*.uplc)
uplc-examples_tests     := $(filter-out $(failing_tests), $(all_uplc-examples_tests))

test-uplc-examples: $(uplc-examples_tests:=.run)

# benchmark-validation-examples Tests

all_benchmark-validation-examples_tests := $(wildcard tests/textual/benchmark-validation-examples/*.uplc)
benchmark-validation-examples_tests     := $(filter-out $(failing_tests), $(all_benchmark-validation-examples_tests))

test-benchmark-validation-examples: $(benchmark-validation-examples_tests:=.run)

# nofib-exe-examples Tests

all_nofib-exe-examples_tests := $(wildcard tests/textual/nofib-exe-examples/*.uplc)
nofib-exe-examples_tests     := $(filter-out $(failing_tests), $(all_nofib-exe-examples_tests))

test-nofib-exe-examples: $(nofib-exe-examples_tests:=.run)

# flat format Tests

all_flat_tests := $(wildcard tests/flat/*/*.flat)
flat_tests     := $(filter-out $(failing_tests), $(all_flat_tests))

test-flat: $(flat_tests:=.run)

# new-syntax Tests
#
# These tests cannot be run using UPLC at the moment. Once support is added to parse new syntax, it
# will be moved to the simple tests directory

all_new_syntax_tests := $(wildcard tests/textual/new-syntax/*.uplc)
new_syntax_tests     := $(filter-out $(failing_tests), $(all_new_syntax_tests))

test-new-syntax: $(new_syntax_tests:=.krun)

# error Tests
#
# These tests will evaluate to an `(error)` term. When uplc encounters this term during evaluation,
# it will exit with a non-zero status and therefore require different recipes to update the result.
# TODO: figure out how to incorporate uplc for these tests.

all_error_tests := $(wildcard tests/textual/error/*.uplc)
error_tests     := $(filter-out $(failing_tests), $(all_error_tests))

test-error: $(error_tests:=.krun)

#
# Update flat tests
#
CONVRT_MSG:= ">>  Converting "
all_textual_tests := $(wildcard tests/textual/*/*.uplc)
textual_tests     := $(filter-out $(failing_tests), $(all_textual_tests))
textual_tests     := $(filter-out $(error_tests), $(textual_tests))
textual_tests     := $(filter-out $(new_syntax_tests), $(textual_tests))

update-flat: $(textual_tests:=.textual2flat)

CONVERT = uplc convert --of flat -i
tests/%.uplc.textual2flat: tests/%.uplc
	@echo $(BWhite)$(CONVRT_MSG)$(Color_off)$(Green)$<$(Color_Off)"\n"
	$(CONVERT) $< > tests/flat/$(shell basename `dirname $<`)/$(shell basename $< .uplc).flat
