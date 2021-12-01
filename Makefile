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
KPLUTUS         := kplutus

KPLUTUS_VERSION     ?= 0.1.0
KPLUTUS_RELEASE_TAG := $(shell git describe --tags --dirty --long)

K_SUBMODULE := $(DEPS_DIR)/k

PATH := $(abspath $(KPLUTUS_BIN)):$(abspath $(KPLUTUS_K_BIN)):$(LOCAL_BIN):$(PATH)
export PATH

PLUGIN_SUBMODULE := $(abspath $(DEPS_DIR)/blockchain-k-plugin)
PLUGIN_SOURCE    := $(KPLUTUS_INCLUDE)/kframework/blockchain-k-plugin/krypto.md
export PLUGIN_SUBMODULE

.PHONY: all clean distclean     \
        deps k-deps plugin-deps \
        build build-kplutus     \
        install uninstall
.SECONDARY:

all: build

clean:
	rm -rf $(KPLUTUS_BIN) $(KPLUTUS_LIB)

distclean:
	rm -rf $(BUILD_DIR)
	git clean -dffx -- tests/

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

plugin_include    := $(KPLUTUS_LIB)/blockchain-k-plugin/include
plugin_k          := krypto.md
plugin_c          := plugin_util.cpp crypto.cpp blake2.cpp plugin_util.h blake2.h
plugin_includes   := $(patsubst %, $(plugin_include)/kframework/%, $(plugin_k))
plugin_c_includes := $(patsubst %, $(plugin_include)/c/%,          $(plugin_c))

$(plugin_include)/c/%: $(PLUGIN_SUBMODULE)/plugin-c/%
	@mkdir -p $(dir $@)
	install $< $@

$(plugin_include)/kframework/%: $(PLUGIN_SUBMODULE)/plugin/%
	@mkdir -p $(dir $@)
	install $< $@

plugin-deps: $(plugin_includes) $(plugin_c_includes)

# Installing
# ----------

install_bins := kplutus

install_libs := release.md \
                version

build_bins := $(install_bins)

build_libs := $(install_libs)

$(KPLUTUS_BIN)/kplutus: kplutus
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

build-kplutus: $(KPLUTUS_BIN)/kplutus $(plugin_includes)

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
