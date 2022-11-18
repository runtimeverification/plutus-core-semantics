KPlutus - K Semantics of Plutus-Core
====================================

Dependencies
------------

-   Install needed K dependencies: <https://github.com/kframework/k>
-   Update submodules: `git submodule update --init --recursive`
-   Make dependencies: `make deps RELEASE=true`

### Ubuntu (20.04 Focal, 22.04 Jammy)

```
sudo apt-get install build-essential m4 openjdk-11-jdk libgmp-dev libmpfr-dev pkg-config flex bison z3 libsecp256k1-dev libz3-dev maven python3 python3-dev python3-pip cmake gcc clang-12 lld-12 llvm-12-tools zlib1g-dev libboost-test-dev libyaml-dev libjemalloc-dev libsecp256k1-dev libssl-dev xxd
pip install virtualenv poetry
```

### MacOS (12.6 Monterey over Intel)

Follow instructions for K's dependencies in MacOS from <https://github.com/kframework/k>. However, there are some remarks regarding:

- llvm: make sure you install llvm@14, not llvm@15 or later, unless K supports it.
- openssl: make sure you install openssl@3, or the version required by K. Your system may ship with openssl@1.1.
- secp256k1: you may need to build it from
  [source](https://github.com/bitcoin-core/secp256k1). For that you will
  need to install GNU's autotools using homebrew.
  * `brew install autoconf`
  * `brew install automake`
  While building secp256k1, make sure you run:
  * `./configure  --enable-module-recovery`
  in the appropriate step.

Building
--------

-   Build everything: `make build -j8`
-   Build LLVM backend: `make build-llvm -j8`
-   Build Haskell backend: `make build-haskell -j8`
-   Build just KPlutus runner and includes: `make build-kplutus -j8`

### A note about Crypto++

Due to a bug in the ubuntu bionic and focal packages for crypto++ that affects us here, we build a static library for it from source.
If you're certain that you have a crypto++ package installed that doesn't have this bug, you can run the make commands with `NOBUILD_CRYPTOPP=true`

### Update your `PATH` variable

- You might want to update your shell resource file to extend `PATH`:
  ```shell
  export PATH=`<PATH-TO-PLUTUS-CORE-SEMANTICS>/plutus-core-semantics/.build/usr/bin:$PATH
  ``

- Optionally,
  ```shell
  export PATH=`<PATH-TO-PLUTUS-CORE-SEMANTICS>/plutus-core-semantics/.build/usr/lib/kplutus/kframework/bin
  ```
  to have direct access to the package's K tools, in case there is
  something you need that the `kplc` command doesn't offer.
  
  
### Building with nix

We now support building KPlutus using [nix flakes](https://nixos.wiki/wiki/Flakes).
To set up nix flakes you will need to be on `nix` 2.4 or higher and follow the instructions [here](https://nixos.wiki/wiki/Flakes).

For example, if you are on a standard Linux distribution, such as Ubuntu, first [install nix](https://nixos.org/download.html#download-nix)
and then enable flakes by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` and adding:

```
experimental-features = nix-command flakes
```

This is needed to expose the Nix 2.0 CLI and flakes support that are hidden behind feature-flags.


By default, Nix will build the project and its transitive dependencies from
source, which can take up to an hour. We recommend setting up
[the binary cache](https://app.cachix.org/cache/kore) to speed up the build
process significantly. You will also need to add the following sections to `/etc/nix/nix.conf` or, if you are a trusted user, `~/.config/nix/nix.conf` (if you don't know what a "trusted user" is, you probably want to do the former):

```
trusted-public-keys = ... hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
substituters = ... https://cache.iog.io
```

i.e. if the file was originally

```
substituters = https://cache.nixos.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

it will now read

```
substituters = https://cache.nixos.org https://cache.iog.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

The above cache is used for building general haskell packages, however also consider installing cachix to add our kframework specific cache:

```
nix-env -iA cachix -f https://cachix.org/api/v1/install
```

The above will install cachix, afterwards add the `k-framework` cache:

```
cachix use k-framework
```

To build the KPlutus via nix, run:

```bash
nix build .#kplutus
```

This will build the KPlutus binary along with K and put a link to the resulting binaries in the `result/` folder.


_Note: Mac users, especially those running M1/M2 Macs may find nix segfaulting on occasion. If this happens, try running the nix command like this: `GC_DONT_GC=1 nix build .#kplutus`_ 


If you want to temporarily add the KPlutus binary to the current shell, run

```bash
nix shell .#kplutus
```

Testing
-------

-   Conformance tests: `make conformance-test`
    Runs all tests where an input program is executed by the `kplc run` command.
    The output is compared against the reference implementation as well as a previous run of `kplc run`

-   Simple hand-written tests: `make test-simple`
    These contain small tests that have been written during the development of the semantics.

-   Examples generated by the uplc program: `make test-uplc-examples`
    The uplc program contains several small examples. These are slightly larger programs than files in simple tests.
    The uplc program is a part of the plutus repository https://github.com/input-output-hk/plutus

-   Examples benchmark examples: `test-benchmark-validation-examples`
    This contains many examples derived from the plutus-apps directory. These represent are more realistic plutus programs.
    The tests from this directory were extracted from https://github.com/input-output-hk/plutus/tree/master/plutus-benchmark/validation/data

-   Examples generated by the nofib-exe program: test-nofib-exe-examples
    nofib-exe is a program that generates large programs that can serve as performance tests.
    This program is a part of the plutus repository.

-   Tests that evaluate to an `error` term: `make test-error`

-   Update test results: `make update-results`
    Update all test results. Note: this requires having the uplc program installed in the machine's PATH.

Rule Coverage
-------------

This project contains facilities to generate coverage metrics for K rewrite rules that were executed by `kplc run`.
This is helpful in ensuring that the test suite contains input programs that exercise all rewrite rules in the semantics.

To generate this information, run the following command:

```
  make fresh-test-coverage
```

This command generates a `.build/coverage.xml` file. This file contains information about the K
rewrite rules that have been exercised for all tests in the tests/ directory.

After running `make fresh-test-coverage`, the execution of
`./no-hits.py -r` will show all the rules that have not been exercised
in the last execution of `make fresh-test-coverage`. (By executing
`./no-hits.py -t`, a Python 3 dictionary containing file names and
line numbers is pretty-printed. This dictionary denotes the same as the rules printed with option `-r`.)

Profiling with perf
-------------------

There are build targets for profiling the semantics with perf.

```
make k-deps-profiling
make build-llvm-profiling
```

This will build kplc with optimizations but debug info left in so perf can collect more meaningful data.

You can install perf on ubuntu by installing the `linux-tools-common` package. Information about perf and how to use it
can be found on the [perf wiki](https://perf.wiki.kernel.org/index.php/Main_Page).

Here's an example of steps you can follow to generate and view a report:
```
perf record -g -- kplc run tests/textual/nofib-exe-examples/lastpiece.uplc
```
lastpiece.uplc is a very long running program. You can kill it after about one minute and perf will have collected
a sizeable amount of data.

View the report with
```
perf report -g -c interpreter
```
