KPlutus - K Semantics of Plutus-Core
====================================

Dependencies
------------

-   Install needed K dependencies: <https://github.com/kframework/k>
-   Update submodules: `git submodule update --init --recursive`
-   Make dependencies: `make deps RELEASE=true`

Building
--------

-   Build everything: `make build -j8`
-   Build LLVM backend: `make build-llvm -j8`
-   Build just KPlutus runner and includes: `make build-kplutus -j8`

Testing
-------

-   Simple hand-written tests: `make test-simple -j8`

