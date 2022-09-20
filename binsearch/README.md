# Binary search in UPLC

- File `binarysearch.md` constains the fixpoint operator `REC` and the
  recursive definition of binary search, implemented using 3 aliases:
  function, function body, function loop.

- File `verification.md` only includes UPLC and binary search syntax.

- File `binarysearch-unit-tests.md` defines some claims.

- To check claims:  
  1. Compile `verification.md`  
    ```shell
    deps/k/k-distribution/bin/kompile --backend haskell \
                                      --directory binsearch/verification/haskell \
                                      binsearch/verification.md \
                                      -I /data/RV/plutus-core-semantics/.build/usr/lib/kplutus/include/kframework \
                                      -I /data/RV/plutus-core-semantics/.build/usr/lib/kplutus/blockchain-k-plugin/include/kframework
    ```
  2. Run the prover on the claims.  
    ```shell
    deps/k/k-distribution/bin/kprove --directory binsearch/verification/haskell \
                                     binsearch/binsearch-unit-test.md \
                                     -I /data/RV/plutus-core-semantics/.build/usr/lib/kplutus/include/kframework \
                                     -I /data/RV/plutus-core-semantics/.build/usr/lib/kplutus/blockchain-k-plugin/include/kframework
    ```
