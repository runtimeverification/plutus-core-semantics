# Stablecoin contract

## Introduction

This contract implements MiniDJED stable coin, as described [here](https://iohk.io/en/research/library/papers/djed-a-formally-verified-crypto-backed-pegged-algorithmic-stablecoin/). It's   
part of `plutus-use-cases` in `plutus-apps` [repo](https://github.com/input-output-hk/plutus-apps). (For completeness, there exists a prototype for DJED in Scala [here](https://github.com/input-output-hk/djed-stablecoin-prototype).)

## How to generate Stablecoin policy UPLC scripts

1. Follow instructions in issue [#285](https://github.com/runtimeverification/plutus-core-semantics/issues/285) to get `nix`, `plutus` and `plutus-apps` up and running in your system, if necessary.
2. Change to `plutus-semantics/IOG-contracts/stablecoin/stablecoin`.
3. Run `cabal build`.
4. Run `cabal run stablecoin-scripts out_scripts scripts`. There will be 11 files in `out_scripts`.
   Ten flat format files, `stablecoinPolicy.uplc`, and `stablecoinPolicy.pir`.
5. Run the following command.
   ```shell
   cat out_scripts/stablecoinPolicy.uplc | sed 's/_[0-9]\+//g' > out_scripts/stablecoinPolicy.uplc.noindices
   ```
   This will generate `out_scripts/stablecoinPolicy.uplc.noindices` file
   containing the Stablecoin policy without any indices.
6. In bash, run the following command. (Make sure 
   `plutus/dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/plutus-core-0.1.0.0/x/uplc/build/uplc/uplc` is in your `PATH`.)
   ```shell
   for i in out_scripts/*.flat ; do uplc convert --if flat --of textual --print-mode Debug -i $i -o $i.uplc ; done
   ```
   This will generate UPLC scripts from the flat format files in `out_scripts`.
```