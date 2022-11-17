# Stablecoin contract

## Introduction

This contract implements MiniDJED stable coin, as described [here](https://iohk.io/en/research/library/papers/djed-a-formally-verified-crypto-backed-pegged-algorithmic-stablecoin/). It's   
part of `plutus-use-cases` in `plutus-apps` [repo](https://github.com/input-output-hk/plutus-apps). (For completeness, there exists a prototype for DJED in Scala [here](https://github.com/input-output-hk/djed-stablecoin-prototype).)

## How to generate Stablecoin policy UPLC scripts

1. Run `cabal run stablecoin-scripts out_scripts scripts`. There will be 11 files in `out_scripts`.
   Ten flat format files, `stablecoinPolicy.uplc`, and `stablecoinPolicy.pir`.
2. Run the following command.
   ```shell
   cat out_scripts/stablecoinPolicy.uplc | sed 's/_[0-9]\+//g' > out_scripts/stablecoinPolicy.uplc.noindices
   ```
   This will generate `out_scripts/stablecoinPolicy.uplc.noindices` file
   containing the Stablecoin policy without any indices.
3. In bash, run the following command.
   ```shell
   for i in out_scripts/*.flat ; do cabal run uplc -- convert --if flat --of textual --print-mode Debug -i $i -o $i.uplc ; done
   ```
   This will generate UPLC scripts from the flat format files in `out_scripts`.
```
