# Native tokens contracts

## Introduction

The contracts in this directory implement minting policies for native
tokens. The contract `Free.hs` implements the trivial policy that
generates true to every input whereas contract `NFT.hs` is a more elaborated
one for non-fungible native tokens. They are originally available from
the Plutus Pioneer Program. 

## How to generate policies for native tokens

1. Follow instructions in issue
   [#285](https://github.com/runtimeverification/plutus-core-semantics/issues/285)
   to get `nix`, `plutus` and `plutus-apps` up and running in your
   system, if necessary.
2. Change to `plutus-semantics/IOG-contracts/native-tokens/native-tokens`.
3. Run `cabal build`.
4. Run `cabal run native-tokens-scripts out_scripts scripts`. There
   will be 9 files in total. 
5. There will be 3 UPLC files in `out_scripts` at this point and 3 PIR files.
   Run the following command.
   ```shell
   for i in out_scripts/*.uplc ; do cat $i | sed 's/_[0-9]\+//g' > $i.noindices ; done
   ```
   This will generate 3 `noindices` files containing the contracts'
   (Free and NFT) policies without any indices.
6. Five flat format will in `out_scripts`.
   In bash, run the following command. (Make sure 
   `plutus/dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/plutus-core-0.1.0.0/x/uplc/build/uplc/uplc`
   is in your `PATH`.) 
   ```shell
   for i in out_scripts/*.flat ; do uplc convert --if flat -i $i --of textual -o $i.uplc ; done
   ```
   This will generate UPLC code from traces of Free and NFT. Note that
   these UPLC files do not have any variable name treatment. They are
   only useful for execution with `kplc run`.
