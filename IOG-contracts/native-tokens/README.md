# Native tokens contracts

## Introduction

The contracts in this directory implement minting policies for native
tokens. The contract `Free.hs` implements the trivial policy that
generates true to every input whereas contract `NFT.hs` is a more elaborated
one for non-fungible native tokens. They are originally available from
the Plutus Pioneer Program.

## How to generate policies for native tokens

1. Run `cabal run native-tokens-scripts out_scripts scripts`. There
   will be 9 files in total.
2. There will be 3 UPLC files in `out_scripts` at this point and 3 PIR files.
   Run the following command.
   ```shell
   for i in out_scripts/*.uplc ; do cat $i | sed 's/_[0-9]\+//g' > $i.noindices ; done
   ```
   This will generate 3 `noindices` files containing the contracts'
   (Free and NFT) policies without any indices.
3. Five flat format will in `out_scripts`.
   In bash, run the following command.
   ```shell
   for i in out_scripts/*.flat ; do cabal run uplc -- convert --if flat -i $i --of textual -o $i.uplc ; done
   ```
   This will generate UPLC code from traces of Free and NFT. Note that
   these UPLC files do not have any variable name treatment. They are
   only useful for execution with `kplc run`.
