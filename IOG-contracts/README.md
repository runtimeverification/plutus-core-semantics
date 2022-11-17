# Native tokens and Stablecoin contracts 

This directory contains Haskell files that implement native tokens and
a stablecoin contract in PlutusTx. There are two subdirectories named
after each kind of contract. Assuming a proper installation of `nix`,
`plutus` and `plutus-apps`, the files and instructions here should be
enough to generate UPLC scripts from the minting/validation policy of
each contract and from particular executions of each contract. 

The general directory structure is as follows, where `dir` is either
native-tokens or stablecoin and `contract` is either `Free.hs`,
`NFT.hs` or `Stablecoin.hs`:
```
dir
  src
   Contracts
    contract
  test
   Main.hs
   Spec
    contract
``` 

A `Contracts/contract` file contain the PlutusTx implementation of its
associated contract. A `Spec/contract` file contains code related to the
generation of UPLC code for traces of the contract or for the
contract's policy. 

The file `Main.hs` is a driver script in Haskell to the UPLC code
generation process that essentially pretty-prints the compilation of
the contract's policy to UPLC. This file was adapted from the [one](
https://github.com/input-output-hk/plutus-apps/blob/main/plutus-use-cases/scripts/Main.hs)
available for the `plutus-use-cases` apps in the `plutus-apps`
repository, which generates UPLC code from particular traces of the
contracts encoded in `Spec` directory.

In the following, we describe the contracts, leaving to the
`README.md` files in each subdirectory the responsibility to explain how to
generate UPLC code from their associated contracts.

### Nix-shell

The best way to work with these contracts is to use IOG's Nix development environment from the plutus-apps dependency.
See [how to build the projects artifacts](https://github.com/input-output-hk/plutus-apps/blob/main/README.adoc#how-to-build-the-projects-artifacts)
from the plutus-apps readme for information on setting up nix.

With nix set up, you need to enter the nix-shell. There are two ways to do this, be sure to run these commands in the `IOG-contracts` directory:

The preferred way is if the `cabal-install` package is installed in your environment:
```
$ cabal update
$ cd dist-newstyle/src/plutus-ap_-348916df41c59834
$ nix-shell --extra-experimental-features flakes
$ cd ../../..
```

Without `cabal-install`, you need to manually pull the dependency:
```
$ git clone https://github.com/input-output-hk/plutus-apps
$ cd plutus-apps
$ git checkout 6866224e1aadca6a1fd29419fc75beb903a2ce0f
$ nix-shell --extra-experimental-features flakes
$ cd ..
```

## Native tokens

[README.md](native-tokens/README.md)

The contracts in this subfolder were developed by the [Plutus Pioneer
Program](https://github.com/input-output-hk/plutus-pioneer-program).
They are used during its [5th
week](https://github.com/input-output-hk/plutus-pioneer-program) of
training when [native tokens](https://docs.cardano.org/native-tokens/learn) are explained.

The contract `Free.hs` implements a trivial validation policy. It
always returns true. As opposed to the one implemented in `NFT.hs`
which deals with [non-fungible tokens](https://developers.cardano.org/docs/native-tokens/minting-nfts/). 

## Stablecoin

[README.md](stablecoin/README.md)

This contract implements MiniDJED, an algorithmic stablecoin
prototype, described [here](https://developers.cardano.org/docs/native-tokens/minting-nfts/). 

## New variables with the compiled policies in the contracts

We tried not to change the Haskell source of the contracts. Currently,
however, there is an additional binding that holds the compiled policy of the contract. For example, in the NFT contract we have:
```haskell
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

compiledNFTPolicy :: (PlutusTx.CompiledCode
                        (TxOutRef -> TokenName -> Scripts.WrappedMintingPolicyType))
compiledNFTPolicy = $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
```

The contract's policy is bound to `mkPolicy` variable. We have added a
new binding, not present in the original contract, to the variable
`compiledNFTPolicy`, which has the compiled version of `mkPolicy`. This
variable is then pretty-printed to UPLC code by the associated
`Main.hs` script.

## Cabal configurations

There are some requirements for a contract to be executable by `kplc`,
as follows.

### Compiler options

The following options should either be added to the contract's Haskell
file header or to the `ghc-options` of the target that generates the
UPLC code in contract's cabal file.

They prevent simplifications from taking place and their absence may
produce incorrect UPLC code.
```
-fplugin-opt PlutusTx.Plugin:no-simplifier-inline
-fplugin-opt PlutusTx.Plugin:no-simplifier-beta
```

### Haskell language extension

The `TypeApplications` language extension is necessary for the UPLC code
generation function. Should the code generation process be started in
the cabal repl, it is then necessary to set this option with:
```
:set -XTypeApplications
```

This option is already at the top of each file or in the cabal
file. However, both this setting and the previous one need to be added
manually should any of the files change.

## Known problems

### Liblzma error

This error happened when we tried to run the code generation process
using the nix-shell configuration in the `plutus` repo. When we
switched to the one in the `plutus-apps` repo this problem did not
happen anymore.

Should it happen anyways, here are a couple of steps that may solve
the problem.
1. Make sure `liblzma-dev` is installed in your system. 
2. Make sure file `shell.nix` has code similar to the following, if
   it doesn't already. The relevant issue is to have `lzma` there: 
   ```shell
   # build inputs from nixpkgs ( -> ./nix/default.nix )
   nixpkgsInputs = (with pkgs; [
    # ...
    lzma
    # ...
   ] ++ (lib.optionals (!stdenv.isDarwin) [ rPackages.plotly R ]));
   ```
