# Stablecoin contract

## Introduction

This contract implements MiniDJED stable coin as described [here](https://iohk.io/en/research/library/papers/djed-a-formally-verified-crypto-backed-pegged-algorithmic-stablecoin/). It's
part of `plutus-use-cases` in `plutus-apps` [repo](https://github.com/input-output-hk/plutus-apps). (Just for completeness, there exists a prototype for DJED in Scala [here](https://github.com/input-output-hk/djed-stablecoin-prototype).)

## How to generate Stablecoin policy UPLC scripts

1. Install nix.
2. Get IOG's `plutus` working on your system. 
   - Clone Plutus commit ID: bcdd1ceffc93c733d90f5b89bcfae47d21dc2fcd. (It's the one these instructions have been tested with.)
   - Follow instructions at `https://github.com/input-output-hk/plutus#how-to-build-the-projects-artifacts`. 
   - Make sure `liblzma-dev` is installed in your system. 
   - Make sure file `shell.nix` has code similar to the following, if it doesn't already. The relevant issue is to have `lzma` there:
   ```shell
   # build inputs from nixpkgs ( -> ./nix/default.nix )
   nixpkgsInputs = (with pkgs; [
    # ...
    lzma
    # ...
   ] ++ (lib.optionals (!stdenv.isDarwin) [ rPackages.plotly R ]));
   ```
3. Change to `plutus-semantics/stablecoin/stablecoin`.
4. Create a symbolic link to `plutus/shell.nix`.
5. Run `nix-shell --command 'cabal build'`.
6. Run `nix-shell --command 'cabal run stablecoin-scripts out_scripts scripts`.
7. There will be 10 flat format files in `out_scripts`.
   In bash, run the following command. (Make sure 
   `.../plutus/dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/plutus-core-0.1.0.0/x/uplc/build/uplc/uplc` is in your `PATH`.)
   ```shell
   for i in out_scripts/*.flat ; do uplc convert --if flat -i $i --of textual --print-mode Debug -o $i.uplc ; done
   ```
