# How to generate a trivial native-tokens policy
1. Install nix.
1. Get IOG's `plutus` working on your system.
   - Clone Plutus commit ID: bcdd1ceffc93c733d90f5b89bcfae47d21dc2fcd.
   - Follow instructions at `https://github.com/input-output-hk/plutus#how-to-build-the-projects-artifacts` 
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
1. Change to `native-tokens/native-tokens`.
1. Create a symbolic link from `plutus/shell.nix` to `native-tokens/native-tokens`.
1. Run `nix-shell --command 'cabal build'`.
1. Run `nix-shell --command 'cabal run native-tokens-scripts out_scripts scripts'`.
1. There will be 5 flat format files in `out_scripts`.
1. In bash, run the following command. (Make sure 
   `plutus/dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/plutus-core-0.1.0.0/x/uplc/build/uplc/uplc` is in your `PATH`.)
   ```shell
   for i in out_scripts/*.flat ; do uplc convert --if flat --of textual --print-mode Debug -i $i -o $i.debug.uplc ; done   
   ```

Files `native-tokens/native-tokens/CompiledPolicy*.uplc` were
generated using the procedure described on issue
[#233](https://github.com/runtimeverification/plutus-core-semantics/issue/233). File
`native-tokens/free-1-decoded-bytestrings.uplc` is the same as 
`native-tokens/out_scripts/free-1.uplc` but with the bytestring arguments (at the end of the file) manually
transalated to their textual format.
