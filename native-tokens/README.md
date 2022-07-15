# How to generate a trivial native-tokens policy
1. Get IOG's `plutus` working on your system.
   - Clone Plutus commit ID: bcdd1ceffc93c733d90f5b89bcfae47d21dc2fcd.
   - Follow instructions at `https://github.com/input-output-hk/plutus#how-to-build-the-projects-artifacts` 
   - Make sure `liblzma-dev` is installed in your system. 
   - Make sure file `shell.nix` has the following code if it doesn't already:
   ```shell
   # build inputs from nixpkgs ( -> ./nix/default.nix )
   nixpkgsInputs = (with pkgs; [
    # ...
    lzma
    # ...
   ] ++ (lib.optionals (!stdenv.isDarwin) [ rPackages.plotly R ]));
   ```
2. Move to `plutus-semantics/native-tokens/native-tokens`.
3. Run `cabal build`.
4. Run `cabal run native-tokens-script out_scripts scripts`.
5. There will be 3 flat format files in `out_scripts`.
   In bash, run the following command. Make sure `
   `.../plutus/dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/plutus-core-0.1.0.0/x/uplc/build/uplc/uplc` is your `PATH`.
   ```shell
   for i in out_script/*.flat ; do uplc convert --if flat -i $i -of textual -o $i.uplc ; done
   ```