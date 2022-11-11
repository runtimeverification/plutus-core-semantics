{
  description = "A flake for the KPlutus Semantics";

  inputs = {
    k-framework.url = "github:runtimeverification/k";
    nixpkgs.follows = "k-framework/nixpkgs";
    flake-utils.follows = "k-framework/flake-utils";
    rv-utils.url = "github:runtimeverification/rv-nix-tools";
    poetry2nix.follows = "pyk/poetry2nix";
    blockchain-k-plugin.url =
      "github:runtimeverification/blockchain-k-plugin/3010915ef37b0e614528821a7c8717c797a365a6";
    blockchain-k-plugin.inputs.flake-utils.follows = "k-framework/flake-utils";
    blockchain-k-plugin.inputs.nixpkgs.follows = "k-framework/nixpkgs";
    haskell-backend.follows = "k-framework/haskell-backend";
    pyk.url = "github:runtimeverification/pyk/v0.1.50";
    pyk.inputs.flake-utils.follows = "k-framework/flake-utils";
    pyk.inputs.nixpkgs.follows = "k-framework/nixpkgs";

  };
  outputs = { self, k-framework, haskell-backend, nixpkgs, flake-utils, poetry2nix , blockchain-k-plugin, rv-utils, pyk }:
    let
      buildInputs = pkgs: k:
        with pkgs;
        [
          k
          llvm-backend
          autoconf
          cmake
          clang
          llvmPackages.llvm
          cryptopp.dev
          gmp
          graphviz
          mpfr
          openssl.dev
          pkg-config
          procps
          protobuf
          python39
          secp256k1
          solc
          time
          virtualenv
        ] ++ lib.optional (!stdenv.isDarwin) elfutils
        ++ lib.optionals stdenv.isDarwin [ automake libtool ];

      overlay = final: prev: {
        kplutus = k:
          prev.stdenv.mkDerivation {
            pname = "kplutus";
            version = self.rev or "dirty";
            buildInputs = buildInputs final k;
            nativeBuildInputs = [ prev.makeWrapper ];

            src = prev.stdenv.mkDerivation {
              name = "kplutus-${self.rev or "dirty"}-src";
              src = prev.lib.cleanSource
                (prev.nix-gitignore.gitignoreSourcePure [
                  ./.gitignore
                  ".github/"
                  "result*"
                  "*.nix"
                  "deps/"
                  "kplutus-pyk/"
                ] ./.);
              dontBuild = true;

              installPhase = ''
                mkdir $out
                cp -rv $src/* $out
                chmod -R u+w $out
                mkdir -p $out/deps/plugin
                cp -rv ${prev.blockchain-k-plugin-src}/* $out/deps/plugin/
              '';
            };

            dontUseCmakeConfigure = true;

            patches = [ ];

            postPatch = ''
              substituteInPlace ./cmake/node/CMakeLists.txt \
                --replace 'set(K_LIB ''${K_BIN}/../lib)' 'set(K_LIB ${k}/lib)'
              substituteInPlace ./bin/kplutus \
                --replace 'execute python3 -m kplutus_pyk' 'execute ${final.kplutus-pyk}/bin/kplutus-pyk'
            '';

            buildFlags =
              prev.lib.optional (prev.stdenv.isAarch64 && prev.stdenv.isDarwin)
              "APPLE_SILICON=true";
            enableParallelBuilding = true;

            preBuild = ''
              make plugin-deps
            '';

            installPhase = ''
              mkdir -p $out
              mv .build/usr/* $out/
              wrapProgram $out/bin/kplutus --prefix PATH : ${
                prev.lib.makeBinPath [ final.solc prev.which k ]
              }
              ln -s ${k} $out/lib/kplutus/kframework
            '';
          };

        kplutus-pyk = prev.poetry2nix.mkPoetryApplication {
          python = prev.python39;
          projectDir = ./kplutus-pyk;
          overrides = prev.poetry2nix.overrides.withDefaults
            (finalPython: prevPython: { pyk = prev.pyk; });
          groups = [];
          # We remove `"dev"` from `checkGroups`, so that poetry2nix does not try to resolve dev dependencies.
          checkGroups = [];
        };

      };
    in flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (final: prev: { llvm-backend-release = false; })
            k-framework.overlay
            blockchain-k-plugin.overlay
            poetry2nix.overlay
            pyk.overlay
            overlay
          ];
        };
        kplutus = pkgs.kplutus k-framework.packages.${system}.k;
      in {
        packages.default = kplutus;
        devShell = pkgs.mkShell {
          buildInputs = buildInputs pkgs k-framework.packages.${system}.k;
        };

        packages = {
          inherit (pkgs) kplutus-pyk;
          inherit kplutus;

          check-submodules = rv-utils.lib.check-submodules pkgs {
            inherit k-framework blockchain-k-plugin;
          };

          update-from-submodules =
            rv-utils.lib.update-from-submodules pkgs ./flake.lock {
              k-framework.submodule = "deps/k";
              blockchain-k-plugin.submodule = "deps/plugin";
            };
        };
      }) // {
        overlays.default = nixpkgs.lib.composeManyExtensions [
          k-framework.overlay
          blockchain-k-plugin.overlay
          overlay
        ];
      };
}
