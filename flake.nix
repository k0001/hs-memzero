{
  description = "Haskell 'memzero' library";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs?rev=21eda9bc80bef824a037582b1e5a43ba74e92daa";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.flake-parts.flakeModules.easyOverlay ];
      systems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      perSystem = { config, pkgs, final, ... }: {
        overlayAttrs = {
          haskell = pkgs.haskell // {
            packageOverrides = pkgs.lib.composeExtensions
              (pkgs.haskell.packageOverrides or (_: _: { }))
              (hself: hsuper: { memzero = hself.callPackage ./. { }; });
          };
        };
        packages = {
          default = pkgs.releaseTools.aggregate {
            name = "every output from this flake";
            constituents = [
              config.packages.memzero__ghc943
              config.packages.memzero__ghc943.doc
              config.devShells.ghc943

              config.packages.memzero__ghc925
              config.packages.memzero__ghc925.doc
              config.devShells.ghc925
            ];
          };
          memzero__ghc925 = final.pkgs.haskell.packages.ghc925.memzero;
          memzero__ghc943 = final.pkgs.haskell.packages.ghc943.memzero;
        };
        devShells = let
          shellFor = hpkgs:
            hpkgs.shellFor {
              packages = p: [ p.memzero ];
              withHoogle = true;
              nativeBuildInputs = [ pkgs.cabal-install pkgs.cabal2nix ];
            };
        in {
          default = config.devShells.ghc943;
          ghc925 = shellFor final.pkgs.haskell.packages.ghc925;
          ghc943 = shellFor final.pkgs.haskell.packages.ghc943;
        };
      };
    };
}
