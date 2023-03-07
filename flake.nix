{
  description = "Haskell memzero library";

  outputs = { self, nixpkgs }:
    let
      hsMemzero = import ./.;
      hspkgsOverrides = pself: psuper: hself: hsuper: {
        memzero = hsuper.callPackage hsMemzero { };
      };
      pkgsOverlay = pself: psuper: {
        haskell = psuper.haskell // {
          packageOverrides = hspkgsOverrides pself psuper;
        };
      };
      pkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ pkgsOverlay ];
        };

    in {
      inherit hsMemzero hspkgsOverrides pkgsOverlay;
      packages =
        nixpkgs.lib.genAttrs [ "x86_64-linux" "i686-linux" "aarch64-linux" ]
        (system:
          let pkgs = pkgsFor system;
          in {
            default = pkgs.releaseTools.aggregate {
              name = "every output from this flake";
              constituents = let
                p = self.packages.${system};
                s = self.devShells.${system};
              in [
                p.hs_memzero__ghcDefault
                p.hs_memzero__ghc925
                p.hs_memzero__ghc943

                p.hs_memzero__ghcDefault.doc
                p.hs_memzero__ghc925.doc
                p.hs_memzero__ghc943.doc

                s.hs_memzero__ghcDefault
                s.hs_memzero__ghc925
                s.hs_memzero__ghc943
              ];
            };
            hs_memzero__ghcDefault = pkgs.haskellPackages.memzero;
            hs_memzero__ghc925 = pkgs.haskell.packages.ghc925.memzero;
            hs_memzero__ghc943 = pkgs.haskell.packages.ghc943.memzero;
          });
      devShells =
        nixpkgs.lib.genAttrs [ "x86_64-linux" "i686-linux" "aarch64-linux" ]
        (system:
          let
            pkgs = pkgsFor system;
            mkShellFor = hpkgs:
              hpkgs.shellFor {
                packages = p: [ p.memzero ];
                withHoogle = true;
                nativeBuildInputs = [ pkgs.cabal-install pkgs.cabal2nix ];
              };
          in {
            default = self.devShells.${system}.hs_memzero__ghcDefault;
            hs_memzero__ghcDefault = mkShellFor pkgs.haskellPackages;
            hs_memzero__ghc925 = mkShellFor pkgs.haskell.packages.ghc925;
            hs_memzero__ghc943 = mkShellFor pkgs.haskell.packages.ghc943;
          });
    };

}
