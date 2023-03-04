{
  description = "Haskell memzero library";

  outputs = { self, nixpkgs }:
    let
      haskellOverrides = pself: psuper: hself: hsuper: {
        memzero = hsuper.callPackage ./. { };
      };
      pkgsOverlay = pself: psuper: {
        haskellPackages = psuper.haskellPackages.override {
          overrides = haskellOverrides pself psuper;
        };
      };
      pkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ pkgsOverlay ];
        };

    in {
      packages =
        nixpkgs.lib.genAttrs [ "x86_64-linux" "i686-linux" "aarch64-linux" ]
        (system:
          let pkgs = pkgsFor system;
          in {
            default = self.packages.${system}.hs-memzero;
            hs-memzero = pkgs.haskellPackages.memzero;
          });
      devShells =
        nixpkgs.lib.genAttrs [ "x86_64-linux" "i686-linux" "aarch64-linux" ]
        (system:
          let pkgs = pkgsFor system;
          in {
            default = pkgs.haskellPackages.shellFor {
              packages = p: [ p.memzero ];
              withHoogle = true;
              nativeBuildInputs = [ pkgs.cabal-install pkgs.cabal2nix ];
            };
          });
    };

}
