{ mkDerivation, base, lib, safe-exceptions, transformers }:
mkDerivation {
  pname = "memzero";
  version = "0.1";
  src = lib.sources.cleanSource ./.;
  libraryHaskellDepends = [ base safe-exceptions ];
  testHaskellDepends = [ base safe-exceptions transformers ];
  homepage = "https://github.com/k0001/hs-memzero";
  description = "Securely erase memory contents by writing zeros to it";
  license = lib.licenses.asl20;
}
