{ mkDerivation, aeson, base, blaze-builder, bytestring
, case-insensitive, containers, cookie, data-default-class
, filepath, hspec, hspec-wai, hspec-wai-json, http-types
, mime-types, monad-loops, mtl, path-pieces, random, stdenv
, template-haskell, text, vault, wai, wai-extra
}:
mkDerivation {
  pname = "wai-routes";
  version = "0.10.3";
  src = ./.;
  libraryHaskellDepends = [
    aeson base blaze-builder bytestring case-insensitive containers
    cookie data-default-class filepath http-types mime-types
    monad-loops mtl path-pieces random template-haskell text vault wai
    wai-extra
  ];
  testHaskellDepends = [
    aeson base hspec hspec-wai hspec-wai-json text wai
  ];
  homepage = "https://ajnsit.github.io/wai-routes/";
  description = "Typesafe URLs for Wai applications";
  license = stdenv.lib.licenses.mit;
}
