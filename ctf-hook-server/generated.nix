{ mkDerivation, aeson, base, base64-bytestring, bcrypt, bytestring
, cereal, hedis, hspec, hspec-wai, hspec-wai-json, http-types
, magic, network, random, scotty, stdenv, text, transformers, wai
, wai-extra
}:
mkDerivation {
  pname = "ctf-hook";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring bcrypt bytestring cereal hedis
    http-types magic network random scotty text transformers wai
    wai-extra
  ];
  executableHaskellDepends = [ base bytestring hedis transformers ];
  testHaskellDepends = [
    aeson base bytestring hspec hspec-wai hspec-wai-json http-types
  ];
  homepage = "https://github.com/Prillan/ctf-hook";
  license = stdenv.lib.licenses.bsd3;
}
