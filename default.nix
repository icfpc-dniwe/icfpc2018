{ mkDerivation, base, binary, bytestring, linear, stdenv, tasty
, vector
}:
mkDerivation {
  pname = "icfpc2018";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base binary bytestring linear vector ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base tasty ];
  license = stdenv.lib.licenses.bsd3;
}
