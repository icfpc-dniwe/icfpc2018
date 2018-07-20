{ mkDerivation, base, binary, bytestring, linear, QuickCheck
, stdenv, tasty, tasty-quickcheck, vector
}:
mkDerivation {
  pname = "icfpc2018";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base binary bytestring linear vector ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base linear QuickCheck tasty tasty-quickcheck vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
