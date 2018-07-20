{ mkDerivation, base, binary, bytestring, ChasingBottoms
, containers, linear, pqueue, QuickCheck, stdenv, tasty
, tasty-quickcheck, vector
}:
mkDerivation {
  pname = "icfpc2018";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers linear pqueue vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base ChasingBottoms linear QuickCheck tasty tasty-quickcheck vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
