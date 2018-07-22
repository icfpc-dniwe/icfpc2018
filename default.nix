{ mkDerivation, base, binary, bytestring, ChasingBottoms
, containers, criterion, heredoc, linear, monad-loops, mtl, pqueue
, stdenv, tasty, tasty-hunit, tasty-quickcheck, vector
}:
mkDerivation {
  pname = "icfpc2018";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers linear monad-loops mtl pqueue
    vector
  ];
  executableHaskellDepends = [ base binary bytestring linear ];
  testHaskellDepends = [
    base ChasingBottoms containers heredoc linear tasty tasty-hunit
    tasty-quickcheck vector
  ];
  benchmarkHaskellDepends = [ base binary bytestring criterion ];
  license = stdenv.lib.licenses.bsd3;
}
