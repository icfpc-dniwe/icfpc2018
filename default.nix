{ mkDerivation, base, binary, bytestring, ChasingBottoms
, containers, criterion, deepseq, heredoc, linear, monad-loops, mtl
, pqueue, primitive, stdenv, tasty, tasty-hspec, tasty-hunit
, tasty-quickcheck, transformers, vector
}:
mkDerivation {
  pname = "icfpc2018";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers deepseq linear monad-loops mtl
    pqueue primitive transformers vector
  ];
  executableHaskellDepends = [ base binary bytestring linear ];
  testHaskellDepends = [
    base ChasingBottoms containers heredoc linear tasty tasty-hspec
    tasty-hunit tasty-quickcheck vector
  ];
  benchmarkHaskellDepends = [
    base binary bytestring criterion linear
  ];
  license = stdenv.lib.licenses.bsd3;
}
