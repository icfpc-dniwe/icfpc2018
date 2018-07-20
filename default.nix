{ mkDerivation, base, binary, bytestring, linear, stdenv }:
mkDerivation {
  pname = "icfpc2018";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base binary bytestring linear ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
