{ mkDerivation, aeson, base, bytestring, canonical-filepath
, containers, filepath, MissingH, split, stdenv
}:
mkDerivation {
  pname = "mizu";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring canonical-filepath containers filepath
    MissingH split
  ];
  description = "create templates from files by matching them to colours";
  license = stdenv.lib.licenses.asl20;
}
