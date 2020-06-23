{ mkDerivation, base, containers, split, stdenv, text, time }:
mkDerivation {
  pname = "Clash-of-Code";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers split text time ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
