{ mkDerivation, base, stdenv, containers, text, time }:
mkDerivation {
  pname = "Clash-of-Code";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers text time ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
