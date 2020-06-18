{ mkDerivation, base, stdenv, text }:
mkDerivation {
  pname = "Clash-of-Code";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base text ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
