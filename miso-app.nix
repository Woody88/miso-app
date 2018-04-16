{ mkDerivation, base, containers, miso, stdenv }:
mkDerivation {
  pname = "miso-app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers miso ];
  description = "First miso app";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
