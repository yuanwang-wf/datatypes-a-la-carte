{ mkDerivation, base, polysemy, random, stdenv }:
mkDerivation {
  pname = "datatypes-a-la-carte";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base polysemy random ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
