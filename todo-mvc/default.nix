{ cabal, aeson, ghcjsBase, ghcjsDom, ghcjsPrim, lens, mvc, oHm
, pipes, pipesConcurrency, profunctors, stm, time
}:

cabal.mkDerivation (self: {
  pname = "ohm-todo-mvc";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson ghcjsBase ghcjsDom ghcjsPrim lens mvc oHm pipes
    pipesConcurrency profunctors stm time
  ];
  doCheck = false;
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
