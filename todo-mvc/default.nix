{ mkDerivation, aeson, base, containers, ghcjs-base, ghcjs-dom
, ghcjs-prim, lens, mvc, oHm, pipes, pipes-concurrency, profunctors
, stdenv, stm, text, time, transformers, virtual-dom
}:
mkDerivation {
  pname = "ohm-todo-mvc";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson base containers ghcjs-base ghcjs-dom ghcjs-prim lens mvc oHm
    pipes pipes-concurrency profunctors stm text time transformers
    virtual-dom
  ];
  license = stdenv.lib.licenses.unfree;
}
