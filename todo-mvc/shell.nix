with (import <nixpkgs> {}).pkgs;
with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> {pkgs = (import <nixpkgs> {});});
let
    hsPackages = haskell-ng.packages.ghcjs.override {
      overrides = self: super: {
        virtual-dom = self.callPackage ./virtual-dom {};
        oHm = self.callPackage ./oHm {};
        todo-mvc = self.callPackage ./. { cabal-install = haskell-ng.packages.ghc7101.cabal-install; };
      };
    };
in
  hsPackages.todo-mvc.env
