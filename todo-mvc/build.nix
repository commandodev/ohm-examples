with (import <nixpkgs> {}).pkgs;
let
    hsPackages = haskell-ng.packages.ghcjs.override {
      overrides = self: super: {
        virtual-dom = self.callPackage ./virtual-dom {};
        oHm = self.callPackage ./oHm {};
        todo-mvc = self.callPackage ./. {};
      };
    };
in
  hsPackages.todo-mvc
