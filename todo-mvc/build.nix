with import <nixpkgs> {};
let haskellPackages = pkgs.haskellPackages_ghcjs.override {
      extension = self: super: {
        oHm = self.callPackage ./oHm {};
        mvc = self.callPackage ./oHm/mvc.nix {};
        todo = self.callPackage ./. {};
      };
    };
    inherit (haskellPackages) ghc aeson ghcjsBase ghcjsDom ghcjsPrim
                              oHm lens pipes pipesConcurrency mvc profunctors;
    npm = pkgs.nodePackages.npm;
    browserify = pkgs.nodePackages.browserify;

    client = stdenv.mkDerivation {  
        name = "todo-mvc";  
        version = "1.0";
        src = ./.;
        buildInputs = [ ghc aeson ghcjsBase ghcjsDom ghcjsPrim
                        oHm lens pipes pipesConcurrency mvc profunctors
                        npm browserify closurecompiler
                      ];
        installPhase = ''
          mkdir -p $out
          cp static/index.html $out
          cp static/style.css $out
          cp static/bg.png $out
          cp "${haskellPackages.todo}/bin/todo-mvc.jsexe/all.js" $out
          # closure-compiler $out/all.js --compilation_level=ADVANCED_OPTIMIZATIONS > $out/all.min.js
          # gzip --best -k $out/all.min.js
        '';
      };

in client 
  # { inherit (haskellPackages) ghc aeson ghcjsBase ghcjsDom ghcjsPrim
  #                             oHm ohmChatServer lens pipes pipesConcurrency mvc profunctors;
  #   inherit (pkg.nodePackages)  npm browserify;
  #   closurecompiler = pkgs.closurecompiler;
  # }
