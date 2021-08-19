{
  description = "A GUI library for writing native Haskell applications.";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils/master";
    nixpkgs.url = "github:nixos/nixpkgs/haskell-updates";
  };
  outputs = { self, nixpkgs, flake-utils }:
    with flake-utils.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let
        version = with nixpkgs.lib;
          "${substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
        overlay = self: super:
          with self;
          with haskell.lib;
          with super.haskellPackages.extend (self: super:
            with haskellPackages; rec {
              nanovg = dontCheck (callCabal2nixWithOptions "nanovg"
                (fetchFromGitHub {
                  owner = "cocreature";
                  repo = "nanovg-hs";
                  rev = "cc8dfa0dc18a0792786c973b4e9a232fa7d3ecfd";
                  sha256 =
                    "0vvj4l2dfjqspl80bwq4vkcql5p7s5a7l1cv7vajkak0vn1ryy70";
                }) "-fexamples -fstb_truetype" {
                  GLEW = null;
                  inherit glew;
                  inherit libGL;
                  inherit libGLU;
                  inherit (xorg) libX11;
                });
              GLEW = glew;
            }); rec {
              libraries = recurseIntoAttrs {
                monomer = addExtraLibrary
                  (overrideCabal (callCabal2nix "monomer" ./. { }) (o: {
                    version = "${o.version}.${version}";
                    doCheck = false;
                  })) GLEW;
              };
              executables = {
                todo = mkApp rec {
                  drv = libraries.monomer;
                  exePath = "/bin/todo";
                };
                books = mkApp rec {
                  drv = libraries.monomer;
                  exePath = "/bin/books";
                };
                ticker = mkApp rec {
                  drv = libraries.monomer;
                  exePath = "/bin/ticker";
                };
                generative = mkApp rec {
                  drv = libraries.monomer;
                  exePath = "/bin/generative";
                };
                tutorial = mkApp rec {
                  drv = libraries.monomer;
                  exePath = "/bin/tutorial";
                };
              };
            };
        overlays = [ overlay ];
      in with (import nixpkgs { inherit system overlays; }); rec {
        packages =
          flattenTree (recurseIntoAttrs { inherit (libraries) monomer; });
        defaultPackage = packages.monomer;
        apps = { inherit (executables) tutorial todo books ticker generative; };
        defaultApp = apps.tutorial;
      });
}
