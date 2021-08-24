{ system, version, flake-utils, ... }:
self: super:
with self;
with haskell.lib;
with flake-utils.lib;
with super.haskellPackages.extend (self: super:
  with haskellPackages; rec {
    # nanovg = dontCheck (callHackage "nanovg" "0.8.0.0" {
    #   inherit GLEW glew libGL libGLU;
    #   inherit (xorg) libX11;
    # });

    nanovg = dontCheck (callCabal2nixWithOptions "nanovg" (fetchFromGitHub {
      owner = "cocreature";
      repo = "nanovg-hs";
      rev = "cc8dfa0dc18a0792786c973b4e9a232fa7d3ecfd";
      sha256 = "0vvj4l2dfjqspl80bwq4vkcql5p7s5a7l1cv7vajkak0vn1ryy70";
    }) "-fexamples -fstb_truetype" {
      inherit GLEW glew libGL libGLU;
      inherit (xorg) libX11;
    });
    GLEW = glew;
  }); rec {
    libraries = recurseIntoAttrs rec {
      monomer = addExtraLibrary
        (overrideCabal (callCabal2nix "monomer" ../. { }) (o: {
          version = "${o.version}.${version}";
          doCheck = true;
          checkPhase = ''
            runHook preCheck
            ${xvfb_run}/bin/xvfb-run ./Setup test
            runHook postCheck
          '';
        })) GLEW;
    };
    executables = {
      todo = mkApp rec {
        drv = libraries.monomer;
        name = "todo";
      };
      books = mkApp rec {
        drv = libraries.monomer;
        name = "books";
      };
      ticker = mkApp rec {
        drv = libraries.monomer;
        name = "ticker";
      };
      generative = mkApp rec {
        drv = libraries.monomer;
        name = "generative";
      };
      tutorial = mkApp rec {
        drv = libraries.monomer;
        name = "tutorial";
      };
    };
  }
