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
      rev = "283516a337c4d3606555728df0a39294e78a7cdf";
      sha256 = "1vggli76ijqmx633yix4yg5dv58a14p8561jnprjc061sjngphzv";
    }) "-fexamples -fstb_truetype" {
      inherit GLEW glew libGL libGLU;
      inherit (xorg) libX11;
    });
    GLEW = glew;
  }); rec {
    libraries = recurseIntoAttrs {
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
    executables = builtins.mapAttrs (name: _:
      mkApp {
        inherit name;
        drv = libraries.monomer;
      }) (builtins.readDir ../examples);
  }
