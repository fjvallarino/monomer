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
        overlays = [
          (import ./nix/monomer.nix { inherit system version flake-utils; })
          (import ./nix/qemu.nix {
            inherit system version flake-utils nixpkgs;
          })
        ];
      in with (import nixpkgs { inherit system overlays; }); rec {
        packages = flattenTree (recurseIntoAttrs {
          inherit (libraries) monomer;
          inherit (qemu) monomer-vm;
        });
        apps = executables // { inherit vm; };
        defaultPackage = packages.monomer;
        defaultApp = apps.tutorial;
      });
}
