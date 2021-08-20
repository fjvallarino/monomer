{ system, version, flake-utils, nixpkgs, ... }:
self: super:
with self;
with haskell.lib;
with flake-utils.lib;
with haskellPackages;
let hostname = "nixos";
in rec {
  qemu = recurseIntoAttrs {
    "${hostname}" = (import "${nixpkgs}/nixos" {
      inherit system;
      configuration = { config, pkgs, ... }:
        with (import ./monomer.nix { inherit system version flake-utils; } self
          super);
        let
          mkSystemAppFromLib = { library, app }:
            with pkgs; rec {
              bin = "${library}/bin/${app}";
              drv = runCommand "${library}-${app}" {
                buildInputs = [ makeWrapper ];
              } ''
                mkdir -pv $out/bin
                makeWrapper ${bin} $out/bin/${library}-${app}
              '';
            };

          mkUser = { name, passwd ? "${name}" }: {
            "${name}" = {
              isNormalUser = true;
              createHome = true;
              password = "${passwd}";
              shell = fish;
              extraGroups = [ "wheel" ];
            };
          };
          mkSystemPackages = { library, app }: [
            libraries."${library}"
            (mkSystemAppFromLib {
              library = libraries."${library}";
              inherit app;
            }).drv
          ];
        in {
          networking = { hostName = hostname; };
          environment = {
            systemPackages = (with pkgs; [ htop ]) ++ (mkSystemPackages {
              library = "monomer";
              app = "todo";
            }) ++ (mkSystemPackages {
              library = "monomer";
              app = "books";
            }) ++ (mkSystemPackages {
              library = "monomer";
              app = "ticker";
            }) ++ (mkSystemPackages {
              library = "monomer";
              app = "generative";
            }) ++ (mkSystemPackages {
              library = "monomer";
              app = "tutorial";
            });
          };
          users = {
            mutableUsers = false;
            users = {
              root = { password = "root"; };
              inherit (mkUser { name = "monomer"; }) monomer;
            };
          };
          security = {
            sudo = {
              enable = true;
              wheelNeedsPassword = false;
            };
          };
          virtualisation = {
            graphics = true;
            cores = 4;
            qemu.networkingOptions = [
              "-device virtio-net-pci,netdev=user.0"
              "-netdev type=user,id=user.0\${QEMU_NET_OPTS:+,$QEMU_NET_OPTS}"
            ];
          };
        };
    }).vm;
  };
}
