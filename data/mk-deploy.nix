{ deploy ? import ./deploy.nix }:
let
  lib = import <nixos/lib>;
  evalConfig = import <nixos/nixos/lib/eval-config.nix>;

in (evalConfig {
  prefix = [];
  check = false;
  modules = [deploy];
  baseModules = [ ./modules/machine.nix ];
  pkgs = import <nixpkgs> {};
  system = builtins.currentSystem;
})
