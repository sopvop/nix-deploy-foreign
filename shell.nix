{ pkgs ? import ~/.nix-defexpr/channels_root/nixpkgs {} }:
let d = pkgs.haskellPackages.callCabal2nix "nix-deploy-foreign" ./. {};
in d.env
