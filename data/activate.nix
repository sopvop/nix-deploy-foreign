{ nixpkgs ? import <nixpkgs> {} }:
let
  ghc = nixpkgs.haskellPackages;
  lib = nixpkgs.haskell.lib;
  d = ghc.callCabal2nix "nix-deploy-foreign-activate" ./activate {};
in
 lib.justStaticExecutables d
