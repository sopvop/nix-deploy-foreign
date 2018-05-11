{ nixpkgs ? import <nixpkgs> {} }:
let
  ghc = nixpkgs.haskellPackages;
  d = ghc.callCabal2nix "activate" <activate-src> {};
in
 lib.justStaticExecutables d
