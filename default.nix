{ pkgs ? import <nixpkgs> {} }:
let
  data = pkgs.copyPathToStore ./data;
  d = pkgs.haskellPackages.callCabal2nix "nix-deploy-foreign" ./. {};
in
  pkgs.haskell.lib.overrideCabal d (der:{
    executableToolDepends = [pkgs.makeWrapper];
    postFixup =
    ''
      wrapProgram $out/bin/nix-deploy-foreign \
        --set NIX_DEPLOY_FOREIGN_DATA ${data}
    '';
  })
