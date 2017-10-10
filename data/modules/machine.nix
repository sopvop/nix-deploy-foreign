{ config, pkgs, lib, ...}:
with lib;
let
  cfg = config;
  machineOptions = {
    host = mkOption {
       type = types.str;
    };
    user = mkOption {
       type = types.nullOr types.str;
       default = "deploy";
    };
    name = mkOption {
      internal = true;
      type = types.str;
    };
    profile = mkOption {
      type = types.path;
      default = "/nix/var/nix/profiles/deploy";
    };

    paths = mkOption {
      default = [];
      type = types.listOf types.package;
    };
    result = mkOption {
      internal = true;
      type = types.package;
    };
    nixpkgs = mkOption {
      type = types.attrs;
      default = cfg.nixpkgs;
    };
  };

  machineModule = {name, config, ...}:
  { options = machineOptions;
    config.name = name;
    config.result = pkgs.buildEnv {
      name = "deploy";
      paths = config.paths;
    };
   config._module.args.pkgs = config.nixpkgs;
   imports = [./upstart.nix ] ++ cfg.machineModules;
  };

machineConfig = _:args: {
  name = args.name;
  host = args.host;
  user = args.user;
  profile = args.profile;
  env = args.result;
};

 _pkgs = config.nixpkgs;

in {

options = {
  machine = mkOption {
    default = {};
    type = with types; attrsOf (submodule machineModule );
  };

  deploy = mkOption {
    default = [];
    type = types.attrs;
    internal = true;
  };

  nixpkgs = mkOption {
    type = types.attrs;
    default = import <nixpkgs> {};
  };


  machineModules = mkOption {
    default = [];
    type = types.listOf types.path;
  };

};

config = {
  deploy = lib.mapAttrs machineConfig config.machine;
  config = {
    _module.args = {
       pkgs = _pkgs;
       pkgs_i686 = _pkgs.pkgsi686Linux;
    };
  };
};

}
