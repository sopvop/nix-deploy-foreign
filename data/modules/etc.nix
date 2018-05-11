{ config, lib, pkgs, ...}:
with lib;

let
mkFile = name: config: pkgs.writeTextFile {
  name = "etc-${baseNameOf name}";
  destination = "/etc/${name}";
  text = config.body;
};

fileSubmodule = { name, config, ... }:
{
  options = {
    body = mkOption {
      type = types.str;
    };
    file = mkOption {
      internal = true;
      type = types.package;
    };
  };
  config.file = mkFile name config;
};

in {
options = {
  etc = mkOption {
    default = {};
    type = with types; attrsOf (submodule fileSubmodule);
  };
};

config = {
  paths = mapAttrsToList (k: v: v.file) config.etc;
};

}
