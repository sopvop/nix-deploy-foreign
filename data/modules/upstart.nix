{ config, lib, pkgs, ...}:
with lib;

let

  mkService = name: config: pkgs.writeTextFile {
    name = "upstart-${name}";
    destination = "/etc/init/${name}.conf";
    text = config.body;
  };

  serviceSubmodule = { name, config, ... }:
  {
    options = {
      body = mkOption {
        type = types.str;
      };

      unit = mkOption {
        internal = true;
        type = types.package;
      };
    };
    config.unit = mkService name config;
  };

in {

options = {

  upstart = mkOption {
    default = {};
    type = with types; attrsOf (submodule serviceSubmodule);
  };

};

config = {
   paths = mapAttrsToList (k: v: v.unit) config.upstart;
};

}
