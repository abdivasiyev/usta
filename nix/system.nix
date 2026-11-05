# NixOS and nix-darwin: programs.usta installs usta for every user and
# writes settings that hold for all of them to /etc/xdg/usta/config.json;
# a user's own Home Manager settings win over these.
self: {
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.programs.usta;
  settings = import ./settings.nix lib;
  fixed = settings.toConfig cfg.settings;
in {
  options.programs.usta = {
    enable = lib.mkEnableOption "usta, a native client for code forges";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.${pkgs.stdenv.hostPlatform.system}.default;
      defaultText = lib.literalExpression "usta.packages.\${system}.default";
      description = "The usta to install.";
    };
    settings = settings.option;
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [cfg.package];
    environment.etc."xdg/usta/config.json" = lib.mkIf (fixed != {}) {
      source = (pkgs.formats.json {}).generate "usta-config.json" fixed;
    };
  };
}
