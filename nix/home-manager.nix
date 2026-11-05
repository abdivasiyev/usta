# Home Manager: programs.usta installs usta (on macOS as usta.app, which
# Home Manager links into ~/Applications) and writes its settings to
# ~/.config/usta/config.json.
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
    home.packages = [cfg.package];
    xdg.configFile."usta/config.json" = lib.mkIf (fixed != {}) {
      source = (pkgs.formats.json {}).generate "usta-config.json" fixed;
    };
  };
}
