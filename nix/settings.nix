# The choices programs.usta.settings takes, in Nix's words, and how they
# become config.json: usta's own names, with unset ones left out so they
# stay the user's to choose in Settings.
lib: let
  inherit (lib) mkOption types;
  intervals = {
    "1m" = "EveryMinute";
    "2m" = "EveryTwoMinutes";
    "5m" = "EveryFiveMinutes";
    "15m" = "EveryFifteenMinutes";
    manual = "Manually";
  };
  themes = {
    system = "System";
    light = "Light";
    dark = "Dark";
  };
  flag = description:
    mkOption {
      type = types.nullOr types.bool;
      default = null;
      inherit description;
    };
in {
  option = mkOption {
    default = {};
    description = ''
      Choices fixed for usta. Each one set here shows in usta's Settings
      as "Set in Nix" and cannot be changed there; the ones left unset
      stay the user's.
    '';
    type = types.submodule {
      options = {
        interval = mkOption {
          type = types.nullOr (types.enum (builtins.attrNames intervals));
          default = null;
          example = "5m";
          description = "How often usta syncs the forges.";
        };
        theme = mkOption {
          type = types.nullOr (types.enum (builtins.attrNames themes));
          default = null;
          example = "dark";
          description = "The light or dark theme, or the system's.";
        };
        notifications = flag "Desktop notifications for review requests, mentions and failed checks.";
        onlyInBackground = flag "Notify only while usta is not the active window.";
        openLinks = flag "Open forge links in usta rather than the browser.";
        checkUpdates = flag "Check once a day whether a newer usta is out.";
        browser = mkOption {
          type = types.nullOr types.str;
          default = null;
          example = "app.zen-browser.zen";
          description = "On macOS, the bundle id of the browser links usta does not open go to; the system's default when null.";
        };
      };
    };
  };

  # The config.json usta reads.
  toConfig = s:
    lib.filterAttrs (_: v: v != null) {
      interval =
        if s.interval == null
        then null
        else intervals.${s.interval};
      theme =
        if s.theme == null
        then null
        else themes.${s.theme};
      notify = s.notifications;
      inherit (s) onlyInBackground openLinks checkUpdates browser;
    };
}
