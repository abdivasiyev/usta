# usta

[![Tests](https://github.com/abdivasiyev/usta/actions/workflows/test.yml/badge.svg)](https://github.com/abdivasiyev/usta/actions/workflows/test.yml)
[![Release](https://img.shields.io/github/v/release/abdivasiyev/usta?label=release)](https://github.com/abdivasiyev/usta/releases/latest)
[![Build the release](https://github.com/abdivasiyev/usta/actions/workflows/release.yml/badge.svg)](https://github.com/abdivasiyev/usta/actions/workflows/release.yml)
[![Landing page](https://github.com/abdivasiyev/usta/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/abdivasiyev/usta/actions/workflows/pages/pages-build-deployment)

A desktop client to connect with your forge.

usta shows your pull requests, issues and notifications from GitHub and
Forgejo in one fast native window, for macOS and Linux. It is written in
Haskell with GTK 4. See it at <https://usta.azizovich.uz>.

**usta is in beta** (0.1.0). GitHub and Forgejo work end to end: the
inbox, pull request reviews on local git diffs, checks and workflows,
issues, repositories, and an offline cache. GitLab comes next. What is in
each release is in [CHANGELOG.md](CHANGELOG.md); the design lives in
[docs/DESIGN.md](docs/DESIGN.md).

## Install

Each [release](https://github.com/abdivasiyev/usta/releases) has
`usta.dmg` for macOS, and `usta-x86_64.AppImage`, a `.deb` and an `.rpm`
for Linux. The macOS app is not signed by Apple yet: the first time,
open it with right-click › Open, or allow it in System Settings › Privacy
& Security.

Installed, usta opens `usta://<forge link>` links, such as
`usta://codeberg.org/forgejo/forgejo/pulls/1`, from anywhere on the
system. Once a day it checks for a newer release (Settings > Updates).

### With Nix

The flake has a Home Manager module, for macOS and Linux, and NixOS and
nix-darwin modules. On macOS they install `usta.app` (Home Manager links
it into `~/Applications/Home Manager Apps`), so `usta://` links and
notifications work.

```nix
{
  inputs.usta.url = "github:abdivasiyev/usta";

  # in a Home Manager configuration
  imports = [ inputs.usta.homeManagerModules.default ];
  programs.usta = {
    enable = true;
    settings = {
      interval = "5m";        # "1m", "2m", "5m", "15m" or "manual"
      theme = "dark";         # "system", "light" or "dark"
      notifications = true;
      onlyInBackground = false;
      openLinks = true;
      checkUpdates = false;
    };
  };
}
```

Each setting is optional. One that is set shows as "Set in Nix" in
usta's Settings and is not changed there; the rest stay yours. Home
Manager writes them to `~/.config/usta/config.json`.
`nixosModules.default` and `darwinModules.default` take the same
`programs.usta` options for every user, written to
`/etc/xdg/usta/config.json`; a user's own Home Manager settings win
over them. `overlays.default` adds `pkgs.usta`.

## Run

With [Nix](https://nixos.org) and flakes enabled:

```sh
nix run github:abdivasiyev/usta
```

## Build

Everything comes from the flake: GHC 9.12, GTK 4 and the Haskell
dependencies.

```sh
nix build        # the app, in ./result/bin/usta
nix develop      # a shell with GHC, cabal and haskell-language-server
```

Inside `nix develop`:

```sh
cabal build
cabal run usta
```

`cabal run usta -- <flag>` opens other screens, for working on them:

| Flag           | Opens                                    |
|----------------|------------------------------------------|
| `--gallery`    | every component on one page              |
| `--welcome`    | the first-run screen, before any account |
| `--first-sync` | the inbox while the first sync runs      |

## Release

`scripts/macos-app.sh` builds `dist/usta.app` and `dist/usta.dmg` with
every library inside, so they run without Nix; set `USTA_SIGN_IDENTITY`
to sign with a Developer ID. Pushing a tag `v<version>` that matches
`usta.cabal` runs `.github/workflows/release.yml`, which builds both
downloads and attaches them to a new release; its header lists the
secrets for signing and notarizing.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for setting up, testing and pull
requests, and [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) for how the
code is built. The landing page is <https://usta.azizovich.uz>.

## License

[Apache-2.0](LICENSE)
