# usta

A desktop client to connect with your forge.

usta shows your pull requests, issues and notifications from GitHub, GitLab
and Forgejo in one fast native window. It is written in Haskell with GTK 4.

Forgejo works end to end: inbox, pull request reviews on local git
diffs, issues, repositories, an offline cache. GitHub and GitLab come
next. The design lives in [docs/DESIGN.md](docs/DESIGN.md).

## Install

Each [release](https://github.com/abdivasiyev/usta/releases) has
`usta.dmg` for macOS and `usta-x86_64.AppImage` for Linux. Installed,
usta opens `usta://<forge link>` links, such as
`usta://codeberg.org/forgejo/forgejo/pulls/1`, from anywhere on the
system. Once a day it checks for a newer release (Settings > Updates).

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

See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

[Apache-2.0](LICENSE)
