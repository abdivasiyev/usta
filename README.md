# usta

A desktop client to connect with your forge.

usta shows your pull requests, issues and notifications from GitHub, GitLab
and Forgejo in one fast native window. It is written in Haskell with GTK 4.

The interface is in place and runs on sample data; talking to the forges
comes next. The design lives in [docs/DESIGN.md](docs/DESIGN.md).

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

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

[Apache-2.0](LICENSE)
