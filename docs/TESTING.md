# Testing usta

usta has three suites and a set of smoke tests:

| What | Command | Needs |
|------|---------|-------|
| Everything without a window | `cabal test usta-test` | nothing |
| The window, driven as a person would | `cabal test usta-ui-test` | a display |
| Speed | `cabal bench` | nothing |
| What is shipped | `nix flake check` | nix |

Run them inside `nix develop`. `nix build` runs `usta-test` too.

## usta-test

`test/Main.hs` gathers the modules under `test/`, one per module of
usta's, named after it with `Tests` (`Usta.Types.LinkTests` tests
`Usta.Types.Link`). The tests run with a home folder of their own, so
what usta keeps beside its cache never touches yours.

- **Unit tests** (`tasty-hunit`): a function's answer for chosen inputs.
- **Property tests** (`tasty-hedgehog`): what holds for any input. Every
  domain type keeps `parse . unparse ≡ Right` (`ParseTests`); a diff
  written out reads back the same; broken diagrams never throw; lanes of
  the commit graph connect.
- **Behaviour tests**: whole flows on the sample forge and a real SQLite
  cache: syncing (`SyncTests`), what the cache keeps (`CacheTests`),
  signing in and out (`AccountsTests`), drafts, pins and waiting actions
  (`LocalFilesTests`), browser sign-in (`OAuthTests`).
- **Contract tests** (`Contract/`): the GitHub and Forgejo code against
  answers recorded from the real forges, in `test/contract/`. Every
  reading operation reads its answer to the end, and a request that was
  not recorded fails.
- **Write tests** (`Contract/WritesTests`): every write usta sends either
  forge (comments, reviews, merges, labels, forks, workflows…) is one
  the forge's own description of its API takes: Forgejo 13's swagger and
  GitHub's OpenAPI description and GraphQL schema, the parts usta uses,
  in `test/contract/`. A stand-in answers the reads as recorded and keeps
  the writes, so nothing changes on a real forge.
- **Git tests** (`GitMirrorTests`, `MirrorsTests`): usta's git commands
  on a real repository made for each test, its mirrors, and reads that
  take the mirror or the forge.
- **Security tests**: tokens stay with their forge and out of errors.
- **Golden tests**: every diagram's layout and the syntax colors, in
  `test/golden/`.
- **Migration tests**: caches written by earlier versions, in
  `test/caches/`, still open and keep what they hold.

Run one test or group with `-p`:

```sh
cabal test usta-test --test-options='-p /Sync/'
```

### Golden files

A change to a layout or to the colors shows as a diff of `test/golden/`.
When it is meant, keep the new output and commit it with the change:

```sh
cabal test usta-test --test-options='-p Golden --accept'
```

### Recording the forges' answers

Record again when usta asks a forge for something new, or a forge
changes what it sends.

Forgejo's come from the local test server (see "A forge of your own" in
[CONTRIBUTING.md](../CONTRIBUTING.md)), signed in as a second user with
access to `tester/usta` and `tester/made-by-usta`:

```sh
USTA_RECORD=forgejo USTA_FORGEJO_TOKEN_FILE=path/to/token \
  cabal test usta-test --test-options='-p /Contract.forgejo/'
```

GitHub's are asked through your `gh`, so no token passes through the
tests, and only about `abdivasiyev/usta`: searches are asked about it
instead of you, repositories other than usta are cut out, and your
notifications and organizations are made up. It takes a few minutes.

```sh
USTA_RECORD=github cabal test usta-test --test-options='-p /Contract.github/'
```

Read the diff of `test/contract/` before committing it.

### GitHub's API descriptions

`test/contract/github-openapi.json` and `github-schema.graphql` are cut
from GitHub's published ones down to what usta's writes use. When usta
sends GitHub something new, cut them again as
`test/contract/trim-github.py` says.

### A cache for each release

Each release keeps the cache it writes, so later versions are tested
against it. At a release:

```sh
USTA_WRITE_CACHE=0.2.0 cabal test usta-test --test-options='-p /write a cache/'
```

and commit `test/caches/0.2.0.db`.

## usta-ui-test

`test-ui/` builds usta's window from the sample data and drives it: every
page in the light and the dark theme, the search's shortcut, Enter and
the Emacs keys in text fields, the filters, the dialogs, a pull request's
files. GTK runs on the main thread; the tests hand it work with
`onMain` (`test-ui/Harness.hs`). Keys reach widgets through their own
controllers, as GTK would send them; `test-ui/keys.c` sends a key and
reads whether a handler took it.

The tests open windows on your screen, one at a time; leave the
keyboard alone while they run. Screenshots of every page go to
`dist-newstyle/ui-shots/`: look at them for a visible change.

## Benchmarks

`bench/Main.hs` times the heavy parts: big diffs, syntax colors, big
diagrams, the commit graph, and the cache with a thousand items.
`bench/baseline.csv` was taken on an Apple silicon Mac; compare on a
like machine:

```sh
cabal bench --benchmark-options='--baseline bench/baseline.csv --fail-if-slower 20'
```

and write a new baseline with `--csv bench/baseline.csv` when a change
is meant to be slower or faster.

## nix flake check

Builds usta with its tests, checks that the installed program starts and
says its version, and on each system what is shipped: on Linux the
desktop entry and the AppStream data, on macOS `usta.app`'s icon,
version and program.

## Writing a test

- Put it in the module that tests the code it is about, or start one
  beside it, and add the module to `test/Main.hs` and `usta.cabal`.
- A bug fix comes with the test that would have caught it.
- Test behaviour through the code's own types: build values with their
  parsers, and use `Forge.Sample` for a forge.
- Nothing in a test reaches the network or your real files.
