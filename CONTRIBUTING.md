# Contributing to usta

Thanks for helping. This page is how to work on usta: setting up, running
it against a forge, what a pull request needs, and how to report a bug.

- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) is how the code is built:
  the parts, where they are, the conventions, and how to add a forge
  operation or a page.
- [docs/DESIGN.md](docs/DESIGN.md) is the design every screen follows.
- [docs/SITE.md](docs/SITE.md) is how the landing page is published.

## Getting set up

usta builds with [Nix](https://nixos.org) and flakes; nothing else needs
installing. On macOS and Linux:

```sh
git clone https://github.com/abdivasiyev/usta.git
cd usta
nix develop          # GHC 9.12, cabal, haskell-language-server, GTK 4
cabal build
cabal run usta
```

Work inside `nix develop`. Besides the tools, it points git at the hooks
in [`.githooks`](.githooks), which check the rules below on every commit,
and it sets the paths GTK needs for its settings and icons. Run anything
that opens a window from inside it, or icons go missing.

`nix build` builds the app the way it ships (`./result/bin/usta`; on macOS
`nix build .#usta-app` builds `usta.app`). Run it before opening a pull
request: it starts from scratch, so it shows warnings `cabal build` has
cached away.

## Running usta while you work

`cabal run usta` opens your real cache and accounts. For work that should
not touch them, there are flags:

| Flag           | Opens                                                        |
|----------------|--------------------------------------------------------------|
| `--sample`     | two sample forges in a fresh cache, syncing and changing     |
| `--gallery`    | every component on one page                                  |
| `--welcome`    | the first-run screen, before any account                     |
| `--first-sync` | the inbox while the first sync runs                          |

`--sample` covers lists, the inbox and the pages built from them. Anything
that writes (comments, reviews, merges, forks, workflows) or reads a
conversation needs a real forge.

### A forge of your own

Test what writes against a forge you own, never against other people's
repositories: comments notify people, and starting a workflow runs their
CI. The easiest is a local [Forgejo](https://forgejo.org) in a container.
usta speaks HTTPS to forges, so put a TLS proxy in front of it:

```sh
docker run -d --name usta-forgejo -p 127.0.0.1:3300:3000 \
  -e FORGEJO__server__ROOT_URL=https://localhost:3443/ \
  codeberg.org/forgejo/forgejo:13
caddy reverse-proxy --from localhost:3443 --to 127.0.0.1:3300
```

Open `https://localhost:3443`, finish the install page, create a user
and an access token (Settings › Applications), then add the forge in usta
with host `localhost:3443` and the token. Caddy signs with its own local
authority; `caddy trust` adds it to the system, and git needs
`GIT_SSL_CAINFO` pointing at Caddy's root certificate for mirrors. A
second user lets you test reviews and review requests. For Actions,
register a [Forgejo runner](https://forgejo.org/docs/latest/admin/actions/)
against the container.

For GitHub, sign in with a personal access token (scopes `repo`,
`read:org`, `read:user`, `notifications`) and test writes on a repository
you created for it.

## Making a change

1. **Talk first about anything big.** Open an issue for a new feature or a
   change in how something works, so we agree before you build it.
2. **Design visible changes first.** A new screen, dialog or control starts
   as a design (see Proposing a design), agreed on before code.
3. **Follow the code around your change.** The conventions are in
   [ARCHITECTURE.md](docs/ARCHITECTURE.md#conventions): parse raw data
   once into typed values, servant `NamedRoutes` for every HTTP client,
   record dot for fields, nothing slow on the main thread, the existing
   components for buttons and pickers.
4. **Do it for every forge.** An operation works on GitHub and Forgejo, or
   the pull request says which is left and why.
5. **Check it yourself** (see Checking a change).

### Proposing a design

Draw the screen or control in the style of [DESIGN.md](docs/DESIGN.md):
its states (empty, loading, failed, full), light and dark, and what each
control does. A sketch, a mockup or an HTML page all work; put it in the
issue. Once it is agreed, the code follows it.

### Checking a change

usta has no test suite yet, so a change is checked by running it:

- `nix build` passes with no new warnings.
- Open every screen you touched, in light and dark, at a narrow and a wide
  window, and on the states the change affects: empty, loading, failed,
  many items.
- For a forge operation, try it on GitHub and Forgejo, including what
  happens when the forge refuses it or is offline.
- Say in the pull request what you checked and how.

## Pull requests

- One change per pull request, and keep it small: four files or fewer is a
  good limit. Split bigger work into a stack of pull requests.
- Commit messages follow [Conventional Commits](https://www.conventionalcommits.org):
  `feat(ui): add sync status chip`, `fix(ui): …`, `docs: …`.
- Pull requests are squash-merged, so the title becomes the commit on
  `master`.
- Build with `nix build` before you open one. For UI changes, open the
  screens you touched in light and dark mode, and say what you checked.

## Reporting a bug

Open an [issue](https://github.com/abdivasiyev/usta/issues) with what you
did, what you expected and what happened, your system (macOS or Linux and
its version), and how you installed usta. "Report a problem" in the
command palette (⌘K) collects the end of usta's log for you; read it
before you attach it, and remove anything you would rather not share.

## Releases

Maintainers cut releases: a tag `v<version>` matching `usta.cabal` runs
[`.github/workflows/release.yml`](.github/workflows/release.yml), which
builds `usta.dmg`, the AppImage, a `.deb` and an `.rpm` and attaches them
to a new GitHub release. The landing page's download links follow the
newest release on their own.

## License

usta is licensed under [Apache-2.0](LICENSE). By contributing, you agree
your contribution is licensed under it too.

## Using AI tools

You may use AI tools to help you write code, tests or docs. What you submit
is still yours, and these rules say what that means.

1. **You are the author.** Commit under your own name. Don't credit a tool:
   no `Co-Authored-By:` lines for AI tools and no "Generated with …"
   footers, in commits or pull request descriptions. The `commit-msg` hook
   rejects them.
2. **Understand every line.** You must be able to explain any part of your
   change in review without going back to the tool. If you can't explain
   it, don't submit it.
3. **Review it like someone else's code.** Read the whole diff before you
   commit. Remove what the change doesn't need: extra abstractions, unused
   code, comments that only narrate the code.
4. **Run it.** A tool saying it works is not testing. Build it, run it, and
   check the cases your change touches.
5. **Fit the project, not the tool's habits.** Follow the design and the
   code around your change: its components, naming and style. Don't add
   dependencies without discussing them first.
6. **Make sure you may contribute it.** Everything here is licensed under
   Apache-2.0. You are responsible for your contribution being yours to
   license, so don't submit generated code that copies someone else's.
7. **Answer review yourself.** Reviewers talk to you, not to your tool.
   Reply in your own words, and fix what they point out by understanding
   it.
8. **Keep tool setup out of the repository.** Files that configure AI tools
   (`.claude/`, `CLAUDE.md`, `AGENTS.md`, `.cursor/`, `.cursorrules` and
   the like) stay on your machine. The `pre-commit` hook rejects them.

You don't need to say whether you used a tool. You do need to stand behind
the change as if you had written every line yourself, because you are
answerable for it.
