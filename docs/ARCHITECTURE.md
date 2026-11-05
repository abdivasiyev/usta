# How usta is built

This is the map for working on usta's code. [CONTRIBUTING.md](../CONTRIBUTING.md)
covers setting up, the rules for pull requests and how to test;
[DESIGN.md](DESIGN.md) is how every screen looks and behaves.

## The shape of it

usta is one Haskell program (GHC 9.12, `GHC2024`) with a GTK 4 window. It
never talks to a forge from the window directly:

```
 GitHub / Forgejo ──► Forge record ──► Sync ──► SQLite cache ──► Bus ──► pages reload
        ▲                                                                    │
        └─────────── Details, Accounts (writes and detail views) ◄──── Source ┘
```

- **A forge is a record of operations**, `Usta.Forge.Forge`: list
  notifications, pull requests, issues and repositories, read a
  conversation or a diff, comment, review, merge, fork, run a workflow and
  so on. `Usta.Forge.GitHub` and `Usta.Forge.Forgejo` each build one from
  an account's token; `Usta.Forge.Sample` builds one from fixed data for
  `--sample` and for checks.
- **Sync** (`Usta.Sync`) asks every account's forge for its lists on a
  timer, saves what changed in the SQLite cache and tells the window which
  list changed, over `Usta.Bus`. A list that did not change (an ETag answer
  of 304 on GitHub) costs nothing.
- **The cache** (`Usta.DB.*`) holds what was synced, so usta opens at once
  and works offline. Pages read lists from it, never from the network.
- **Details** (`Usta.Details`) reads what is too big to sync: diffs,
  commits, files, blame. Each read goes to the repository's local git
  mirror when there is one and to the forge's API otherwise, and says which
  it used (`From`).
- **Accounts** (`Usta.Accounts`) signs in and out, keeps tokens in the
  system keyring (`Usta.Keyring`), and hands out each account's `Forge`.
- **Source** (`Usta.UI.Source`) is everything the window can ask for, as
  one record. It is built in layers (`fromDatabase`, then `withAccounts`,
  `withMirrors`, `withDetails`), and `Source.samples` is a whole one from
  fixed lists, for the preview flags. Pages only see a `Source`.

## Where things are

| Path | What is there |
|------|---------------|
| `app/Main.hs` | Starts usta: the cache, sync, accounts, the window; the preview flags |
| `src/Usta/Types/` | The application's types, each parsed from raw data once (see Conventions) |
| `src/Usta/Forge.hs` | The `Forge` record and the types its operations take |
| `src/Usta/Forge/GitHub.hs`, `GitHub/Api.hs` | GitHub: REST and GraphQL routes, and the adapter |
| `src/Usta/Forge/Forgejo.hs`, `Forgejo/Api.hs` | Forgejo: REST routes, and the adapter |
| `src/Usta/Forge/Http.hs` | The HTTP client the forges share: running routes, 304s, redirects |
| `src/Usta/Forge/Sample.hs` | A forge made of fixed data |
| `src/Usta/Sync.hs` | The sync loop |
| `src/Usta/DB/Schema/` | One module per table; `Schema.hs` gathers them for migrations |
| `src/Usta/DB/*.hs` | Reading and writing each table |
| `src/Usta/Details.hs`, `Git.hs`, `Mirrors.hs` | Mirror-or-forge reads, typed git calls, the background mirror manager |
| `src/Usta/Accounts.hs`, `Keyring.hs`, `OAuth.hs` | Sign-in, tokens in the keyring, browser sign-in |
| `src/Usta/Preferences.hs` | Settings, and the ones fixed by Nix (`config.json`) |
| `src/Usta/Mermaid.hs`, `Diagram/` | Mermaid parsed and laid out natively, drawn with GTK |
| `src/Usta/UI/Window.hs` | The window: header, sidebar, pages, and what each page's actions do |
| `src/Usta/UI/Source.hs` | What the window can ask for |
| `src/Usta/UI/Pages/` | One module per page; pull request tabs under `PullRequest/` |
| `src/Usta/UI/Components/` | Reusable widgets: buttons, pickers, diffs, Markdown, dialogs |
| `src/Usta/UI/Style.hs` | Color tokens and the CSS of the parts that have none of their own |
| `cbits/` | The few macOS and Linux calls Haskell cannot make: keyring, appearance, links, notifications |
| `data/` | The app icon, file-type icons, macOS `Info.plist`, Linux desktop files |
| `nix/` | The Home Manager, NixOS and nix-darwin modules |
| `site/` | The landing page; [SITE.md](SITE.md) says how it is published |
| `docs/` | This file, the design, and the design's images |

## Conventions

These hold across the code. Following them keeps a change reviewable.

### Types and data

- **Parse, don't validate.** Raw data (JSON, query strings, database
  columns, command output) becomes a typed value once, at the edge, with
  `Usta.Types.Parse` (`parse :: Raw a -> Either Text a`, `unparse`). A
  `RepoName`, `Host`, `Login` or `Number` inside the program is always
  valid; nothing checks it again.
- **Application types and table rows are separate.** `Usta/Types/*.hs` is
  what the program works with; `Usta/DB/Schema/*.hs` is what is stored.
  `Usta/DB/*.hs` converts between them. Columns reuse the value types
  (`RepoName`, `Count`), so the conversions are field by field.
- **Records, read with record dot.** `DuplicateRecordFields`,
  `NoFieldSelectors` and `OverloadedRecordDot` are on everywhere: write
  `pr.title`, never a selector function. Prefer records to positional
  fields, and give every type a standalone kind signature.

### Talking to the outside

- **Every HTTP client is a servant `NamedRoutes` record** (`mode :- …`),
  never `:<|>` or hand-built URLs. A forge's routes live in its `Api`
  module; the adapter calls them as `Api.api.pullRequest owner repo n`.
- **Every command is a typed function.** git goes through `Usta.Git`
  (typed-process); raw arguments exist only inside it.
- **Platform differences go through `Usta.Types.Platform`.** Don't read
  `System.Info` anywhere else. C code in `cbits/` is for what has no
  Haskell or GTK API, with a stub for the other platform.

### The window

- **Never block the main thread.** Anything that waits on the network or
  the disk runs in `forkIO` and comes back with `GLib.idleAdd` to touch
  widgets. `Button.busy` does this for a button that starts work.
- **Use the components.** Buttons are `Button.new` (and its small and menu
  variants), never a bare `Gtk.Button` for an action; dropdowns that filter
  a list are `Choices`; lists that page from the forge use `Pager`.
- **Follow the design.** A new screen or a visible change starts as a
  design that is agreed on before code (see Proposing a design in
  CONTRIBUTING.md). Colors come from the tokens in `Style.hs`; check light
  and dark.
- **Loading shows where it happens.** A page that loads shows skeletons or
  a spinner in place; background reloads run quietly (`Usta.UI.Quiet`) so
  the page does not jump.
- **Redraw in place.** A change on a page (a new comment, another diff
  option) redraws the part it changes, keeping the scroll position, not the
  whole page.

### Writing code

- Small functions with plain names; reuse what the libraries and the code
  already have before writing your own.
- Comments say why, in plain sentences; a Haddock line on each exported
  function says what it is for. Don't narrate what the code does.
- `-Wall` is on: a change adds no warnings.

## Adding a forge operation

Say usta should be able to lock a conversation.

1. **Routes.** Add the route to `GitHub/Api.hs` and `Forgejo/Api.hs`, as
   fields of their route records, with typed captures and bodies.
2. **The operation.** Add a field to `Forge` in `Usta/Forge.hs`, with a
   Haddock line: `lock :: RepoName -> Number -> IO ()`.
3. **Each forge.** Implement it in `Usta.Forge.GitHub`,
   `Usta.Forge.Forgejo` and `Usta.Forge.Sample`. When a forge cannot do
   it, throw `Refused` with a sentence the user can read; the UI then
   leaves the control out or says why.
4. **Source.** Add a field to `Source` (`Usta/UI/Source.hs`), give it a
   default in `fromDatabase` and `samples`, and the real implementation in
   `withAccounts` through `onRepo host repo`, which finds the account for
   the repository.
5. **The page.** Call it from the page's actions in `Window.hs`, off the
   main thread, and show the result where the user acted.

Build both forges in the same change, or say in the pull request which one
is left and why.

## Adding a page or a component

- A page is a module in `Usta/UI/Pages/` with a `new` that takes its data
  and an `Actions` record of what it can ask for, and returns its widget.
  It does no I/O of its own beyond calling those actions. `Window.hs`
  builds the `Actions` from the `Source`.
- A component goes in `Usta/UI/Components/` when two places need it.
- Each module with styles exports a `css :: Text`; add it to the list at
  the end of `Window.hs`.
- Add the module to `other-modules` in `usta.cabal`.

## The cache and migrations

Tables are `persistent` entities, one module per table under
`Usta/DB/Schema/`, gathered by `Usta/DB/Schema.hs`. usta runs the
migrations at start, so adding a table or a nullable column needs nothing
more. Renaming or removing a column, or making one required, needs a
migration that keeps people's caches working; say so in the pull request.
The cache can always be rebuilt from the forges (Settings › Clear cache).

## Logs

usta writes `usta.log` in its data folder, beside the cache
(`~/Library/Application Support/usta` on macOS, `$XDG_DATA_HOME/usta`,
usually `~/.local/share/usta`, on Linux): syncs, links opened, errors.
"Report a problem" in the command palette (⌘K) shows its end, ready to
attach to an issue.
