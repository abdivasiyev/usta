# Changelog

## Unreleased

### Fixed

- The Releases page reads releases published by a bot, such as
  github-actions[bot].
- Job logs color 256-color and RGB text as plain text, not red.
- Files whose names the icon map writes in capitals, ".Rhistory" or
  "Dockerfile", have their icons.
- Mermaid flowcharts draw links with arrows at both ends, circles and
  crosses, and statements on the header's line, "graph TD; A-->B".
- A comment or a done made offline is no longer lost when the waiting
  ones are sent, nor sent twice.
- Drafts, viewed files and pins are written safely: two changes at once
  keep both, and stopping half way loses nothing.
- Syncing GitHub no longer fails now and then on accounts with many
  repositories.
- A merged GitHub pull request whose branch is gone tells its readiness.
- A new window no longer loads usta's styles again, and a closed one
  stops following the system's theme.
- A repository deleted on the forge, or one you lost access to, leaves
  the Repositories page at the next full sync.

### Added

- `usta --version`.
- Tests: unit, property, behaviour, contract, security, golden,
  migration and UI tests, and benchmarks (see docs/TESTING.md).

## 0.1.0 (beta)

The first release of usta: a native desktop client for GitHub and Forgejo,
for macOS and Linux. It is a beta: things will change, and some parts are
still missing (see Not yet, below).

### Inbox and notifications

- One inbox for review requests, mentions, assignments and failed checks,
  across every forge you sign in to; mark done, all at once, or move back.
- Desktop notifications for what needs you, also while usta is in front.
- Pull request and issue lists with tabs, filters by forge, repository,
  label and author, sorting, and the forge's counts.

### Pull requests

- The conversation in one live timeline: comments, reviews, commits and
  events, with replies on Enter, reactions, editing and drafts that stay.
- Reviews on real git diffs from a local mirror, or the forge's API: split
  or unified, whitespace hidden, changed words marked, more context on
  demand, and comments on a line or a range.
- The file's header stays at the top of a long diff, and files you viewed
  stay viewed until they change.
- Reviewers, assignees, labels and milestones; merge, squash or rebase,
  auto-merge, update the branch, draft and ready.
- Checks with live job logs in color, folded groups and steps; run a
  workflow with its inputs, re-run failed jobs or cancel a run (GitHub).
- New pull requests, also from a fork; check out a pull request locally.

### Repositories

- Every repository you can see, filtered by forge and owner, and any other
  repository a link or a search opens.
- Files with type icons, syntax colors, blame and history; README and
  Markdown with Mermaid diagrams drawn natively.
- Commits, releases, insights and the repository's actions.
- Fork, clone, star, pin, mirror, and create new repositories.

### Everywhere

- ⌘K / Ctrl K searches pull requests, issues and repositories on every
  forge, and runs commands; links you paste open their page.
- Keyboard first: j and k, Enter, e; Emacs keys in every text field.
- Works offline from its cache, and syncs in the background.
- usta can be the default browser on macOS: forge links from any app open
  in usta, every other link goes on to the browser you choose.
- Light and dark themes; settings can be set from Nix (Home Manager,
  NixOS, nix-darwin).
- Sign in with a token (GitHub, Forgejo) or in the browser (Forgejo);
  tokens stay in the system keyring.
- A daily check for a newer release, and Report a problem with the log.

### Not yet

- GitLab.
- Signing in to GitHub in the browser; tokens work.
- A signed macOS app: macOS asks once before opening it.
- Forgejo review texts in the conversation, and re-running or cancelling
  Forgejo Actions runs (Forgejo has no API for them).
