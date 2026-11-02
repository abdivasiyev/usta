# Contributing to usta

Thanks for helping. The [README](README.md) explains how to build and run
usta; [docs/DESIGN.md](docs/DESIGN.md) is the design every screen follows.

## Getting set up

Work inside `nix develop`. Besides GHC and cabal, it points git at the hooks
in [`.githooks`](.githooks), which check the rules below on every commit.

## Pull requests

- One change per pull request, and keep it small: four files or fewer is a
  good limit. Split bigger work into a stack of pull requests.
- Commit messages follow [Conventional Commits](https://www.conventionalcommits.org):
  `feat(ui): add sync status chip`, `fix(ui): …`, `docs: …`.
- Pull requests are squash-merged, so the title becomes the commit on
  `master`.
- Build with `nix build` before you open one. For UI changes, open the
  screens you touched in light and dark mode, and say what you checked.

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
