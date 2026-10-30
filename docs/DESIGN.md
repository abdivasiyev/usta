# usta design

The design spec for usta: principles, colors, layout and every screen.
Screenshots are in [design/](design/).

## Principles

- **Fast first.** Everything renders from a local cache; the network only
  syncs in the background. Diffs come from local bare clones, never the API.
- **Keyboard first.** Every screen has shortcuts, shown in a strip at the
  bottom. `Ctrl K` opens search and commands from anywhere.
- **One inbox for every forge.** GitHub, GitLab and Forgejo look the same;
  a colored dot shows which forge an item comes from.
- **Quiet look.** Neutral grays, one accent color, color only where it
  carries meaning (status, diff lines).

## Logo

An anvil with three sparks: usta means master craftsman. The source is
[design/logo.svg](design/logo.svg) (64 × 64 grid). The tile is always
`#2F5FD0` with a white anvil and `#F2C25C` sparks, in both themes; the
wordmark next to it is Inter Bold in the `text` color.

![Logo concepts, B chosen](design/logo-concepts.png)

## Typography

| Use | Font |
|---|---|
| Interface text | Inter, 14 px base |
| Code, branches, repository names, shortcuts | JetBrains Mono, 13 px |

## Color tokens

Every screen uses only these colors. Dark values keep text at 4.5:1 contrast
or better on their surfaces.

| Token | Light | Dark | Used for |
|---|---|---|---|
| `window` | `#FAFAFA` | `#16181C` | Window background |
| `surface` | `#FFFFFF` | `#1E2126` | Lists, cards, header bar |
| `sidebar` | `#F1F2F4` | `#191B1F` | Sidebars and file tree |
| `border` | `#E2E4E8` | `#2E3238` | Pane and card borders |
| `border-subtle` | `#EEF0F3` | `#262A30` | Row separators |
| `input-border` | `#D5D8DE` | `#3A3F47` | Inputs and secondary buttons |
| `text` | `#1B1D21` | `#E6E8EC` | Primary text |
| `text-muted` | `#5B616B` | `#9AA0A9` | Metadata, timestamps, counts |
| `accent` | `#2F5FD0` | `#7AA5FF` | Primary buttons, links, selection edge |
| `on-accent` | `#FFFFFF` | `#0E1420` | Text on accent buttons |
| `selected` | `#EEF3FD` | `#22314F` | Selected row |
| `success` | `#1F7A3A` | `#5BD38A` | Open, passed, additions |
| `danger` | `#B42318` | `#F28B82` | Failed, deletions, destructive |
| `warning` | `#8A5A00` | `#F2C25C` | Review requested, pending |
| `merged` | `#5B21B6` | `#C4A7FF` | Merged |
| `diff-add-bg` | `#E6F4EA` | `#18301F` | Added line background |
| `diff-del-bg` | `#FDECEA` | `#3A1D1D` | Deleted line background |

Forge dots: GitHub uses `text`, Forgejo `#C2410C` (dark `#F08A4B`),
GitLab `#6D28D9`.

![Color tokens](design/color-tokens.png)

### Dark theme

Dark screens use the same layouts with the dark token values.

![Inbox, dark](design/inbox-dark.png)
![Pull request files, dark](design/pull-request-files-dark.png)
![Search and commands, dark](design/command-palette-dark.png)

## Layout

- **Header bar:** app mark, search field (opens the palette), sync status,
  settings, account avatar.
- **Sidebar:** Inbox sections (Inbox, Review requests, Assigned to me,
  Mentions, Done), then Pull requests, Issues, Repositories, then forges
  and pinned repositories. Settings at the bottom.
- **Content:** list and detail panes side by side where it helps (inbox,
  repositories), full width otherwise.
- **Footer:** keyboard shortcuts for the current screen.
- Touch targets are at least 44 px; inputs always have labels.

## Screens

### Getting started

1. **Welcome (first run).** Connect GitHub, GitLab or Forgejo. States the
   promises: data cached locally, tokens in the system keyring, works
   offline.

   ![Welcome](design/welcome.png)

2. **Add a forge account.** Forge type, server URL with a live reachability
   check, sign in with an access token or through the browser. Lists the
   token scopes needed.

   ![Add a forge account](design/add-account.png)

3. **Settings.** Accounts (re-authenticate, remove), sync interval, local
   git mirrors location, offline cache, theme (system, light, dark), diff
   layout (unified, side by side).

   ![Settings](design/settings.png)

### Inbox and search

4. **Inbox.** Notifications from all forges with filters All, Unread,
   Participating. Each row: forge dot, repository and number, title,
   reason badge (review requested, checks failed, mentioned, assigned,
   merged), time. Unread rows are bold. A preview pane shows the selected
   item with **Open review**, **Mark as done**, **Open in browser**.

   ![Inbox](design/inbox.png)

5. **Search and commands (`Ctrl K`).** One box for everything. Prefixes:
   `pr:`, `is:`, `repo:`, `>` for commands. Shows recent items and commands
   with their shortcuts. Searches the local cache.

   ![Search and commands](design/command-palette.png)

6. **Repositories.** All repositories on the left; on the right open pull
   requests and issues, default branch, local mirror status, recent
   commits and the rendered README.

   ![Repositories](design/repositories.png)

### Pull requests

7. **List.** Review requested, Created by me, Assigned, All open; filters
   for forge, repository, sort. Rows show checks, review state, comments,
   last update; drafts are marked.

   ![Pull requests](design/pull-requests.png)

8. **Conversation.** Description, timeline (commits, checks, review
   requests), inline comment threads, merge box (checks, approvals, merge
   method, delete branch after merge), comment field.

   ![Pull request conversation](design/pull-request-conversation.png)

9. **Files.** Changed files with a "viewed" checkbox; unified diff from the
   local clone with the commit range shown; inline comment threads; review
   panel (Comment, Approve, Request changes) with checks and merge button.

   ![Pull request files](design/pull-request-files.png)

10. **Checks.** Check runs on the left; log viewer on the right with
    failing lines highlighted; re-run and copy log.

    ![Pull request checks](design/pull-request-checks.png)

11. **New pull request.** Target and source branches from the local clone,
    merge status, title, Markdown description with preview, commits and
    files included, reviewers, labels; create or create as draft.

    ![New pull request](design/new-pull-request.png)

### Issues

12. **List.** Assigned to me, Created by me, Mentioned, Closed; filters for
    repository and label; colored labels.

    ![Issues](design/issues.png)

13. **Issue.** Comment thread, timeline events, Markdown reply, sidebar with
    assignees, labels, milestone, linked pull requests.

    ![Issue](design/issue.png)

14. **New issue.** Repository, title, Markdown body, labels, assignee.
    Saved as a draft when offline.

    ![New issue](design/new-issue.png)

## Loading states

Cached data always shows first. Loading UI appears only when there is
nothing local to show yet.

1. Never blank a screen that has cached data; refresh in place.
2. Show skeletons only after 300 ms, so fast loads never flicker.
3. Skeletons match the real layout, so nothing jumps when data lands.
4. Show a percentage when the size is known (git fetch); otherwise a thin
   moving bar under the header.
5. With reduced motion, the shimmer and spinners stop moving.

The sync status in the header has four states: synced, syncing (spinner
with progress), offline (showing cached data) and sync failed (with
Retry).

![Loading patterns](design/loading-patterns.png)

**First sync.** The inbox fills in as notifications arrive; each forge
shows its own sync state in the sidebar.

![Inbox, first sync](design/inbox-first-sync.png)

**Large diffs.** The file list comes from the forge API first, while the
branch is fetched into the local mirror with a progress bar. Files render
as soon as they are available, and the ones in view render first.

![Large diff loading](design/large-diff-loading.png)

## Keyboard shortcuts

| Where | Keys |
|---|---|
| Everywhere | `Ctrl K` search and commands, `Ctrl R` sync now |
| Inbox | `j` / `k` move, `Enter` open, `e` done, `s` star, `o` open in browser |
| Pull request files | `n` / `p` next / previous file, `c` comment on line, `v` mark viewed, `Ctrl Enter` submit review |
| Palette | `↑` / `↓` move, `Enter` open, `Ctrl Enter` open in browser, `Esc` close |
| Creating | `Ctrl Shift P` new pull request, `Ctrl Shift I` new issue |
