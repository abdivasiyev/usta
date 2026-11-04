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

   **Links.** Open forge links in usta: links to your forges from mail,
   chat or a terminal open the pull request, issue or file in usta, which
   becomes the system's link handler; every other link goes to the browser
   it remembered, and turning it off gives links back. A test field shows
   where a link would open. **About** shows the version and checks the
   GitHub releases for updates once a day.

   ![Settings: links and updates](design/settings-links.png)

   **Sign in with the browser.** usta opens the forge to approve it and
   waits; the token goes to the keyring. A Forgejo server needs an OAuth2
   application for usta once; the dialog says how and takes its client
   ID.

   ![Sign in with the browser](design/add-account-browser.png)

### Inbox and search

4. **Inbox.** Notifications from all forges with filters All, Unread,
   Participating. Each row: forge dot, repository and number, title,
   reason badge (review requested, checks failed, mentioned, assigned,
   merged), time. Unread rows are bold. **Mark all done** in the list's
   toolbar (`Shift E`) marks everything in the current view and filter as
   done, here and on the forge; a toast offers Undo instead of asking
   first.

   ![Inbox](design/inbox.png)

   Clicking a row, or `j` / `k`, only selects it; `Enter`, a double-click
   or the Open button opens the page. The preview pane shows, under
   **Open review**, **Mark as done** and **Open in browser**:

   - why it is in the inbox ("requested your review", "mentioned you"),
   - the description, rendered as Markdown,
   - the latest comments, new ones marked, mentions of you highlighted,
   - for pull requests, the changed files.

   Issues show their labels, comments, assignees and milestone instead of
   checks and changes. Commits preview their message and files; releases
   their notes.

   ![Inbox: a pull request selected](design/inbox-preview.png)

   ![Inbox: an issue selected](design/inbox-preview-issue.png)

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

   **Review threads.** Every thread stays in the diff, so a second review
   still shows what was said. Open threads are expanded; resolved ones are
   one collapsed line at their place saying who resolved them and how the
   thread started. A thread whose line changed since sits at the nearest
   line that still exists, marked Outdated, with the code it was about. A
   "new reply" tag marks what was said since your last review. The file
   list counts open, resolved and outdated threads; All threads, Open only
   and Hidden filter them; the review panel lists every thread to jump
   to. A thread you open stays open until you hide it.

   ![Review threads](design/review-threads.png)

   **Diff options.** Unified or split, hide whitespace-only changes,
   highlight the changed words in a line, and expand the context 20 lines
   at a time or to the next change. The choice is kept per pull request;
   Settings has the default.

   ![Diff options](design/diff-options.png)

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

### Writing and discussing

15. **Markdown editor.** Every Markdown box (comments, descriptions,
    replies, review summaries) has Write and Preview and a toolbar:
    heading, bold, italic, code, link, quote, lists, task list, code
    block, Mermaid block, emoji and attach. Each button wraps the
    selection, or inserts a placeholder and selects it. The emoji picker
    (`Ctrl .`) searches, groups by category and keeps recently used ones;
    typing `:` and two letters offers shortcodes as you type (`:smile:`
    becomes 😄). Pasted, dropped or attached images upload to the forge
    with their progress under the text. Unsent text is kept as a draft per
    issue, pull request and review, through refreshes and restarts.

    ![Markdown editor](design/markdown-editor.png)

16. **Diagrams.** Mermaid diagrams show zoom out, percentage, zoom in, fit
    and fullscreen on hover or focus. `Ctrl` + scroll and pinch zoom
    around the pointer; dragging pans. Fullscreen fits the diagram to the
    window with a minimap, and `←` / `→` move to the document's other
    diagrams; `Esc` closes.

    ![Diagram zoom and fullscreen](design/diagram-tools.png)

17. **Editing, reactions and timeline.** Your own comments and
    descriptions have Edit, Quote reply, Copy link and Delete in their
    ⋯ menu; an edit happens in place, and edited comments say so. Titles
    edit in place with the pencil. Reactions sit under each comment and
    toggle with one click. Timeline events (labels, assignments,
    references, renames, closes) sit between the comments, and linked
    pull requests or issues show in the sidebar. **Subscribed** in the
    header switches between every activity, only when participating, and
    muted.

    ![Editing, reactions and timeline](design/conversation-editing.png)

### Review and triage

18. **Reviewers, labels and state.** The sidebar lists reviewers with
    their state (approved, changes requested, waiting) and the approvals
    branch protection needs; re-request or remove a request in place.
    Request review opens a picker with search, people who changed these
    files before, collaborators and teams. Assignees, labels and milestone
    each have their own picker. The merge box shows only the states that
    apply: draft (Ready for review), behind the base branch (Update branch
    by merge or rebase), conflicts (with the files, and Check out
    locally), and merging automatically once checks and approvals pass.

    ![Reviewers, labels and state](design/pull-request-people.png)

19. **Labels.** The picker filters as you type, shows each label's colour
    and description, and offers to create a label with the name typed.
    Creating or editing a label takes a name, an optional description and
    a colour (swatches or hex) with a live preview; Manage labels lists
    them with their open counts.

    ![Labels](design/labels.png)

20. **Check out locally.** A worktree of the local mirror (small, shares
    its objects) or a separate clone, in a folder you choose, on a local
    branch that tracks the pull request's, so `git push` updates it.

    ![Check out locally](design/check-out.png)

### Repositories

21. **Branches and history.** The header has the branch picker (search,
    default first, recently pushed with ahead and behind counts, tags) and
    Star and Pin; pinned repositories sit in the sidebar. Overview, Files,
    Commits and Insights follow the chosen branch. Commits lists the
    branch's history by day; a commit opens its diff.

    ![Branches and history](design/repository-branches.png)

22. **Insights.** For a month, three months, a year or all time: commits
    per week, lines added and removed per week, and contributors with
    their commits, lines and a weekly sparkline. Computed from the local
    mirror and kept until the branch moves.

    ![Insights](design/repository-insights.png)

23. **Files: blame, find and go to file.** The code view has Code, Blame
    and History. `Ctrl F` finds in the file; `Ctrl P` goes to any file by
    fuzzy match. A line's menu copies a permalink (at the commit) or a
    link, shows the commit, or opens it on the forge.

    ![Blame, find and go to file](design/files-tools.png)

24. **Releases and tags.** Releases with Latest and Pre-release marks,
    their notes rendered, and assets to download; tags beside them.

    ![Releases and tags](design/repository-releases.png)

25. **Actions.** The repository's workflow runs, filtered by workflow,
    branch and status; a run shows its jobs, and its logs open in the same
    viewer as a pull request's Checks. Re-run and Cancel show only when
    the forge allows them.

    ![Actions](design/repository-actions.png)

26. **Search on the forge and the offline queue.** `Ctrl K` searches the
    synced cache first, then the forges themselves (issues, pull requests,
    code and repositories, closed ones too), filtered with chips. Without
    a connection, comments and actions wait in order and go out once the
    forge answers; the header shows how many wait, and each can be
    discarded.

    ![Search on the forge and the offline queue](design/forge-search.png)

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
with progress), offline (showing cached data) and sync failed. The chip
only says where sync stands; the refresh button next to it syncs again.

**Busy buttons.** A button waiting for the forge spins and names what it
does ("Commenting…", "Merging…"); it is never narrower than before, and
the buttons beside it wait too, so nothing is sent twice. On failure it
comes back and the reason shows next to it.

![Loading patterns](design/loading-patterns.png)

**First sync.** The inbox fills in as notifications arrive; each forge
shows its own sync state in the sidebar.

![Inbox, first sync](design/inbox-first-sync.png)

**Large diffs.** The file list comes from the forge API first, while the
branch is fetched into the local mirror with a progress bar. Files render
as soon as they are available, and the ones in view render first.

![Large diff loading](design/large-diff-loading.png)

## Problem reports

When usta quits unexpectedly, the next start offers to report it. Report
a problem (in Settings, the palette and error banners) opens a new issue
form on `abdivasiyev/usta` with the version and system, the last log
lines (tokens, request bodies and the home folder removed) and the macOS
crash report. Everything is shown and editable before it leaves the
machine; a log too long for a link is saved to Downloads to attach.

![Report a problem](design/report-problem.png)

## Keyboard shortcuts

| Where | Keys |
|---|---|
| Everywhere | `Ctrl K` search and commands, `Ctrl R` sync now |
| Inbox | `j` / `k` move, `Enter` open, `e` done, `Shift E` all done, `s` star, `o` open in browser |
| Pull request files | `n` / `p` next / previous file, `[` / `]` previous / next thread, `Enter` fold / unfold that thread, `r` reply to it, `c` comment on line, `v` mark viewed, `s` split, `w` whitespace, `x` expand context, `Ctrl Enter` submit review |
| Markdown editor | `Ctrl B` bold, `Ctrl I` italic, `Ctrl E` code, `Ctrl K` link, `Ctrl Shift P` Write / Preview |
| Palette | `↑` / `↓` move, `Enter` open, `Ctrl Enter` open in browser, `Esc` close |
| Creating | `Ctrl Shift P` new pull request, `Ctrl Shift I` new issue |
