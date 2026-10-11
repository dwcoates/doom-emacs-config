# Recently Merged, grouped by repository

The owner asked (2026-10-11) for the sidebar's Recently Merged band to be
separated out by repository, showing only the repositories that are not
collapsed in the main workspace list. The main list is the only place a
repository's workspaces are collapsed: collapsing a repository there takes its
group out of Recently Merged, and the groups carry no fold of their own. The
owner asked for the work to be planned and executed without a design round, so
every decision below was made by the implementing agent on the owner's stated
intent.

## Landed changes

### `frontend.v1.RosterMergedSection` carries per-repository groups

- **What changed.** `frontend.v1.RosterMergedSection.rows` (field 2) is
  removed and reserved. The band now carries `groups`, one
  `frontend.v1.RosterMergedRepoGroup` per repository with at least one merged
  workspace, in the repository grouping's order. A group has the repository's
  key, a header holding only the repository's label, its rows (flat, most
  recently merged first), and a `drawn` oneof: `shown` when the repository is
  expanded, `repository_collapsed` when it is collapsed.
- **Why.** The owner wants Recently Merged split by repository, and wants the
  main list's repository fold to be the one fold that governs a repository's
  workspaces everywhere, so a group has no toggle and follows its repository.
- **Consequences.**
  - A collapsed repository's group still rides the wire, so every reader that
    walks roster rows (`lisp/roster.el` `agent-repl-roster-walk`, the daemon's
    `daemon/internal/resolve/sidebar/taborder.go` `TabOrder`,
    `proto/helpers/go/rosterwalk/rosterwalk.go` `AllRows`) still sees every
    merged workspace. Dropping the group from the wire instead was rejected:
    the merge-queue verb and the Emacs reconciler look merged workspaces up by
    walking the roster, and would lose a collapsed repository's merges.
  - The band's header count is the row count of its shown groups only.
  - The group's `drawn` arm is resolved from the same `wsm.Repository.Folded`
    as `RosterRepoSection.fold`, in the same render, so the two cannot
    disagree. The webapp also tracks the group under the repository section's
    fold view key, so an in-flight fold gesture moves the group at once.
  - The Emacs tab bar walks a `repository_collapsed` group's rows as
    collapsed, exactly as it walks that repository's section rows.
  - The webapp's fit (`webapp/src/sidebar/merged-fit.ts`) now spends group
    header height as well as row height, and picks the most recently merged
    rows across groups (by each row's merged instant, which the row already
    carries for its drawn time), hiding a group whose rows are all past the fit.
