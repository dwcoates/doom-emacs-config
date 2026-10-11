# Sidebar row commit count

The owner wants the sidebar's workspace hover panel (`frontend.v1.RosterRowDetail`)
to say how many commits the workspace's branch holds, counted from its
merge-base with the branch it was cut from. The same panel opens when the
pointer hovers a Recently Merged row's name.

## Landed changes

### `frontend.v1.RosterRowDetail.commits` (2026-10-11)

- **What:** a new optional element message, `RosterRowDetailCommits { uint32 count }`,
  as field 4 of the detail panel.
- **Why:** the owner asked for the count in both hovers (the live rows' status-dot
  hover, and the merged rows' name hover).
- **Freshness (owner ruling):** the daemon recounts a live workspace when one of
  its turns closes, and ships the count. A commit made between turns (by hand,
  in magit) shows only after the next turn ends; the owner accepted that.
- **Merged rows:** after a merge lands, `parent..branch` reads zero and the
  worktree reaper later deletes the branch, so a merged row's count can not be
  computed when it is drawn. The count is frozen when the merge lands, from the
  commits the merge commit brought in (`<merge>^1..<merge>^2`, merges
  included), and a turn end never recounts a merged workspace.
- **Persistence:** the count is a `workspaces.commit_count` column in the
  daemon's SQLite store, so it survives a restart and the branch being reaped.
  Unset (NULL) until the first count lands, which the panel draws as no line.
- **Counting rule:** `git rev-list --count <parent>..<branch>`, merges included,
  the same rule the landed range uses, so a workspace's count does not jump when
  it merges.
