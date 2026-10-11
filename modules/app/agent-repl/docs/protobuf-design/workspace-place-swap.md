# Workspace place swap

The owner wants `SPC TAB p` (`agent-repl-workspace-place`) to read one digit
0-9 without a return, naming a drawn tab slot (0 = slot 10), and swap that
workspace's roster place with the current workspace's, so both the tab bar and
the sidebar draw the two swapped. Roster order today is computed by the
daemon's resolver (priority, then name, then id) and nothing stores a place,
so a swap needs a stored place and a daemon rpc to exchange two of them.

## Core design principles

### A stored place orders the roster; priority only picks where a created workspace lands

- **Principle (owner, 2026-10-11):** priority only controls where a new
  workspace lands by default in the tab-bar and sidebar order; the user may
  reorder freely so that workspaces of one priority need not be contiguous.
- **Amended (owner, 2026-10-11, later the same day):** opening a workspace
  always puts it in the final position (last in the tab bar), whatever its
  priority. The earlier wording had priority deciding the re-open place too.
- **Consequences:** every workspace carries a persisted place; roster order
  among siblings is place order; creating a workspace assigns a place derived
  from its priority, and re-opening one moves it after every other place; setting a priority later does not move the
  workspace.
- **Reopens:** `claude-repld.internal.resolve.sidebar.lessWorkspace()`'s
  priority-then-name order, and the `SetWorkspacePriority` comment "the roster
  orders by it".
- **Does not claim:** that priority is retired; it still draws its badge and
  still decides the landing place.

### Swaps happen only between siblings, and the tab bar stays the sidebar's walk

- **Principle (owner, 2026-10-11):** a swap between workspaces with different
  parents is refused; tabs keep following the sidebar's depth-first walk.
- **Consequences:** one stored place serves both surfaces; the daemon refuses
  a swap of two workspaces that are not siblings in the repository view.
- **Does not claim:** anything about the recently-merged section's order,
  which stays merge-time ordered.

### Digit 0 names slot 10

- Owner ruling, 2026-10-11: the digit prompt maps 1-9 to slots 1-9 and 0 to
  the tenth drawn tab.

## Landed changes

### SwapWorkspacePlaces (2026-10-11)

- **What:** added `agentrepl.v1.SwapWorkspacePlaces` and its endpoint file
  `endpoint_swap_workspace_places.proto`, listed in `service.proto` beside
  `SetWorkspacePriority`.
- **Why:** the owner's `SPC TAB p` swaps the current workspace with the one at
  a drawn tab slot; the daemon owns roster order, so the swap is a daemon
  write.
- **Shape decisions:** the request carries two roster-served
  `workspace.v1.WorkspaceRef`s, never a slot number, because only Emacs knows
  which tabs are hidden under folded repositories. The response returns no
  order; the roster stream pushes it. Refusing a non-sibling pair is the
  `not_siblings` failure arm, because no place exchange can serve it.
- **Consequences:** the daemon needs a stored place per workspace (schema
  column + migration seeded from today's order), the resolver orders siblings
  by place, and create / re-open assign a landing place from priority.
- **Settled below:** whether `SetWorkspacePriority` stops moving a workspace that
  already has a place (yes).

### SetWorkspacePriority no longer moves a placed workspace (2026-10-11)

- **What:** the owner ruled that changing a workspace's priority leaves its
  stored place alone; priority decides only the landing place at create and
  re-open. No schema shape changes; the `SetWorkspacePriority` rpc comment
  ("the roster orders by it") is restated to say so.
- **Why:** a place the user chose survives a later priority change.

### Implementation consequences (2026-10-11)

- **Storage:** `workspaces.place` (layout 32), unique, at least 1 at rest. The
  migration seeds it from the old order (priority, name, id) so the first
  roster after the upgrade draws what the last one before it did.
- **The migration is additive, by a trigger.** The previous build's INSERT
  names no place, so a row it writes during a joining handover takes the
  default 0; `workspaces_land_unplaced` moves it after every place in the same
  statement. A breaking step was rejected because it forces a restart deploy
  and makes the joining-migration tests unable to run against the real list.
- **SQLite gotcha:** a comment directly above the table's LAST column breaks
  `ALTER TABLE ... DROP COLUMN` (the rewrite swallows the closing paren), so
  the column's comment sits on its index. The integration fixture's layout-3
  undo relies on that drop.
- **Landing:** create puts a workspace just before the first
  workspace of its repository whose priority is weaker, which is the end of
  its own priority's run; unprioritized lands last. The create path now passes
  the priority into RegisterWorkspace, because a later SetWorkspacePriority no
  longer moves anything.
- **Sibling test:** the daemon judges siblings against the roster it last
  published (`rosterwalk.RepositorySiblings`), the same tree the user sees.
- **Re-open:** both re-open paths (OpenWorkspace, and a RegisterWorkspace
  announcement of a closed workspace) go through `wsm.SetClosed(false)`,
  which moves the workspace to the place after every other one. Since the
  place is compared only among siblings, "last" means last within its
  repository section (and last under its parent when nested); the tab bar
  walks sections in repository-name order, so the workspace is the last tab
  of its own section, not of the whole bar.

## Rulings that reshape the design (owner, 2026-10-11, later)

These supersede "Swaps happen only between siblings, and the tab bar stays the
sidebar's walk" and the create-landing rule above. Not yet implemented.

- **The registry holds one canonical order; the sidebar and tab bar are views
  of it.** The tab bar lists open workspaces in registry order alone, so
  repositories may interleave and a child need not follow its parent. The
  sidebar groups by repository and nests children, sorting each group by
  registry order, so a child moves within its parent's list when its place
  moves past a sibling's.
- **Worked example (owner's):** registry A=1, B=2, C=3, D=4 with D a child of
  B. Sidebar: A, B (D nested), C. Tab bar: | A | B | C | D |.
- **Closed workspaces appear in a HIDDEN WORKSPACES sidebar section**, nested
  and in registry order; Recently Merged keeps its own ordering among its
  peers.
- **Creation lands second-to-last on the Emacs tab bar.** With a priority, the
  new workspace lands second-to-last within its priority bucket. Either way it
  can be moved freely afterwards.
- **Any two workspaces can swap**, so the `not_siblings` refusal and
  `rosterwalk.RepositorySiblings` are obviated by these rulings.
- **Creation landing, refined:** a new workspace lands second-to-last among
  the open workspaces. With none open it takes the first place; with exactly
  one open it lands after that one. Its priority bucket matters only when the
  workspace it was created from (as its child, or from the master workspace)
  has a priority; when that bucket has no open workspace, it lands
  second-to-last as usual.
- **Priority inheritance (owner, 2026-10-11):** a workspace created as a child
  (branched from its parent's branch) inherits the parent's priority. A
  workspace created from another workspace but branched from the master
  branch (a tangential hotfix made during unrelated work) does not.
- **Evidence (live registry, read-only, 2026-10-11):** the registry records a
  parent for BOTH kinds; only `parent_branch` tells them apart. Of 7
  recorded parents, 4 are master-based (`feed-development` and `ship-gns`
  from `iterm-2`, `revert-green-background-fro` from
  `unambiguous-file-links-rzv`, `ad-hoc-deploys-gje` from
  `emacs-bindings-support`) and 3 are true children whose `parent_branch` is
  a commit (`unambiguous-file-links-rzv`, `fork-cache-miss-oiu`,
  `feed-tick-flicker-ncg`). No workspace in the registry carries a priority.
- **Recently Merged (owner, 2026-10-11):** may order by registry order, but
  keeps its merged-today marking (the white line on the bar) and the count
  beside the "Recently Merged" heading unchanged.
- **Re-open (owner, 2026-10-11, supersedes "re-open goes last"):** a
  re-opened workspace returns to the place it held in the registry when it
  was closed.
- **Priority change (owner, 2026-10-11, supersedes "a priority change moves
  nothing"):** setting a non-null priority moves the workspace to
  second-to-last within that priority's open workspaces; it can be reordered
  freely afterwards. Clearing a priority moves nothing.
- **Merges are out of scope:** nothing about merge targets or the merge queue
  changes; only the sidebar and tab-bar views do.
- **Verified creation facts (code, 2026-10-11):** `SPC TAB n` sends no parent
  and no base ref, so the workspace is cut from the default branch and merges
  into the main worktree. `SPC TAB c`/`C` send a parent but no base ref; the
  daemon defaults the base to the default branch
  (`workspace/create.go:204`) while the merge target is the parent's worktree
  (`mergeTargetDir`, `create.go:550`). The workspace skill always records its
  source workspace as the parent and bases on the source's HEAD, or on the
  main branch with `--master`.
