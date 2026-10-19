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
- **Reopens:** `agent-repld.internal.resolve.sidebar.lessWorkspace()`'s
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

## Create-command rework rulings (owner, 2026-10-11)

- `SPC TAB c` and `SPC TAB C` are removed.
- A workspace's code is cut from `master` and merges into `master`, or is cut
  from another workspace's tip and merges into that workspace; the two mixed
  combinations are not created. Nesting follows the merge target.
- The conversation is a separate axis: new for `n`/`N`, copied from the
  current workspace for both kinds of fork.
- `f` forks into `master` (code from `master`, conversation copied); `F` forks
  as a child (code from the current tip, merges back into it). `n` makes a
  new workspace in the current repository; `N` lets the user pick the
  repository. Each has a prompt version and a name version; how the two are
  told apart is open.
- A fork into `master` gets a prompt prefix: it says the workspace is a fork,
  names the originating workspace, and says the code came from `master` with
  only the conversation copied. The agent raises that only when a later user
  prompt relies on changes from the originating workspace that are not
  present. Calling the user's expectation otherwise is operator error, not a
  defect.
- **Fork-to-master prefix text:** approved as drafted (new template in
  `prompts/`, origin name filled by the daemon).
- **Named versions** start exactly as `SPC TAB N` does today: idle, no prompt.
- **Scope:** the ordering work and the create-command rework both land in
  this workspace.
- **`SPC TAB p` is a move:** the current workspace takes the pressed tab's
  number; moving right shifts the tabs it passes left (`| A | B | C | D |`,
  A to tab 3 gives `| B | C | A | D |`), moving left shifts them right.
- **Collapsed repositories** keep taking their workspaces off the tab bar.
- **Hidden workspaces** keep their registry place; showing one again draws it
  where the registry says.
- **Repository sections** keep their current sort; only rows within a section,
  and children under their parent, sort by registry order.
- **Keys (owner, 2026-10-11):** lowercase asks for a prompt, capital for a
  name. `n`/`N` stay as they are. `f`/`F` fork into `master`. `c`/`C` become
  the child fork (cut from the current workspace's tip, merging back into
  it), replacing today's child commands.
- **Base ref stays, checked by the daemon (owner, 2026-10-11):** a create
  that names a parent refuses any base ref and always cuts from the parent's
  tip; a create with no parent accepts a base ref only when it is an
  ancestor of (or equal to) the primary branch. Both refusals are typed
  error arms. The four existing workspaces cut from `master` that merge into
  another workspace are left alone.

### Tab order on the roster (2026-10-11)

- **What:** `frontend.v1.WorkspaceRoster.tab_bar` (`RosterTabBar`, a list of
  `RosterTab`: a `workspace.v1.WorkspaceRef` plus a `visibility` oneof of
  drawn, withheld because the repository is collapsed, or withheld because
  the user hid it).
- **Why:** the tab bar is now the registry order, not the sidebar read top to
  bottom, so the daemon resolves it and Emacs draws it as sent.
- **Consequences:** Emacs' tab bar, numerals, `s-{`/`s-}` and the daemon's
  startup bring-up order read this list; `agent-repl-roster-walk` and
  `sidebar.TabOrder()` stop deriving tab order from the groupings. Collapsed
  and hidden workspaces behave exactly as today (no number, no stop).

### MoveWorkspace replaces SwapWorkspacePlaces (2026-10-11)

- **What:** `agentrepl.v1.MoveWorkspace` (`endpoint_move_workspace.proto`)
  replaces `SwapWorkspacePlaces` and its file, including the `not_siblings`
  arm, as agreed.
- **Why:** `SPC TAB p` moves the current workspace onto the pressed tab;
  any two workspaces can move.

### Creation contract (2026-10-11)

- **What:** `CreateWorkspaceRequest.fork_source` (`CreateWorkspaceForkSource`)
  for a top-level fork; three new error arms (`base_ref_off_primary_branch`,
  `unknown_fork_source`, `fork_source_has_no_conversation`); comments on
  `parent`, `base_ref`, `priority` and the priority files restated for the
  rules (a child is cut from its parent's tip and inherits its priority; a base
  ref beside a parent is InvalidArgument; landing is second-to-last within the
  priority).
- **Shape chosen by the implementer, not sketched to the owner:** the owner
  agreed the description and asked to proceed; the field and arm names above
  are the implementer's, kept additive so `parent.fork` (the child fork) is
  untouched. Flagged in the session report for the owner to revisit.
- **Sweep:** `SwapWorkspacePlaces*` removed; nothing else obviated.
- **Nesting follows the merge target only (owner, 2026-10-11):** a row nests
  only under the workspace it merges into, which is its recorded parent. The
  branch-lineage nesting (a workspace registered from Emacs nested under the
  workspace owning the branch it was cut from) is removed, since such a
  workspace merges into the primary branch.
- **Creation contract approved (owner, 2026-10-11):** the owner reviewed the
  `fork_source` field and the three new error arms as landed and approved
  them unchanged, settling the "shape chosen by the implementer" flag above.

## `SPC TAB p` is a swap again (owner, 2026-10-17)

- **Ruling:** `SPC TAB p` swaps the current workspace with the workspace on
  the pressed tab, instead of moving the current workspace there. This
  supersedes "`SPC TAB p` is a move" above. On `| A | B | C | D |`, A to tab
  3 gives `| C | B | A | D |`: A and C exchange places and B stays put.
- **Unchanged:** any two workspaces can swap, across repositories and parents;
  the digit still counts drawn tabs, 0 naming slot 10; the sidebar and tab bar
  stay views of the one registry order.

### SwapWorkspaces replaces MoveWorkspace (2026-10-17)

- **What:** `agentrepl.v1.SwapWorkspaces` (`endpoint_swap_workspaces.proto`)
  replaces `MoveWorkspace` and its file. The request keeps the same two
  roster-served refs, and the error arms are unchanged
  (`unknown_workspace`, `workspace_ref_mismatch`, `transferring_away`,
  `not_yet_adopted`).
- **Why a rename, not a reinterpretation of MoveWorkspace:** the name is a
  premise every reader reasons from, and a `MoveWorkspace` that swaps would
  mislead the next change to the ordering. `SPC TAB p` was its only caller, so
  nothing keeps a move rpc alive.
- **Sweep:** `MoveWorkspace*` messages and `endpoint_move_workspace.proto`
  were removed. Nothing else became obsolete.
- **Consequences:** `wsm.SwapPlaces` replaces `MovePlace`. It parks the mover
  on the parking slot and exchanges the two places in one transaction, so no
  other row's place is written. The workspace verb, server handler, elisp
  codec, verb (`agent-repl-verb-swap`) and command
  (`agent-repl-workspace-swap`) are renamed the same way. Log operations
  become `daemon.wsm.swap_places`, `daemon.workspace.swap` and
  `elisp.commands.swap*`.

## `SPC TAB P` moves, beside the swap on `SPC TAB p` (owner, 2026-10-18)

- **Ruling:** the move (`SPC TAB p` before 2026-10-17) comes back on
  `SPC TAB P`, and `SPC TAB p` stays the swap.
- **Why:** the sidebar's arrangement now groups the tab bar by repository,
  then each repository's unsectioned workspaces and each subsection's, each
  family depth-first (`endpoint_arrange_sidebar.proto`). A swap of two
  workspaces of different repositories or different families exchanges their
  stored places but moves no tab, so the owner asked for the insert form
  beside it.

### MoveWorkspace returns, judged against the grouped tab bar (2026-10-18)

- **What:** `agentrepl.v1.MoveWorkspace` (`endpoint_move_workspace.proto`),
  listed in `service.proto` beside `SwapWorkspaces`. Same two roster-served
  refs as before; two new error arms, `other_repository` and `not_siblings`.
- **Semantics (implementer's, carried through under the owner's 2026-10-13
  standing approval of contract changes, reported to the owner):**
  - two top-level workspaces of one repository: the mover joins the target's
    subsection (or none), just before the target when it comes from a later
    tab, just after the target's whole family when it comes from an earlier
    one;
  - two children of one parent: the same rule among the siblings;
  - every other pairing is refused, because no tab of the target is a place
    the mover can occupy: different repositories never interleave
    (`other_repository`), and a child always follows its parent's family
    (`not_siblings`).
- **Why the direction rule:** it is the old move's rule ("moving right shifts
  the tabs it passes left") lifted from single tabs to families, so the mover
  ends exactly where the target's family stood.
- **Why the store judges it in one transaction:** the direction and the anchor
  ("after the target's family" is "before the next root in the target's
  bucket") are read and written in the same write, so an arrangement landing
  between a read and the write cannot misplace the mover.
- **Rejected:** composing the move in the editor from `ArrangeSidebar
  place_workspace`. That needs a read of the arrangement before the write, and
  `place_workspace` places only top-level workspaces, so siblings could not
  move at all.

## A swap stays inside one repository (owner, 2026-10-19)

- **Ruling:** `SPC TAB p` must not work across repositories. This supersedes
  "any two workspaces can swap" (2026-10-17) for pairs of different
  repositories.
- **What:** `agentrepl.v1.SwapWorkspacesError.other_repository`, an empty
  arm; the request comment states the one-repository rule.
- **Why:** the tab bar groups by repository, so a swap across repositories
  exchanged stored places and moved no tab.
- **Consequences:** `wsm.SwapPlaces` reads both repositories in its
  transaction and refuses `ErrOtherRepository` before writing; the workspace
  verb answers `other_repository`; Emacs says so in words at info.
- **Still open:** a swap of a child with a workspace outside its family
  exchanges places and moves no tab; whether it is refused too (as
  `MoveWorkspace` refuses `not_siblings`) is put to the owner.

## One roster listing, three states, and family rules (owner, 2026-10-19)

### Core design principles

- **The roster is one listing the tab bar and the sidebar share.** Every
  workspace the roster draws is in exactly one of three lists: OPEN (the
  repository sections), HIDDEN, MERGED. The tab bar is the Open list's
  depth-first walk; nothing else orders tabs.
  - Consequences: `frontend.v1.WorkspaceRoster.tab_bar` is retired; each open
    row states its own tab (`RosterOpenRow.tab`: numbered or folded).
  - Does not claim: that a folded repository's workspaces are closed. They
    stay open, folded out of both the sidebar and the tab bar.
- **Ordering conditions** (each lives in the implementation's docstrings):
  numbers match position (tab N immediately before N+1, in both surfaces);
  the order runs unbroken across sections, repositories and families; a
  subtree is one unbroken run at every depth; a subsection never spans
  repositories; a family never splits across subsections; every workspace
  is in one list; a workspace is in Open iff it has a tab (folding aside);
  hidden workspaces have no tab and merged workspaces are never open (OPEN
  BEATS MERGED: a reopened merged workspace returns to Open); numbers run
  1..N with no gap; only the daemon holds the order and no client reorders
  locally; only an explicit request changes the order; a request that would
  break a condition is refused and changes nothing; the order survives
  restarts.
- **Family rules.** A workspace with open children (hidden children count as
  open) cannot be hidden, closed, killed, nuked, or merged by a merge that
  closes it; the merge refusal names the children, to be resolved and
  closed first. A workspace cannot be created, re-parented, reopened or
  shown as the open child of a hidden, closed or CLOSING parent (one a
  merge waiting in its queue closes once it lands). A hidden merge
  workspace is no open child: it is the merge's machinery, drawn nowhere.
  Every rule is checked in the same wsm transaction as the write it guards,
  so no race between a parent's close and a child's creation can leave an
  open child under a closed parent. Families that
  already break the rule keep drawing by the old rule: an open workspace
  whose parent is not open takes its parent's place.

### Landed changes (2026-10-19)

- **What:** `RosterOpenRow`/`RosterOpenRows`, `RosterOpenRowTab` (numbered,
  folded), `RosterHiddenRow`/`RosterHiddenRows`, `RosterMergedRow`/
  `RosterMergedRows`; the sections' `rows` fields become `open_rows`,
  `hidden_rows`, `merged_rows` under new field numbers (the old ones
  reserved); `RosterRow.children` retired (families nest in their list's row
  type); `RosterRows`, `RosterTabBar`, `RosterTab` and its arms removed.
  `agentrepl.v1.family.proto`: `WorkspaceHasOpenChildren` (on CloseWorkspace,
  KillWorkspace, NukeWorkspace, SetWorkspaceHidden, MergeWorkspace) and
  `WorkspaceParentNotOpen` (on SetWorkspaceHidden, SetWorkspaceParent,
  CreateWorkspace, OpenWorkspace). `SwapWorkspacesError.not_siblings`.
- **Added after (2026-10-19):** `WorkspaceParentNotOpen.closing` (a merge
  queued to close the parent would otherwise strand a child created while
  it waits) and `RegisterWorkspaceError.parent_not_open` (an editor
  announcing a closed workspace re-opens it, which the family rules govern
  as they govern OpenWorkspace).
- **Why the new field numbers:** a rolling deploy can pair a new daemon with
  an old client for a moment; a reused number with a new type would decode
  as garbage, while a new number decodes as an unknown field.
- **Why shared family messages:** the same two refusals reach nine
  endpoints; one definition keeps their facts identical.
- **Kept on RosterRow, deliberately:** `closed` and `closure` (the editor's
  switcher reads who closed a merged row) and the when-column's `merged` and
  `hidden` arms; moving them was not needed for any condition.
- **Consequences:** the daemon hoists (an open workspace whose parent is not
  open takes its place), which clients used to do; Emacs builds its tabs
  from the Open walk, plus an undrawn entry per hidden workspace so a hidden
  workspace keeps its perspective; the webapp paints each row's own number.
