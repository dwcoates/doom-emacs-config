# The sidebar's Viewed band

The owner asked (2026-10-12) for "viewed" workspaces to leave the main
workspace list for a section of their own just under it, titled VIEWED in the
same purple as the other band titles, each row showing how long it has been
since the workspace was last viewed, the band keeping the canonical registry
order rather than sorting by that duration. The hidden section's title becomes
HIDDEN in the same change. The owner asked for the work to proceed without a
design round, so every decision below was made by the implementing agent.

## Landed changes

### `frontend.v1.WorkspaceRoster.viewed` (a `frontend.v1.RosterViewedSection`)

- WHAT: a third keyless band beside `hidden` and `recently_merged`: header,
  rows, and a daemon-held fold, drawn under every grouping, directly below it
  and above Hidden.
- WHY: a workspace whose last result has been read matters less than one with
  work running or a result still unread, so it is moved out of the grouping.
- MEMBERSHIP IS THE VIEWED MARKER (`frontend.v1.RosterRowViewed`). The daemon
  resolves each workspace's status arm and marker ONCE per render
  (`agent-repld.internal.resolve.sidebar.resolver.facts()`), and the partition
  that files a row under the band reads the same value the row's marker is
  drawn from, so the two cannot disagree.
- Hidden outranks viewed: a hidden workspace stays in `hidden` read or not. A
  merged workspace is always in `recently_merged`.
- Order is roster order (`sortWorkspaces`), nested by lineage among the viewed
  rows alone, exactly as the hidden section's rows are. Viewing order and the
  when-column never sort it.
- The band ignores repository folds, as the hidden section does. The tab bar
  still withholds a collapsed repository's tabs from `RosterTabBar`.

### `frontend.v1.RosterRowWhen.viewed` (a `frontend.v1.RosterRowWhenViewed`)

- WHAT: the when-column's arm on a viewed row: the later of the instant the
  result was read and the durable last selection.
- WHY: "how long since last viewed" is what the owner asked the band to show.
- Precedence is merged, viewed, turn ended, created. A viewed row with no
  viewing instant on record (a result read before layout 39, never selected)
  falls to the next arm.
- The read instant is durable: `wsm.TurnResult.ReadAt`, column
  `workspaces.result_read_at` (layout 39), so the age survives restarts.
  Re-dwelling on an already-read row does not move the read instant; a later
  selection moves the column instead.

### `frontend.v1.RosterRowWorkspace.repository`

- WHAT: each row states its repository's `workspace.v1.RepositoryRef`, equal to
  its repository section's key.
- WHY: Emacs found a workspace's repository by which repository section held
  its row (`agent-repl-roster-repository-of`, `agent-repl-verbs--section-of-ws`)
  and labelled tabs by that section. A viewed row is in no repository section,
  so the create default and the merge-queue pause verbs would fail for exactly
  the workspaces the user has just looked at, and a colliding tab name would
  rename every time a row entered or left the band.

### `agentrepl.v1.SidebarViewFoldSection.viewed` (a `agentrepl.v1.SidebarViewViewed`)

- WHAT: the fold arm for the band, durable as `sidebar_view.viewed_folded`.

## Consequences

- Every reader that walks rows by section must include the band:
  `rosterwalk.AllRows` (sidecar log forwarder, merge-queue verb), Emacs'
  `agent-repl-roster-walk` (tabs; a missed row tears its tab down), and the
  webapp.
- A selected workspace moves into the band as soon as its dwell reports it
  read, and back to its grouping when its next turn starts.
