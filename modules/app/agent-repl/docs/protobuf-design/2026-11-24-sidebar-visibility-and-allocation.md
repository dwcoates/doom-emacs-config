# Sidebar visibility and workspace resource allocation

The owner wants the sidebar's folds to follow one plain rule, and wants that
rule to decide which workspaces hold resources. A fold means "what is under
this is not pertinent right now": a row a fold covers is hidden unless it
works or holds an unread result, at every depth below the fold. Only the
NEAREST container's fold covers a row: an unfolded subsection inside a
collapsed repository section shows every one of its top-level rows. A
subsection with members always draws its header, because unfolding it is how
its hidden rows come back. Whether a row is shown then bounds whether its
workspace holds a page and a running shim: a workspace that is not shown never
has either.

Design mode: autonomous. Provenance: figma→idl (frontend.v1 is the sidebar's
server-driven UI).

## Core design principles

- **Not shown means no page and a parked shim.** In the owner's terms: if a
  workspace is not shown in the sidebar, it must necessarily and structurally
  be the case that its page is not loaded and its shim is parked. The inverse
  does not hold: a shown workspace need not hold either.
  - Consequences: the daemon derives one allocation value per workspace from
    which both the page and the shim are read, and a not-shown arm carries no
    page and no running shim by construction. The named exceptions (deploy
    mediators and workspaces in queue bypass mode, which are always shown; a
    sync merge's active phases and keep-alive beats; one keep-alive tick after
    a backlog or close) are stated at the derivation.
  - Reopens: the run rule `wsm.Workspace.Runs` (every visible workspace runs)
    and Emacs's own page-residency rules in `lisp/page-unload.el`.
  - Does not claim: that a shown workspace's shim runs, or that its page is
    loaded.
- **The daemon is the one source of truth for "viewed".** The viewed mark, the
  recency walk's order and every allocation clock read the same recorded
  interactions.

## Landed changes

### 1. `frontend.v1.RosterOpenRow.fold_cover` replaces `surfaced`

- WHAT: `RosterOpenRow.surfaced` (field 7, a bool) is retired and reserved.
  `RosterOpenRow.fold_cover` (field 9) is a `RosterOpenRowFoldCover` whose
  oneof states the row's standing under the folds: `uncovered`, `hidden` (a
  fold covers it and the sidebar does not draw it) or `surfaced` (a fold covers
  it and it is drawn anyway, because it works, holds an unread result, starts,
  has top priority, is selected, or stands above such a row).
- WHY: the webapp re-derived which rows a fold covers from the DOM, with
  descendant selectors that let a collapsed repository section reach into its
  subsections, which is the defect the owner reported. The daemon already
  decides coverage (`resolve/sidebar/taborder.go` `openRegion.fold`, and the
  children fold), so the row now carries the verdict and the stylesheet only
  draws it.
- Why a oneof and not a second bool beside `surfaced`: `surfaced` without
  coverage was a meaningless state the two bools could express; the three arms
  are the three states there are.
- Consequences: Emacs's strict decoder (`lisp/wire-roster.el`,
  `agent-repl-wire-decode-roster-open-row`) must learn `foldCover` and drop
  `surfaced` in the same change, or every roster push is refused. The webapp's
  fold rules in `webapp/src/styles.css` (the `.repo.folded .rows ...` block
  near line 1045 and the reduced-motion copy near line 5190) become rules
  keyed on the row's own class. `webapp/src/sidebar/row.ts`,
  `fold-fade.ts` and `open-count.ts` read the arm.

## Workspace resource allocation: the settled rules

Workspace resource allocation is whether a workspace has its page loaded and
its shim unparked. The owner's rulings of 2026-11-24, as this change builds
them. Each clock is a product rule, not a bound on an operation.

### Visible

- A VISIBLE workspace is one that gets a number in the sidebar's Open
  section: an open workspace that no fold has rendered invisible as inactive.
  Backlogged, closed, killed, nuked and merged workspaces are never visible.
- A row a fold covers stays visible while it is selected, working, unread,
  starting, of priority P0.5 or P1, a standing mediator or a queue-bypass
  holder; the rows above a visible row stay visible too.
- A subsection with members always draws its header.
- `SPC p p` and the recency walk may select a workspace that is not visible,
  or not even open. It holds its allocations while selected, and loses them
  when the user moves on unless it is visible.

### Page

- Not visible: never loaded.
- Selected: loaded.
- A one-shot, or a workspace in a synchronous merge, that is not selected:
  loaded for one minute after the user switches away from it, or after it
  becomes one, then unloaded (owner ruling, 2026-11-24, which replaces "at
  once"). An async merge is an ordinary workspace.
- Working (starting and top priority count as working): loaded.
- Unread: loaded until one hour after its turn ended.
- Viewed: unloaded 10 seconds after the user switches away, except when the
  visit being left is the one that first marked it viewed; then 15 minutes.
  The 15 minutes run from the switch away, so a long first visit still keeps
  the page for 15 minutes after it.

### Shim

- Never parked while initializing (brought up for work whose turn has not
  closed, with no instant between the bring-up and the turn), working, a
  keep-alive beat awaits its answer, selected, remote control is on, or it is
  a mediator, a queue-bypass holder or a one-shot.
- Selecting a parked workspace never brings its session up.
- Otherwise it parks 15 minutes after the later of its last turn's end and
  the user's last interaction with it (owner ruling, 2026-11-24, which
  replaces the unread and left rules), or 15 minutes after a start the user
  asked for that runs no turn.
- It parks the moment it stops being visible, and after a keep-alive answer
  when it was restored only for that beat.
- A synchronous merge: parked while it waits in line; brought back up when it
  leaves the line without landing (evicted, dequeued, or behind a merge that
  did not land) and when it reaches any active stage. A merge awaiting its
  workspace's freedom holds the session.
- Backlogged, closed, killed or nuked: its running turn is interrupted, it
  parks, one last keep-alive beat goes out, and none after it. A close ends
  the session as a kill does.
- A session at its cold gate is parked; a settings visit parks on the switch
  away when no turn ran.

### Viewed

- Only a turn's start lowers the viewed marker (owner ruling, 2026-11-24,
  which narrows the earlier "start or end"): an end a deploy replays must
  never mark a read workspace unread.
- An interaction within 10 minutes of a turn's end marks nothing viewed; it
  still counts toward recency.
- The recency walk orders by interactions alone, and reaches every open
  workspace.

### Naming

- The section the sidebar labels "Backlog" is BACKLOGGED in code and harness,
  never "hidden", which reads as "not visible".

## Landed changes, continued

### 2. `frontend.v1.RosterRow.page` replaces `dormant`; the page reasons follow

- WHAT: `RosterRow.dormant` (field 80) and `RosterRowDormant` are retired.
  `RosterRow.page` (field 82) is a `RosterRowPage` whose oneof is `loaded` or
  `unloaded`: the daemon's page verdict. `agentrepl.v1.ReportPageState`'s
  load reason `awake` (2) is retired for `allocated` (7), and its unload
  reason `dormant` (1) for `deallocated` (4).
- WHY: the owner wants one source of truth for workspace resource
  allocation, with "not shown, so no page" structurally guaranteed. The editor
  used to weigh dormancy, visits and its own ten-second timer
  (`lisp/page-unload.el`); the daemon now derives the verdict
  (`daemon/internal/resolve/sidebar/page.go`, `pageVerdict`), reading
  visibility first so a not-visible workspace can never be `loaded`, and arms
  one timer for the earliest clock so the verdict flips on time.
- Ruling on the obviated declarations: `wsm.Session.Dormant` had no reader
  left and was removed; `wsm.Session.KeepaliveRestoredAt` stays as recorded
  evidence of why a session came up (dropping it needs a store migration for
  no gain).
- Consequences: the metrics' `reason` labels change (`awake` to `allocated`,
  `dormant` to `deallocated`; `skills/agent-repl-metrics/CATALOG.md`). Every
  verdict change is recorded at INFO (`daemon.sidebar.page_verdict_changed`)
  with the inputs that decided it.
