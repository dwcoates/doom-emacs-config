# The sidebar: folds, visibility and workspace resource allocation

The sidebar draws what the daemon decides. Every rule below is derived in the
daemon's roster (`daemon/internal/resolve/sidebar`) and carried on the wire;
the webapp and Emacs draw and carry it out, and derive none of it themselves.
The settled rulings and their reasons are in
`docs/protobuf-design/2026-11-24-sidebar-visibility-and-allocation.md`.

## Folds

- A fold means "what is under this is not pertinent right now". Folded
  containers are repository sections, subsections and parent workspaces.
- Only a row's NEAREST container's fold covers it.
  - A top-level row in a subsection is covered by the subsection's fold alone,
    never by its repository section's: an unfolded subsection inside a
    collapsed section shows every one of its top-level rows.
  - A top-level row in no subsection is covered by its section's fold.
  - A parent's children fold covers every row beneath it, at every depth, and
    a child of a covered row is covered too.
  - The one statement is `openRegion.fold` and `childrenFolded`
    (`daemon/internal/resolve/sidebar/taborder.go`); the tab bar, the
    visibility and the rows all read it.
- A covered row stays visible (surfaced) while it is selected, working,
  unread, starting, of priority P0.5 or P1, a standing mediator or a
  queue-bypass holder. The rows above a surfaced row stay visible too.
- Each open row carries its standing as `frontend.v1.RosterOpenRow.fold_cover`
  (`uncovered`, `hidden`, `surfaced`). The stylesheet hides a row by its own
  `.covered` and `.surfaced` classes (`row.ts`, `styles.css`), never by
  selectors that walk the folds around it.
- A subsection with members always draws its header, whatever folds it: its
  header is how its hidden rows are unfolded again.

## Visibility

- A VISIBLE workspace is one that gets a number in the sidebar's Open
  section (`frontend.v1.RosterOpenRowTab`, numbered by the daemon,
  `daemon/internal/resolve/sidebar/taborder.go`). So it is an open workspace
  that no fold has rendered invisible as inactive; a backlogged, closed or
  merged workspace is never visible. The daemon states it on every row
  (`frontend.v1.RosterRowVisibility`, derived by `wsm.Visibilities`).
- Visibility closely corresponds to whether a workspace's allocations are
  present: its page in the webapp, its shim, and Emacs's slot for it (its
  top bar, its composer, its perspective). The selection is the one
  workspace that holds them without being visible.
- `SPC p p` is not limited to visible workspaces: it is the way to select a
  workspace that is not visible, or not even open. A workspace it or the
  recency walk lands on is selected, so it holds its allocations while
  selected, and loses them when the user moves on unless it is visible.

## Workspace resource allocation

Workspace resource allocation is whether a workspace has its page loaded and
its shim unparked. NOT VISIBLE MEANS NO PAGE AND A PARKED SHIM, structurally:
both halves below read visibility first. Visible does not mean loaded.

- The page verdict (`frontend.v1.RosterRowPage`,
  `daemon/internal/resolve/sidebar/page.go`, `pageVerdict`), first rule that
  holds:
  - not visible: unloaded;
  - the selection: loaded;
  - a one-shot or a synchronous merge (an async merge is an ordinary
    workspace): loaded for one minute after the user switched away from it,
    or after it became one while not selected (`PageAwayGrace`), then
    unloaded;
  - working, starting or top priority: loaded;
  - an unread result: loaded for an hour after its turn ended
    (`PageUnreadHold`);
  - a workspace the user left: loaded for ten seconds (`PageLeaveGrace`), or
    fifteen minutes when the visit being left first marked it viewed
    (`PageFirstViewGrace`);
  - otherwise unloaded.
  The roster arms one timer for the earliest of these clocks, so the verdict
  flips on time. Emacs follows it (`lisp/page-unload.el`); the cold gate's
  hold-back is the only page rule Emacs keeps.
- The shim is NEVER parked while:
  - it is initializing: a session brought up for work (a prompt, a create, a
    fork, a thread, a merge step, a compaction) is held from its own durable
    row until a turn closes after the bring-up (`wsm.Session.Initializing`,
    `WorkBroughtUpAt`), so the hold passes from the bring-up to the turn with
    no instant between them;
  - it works: a turn, detached work, a question or permission ask, a clear or
    a compaction (the drain's gate, `drain` `sweep.go` `gate`, also refuses
    while a prompt is reviving or a start is under way);
  - a keep-alive beat awaits its answer;
  - it is selected (the selection holds a running shim up; selecting a parked
    one never brings it up, `wsm.Workspace.Runs`);
  - remote control is on, or it is a standing mediator, a queue-bypass holder
    or a one-shot.
- Otherwise the shim hold (`wsm.ShimHold`, `shimHoldOf` in `page.go`, joined
  with the session's own `wsm.Session.BringUpHold`) says until when an idle
  shim stays up; the cache keeper parks by it
  (`daemon/internal/cachelapse/standdown.go`, `parkWaits`):
  - not visible: no hold;
  - a synchronous merge waiting in line: no hold (its enqueue parks it,
    `drain.Controller.MergeQueuedDown`); one awaiting its workspace's
    freedom or at an active step is held while that stands;
  - a start the user asked for that runs no turn (a restart, a relaunch, a
    rebind, a swap, a cold gate's answer, a merge leaving its line without
    landing): `wsm.ShimIdleHold` after that bring-up, unless a turn closes
    first (`UserBroughtUpAt`);
  - any other: `wsm.ShimIdleHold` (fifteen minutes) after the later of its
    last turn's end and the user's last interaction with it.
- A synchronous merge that leaves its line without landing (evicted,
  dequeued, or behind a merge that did not land) brings back up the session
  its wait parked (`merge.orchestrator.returnSession`, start cause
  `merge_left_line`); its active steps bring it up as they need it
  (`ensureSession`).
- A backlog or a close interrupts the running turn first
  (`workspace.verbs.interruptBeforeLeaving`). The workspace is no longer
  visible, so nothing holds its session: it parks, gets one last keep-alive
  beat (`cachelapse` schedule's `spent`), and is never brought up for a later
  one. A close ends the session as a kill does.
- A settings visit (a model, permission-mode or effort pick that started the
  shim) parks on the switch away when no turn ran
  (`drain.Controller.VisitDown`), and a session at its cold gate is parked.

## Viewed

- The daemon is the one source of the viewed marker
  (`frontend.v1.RosterRowViewed`).
- Only the user's interactions raise it: scrolling the feed, typing in the
  composer, moving through the feed, following a link.
- Only a turn's start lowers it. A turn's end, a deploy, a daemon restart, a
  park, a revival, a link blip or a fold never does. It is durable: written to
  wsm's `viewed_status` on every change and seeded from it at boot.
- An interaction within ten minutes of a turn's end marks nothing viewed; it
  still counts toward recency (`sidebar.ViewExemption`).

## The recency walk

- `s-[` and `s-]` order workspaces by the user's last interaction, never by
  switches (`agent-repl-roster-selection-recency-order`).
- The walk reaches every open workspace, one a fold hides included
  (`agent-repl-roster-open-entries`); a step to a workspace Emacs holds no tab
  for selects it by its ref and keeps the walk.
- Every recorded interaction is logged at INFO (`daemon.workspace.interaction`),
  so a walk's order can be read off the logs.
