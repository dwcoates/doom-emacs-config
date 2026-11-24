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
- A covered row is still drawn (surfaced) when it is a keeper: selected,
  working, unread, starting, of priority P0.5 or P1, a standing mediator or a
  queue-bypass holder. The rows above a surfaced row stay drawn too.
- Each open row carries its standing as `frontend.v1.RosterOpenRow.fold_cover`
  (`uncovered`, `hidden`, `surfaced`). The stylesheet hides a row by its own
  `.covered` and `.surfaced` classes (`row.ts`, `styles.css`), never by
  selectors that walk the folds around it.
- A subsection with members always draws its header, whatever folds it: its
  header is how its hidden rows are unfolded again.

## Visibility

- Visibility (`frontend.v1.RosterRowVisibility`, `wsm.Visibilities`) is
  whether the user can see the workspace in the sidebar: an open row no fold
  covers, a surfaced one, or the selection.
- A workspace the recency walk lands on is selected, so it shows; it returns
  to not visible once the user moves on.

## Workspace resource allocation

Workspace resource allocation is whether a workspace has its page loaded and
its shim unparked. NOT VISIBLE MEANS NO PAGE AND A PARKED SHIM, structurally:
both halves below read visibility first. Visible does not mean loaded.

- The page verdict (`frontend.v1.RosterRowPage`,
  `daemon/internal/resolve/sidebar/page.go`, `pageVerdict`), first rule that
  holds:
  - not visible: unloaded;
  - the selection: loaded;
  - a one-shot or a synchronous merge: unloaded (loaded only while selected);
  - working, starting or top priority: loaded;
  - an unread result: loaded for an hour after its turn ended;
  - a workspace the user left: loaded for ten seconds, or fifteen minutes
    when the visit being left first marked it viewed;
  - otherwise unloaded.
  The roster arms one timer for the earliest of these clocks, so the verdict
  flips on time. Emacs follows it (`lisp/page-unload.el`); the cold gate's
  hold-back is the only page rule Emacs keeps.
- The shim hold (`wsm.ShimHold`, `shimHoldOf` in the same file) says until
  when an idle shim stays up; the cache keeper parks by it
  (`daemon/internal/cachelapse/standdown.go`, `parkWaits`):
  - not visible: no hold;
  - the selection, work, a mediator, a queue-bypass holder, a one-shot: held
    while that stands;
  - a synchronous merge: none (its merge parks it while it waits in line,
    `drain.Controller.MergeQueuedDown`, and its steps bring it up);
  - an unread result: fifteen minutes after its turn ended;
  - a workspace the user left: as long as its page.
- A backlog or a close interrupts the running turn first
  (`workspace.verbs.interruptBeforeLeaving`). The workspace's cache gets one
  last keep-alive beat (`cachelapse` schedule's `spent`), then lapses.

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
