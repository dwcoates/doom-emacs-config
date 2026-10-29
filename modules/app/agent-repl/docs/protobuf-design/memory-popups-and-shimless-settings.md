# Memory popups, and settings changed with no shim

Two owner requests of 2026-10-29, built on branch `page-residency-by-status`
with the page residency change they depend on:

1. The rail's version hovers a popup of agent-repl's memory taken as a whole,
   and a workspace row in the Open section hovers that workspace's own memory.
2. The topbar's model, effort and permission-mode controls keep working while a
   workspace has no shim: the last-known selections and options are kept in wsm
   and drawn while the shim is down; a pick starts the shim, applies once it is
   up, and the shim is stood down again on the switch away unless a prompt was
   sent meanwhile.

The owner's standing ruling of 2026-10-13 has the lead settle contract shapes
and report after; every decision below was taken that way.

## Core design principles

- **Every figure in a memory popup is measured, never estimated** (owner,
  2026-10-29, approving the proposed lines). Consequence: no per-page byte
  figure exists, because every workspace page shares one WebKit web content
  process, which reports one footprint. The workspace hover states the page's
  residency instead (loaded, and when it unloads). Does not claim: that the
  page process's figure could not be broken down some other way later (a JS
  heap snapshot per page would be a new measurement, not an estimate).
- **The version hover names no workspace; workspace-specific lines appear only
  on an Open-section row's hover** (owner, 2026-10-29).

## Landed changes

### Memory figures are pulled on hover, not pushed on the roster

- WHAT: `agentrepl.v1.GetMemoryOverview` and `GetWorkspaceMemory` (SIDEBAR
  section) answer `frontend.v1.SidebarMemoryOverview` and
  `SidebarWorkspaceMemory` (new file `frontend/v1/sidebar_memory.proto`). The
  page asks when a popup opens and again while it stays open.
- WHY: the figures move every collector sample (seconds). Riding the roster
  would push the whole roster every sample; a roster push is decoded by Emacs
  and redraws every row, and pushes in bulk are what froze Emacs on deploys
  (perf list, "Deploy handover freezes Emacs", 2026-10-25).
- Alternatives that lost: RosterRow and RosterVersion carrying the figures
  (the churn above); the webapp host deriving page residency itself (the host
  knows which frames it holds but not page-unload.el's unload rule, and the
  shim figures are the daemon's anyway).
- Consequence: a popup shows figures as of its last pull; the page re-pulls
  while it is open.

### Emacs reports page residency (`ReportPageResidency`, HOST section)

- WHAT: Emacs states, per workspace, `PageResidency`: loaded with a release
  rule (held, on leave, or at an instant) or not loaded. The daemon keeps the
  latest report in memory and serves it in `SidebarWorkspaceMemory.page`.
- WHY: page-unload.el owns which pages are held and when they go (owner
  ruling 2026-10-29, `open-tabs.md`); the daemon cannot derive it.
- Consequence: a successor daemon holds no residency until Emacs reports
  again; Emacs reports every workspace on every link-up, and the page line is
  absent (`page` unset) until then.
- The editor-side `PageResidency` and the frontend
  `SidebarWorkspaceMemoryPage` are two spellings of one fact across the
  backend→frontend boundary, mapped in one place (the daemon's handler), per
  the figma→idl precedence.

### The overview's shape

- Pages: the web content process footprint (unset with no web view), with
  `loaded` and `not_loaded` counts of reported open workspaces (the partial
  arm carries what is omitted, so the line draws "14 of 58" without a total).
- Strain: stalls and blocked milliseconds within the stall detector's window
  (`stallbeat.Window`, 10 minutes), with the window's seconds served so the
  line names it.
- Agents: a oneof of none or running (count, sum, average resolved by the
  daemon so no client divides).
- Processes: daemon, Emacs, store, sidecar, each optional when not running.
- Total: the sum of every footprint above.
- Outcome: success carries `measured` or `not_yet_measured` (no sample since
  the daemon started is an answer, not a failure); the one failure is
  `measurement_failed`, when the latest sample could not list processes.

### `conversation.v1.ModelCatalog`

- WHAT: a record wrapping `repeated ModelOption`, so wsm can store a login's
  last-served catalog as one blob.
- WHY: the topbar's options come only from a session's `SessionStarted`; with
  no shim since the daemon started, the selector had nothing to offer.
- `SessionStarted.model_catalog` keeps its own repeated field (a wire shape
  the shim emits); the record is the daemon's storage form only.

## Settings with no shim (no contract change)

- Persisted in wsm (layout 89): the last model catalog per account root, and
  the last model, permission mode and picked effort per workspace. The topbar
  is seeded from them, and draws its three controls while sessionless except
  at a cold gate, where the gate's answer comes first.
- A model or permission-mode pick with no shim is held by the queue as a
  revival-pending act (as a prompt is) and starts the shim with the new start
  cause `settings`; an effort pick is recorded as the pick and starts the shim
  the same way, and the start re-applies it (`reapplyEffort`).
- The switch away stands such a shim down through the keep-alive cycle's own
  guarded path with the new reason `settings_visit`: never on the workspace in
  view, never under a turn, and never once a real turn has run since the
  settings start.
