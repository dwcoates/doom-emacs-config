# Sidebar section heights

The owner asked (2026-11-11) that the unfolded sections of the sidebar be
resizable by clicking and dragging the grey rules that delimit them: dragging
the rule between Open and Merge Queue upward makes Open shorter and Merge
Queue taller.

## Core design principles

- **Nothing about the sidebar's view is a page's own** (owner rulings,
  2026-10-06; `webapp/src/sidebar/sidebar.ts`). A section's height is view
  state like its fold, so it is daemon-held, durable across restarts, and
  every page draws it the same.
  - Consequence: the height travels as a new `UpdateSidebarView` arm and
    on the roster push, beside each section's fold.
  - Does not claim: a drag in progress is not view state. The dragging page
    paints its own sections while the pointer is down and asks the daemon
    only on release.

## Assumptions (autonomous design)

- Heights are counted in LINES, not pixels: the band fit
  (`webapp/src/sidebar/band-fit.ts`) never cuts a line except at the rail's
  foot, so a drag snaps to whole lines, and lines mean the same on every page.
- The Open section is never sized. It is the section that fills whatever the
  bands below it leave, so a sized band keeps its lines through a window
  resize and Open absorbs the change.
- Only the bands (Merge Queue, Recently Merged, Hidden Workspaces) are sized.
  A drag of the rule between two bands sizes both of them. A drag of the rule
  under Open sizes the band below it only.
- A double click on a rule returns the sections beside it to fitted.

## Iteration sequence

Autonomous default: one change to an existing endpoint
(`agentrepl.v1.UpdateSidebarView`), then the roster view it publishes
(`frontend.v1.Roster*Section`). No new rpc.

## Landed changes

### 1. `UpdateSidebarView.change.size_sections`

- WHAT: a `SidebarViewSizeSections` arm carrying one `SidebarViewSectionSize`
  per section sized. Each size names its section with the arm messages
  `SidebarViewFoldSection` already uses, and its height as a oneof of
  `fitted` or `lines`.
- WHY: one drag changes two sections at once, and both must change in one
  write so no page ever draws half a drag.
- Consequences:
  - The server refuses an empty `sizes` or a section named twice as
    InvalidArgument (`daemon/internal/server/validate*.go`).
  - `fitted` is a oneof arm rather than an optional `lines`, per the
    no-sentinel rule. `lines = 0` is legal: it shows the header alone.

### 2. `RosterMergeQueueSection.height`, `RosterMergedSection.height`, `RosterBacklogSection.height`

- WHAT: each band section carries a oneof `height { fitted; sized { lines } }`
  with its own dedicated arm messages (figma→idl: one fact in three views is
  three wrappers).
- WHY: the roster push is what every page draws from, the same way it carries
  the fold.
- Consequences:
  - Storage: three nullable INTEGER columns on the `sidebar_view` singleton,
    NULL meaning fitted (`daemon/internal/wsm/sidebarview.go`).
  - The webapp requires the arm on every push (`requireCase`), so every
    roster fixture in the webapp tests states it.
  - The band fit gains a sized mode: a sized band shows
    `min(lines, its total)` lines, an unsized band its fitted share, and Open
    fills the rest. A rail too short shrinks unsized bands first, then sized
    ones lowest first.
