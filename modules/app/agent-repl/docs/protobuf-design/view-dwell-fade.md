# The sidebar recedes a name over the view dwell

## Problem

Selecting a workspace that has not been viewed yet starts the editor's view
dwell (`agent-repl--tab-dwell-arm`, five seconds, or one for a `done` the user
watched land). While it counts, the sidebar row's name keeps its unviewed
colour, the green of a finished turn. When the dwell completes, the editor
reports the workspace viewed and the name jumps to the viewed grey. The owner
asked for a continuous change to the grey over the dwell, snapping back to the
unviewed colour when the user leaves before the dwell completes.

## Assumptions (autonomous design)

- The dwell stays an editor fact. Only Emacs knows that a workspace's panels
  are on screen and how long the user has looked at them. Moving the clock
  into the daemon would require a new panels-open report, and the dwell rules
  are owner rulings that live in `status.el`.
- The webapp must not derive the dwell itself. Its length rule (one or five
  seconds) and its eligibility (current, panels open, not demoted) would be a
  second copy of the editor's rule.
- Only the sidebar fades. The Emacs tab bar keeps its flip: animating it would
  mean repainting the tab bar every frame from a timer.

## Landed changes

### `agentrepl.v1.ReportViewDwell` (new endpoint)

- What: the editor reports a dwell `counting` (its length in ms) or `stopped`,
  on a workspace subject, with the same refusal arms as
  `MarkWorkspaceViewed`.
- Why: the daemon is the one source both surfaces draw from, so the dwell has
  to reach it before the sidebar can draw it.
- The daemon stamps the dwell's start on receipt, on its own clock, which is
  the clock every roster instant is on. The editor sends no instant. The cost
  of this choice is one RPC's latency: the sidebar's fade ends that much after
  the editor's timer fires, and the viewed marker follows a round trip later.
- Ordering: two reports in flight for one workspace could be served out of
  order, and a stop served after the count that followed it would end a live
  dwell. The editor (`agent-repl-host-report-view-dwell`) keeps at most one
  report in flight per workspace and sends the latest wanted report when the
  previous one is answered. This rules out reordering by design instead of
  relying on timing. The alternatives considered, and why they lost:
  - A generation or timestamp in the request breaks across Emacs restarts,
    which reset the generation, and ties within one millisecond.
  - A drop derived from the selection alone misses the panels being closed on
    the same workspace.

### `frontend.v1.RosterRowViewDwell` (new marker, `RosterRow.view_dwell = 75`)

- What: `started_at_ms` and `length_ms`. Present only on the selected row
  whose status is a turn end and whose result is unread, with a dwell
  counting.
- Why: a dwell drawn where the viewed marker would not follow (a running
  turn, `vendor_blocked`, an unread result under `idle_async`) would fade the
  name toward a grey it does not keep.
- Ended (`daemon/internal/resolve/sidebar`) by any of:
  - a viewed report (`SetViewed`), whether or not the report takes;
  - a stop (`StopViewDwell`);
  - the selection leaving the workspace (`SetSelected`, which ends the
    previous selection's dwell);
  - a status change (`resolver.row`, on the arm change).
- A dwell reported before its selection lands is kept, and drawn once the
  selection arrives. `ReportViewDwell` and `SelectWorkspace` travel
  independently.
- The editor reports a stop when a deadline finds the workspace ineligible
  (`agent-repl--tab-dwell-fire`). Without that report, the sidebar would have
  carried the name to the grey with no demotion following.

## Consequences

- The webapp crossfades the name's ink copies (`webapp/src/sidebar/view-dwell.ts`,
  `styles.css` `name-view-dwell-*`). This is the breath's compositor technique,
  so the fade keeps moving while the page's main thread is busy.
- A row carrying both `viewed` and `view_dwell` is a contract breach, and the
  webapp throws `MalformedView`.
- Emacs decodes the marker, because the roster decoder checks keys strictly,
  but draws nothing from it.
