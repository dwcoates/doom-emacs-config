# daemon/internal/ssm/

The session-state manager: an in-daemon Go module (not a separate process)
owning the resolved per-workspace state (`thinking`, `idle`, `idle_async`,
`done`, `merged`, `merge_conflict`, …) in its own SQLite database
(append-only state log). It ingests lifecycle events forwarded from shim
streams plus daemon-local merge transitions, resolves current state via SQL
(precedence is a query, not a hardcoded ladder), and loud-logs every
transition with its causing event kind and seq.

This module replaces the Emacs-persisted workspace agent-state entirely.

## The paint axis is a latch that re-arms

Green promises two things: the route is usable AND a frontend has attested
drawing the history. The second half is the paint axis, and it has three
edges rather than one:

- OPENS when a session asserts readiness (`Apply`, alongside `ready`). A new
  route has been attested by nobody, so the workspace is blue until someone
  draws it. Without this edge the axis contributed no candidate at all for a
  workspace that had never been acked, and the documented blue gate never
  engaged — a workspace resolved green with no paint row in the log.
- CLOSES on `ApplyPaintAck`, and only for a render that actually DREW. A
  suspended webview's acknowledgment settles that state's delivery to Emacs
  and attests nothing here.
- RE-OPENS on `ApplyPaintLost`, called by the frontend server when a painting
  connection attaches or leaves. Attestation is a claim by one renderer about
  one connection, so a renderer that is gone cannot keep a workspace green.

`paintWatermark` deliberately drops the seq on withdrawal, so a re-attestation
after a break starts from nothing rather than inheriting the pre-break seq.

Dependencies: `proto/agentshim/` (generated Go), SQLite.
