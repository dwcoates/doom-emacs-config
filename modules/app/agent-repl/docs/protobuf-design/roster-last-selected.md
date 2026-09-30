# The roster carries each workspace's last-selected instant

When the selected workspace closes, every client selects the workspace that was
selected before it (owner ruling, 2026-09-30). Emacs's in-memory selection
history starts empty with Emacs, so after a restart it could not know which
workspace came before. The daemon already keeps that fact durably
(`workspaces.last_selected_at`, stamped on every SelectWorkspace); the roster
did not carry it.

## Landed changes

1. `frontend.v1.RosterRow` gains `optional RosterRowLastSelected last_selected`
   (tag 37), `RosterRowLastSelected { int64 at_ms }`. Settled by the coordinator
   under the owner's pre-authorization of proto decisions for this work.
   - Why a new field rather than the existing `RosterRowWhen.last_selected` arm:
     that arm is a display value in a oneof whose chosen arm is ACTIVITY; it was
     retired as the when-column's source and the daemon never fills it. It does
     not claim to carry the selection instant, so it is not a daemon defect.
     Selection recency is an ordering fact, not a drawn one, and cannot share a
     oneof with the drawn when-column.
   - Producer: the daemon's sidebar resolver fills it from the durable
     `LastSelectedAt` on every roster push; unset when never selected. Closed
     rows carry it too.
   - Consumers: Emacs orders by it (the landing after a close, and
     `agent-repl-open-most-recent-workspace`) through one helper; the webapp
     decodes it and draws nothing from it.
   - Sweep: `RosterRowWhenLastSelected` / `RosterRowWhen.last_selected` is a
     candidate for removal (no producer; kept for wire compatibility with older
     daemons per its own comment). Suggested to the owner, not removed.
