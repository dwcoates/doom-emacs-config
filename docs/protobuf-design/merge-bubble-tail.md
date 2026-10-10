# Merge bubble at the tail of the feed

A merge's bubble does not appear in the feed until the merge starts, so a
merge waiting in the queue shows only through the footer and the sidebar, and
the queue tab's live state is unreachable. The owner wants the bubble to appear
the moment the merge is queued and to stay at the bottom of the root feed for
as long as the merge is queued or merging. When the merge concludes (landed or
failed), the bubble settles where it is at that moment, and newer rows flow in
below it.

Today `frontend.v1.FeedRowOrder` keys are fixed for a row's life, so no row can
stay at the bottom while newer rows arrive. This change amends that contract.

## Context

- Objective (owner, 2026-10-10): the merge bubble appears immediately when a
  merge is queued and stays at the bottom of the feed only from when it is
  first drawn until the merge finishes. Once finished it stays where it
  finished, which is a place in the flow and not the bottom: rows written after
  the finish land below it.
- Ruling on conclusion (owner, 2026-10-10, "Settle where it ends"): pinned to
  the bottom while queued or merging; when it lands or fails it stays where it
  is and new output flows below it. The rejected alternatives were "pinned
  until the next prompt" and "pinned forever".
- The 2026-10-08 gate in `daemon/internal/merge/queue.go:358` ("ONLY A MERGE
  THAT STARTED HAS A BUBBLE") existed because a bubble placed at queue time is
  overtaken by every row the workspace writes while it waits. Pinning the
  bubble to the tail removes that reason, so the gate is removed.

## Landed changes
