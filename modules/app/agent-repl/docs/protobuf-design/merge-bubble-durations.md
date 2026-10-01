# Merge bubble: queue table and tab durations

The owner (2026-10-01) asked for the merge bubble's queue tab to read as a
table: no "you are here" label (this workspace's row is subtly highlighted
instead), even rows, `workspace` / `stage` / `duration` columns with headers,
the duration being how long the entry has been in its CURRENT stage
(restarting at zero when the stage changes), sized like the expanded
footer's fixed columns; and each tab to show how long it was in its state.
The owner delegated the proto decisions to the lead.

## Landed changes

### 1. Start instants on the merge tab and on queue entries

- What: `frontend.v1.FeedMergeTabLive.started_at_ms` and
  `frontend.v1.FeedMergeTabSettled.started_at_ms` (the tab's work began);
  `frontend.v1.FeedMergeQueueMerging.stage_entered_at_ms` (the front's active
  tab began) and `frontend.v1.FeedMergeQueueWaiting.stage_entered_at_ms`
  (the entry was queued).
- Why: a duration the client ticks needs the instant it ticks from, and the
  file's convention ships instants, never durations (the client ticks; the
  daemon pushes nothing as time passes). The settled tab ships its start
  beside its existing end so its run time is fixed.
- The instant sits in each arm rather than beside the oneof, so it can only
  mean the stage that arm states.
- Sources: `merge_tab_intervals.started_at` (wsm) for tabs; for the queue,
  the front's active tab start and each waiting entry's queued time.
- Not modeled: the column headers are presentation chrome drawn by the
  webapp, as the expanded footer's `tokens` / `duration` headers are.
