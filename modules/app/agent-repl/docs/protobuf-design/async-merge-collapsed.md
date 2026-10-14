# Async merge bubbles start collapsed

The owner's specification (2026-10-14): in the feed, an ASYNC merge's bubble
starts collapsed wherever and whenever it is drawn (first appearance, every
progress push, its terminal, every redraw and reload), and only a SYNC merge's
bubble opens on its own. A reader's own open still wins over every later push.
Nothing else about either bubble's look changes.

## Landed changes

### `frontend.v1.FeedMergeHead.mode`, and the switch moves into its async arm

- WHAT: `frontend.v1.FeedMergeHead` gains `optional frontend.v1.FeedMergeMode
  mode = 7`, a oneof of `frontend.v1.FeedMergeModeSync` (empty) and
  `frontend.v1.FeedMergeModeAsync`. The head's `workspace_switch` (tag 5) is
  retired and reserved; the switch now rides
  `frontend.v1.FeedMergeModeAsync.workspace_switch`.
- WHY: the daemon decides every merge bubble's fold at one site
  (`daemon/internal/resolve/feed/mergefold.go` `mergeFoldDefault`), and that
  site sees only the row. Nothing durable on the row told sync from async:
  the switch is set only once the merge workspace is visible, and the pin only
  while a sync merge is in flight. A restored row after a daemon restart must
  still fold by mode, so the mode has to ride the durable head. The webapp
  infers nothing; it applies the fold the daemon ships, as before.
- WHY THE SWITCH MOVED: the switch was meaningful only for an async merge (its
  own comment said UNSET on every sync merge), which is the adjacent-exclusivity
  test: a field meaningful for one mode belongs in that mode's arm.
- CONSEQUENCES:
  - The merge orchestrator sets the mode on every head it publishes
    (`daemon/internal/merge/tabs.go` `headMode`). A head published through
    `upsertDurable` with no mode panics in `settleMergeFold`: it is a producer
    defect, never folded by a guess.
  - ACCEPTED COST: a durable head recorded before this change has no mode. On
    restore it takes the sync default (open, folded on success) and is logged
    at INFO as `daemon.feed.merge_mode_unrecorded`. Such an async row also
    loses its switch, since tag 5 is no longer read. Async merges and their
    switches are recent and merge workspaces are removed after 15 idle days,
    so the affected rows are few and age out.
  - The webapp reads the switch from `mode.async` and treats an unset mode as
    "no switch" (the legacy row above); it never requires the mode, because a
    legacy row must still draw.
