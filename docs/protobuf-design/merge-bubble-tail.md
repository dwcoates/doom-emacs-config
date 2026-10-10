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

## Settled rules

- A feed row is either PLACED (a fixed order key, as every row is today) or
  PINNED (no place in the flow yet; drawn after every placed row). A pinned row
  is placed exactly once, when it settles, at the position it holds at that
  moment: it takes a key just after the feed's newest row, so every row written
  afterward is placed below it. From then on its key never changes. The order
  invariant becomes "fixed once assigned" (owner, 2026-10-10).
- The merge bubble is pinned the moment it is first created (when the merge is
  queued) and placed when the merge resolves (landed, failed, or abandoned)
  (owner, 2026-10-10).

## Landed changes

### 1. FeedRowOrder gains a pinned condition (agent-decided, owner delegated the design 2026-10-10)

- WHAT: `frontend.v1.FeedRowOrder` gains `optional FeedRowOrderPinned pinned`,
  an empty presence message. The order contract changes from "fixed for the
  row's life" to "fixed once assigned". A pinned row's key sorts after every
  unpinned key in its feed. It is re-pushed exactly once with a new key and
  `pinned` unset when it settles, and the new key sorts just after the feed's
  newest row at that moment.
- WHY: the owner wants the merge bubble at the bottom of the feed from the
  moment it is drawn until the merge resolves, and then to stay where it
  resolved.
- Alternatives rejected:
  - Moving `FeedRow.order` into a `oneof placement { order; pinned }`. It is
    more literal, but it churns every key read and fixture (about 120 webapp
    sites and 24 Go sites) for no extra guarantee. With the key-sort design,
    pinned rows sort correctly with no client special case.
  - A client-side pin with no key, drawn after all keyed rows. This would make
    clients decide position, which `FeedRowOrder` forbids ("a client never
    decides a position").
- `pinned` is an optional presence message, not a state oneof: absence means
  no condition, presence means the condition. It does not change how the key
  is interpreted, because sorting is by key either way.
- Consequences:
  - The daemon mints pin keys in a class that sorts after every placed class.
    Placed keys start with `0`, `1` or `2`; pin keys start with `~`.
  - A pin key is minted from the moment of pinning plus a counter. That makes
    it unique across daemon restarts, and the existing durable-row restore
    (`reserveOrder`) parses it like an entry key.
  - The daemon re-records a settled row's durable record at its new key.
  - The webapp accepts exactly one key change per row, from pinned to
    unpinned, by moving the row. Every other key change stays an ERROR.
  - New placed rows follow the newest PLACED row and skip pinned ones (the
    `predecessor` class match), so the pinned row stays below them.

### 2. Only the producer settles a pinned row (agent-decided, 2026-10-10)

- WHAT: the resolver's publication intent is a three-way choice. Keep (the
  default) restates a row as it stands. Draw pinned pins a row. Settle settles
  a pinned row. `UpsertDurablePinned` draws pinned and `UpsertDurable`
  settles. Every other path (a fold, a supersede re-push, a restore) keeps.
- WHY: the reader's fold on a merge bubble restates its head through the
  generic restate path. If "published without pin" had meant "settle", folding
  a queued merge's bubble would have settled it mid-flight.
- Consequences:
  - A placed row published pinned is refused at ERROR
    (`daemon.feed.pin_refused`) and keeps its place.
  - The merge's `publishHead` is the one site that decides pinned versus
    settled: no result means pinned, and any result settles.
  - The merge-in-line, restart, resume and adoption paths now all draw the
    bubble pinned. The 2026-10-08 `run.placed` / `started()` gate is removed,
    and the merge tests and `integration/merge_placement_test.go` that pinned
    "no bubble until started" now pin the reverse, per the owner's 2026-10-10
    ruling.
