# Plan: a late feed row lands where it belongs

Status: implemented 2026-09-28 (branch feat/feed-row-order). Owner ruling (2026-09-27): a late row must be
placed where it would have been if it had not been late.

## The incident

- Two response rows from conversation `d8fc4c04` were written to the vendor
  transcript at 13:30:34 and 13:44:04.
- The daemon placed them live at 13:30/13:44, and again on the history plane
  after its 14:20:38 restart (seq 61, 63 and 173).
- The doom webview first drew them at 14:36:58, at rows 113–114 of 146. That
  was AFTER row 111 (an answer from 14:36:23) and its turn end (112), and
  before the 14:40 prompt (115).
- So they were about an hour out of place.

## Why it happens

- The daemon already knows every row's position.
  - Each feed keeps `rowRank{plane, seq}` per row, plus the ids in sorted
    order (`f.rank`, `f.order`, `daemon/internal/resolve/feed/resolver.go`
    ~360–377).
  - A row's rank is fixed at its first draw (`upsert`, ~741–750;
    `daemon.feed.row_placed`).
  - History rows are ranked in store order, and the store's
    `conversation.v1.HistoryEntryAt.at` is "stable across upserts: order is by
    the entry's FIRST appearance".
  - A row the daemon makes itself can take a neighbor's rank (`at.inherit`).
- `frontend.v1.FeedRow` carries no position: only `id`, `parent`, `turn` and
  the row arm (`proto/src/frontend/v1/feed.proto:116`).
- So the webapp orders rows by arrival. A pushed row it has never seen is
  appended at the end: `order.splice(index < 0 ? order.length : index, 0, id)`,
  called as `adopt(row, known ? -1 : order.length)` (`webapp/src/feed/feed-view.ts`
  ~780–795).

## Design

### 1. Proto

- Add `frontend.v1.FeedRowOrder order` to `frontend.v1.FeedRow`.
- It is an opaque key the client sorts by, set on EVERY row a page or a push
  carries.
- It means "where this row belongs in its feed", never "when it arrived".
- It is additive and required on the wire: a row without it is a malformed
  view, refused and logged, never defaulted.

### 2. Daemon: mint the key from facts that survive a restart

- `seq` is a counter local to one daemon process, so it cannot be the key: a
  handover or restart would reorder a live page.
- A DURABLE row's key comes from the store position of the entry the row came
  from (the entry's first appearance), plus a sub-index for several rows drawn
  from one entry (e.g. a thinking block and a text block of one message).
- A row the daemon makes itself (a turn's end, a queued-prompt mirror, a
  detached head) inherits the store position of the entry it follows, plus its
  own sub-index. This is the `at.inherit` idea made durable.
- One minting function computes the key, and every row placement goes through
  it.

### 3. Webapp: order strictly by key

- `adopt` inserts a new row at its key's position, found by binary search over
  the loaded rows. Arrival order never decides position.
- A row whose key is OLDER than the oldest loaded row belongs to unloaded
  history.
  - It is not drawn.
  - Load-more then brings it in its place through the page path.
  - This is exactly the 14:36:58 case.
- An update to a known row never moves it, because its key never changes.
  Assert that the key is unchanged, and treat a change as a daemon invariant
  violation, logged at ERROR.
- A new row inserted ABOVE the viewport keeps the content under the reader
  still, through the existing `prependCompensation` cause in
  `webapp/src/scroll.ts`. There is no new scroll writer.

### 4. Logging

- Record every row placement at INFO in the webapp: the key, and the outcome
  (inserted at position, appended at the tail, or skipped as unloaded
  history).
- The daemon's re-pushes of already-placed rows are DEBUG-only today, so the
  push that delivered the late rows at 14:36:58 is invisible. Record
  publications of a row to an open tail at INFO, with the reason for the
  re-push.

### 5. Tests

- A late row lands between its neighbors (daemon key plus webapp insert).
- A row older than the loaded page isn't drawn, then appears in place on
  load-more.
- An update never moves a row, and a changed key logs an ERROR.
- Keys are identical before and after a daemon restart for the same entries.
- An insert above the viewport doesn't move the reader, and a following reader
  stays at the tail.
- A source-scan guard fails if `feed-view.ts` places a row by arrival order
  again (e.g. `order.length` as a new row's index).

## Rejected alternative

"Insert after row X" (a predecessor id on each push). It breaks whenever X is
itself late or not loaded, so it only makes misplacement less likely rather
than impossible.

## Related, out of scope for this plan

- Why the two rows were re-pushed at 14:36:58 at all. That will be visible
  once 4 lands.
- Why the doom workspace has a second conversation (`d8fc4c04`) writing into
  its book (`9a632c97`) since the 12:57 bounce.
