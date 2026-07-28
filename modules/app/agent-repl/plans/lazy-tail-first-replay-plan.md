# Tail-first paint, early ready, lazy backward replay

Design plan. Nothing here is implemented. Every load-bearing claim cites the code
it rests on.

## The three settled intents, restated as mechanism

1. **Tail-first paint.** On open/resync the daemon serves the NEWEST window of
   the conversation first; the webview draws it immediately and fills upward.
2. **Early ready.** The paint attestation fires once roughly a screenful of the
   newest content is on screen. That is the moment the user can work, and it is
   what gates the ready signal Emacs sees.
3. **Lazy backward replay.** The initial replay serves a couple of visual pages.
   Scrolling up requests the next older window, repeatedly, until the replay
   floor (the newest clear or compaction) or history start is reached.

## Where the system stands today

The path a conversation takes on mount, end to end:

- The webview mounts, connects, and on the first `StateSnapshot` fires exactly
  one `ResyncCmd(workspace, lastSeq)` — `webapp/src/connect-resync.ts:80-94`,
  driven from `webapp/src/main.ts:839`. Fire-once is a hard requirement because
  the daemon answers a `ResyncCmd` by re-sending a fresh `StateSnapshot` to that
  client (`daemon/internal/frontend/server.go:655-668`), so "resync on every
  snapshot" is an infinite loop.
- `Manager.Resync` (`daemon/internal/sessiondrv/driver.go:781-805`) floors the
  request at the newest clear-or-compaction (`driver.go:908-925`), replays the
  retained ring from there (`sinks.go:860-896`), and — when the request falls
  below the ring floor (`sinks.go:908-917`) — closes the gap with a bounded,
  frontend-initiated store re-pull (`repull.go:119-168`).
- The re-pull crosses the shim as a `core.v1.ReplayRequest{from_seq, to_seq,
  max_events}` (`proto/agentshim/core/v1/core.proto:482-494`), served from a
  throwaway store subscription
  (`agent-shim/claude/shim/src/uds/store-client.ts:757-826`).
- Items land in the store keyed by seq rank, not arrival order
  (`webapp/src/store.ts:818-829`, `webapp/src/streaming.ts:158-167`), and
  `lastSeq` is a monotone max (`store.ts:827`).
- Renders are coalesced onto animation frames (`main.ts:658-688`), and every
  render ends with `attest.painted(paintSnapshot())` (`main.ts:636-651`), whose
  `throughSeq` is `store.state.lastSeq` (`main.ts:631-634`).
- The ack reaches `commandHandler.PaintAck`
  (`daemon/internal/server/frontendcmd.go:530-554`), which settles the delivery
  gate for Emacs (`frontend/server.go:669-676` →
  `frontend/paintgate.go:181-195`) and, on `PAINTED`, advances the SSM paint
  watermark (`ssm/ssm.go:466-489`, `ssm/db.go:223-247`). Green requires it
  (`proto/agentshim/frontend/v1/frontend.proto:45-61`).

Three facts about today's shape matter for everything below.

- **The whole history is served in one shot.** There is no notion of a window in
  the resync path. A ~7,000-event conversation streams ~7,000 deltas before the
  resync's `CommandAck` returns, capped only at `repullMaxEvents = 20000`
  (`repull.go:24`).
- **A tail-first renderer already exists and production never runs it.**
  `FeedRenderer.renderRestored` builds shells, fills the NEWEST chunk first, and
  backfills upward with per-chunk scroll compensation
  (`webapp/src/render.ts:3037-3130`, `backfillChunks` at `render.ts:2318-2327`).
  `main.ts:638` calls `feed.render` and nothing else. The good ideas are already
  written; they are wired to nothing.
- **The seq space is per vendor-session-uuid, and a rotation retires it.** The
  daemon resets cursors, purges the ring, and bumps `driven.rotEpoch`
  (`driver.go:1346-1413`); the client wipes items and `lastSeq` and re-asks from
  zero (`webapp/src/session-rebase.ts:94-108`, `main.ts:799-867`). One seq holder
  is deliberately NOT reset — the SSM paint watermark, the "KNOWN residual" named
  at `driver.go:1332-1343`.

---

## 1. The wire contract

### Decision

Leave `core.v1.ReplayRequest` **unchanged**. It already expresses exactly one
window: `[from_seq exclusive, to_seq exclusive)` with `max_events`
(`core.proto:482-494`). Every window this design serves is one of these. What is
new is a frontend-surface vocabulary for ASKING for a window, and an answer that
says where the window stopped.

Newest-first is a **request-ordering property, not a serving-order property**.
The store serves `WHERE session_id = ? AND seq > ? ORDER BY seq ASC`
(`agent-shim/shim-store/internal/db/query.go:19-43`). Windows are therefore
served ascending WITHIN a window; the client asks for them newest window first.
Nothing on the shim or store hop has to learn to read backwards.

### Three additive changes, all in `frontend.proto`

**(a) `ResyncCmd` gains a tail budget.** Field 2, the first free number.

```proto
message ResyncCmd {
  uint64 from_seq = 1;
  // The NEWEST-events budget for this resync: serve at most this many events,
  // anchored at the conversation's high water rather than at from_seq.
  //
  // A frontend paints the tail first, because the tail is what the user can
  // act on; history it has not scrolled to is history it does not need yet.
  // The daemon therefore serves [max(floor, high_water - tail_events + 1),
  // high_water] and reports the window's real lower bound back on a
  // HistoryWindowView, from which the frontend derives its next request.
  //
  // 0 preserves the pre-windowing behavior — everything from the floor
  // upwards — which is what a frontend that does not implement backward
  // paging must keep receiving.
  uint32 tail_events = 2;
}
```

**(b) A new `HistoryWindowCmd`,** `FrontendCommand` field 23 (22 is
`session_health`, `frontend.proto:467`).

```proto
// Additive: one step of DEMAND-DRIVEN backward history paging.
//
// It is NOT a ResyncCmd with different bounds, and must not become one. The
// daemon answers every ResyncCmd by re-sending a full StateSnapshot to the
// requesting client (frontend/server.go readLoop), which re-runs the paint
// gate's per-workspace adoption and emission. A backward page is a
// CONVERSATION-ONLY request the user generates by scrolling, potentially once
// per gesture, and it must cost neither a snapshot fan-out nor a generation.
message HistoryWindowCmd {
  // EXCLUSIVE upper bound: the OLDEST seq the frontend already holds. The
  // daemon serves the window immediately below it. Zero is a malformed
  // request, not "from the beginning": a frontend with nothing in hand asks
  // with a tail-anchored ResyncCmd, which is a different question.
  uint64 before_seq = 1;
  // The frontend's budget for this one window. 0 takes the daemon's default.
  // Carried by the REQUESTER for the same reason ReplayRequest.max_events is:
  // how much a frontend can absorb in one paint is the frontend's business.
  uint32 max_events = 2;
}
```

**(c) A new `HistoryWindowView` push frame,** `FrontendFrame` field 18 (17 is
`session_health`, `frontend.proto:130`). Correlated by `request_id`, exactly as
`DaemonHealthView` (`frontend.proto:148-150`) and `SessionHealthView` already
are — this is an established idiom on this surface, not a new one.

```proto
// Additive: what ONE served history window actually covered, and whether
// there is anything older to ask for.
//
// It is a separate frame rather than a field on CommandAck because it is an
// ANSWER, not a receipt: a frontend that stops paging must stop because the
// daemon said there is nothing older, never because an ack came back ok.
message HistoryWindowView {
  string request_id = 1;   // the ResyncCmd's or HistoryWindowCmd's id
  string workspace = 2;
  string session_id = 3;
  // INCLUSIVE oldest seq this window actually served; 0 when it served no
  // seq-bearing event at all.
  uint64 served_from_seq = 4;
  // INCLUSIVE newest seq this window served; 0 with served_from_seq.
  uint64 served_to_seq = 5;
  // The before_seq the frontend should send for the NEXT older window.
  // ZERO MEANS STOP: there is nothing older to serve, and a frontend that
  // keeps asking is asking for history that does not exist.
  uint64 next_before_seq = 6;
  HistoryExhausted exhausted = 7;
}

// Why a window is the last one — or that it is not.
enum HistoryExhausted {
  // More history exists above next_before_seq. Keep paging.
  HISTORY_EXHAUSTED_UNSPECIFIED = 0;
  // The window reached the REPLAY FLOOR: the newest clear or compaction, which
  // is itself the oldest item served. Nothing above it will ever be served,
  // because the agent no longer carries it (sessiondrv replayFloor).
  HISTORY_EXHAUSTED_FLOOR = 1;
  // The window reached the start of this conversation's seq space. There is no
  // older history, floor or not.
  HISTORY_EXHAUSTED_HISTORY_START = 2;
  // A BOUND tripped before the requested window was filled (the event cap, an
  // idle store subscription, a lost link). PART of the window arrived and more
  // exists: next_before_seq is set and the frontend may ask again. Reported
  // rather than presented as complete, on the same terms ReplayDone.truncated
  // already is (core.proto ReplayDone).
  HISTORY_EXHAUSTED_TRUNCATED = 3;
}
```

The tail-anchored `ResyncCmd` emits a `HistoryWindowView` too, under the
resync's own request id. One answer shape for both questions; the client needs
no second rule to learn where its history starts.

### Daemon-side plumbing this implies

- `server.Resyncer` (`frontendcmd.go:108-110`) grows to
  `Resync(workspace string, fromSeq uint64, tailEvents uint32) (*frontendv1.HistoryWindowView, error)`
  and gains
  `ReplayWindow(workspace string, beforeSeq uint64, maxEvents uint32) (*frontendv1.HistoryWindowView, error)`.
- `consumer.resync(fromSeq)` (`sinks.go:860-896`) becomes
  `resyncRange(lo, hi uint64, withLocalItems bool)`. Two changes, both load-bearing:
  - an **upper** bound, which it has never had; and
  - a flag for the seq-less retained items (permissions `sinks.go:879-881`,
    failure cards `885-887`, prompt receipts `892-894`). Those rank at the feed
    tail, so they belong to the TAIL window only. Replaying them per backward
    page re-pushes a live permission card on every scroll gesture.
- `startRepull`'s coalescing rule is **wrong for windows today**
  (`repull.go:128`): `covered := sameSpace && cur.fromSeq <= fromSeq` ignores
  `stopAt` entirely. With one request shape that is harmless; with windows, a
  second request whose range sits ABOVE the in-flight pull's `stop_at` would be
  declared served by it and answered with silence. It must become
  `cur.epoch == epoch && cur.fromSeq <= fromSeq && cur.stopAt >= stopAt`.
- Window arithmetic lives in `Manager`, beside `replayFloor` and `lastSeenSeq`
  (`driver.go:908-946`), because both bounds are already computed there:
  - tail window: `hi = lastSeenSeq(d)`, `lo = max(replayFloor(d, fromSeq), hi - tailEvents + 1)`.
  - backward window: `hi = beforeSeq - 1`, `lo = max(replayFloor(d, 0), hi - maxEvents + 1)`.
  - `next_before_seq = lo` when `lo > replayFloor`, else `0` with the matching
    `exhausted` reason.
  - the store hop still takes the EXCLUSIVE lower bound via
    `exclusiveLowerBound` (`driver.go:959-964`) — unchanged.

---

## 2. Renderer semantics

Prepending older content above drawn content touches five things. Four already
work; one is broken today and is the prerequisite for everything else.

### 2.1 Item identity — the one real breakage

`render.ts:2027-2053` keys a feed node by `itemKey(item, index)`. `text`,
`thinking`, `tool`, `permission`, `failure`, `context-cleared`,
`context-compacted` and most `user-turn`s key on a stable identity. But
`result` and `system` — and a `user-turn` with neither request id nor uuid —
fall through to `` `${item.kind}:${index}` ``.

Prepending 40 older items shifts every index by 40. Every `result` node's key
changes, so `renderImpl`'s reconcile (`render.ts:3252-3291`) sees every result
as new, mounts a fresh node, and evicts the old one at `render.ts:3328-3333`.
A backfill step would tear down and rebuild the results in the drawn feed — and
`lastUserTurnId` (`render.ts:2067-2073`) is index-derived too, so a prepend can
fabricate a "fresh prompt" and repin the feed to the tail
(`repinsToTail`, `render.ts:2298-2307`) at the exact moment the user is
scrolling up.

**Decision.** `ResultItem` and `SystemItem` (`store.ts:197-231`, `300-303`) carry
the wire envelope's `uuid` (`frontend.proto:298`), which the adapter already
receives and discards, and `itemKey` keys on it. The index fallback stays for
fixtures only. This is a pure identity fix, lands dark, and is worth landing on
its own merits regardless of the rest of this plan.

### 2.2 Ordering and reconciliation — already correct, do not touch

- `insertBySeq` (`streaming.ts:158-167`) walks backwards from the tail and
  splices at seq rank. An older window's items land above newer ones with no
  special case.
- `mergeItem` (`store.ts:862-888`) replaces content in place and explicitly
  preserves the standing rank (`merged.seq = existing.seq`, line 882), so a
  window that overlaps the previous one by an event re-renders rather than
  duplicates.
- `lastSeq` is monotone (`store.ts:827`), so a backward window carrying LOWER
  `throughSeq` values cannot lower the store's high-water mark. This is what
  makes the live tail continue to rank correctly mid-backfill.
- `SmoothReveal` seeds every non-tail block fully shown (`smooth.ts:112-140`), so
  backfilled prose renders whole instead of typing itself out. No change.

### 2.3 Truncation at clear — stable under backfill, by construction

`itemsFromClearOrCompact` slices from the LAST clear or compaction
(`clear-compact.ts:77-82`). The daemon floors every replay at the newest one
(`driver.go:908-925`), so **a backward window can never deliver an item older
than the current boundary** and therefore can never introduce a new boundary
above it. Backfill cannot move the truncation point. A LIVE clear can, and
`renderImpl` already handles that by rebuilding the feed when the boundary key
changes (`render.ts:3227-3233`) — see §5.

### 2.4 Scroll anchoring

`renderRestoredImpl` already compensates per chunk
(`render.ts:3116-3118`: `scrollTop += scrollHeight - before`), but production
never calls it, and the `scrollHeight` delta conflates growth above the viewport
with growth at the tail.

**Decision.** Anchor on a NODE, not on total height, and put the arithmetic in
`scroll.ts` beside `isPinnedToBottom` and `parkAtTail` (`scroll.ts:72-121`) as a
pure helper with its own test:

```
anchorShift({ before: number; after: number }): number   // after - before
```

`renderImpl` records the topmost mounted node's key and its viewport-relative
top before reconciling, and after reconciling adds the shift to `scrollTop` —
applied exactly when `repinsToTail` returned false, which is already the "the
user owns the scroll position" branch (`render.ts:3215-3220`, `3335-3337`). Tail
growth moves nothing, prepends move exactly their own height, and a pinned feed
still parks.

### 2.5 One render path

**Decision: delete `renderRestored`, `renderRestoredImpl`, `backfillChunks`,
`BACKFILL_CHUNK`, `backfillQueue`, `scheduleBackfill`, `flushBackfill`.**

They exist to make a huge first paint tolerable by spreading it across frames.
Lazy replay removes the huge first paint at the source: the window budget IS the
mechanism that keeps the first paint small, and a second render path that
production never runs is a divergence waiting to be found in a bug report. The
scroll compensation, the one idea worth keeping, moves to `renderImpl` per §2.4.

After this there is one render, and it is tail-anchored by default because
`repinsToTail` + `parkAtTail` already make it so.

---

## 3. Attestation redesign

### 3.1 What "painted enough" now means

Today `PaintAckCmd.through_seq` is documented as "the conversation through
`through_seq` the frontend has painted" (`frontend.proto:490-497`), and readers
take it to mean `[0, through_seq]`. Under lazy replay that reading is false: the
frontend has painted `[oldest_held, through_seq]` and has deliberately not asked
for the rest.

**Decision: keep `through_seq`, keep its number, keep its role, change its
documented meaning, and add its lower bound.**

- `through_seq` remains the NEWEST seq drawn. Its value is unchanged — it is
  already `store.state.lastSeq` (`main.ts:631-634`), the monotone max, which is
  the tail either way. Every consumer keeps working: the SSM versions acks by it
  (`ssm.go:466-489`), `paintWatermark` reads the latest `painted` row's
  `cause_seq` (`ssm/db.go:223-247`), and neither the paint gate
  (`paintgate.go:181-195`) nor `AttestsPaint` (`paintgate.go:328-330`) reads it
  at all.
- New `PaintAckCmd.painted_from_seq` (field 4):

```proto
  // Additive: the OLDEST seq in the painted window.
  //
  // The attestation is a RANGE, and saying so is what keeps it honest under
  // demand-driven backward replay: a frontend paints the newest screenful and
  // asks for older history only when the user scrolls to it, so "painted
  // through X" no longer implies "painted everything below X".
  //
  // EVIDENCE, NOT A GATE. Nothing daemon-side branches on it, deliberately:
  // how much of a conversation is enough to be workable is a judgment only the
  // surface with a viewport can make, and a daemon-side policy over this field
  // would be that judgment made by the wrong end. It exists so the log and any
  // future audit can tell a screenful from a whole history.
  uint64 painted_from_seq = 4;
```

- No new `PaintOutcome`. `PAINTED` and `SUSPENDED` remain the only two, and
  their meanings are untouched (`frontend.proto:524-538`). Adding a
  "partially painted" outcome would import the client's viewport judgment into
  the daemon's vocabulary, which §3.1 exists to avoid.

### 3.2 The first-ack gate

Every render ends in `attest.painted` (`main.ts:650`), and `PaintAttestation`
dedupes by watermark (`paint-attest.ts:86-94`). With tail-first serving, an
early render is now a render of the NEWEST content — which is exactly what
should be attested. One case must still be held back.

A render triggered by the `SessionView` alone, before any `ConversationDelta`
has landed, attests `through_seq = 0` — which the wire defines as a REAL
attestation of an empty history (`frontend.proto:493-496`) and which greens the
workspace. Today the slow whole-history replay masks this by keeping renders
scarce; a fast tail-first mount removes that accident.

**Decision.** `PaintAttestationOptions` gains
`hasContent: () => boolean` — consulted **only before the first PAINTED ack of a
connection**, never after. It returns true when either:

- the feed fills the viewport (`container.scrollHeight >= container.clientHeight`,
  the same geometry `isPinnedToBottom` already reads at `scroll.ts:72-74`), or
- the tail window reported `exhausted != UNSPECIFIED` — a genuinely short
  conversation is fully painted and must reach green.

A session that has never been prompted has neither, and the daemon's own
never-blue readiness path is what covers it: the shim's `ShimReady`
(`core.proto:418-434`) and the settled backfill (`sinks.go:653-668`). The gate
delays a first attestation; it never suppresses one, because the tail window's
`HistoryWindowView` always arrives and always releases it.

### 3.3 Compatibility, and the known residual

- **The residual is unchanged.** `driver.go:1332-1343` names the SSM paint
  watermark as deliberately not reset across a vendor session rotation: a
  retired-space attestation survives and swallows the new space's lower acks
  until they climb past it, erring green rather than permanently blue. Nothing
  here touches it. Worth stating explicitly because it is the one place a
  reviewer will expect this plan to have made things worse, and it has not: the
  client still acks the max seq it holds, and `attest.rebaseSeqSpace()`
  (`paint-attest.ts:123-125`) still resets only the CLIENT's seq gate.
- **Backward windows never lower a watermark.** `paintedSeq` is a max
  (`paint-attest.ts:90`) and `lastSeq` is a max (`store.ts:827`), so the SSM's
  supersede rule (`ssm.go:477-481`) sees a strictly non-decreasing sequence. A
  scroll-up cannot un-green a workspace.
- **A frontend that sends `tail_events = 0` and never sends
  `HistoryWindowCmd` behaves exactly as today.** That is the compatibility
  story, and it is why every wire change here is additive with a
  behavior-preserving zero value.

---

## 4. Ready-signal wiring

The ready signal Emacs sees is a `WorkspaceState` carrying
`RENDER_STATE_READY` (`frontend.proto:88-95`) that the paint gate has released
to observers. Its two halves:

- **Delivery.** `frontend/server.go:669-676` settles the held emission BEFORE
  dispatching the command, so the observer's queue stays in generation order;
  `paintgate.settleLocked` (`paintgate.go:181-195`) refuses an ack naming an
  older generation.
- **Attestation.** `commandHandler.PaintAck` forwards only a `PAINTED` outcome
  to `ssm.ApplyPaintAck` (`frontendcmd.go:550-553`), which appends the
  `painted` row and re-resolves (`ssm.go:483-488`).

Nothing in that path changes. What changes is **when the first ack happens** —
and because both halves hang off the same ack, moving it earlier moves the tab
bar's release and the green together, with no new ordering to reason about.

### Interaction with the in-flight gated handshake

The single gated handshake (`core.proto:344-434`) makes `ShimReady` the
daemon-side "this session is fully wired" assertion: session lock held, SDK
query built, store producer up, standing subscription open at
`DaemonHello.from_seq` and settled. The paint attestation is the frontend-side
"this session is renderable" assertion.

They are now the two symmetric one-shot gates on green, and this plan makes them
fire on comparable timescales instead of the second lagging the first by a whole
history replay. Two concrete dependencies fall out:

- **The tail window's `hi` is only meaningful after the handshake.** `hi` is
  `lastSeenSeq(d)` = max(durable `last_seen_seq`, `newestRetainedSeq()`)
  (`driver.go:940-946`). `DaemonHello.from_seq` is that same durable mark
  (`core.proto:396-416`) and is read AFTER the rotation reconciliation. Serving a
  tail window against a session whose handshake has not completed would anchor
  at a mark the rotation is about to zero. The tail-window path must therefore
  fail loudly on a workspace with no live driver — which `m.existing(workspace)`
  (`driver.go:782-785`) already does — rather than serving an empty window and
  letting the client conclude "history exhausted".
- **`HISTORY_EXHAUSTED_HISTORY_START` is only true post-handshake.** Pre-ready,
  the honest answer is a refusal, not an exhaustion.

### Interaction with the createSession completion ack

`CommandDispatcher.createSession` correlates on the pushed `SessionView`
(`webapp/src/command-dispatch.ts:306-334`). A freshly created session has no
history at all, so tail-first changes nothing for it — and it must stay that way:

**The createSession completion ack must NOT be re-gated on paint.** A brand-new
session's first attestation is the `through_seq = 0` "there was nothing to draw
and I drew it" ack, and §3.2's `hasContent` gate deliberately withholds exactly
that until the tail window answers. Coupling create-completion to the paint ack
would make session creation wait on a webview's viewport measurement. Creation
completes on the daemon's own evidence (registry record, `ShimReady`, the pushed
`SessionView`); green arrives afterwards, from the frontend, as it does for every
other session.

---

## 5. Ordering and epoch hazards

### 5.1 Out-of-order backward windows

Covered structurally by §2.2 — seq rank, monotone `lastSeq`, rank-preserving
merge. But the client needs a **second mark** the store does not have today:

**Decision.** `StoreState` gains `oldestSeq: number` — the lowest seq the store
holds, `0` when it holds none. It is NOT derivable from `lastSeq`, it is the
`before_seq` of the next window request, and it must be reset wherever `lastSeq`
is: `initialState()` (`store.ts:501-528`), `reset()` (`store.ts:656-660`), and
`rebaseSeqSpace()` (`store.ts:678-681`). Forgetting the third is the
rotation-mid-scroll bug, and it is the reason this belongs in `StoreState`
rather than in a controller beside it.

### 5.2 Seq-less items ranked at `lastSeq` — an existing misrank this plan makes likelier

`applyConversationItems` ranks a `throughSeq == 0` push at `this.state.lastSeq`
(`store.ts:818-829`) — the live tail, which is right. But `Manager.Resync`
pushes the retained seq-less items (permissions, failure cards, prompt receipts)
from `consumer.resync` at `driver.go:787`, **before** `startRepull` at
`driver.go:797` streams any history. On a cold daemon the ring is empty, so a
freshly-mounted client with `lastSeq == 0` ranks a pending permission card, a
failure card, and the user's own prompt receipt at rank 0 — the very top of the
feed, above the entire history that is about to arrive.

This is latent today (commit `4e1d88f9` "rank a seq-less daemon push at the feed
tail" moved the rank off a hardcoded 0, but not the ordering that produces the
zero). Tail-first makes it likelier, because the tail window is small and the
client's `lastSeq` starts at 0 either way.

**Decision.** Reorder `Manager.Resync` so the seq-less local items are pushed
**after** the range is served — ring first, then re-pull, then locals. That is
what `resyncRange(lo, hi, withLocalItems)` (§1) is for. Independently landable,
independently valuable, and a prerequisite for trusting the tail window's rank.

### 5.3 Rotation mid-scroll

Daemon side, `onHandshake` (`driver.go:1346-1374`) already resets the registry
cursors, purges the ring, and bumps `driven.rotEpoch` (`driver.go:1398`), and
`startRepull` refuses to coalesce across epochs (`repull.go:135-138`). Two
additions:

- A `HistoryWindowCmd` must be stamped with the epoch it was computed in and
  **refused loudly** when the epoch has moved, exactly as a cross-epoch re-pull
  is. A `before_seq` from a retired space is a number with no meaning in the
  current one — the same class of bug `replayFloor`'s retired-mark ruling exists
  for (`driver.go:898-925`).
- `driver.go`'s SEQ-HOLDER INVENTORY comment (`driver.go:1284-1345`) is a
  checklist a new seq holder must join. The window cursor is a new seq holder.
  It goes in the "RESET ON ROTATION" list, and so does the client-side
  `oldestSeq` in the corresponding client-side reasoning at
  `session-rebase.ts:1-40`.

Client side, `main.ts:806-815` wipes items, `lastSeq`, the reveal cursors and the
attestation seq gate on a `rotated` verdict, then re-resyncs from zero
(`main.ts:861-867`). Under this design that resync carries `tail_events`, any
in-flight `HistoryWindowCmd` is abandoned (its answer counts in the retired
space), and the exhausted latch is cleared.

### 5.4 A clear arriving mid-backfill

The daemon records the floor BEFORE pushing the clear (`sinks.go:524-531`,
`545-562`) and drops unclaimed prompt receipts with it (`sinks.go:558-561`).
Every window served afterwards is bounded below by the new floor
(`driver.go:908-925`), so the daemon will never serve above it — a stale
`before_seq` is CLAMPED, and `served_from_seq` on the answer is what tells the
client its cursor moved.

Client side, the render already truncates at the boundary
(`clear-compact.ts:77-82`) and rebuilds the DOM when the boundary key changes
(`render.ts:3227-3233`). What is new: the backfill controller must reset
`oldestSeq` to the boundary item's seq and clear the exhausted latch, because
the history it had been paging into no longer exists. An in-flight window's
answer is harmless — its items are above the boundary and the truncation drops
them — but its `served_from_seq` must not advance the cursor.

### 5.5 Resync after rebase

`main.ts:861-867` sends `resync(ws, 0)`. Under this design it sends
`resync(ws, 0, tailEvents)`. Zero `from_seq` plus the daemon's own floor is
already the correct question; the budget just stops the answer from being the
whole new space. No other change.

### 5.6 Prompt receipts and the seq-less ranking

Beyond §5.2: the receipts are pushed from the tail path only
(`resyncRange(..., withLocalItems=true)`), never per window. Their client-side
rank is `lastSeq` (`store.ts:822`), which under tail-first is the true tail from
the first window onward — so a receipt lands where a just-sent prompt belongs.
The ordering "range first, locals last" is now **load-bearing** where it used to
be incidental, and gets its own test (§7).

---

## 6. Store and daemon serving

### Where windows come from

- **Inside the ring** (`ringCap = 4096`, `sinks.go:121-125`): served from
  `snapshotRing()` (`sinks.go:305-311`) by the new bounded `resyncRange`. No I/O.
- **Below the ring floor** (`sinks.go:908-917`): served by the existing bounded
  re-pull (`repull.go:119-168`) → `shimclient.Replay` (`shimclient/replay.go:133-183`)
  → `ReplayRequest` → the shim's `serveReplay`
  (`agent-shim/claude/shim/src/uds/uds-session.ts:358-380`) → a throwaway store
  subscription (`store-client.ts:757-826`). No new route, no new socket, no new
  message on the shim hop.

The four standing constraints on a re-pull (`repull.go:83-104`) all still hold:
frontend-initiated, bounded, side channel, conversation only. A window is a
smaller instance of exactly the thing that already exists.

### Cost model

The store's index is `PRIMARY KEY (session_id, seq)`
(`agent-shim/shim-store/internal/db/db.go:83-94`), so
`WHERE session_id = ? AND seq > ? ORDER BY seq ASC` (`query.go:19-24`) is an
index SEEK to `from_seq`, not a scan from zero. Good.

But `ReplayFrom` **materializes the entire tail** into a slice before the server
streams it (`query.go:19-43`, `shim-store/internal/server/server.go:391`), and
`Subscribe` carries no upper bound (`core.proto:454-458`). The shim's replay
discards everything at or above `to_seq` client-side
(`store-client.ts:800-803`). So one window costs
O(events above `from_seq`) inside the store, regardless of the window's size.

For the path that matters this is a **strict improvement**: today's connect
resync asks from the floor and materializes the whole history; the tail window
asks from near the high water and materializes almost nothing. The pathological
direction is the other one — paging back through a long history, where each
successive window asks from a LOWER `from_seq` and materializes MORE.

**Decision: exponential window growth.** The client's `max_events` starts at the
tail budget and doubles per backward step, capped at `repullMaxEvents`
(`repull.go:24`). A full scroll-back through 7,000 events then costs ~8 windows
instead of ~175, and the total store work is O(history) — the same as today's
single resync — but paid only if the user actually asks for it. This is a pure
client-side policy choice; `max_events` is already the requester's to state
(`core.proto:490-493`).

### What the store and the sidecar need: nothing

- **The sidecar** is write-side only. It tails transcript files, keeps its cursor
  per FILE (`dev:inode`), writes into the store, and discards the write ack — it
  has no channel to the daemon at all (`frontend.proto:244-264`). It serves no
  reads and cannot observe a window. Nothing here reaches it.
- **The store** already serves every window this design needs, through the one
  primitive it has. Its API is contractually tiny — schema, seq, dedup, fan-out
  (`sinks.go:98-101`) — and this plan adds no verb to it.

The honest caveat, and the upgrade path if the cost model is wrong: a bounded
read (`to_seq`/`limit` on the store's replay, or a new bounded `ReadRange`
message) would make a window O(window). It is deliberately NOT in this plan —
exponential growth removes the motivating case, and adding a verb to the store to
serve a scroll gesture nobody has measured yet is speculative. The trigger for
revisiting is stated in the risk register.

---

## 7. Tests, rollout, risks

### Test strategy per seam

| Seam | Where | What |
| --- | --- | --- |
| Item identity | `webapp/test/render.test.ts` | a `result`/`system` node keeps its DOM node when 40 items are prepended; `lastUserTurnId` does not change on a prepend |
| Store ranking | `webapp/test/store.test.ts` | a backward window's lower `throughSeq` does not lower `lastSeq`; `oldestSeq` tracks the lowest held seq; `rebaseSeqSpace()` resets both |
| Insertion | `webapp/test/streaming.test.ts` | `insertBySeq` splices an older item above a drawn tail; a re-delivered item keeps its rank |
| Scroll anchor | `webapp/test/scroll.test.ts` | `anchorShift` pure helper, both signs and zero |
| Backfill controller | `webapp/test/history-window.test.ts` (new — one test file per source module) | arms on scroll-top proximity; one request in flight at a time; latches on `next_before_seq == 0`; unlatches on a boundary change; exponential budget |
| Attestation | `webapp/test/paint-attest.test.ts` | first ack withheld while `hasContent` is false; released on exhaustion of a short conversation; later acks never gated; `painted_from_seq` carried |
| Window arithmetic | `daemon/internal/sessiondrv/` (table-driven, AAA) | tail window bounds vs floor vs high water; backward window clamped at the floor; `next_before_seq`/`exhausted` for each of the four enum cases |
| Re-pull coalescing | `daemon/internal/sessiondrv/repull_test.go` | table over (epoch, from_seq, stop_at): only a strictly-covering in-flight pull coalesces |
| Local-item ordering | `daemon/internal/sessiondrv/sinks_test.go` | permissions/failures/receipts are pushed AFTER the range, and only on the tail path |
| Command surface | `daemon/internal/server/frontendcmd_test.go` | `HistoryWindowCmd` with `before_seq == 0` is refused loudly; with no resyncer wired, refused not swallowed |
| Delivery order | `daemon/internal/frontend/paintgate_test.go` | unchanged — asserts this plan did not move the gate |
| End to end | `daemon/e2e/` (beside `rotationresync_e2e_test.go`, `clearcompact_e2e_test.go`) | tail-first resync serves only the tail; a backward window reaches the floor and says so; a clear lands mid-backfill; a rotation lands mid-backfill |

Go tests are table-driven with AAA and use no `time.Sleep` for synchronization.
Webapp: `npm test` and `npm run typecheck` from `webapp/`. Proto: `make lint` in
`proto/`, then `make`.

### Rollout order

Steps 1-5 are **dark**: they change no user-visible behavior and can land
independently, in any order among themselves.

1. **Stable `result`/`system` identity** (§2.1). Pure fix; valuable alone.
2. **Local items pushed after the range** in `Manager.Resync` (§5.2). Fixes an
   existing top-of-feed misrank; valuable alone.
3. **`startRepull` coalescing compares both bounds** (§1). A no-op today,
   required before a second request shape exists.
4. **`consumer.resyncRange(lo, hi, withLocalItems)`** (§1). Existing `resync`
   becomes `resyncRange(replayFrom, MaxUint64, true)`.
5. **The three additive proto messages** + daemon serving (§1). Nothing sends
   them yet. `tail_events = 0` preserves today's behavior exactly.
6. **Scroll-anchored prepend in `renderImpl`; delete `renderRestored`** (§2.4,
   §2.5). Visible only on the gap-fill revisit path that already prepends today,
   where it is an improvement.
7. **The flip**: the client sends `tail_events` on its connect resync and drives
   `HistoryWindowCmd` from scroll (§1, §5.1). This is the one commit that changes
   what the user sees.
8. **`painted_from_seq` + the `hasContent` first-ack gate** (§3).
9. **Documentation**: `frontend.proto`'s `through_seq` comment, `driver.go`'s
   seq-holder inventory, `session-rebase.ts`'s header.

Deploy per `modules/app/agent-repl/AGENTS.md`: `bin/build-frontend.sh` then
bounce `claude-repld` for every step; the store and sidecar are untouched, so
their hand-deploy path is not involved.

### Risk register

| # | Risk | Mitigation |
| --- | --- | --- |
| 1 | **A fixed event budget does not map to a predictable amount of rendered height.** One tool card with a 500-line diff is a screenful; forty seq-less permission items are not. Under-fill turns "lazy" into "eager with extra round trips"; over-fill defeats the point. | The `hasContent` gate measures actual geometry, and the backfill controller arms on scroll-top proximity rather than on item count — so an under-filled window immediately requests the next one and the failure mode is round trips, not a broken feed. |
| 2 | Deep scroll-back is more expensive than modeled, because each window materializes everything above its `from_seq` in the store (§6). | Exponential window growth caps the window count at O(log history). **Trigger for a bounded store read:** a measured window response above ~500 ms, or a store CPU spike correlated with scroll-back, in the daemon log. |
| 3 | A `HistoryWindowCmd` per scroll gesture floods the command channel. | One request in flight per view, enforced client-side; `startRepull` already refuses a non-covering concurrent pull loudly (`repull.go:139-140`) rather than queueing. |
| 4 | The client's window cursor and the daemon's floor disagree after a clear. | The daemon clamps and reports `served_from_seq`; the client adopts the daemon's answer rather than its own arithmetic. The daemon is the only authority on the floor (`driver.go:908-925`). |
| 5 | Deleting `renderRestored` loses a behavior some path depends on. | It has exactly zero production callers (`main.ts:638` calls `feed.render` only); its only references are tests and one comment in `catalogue.ts:564`. |
| 6 | The unrebased SSM paint watermark (`driver.go:1332-1343`) interacts badly with a smaller painted range. | It cannot: both watermarks are maxima and this plan never lowers either (§3.3). Named here because it is where a reviewer will look. |

**The single riskiest assumption is #1** — that "roughly a screenful" can be
requested in units of store events. Everything else in this plan is bounded
arithmetic over identities the code already has; #1 is a guess about content
shape that only production traffic can settle.

---

## Open decisions for the user

1. **The window budget's unit and starting size.** Events is what the wire can
   carry cheaply (`max_events` already exists). Rendered height is what the user
   actually experiences. The plan picks events with a geometry-based gate on top;
   an explicit starting number (80? 120?) wants a real conversation to pick from.

2. **Whether "history exhausted" gets a rendered affordance.** A divider reading
   "the context was cleared here" at a `HISTORY_EXHAUSTED_FLOOR` is honest and
   costs one component; the alternative is a feed that simply stops scrolling.
   The plan takes no position.

3. **Whether to keep `renderRestored` for one release** rather than deleting it
   in step 6. The plan deletes it and argues why; keeping it is defensible if
   there is a rollback path that would want it.
