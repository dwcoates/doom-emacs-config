# PLANNED: message pagination

**Status: designed and agreed, NOT implemented. Blocked on the message-lineage
consolidation landing first.**

This file records the pagination contract as settled, written as though
`MessageLineage` (`top_level_message_id` / `parent_message_id`) and the
Bubble→Message consolidation are already in place. It exists so the design is
not re-derived from a conversation, and so the eventual implementation is a
transcription rather than a rediscovery.

## Why this is blocked, and must stay blocked

The page contract is entirely a statement about *what a message is* and *what
contains it*. Freezing it while bubbles are still a parallel concept would bake
`top_level_message_id` values computed under a model that is being deleted, and
every one of them would then need rewriting. The consolidation lands first; this
follows.

---

## The defect this replaces

Three independent mechanisms let a client obtain the whole conversation, and the
observed symptom — every message replayed on Emacs startup, followed seconds
later by a "load earlier messages" affordance for messages already on screen —
is all three interacting.

### 1. `ResyncCmd.from_seq = 0` means "everything"

`from_seq` is a position the CLIENT computes, in the DAEMON's sequence space,
from its own applied state. Zero is a legal value and means "replay from the
floor" — the entire retained conversation.

`conversation-page.proto`'s own header documents both the cost and the decision
to keep the door open:

> "A cold webview had exactly one way to obtain the conversation it renders:
> ResyncCmd{from_seq: 0}, which replays EVERY store event the session ever
> produced. The worst workspace observed cost 259,000 events and 186MB to draw a
> screen whose visible tail is about ten items."

> "WHAT IT DOES NOT REPLACE. ResyncCmd is untouched … and from_seq=0 remains
> reachable as the compatibility full replay."

Paging was added ALONGSIDE the old door rather than replacing it. A door callers
are merely not supposed to use is a door that gets used: measured across all
webapp logs, `vendor_uuid_rotation` took the full-replay branch 12 times.

### 2. The store→shim path cannot express a bounded recent read AT ALL

```proto
message Subscribe {
  string session_id = 1;
  uint64 from_seq = 2;
}
```

No upper bound. No cap. From a point, everything onward, then live.

```proto
message ReplayRequest {
  string request_id = 1;
  uint64 from_seq = 2;    // EXCLUSIVE lower bound
  uint64 to_seq = 3;      // 0 = NO UPPER BOUND, streams until the replay drains
  uint32 max_events = 4;  // 0 = the serving side's own default
}
```

Bounded reads are expressible, but unboundedness is legal and reachable on both
fields.

Critically, **both read FORWARD FROM A LOWER BOUND**. Neither can express "the
newest N". A reader wanting recent history must GUESS a `from_seq` low enough to
cover it — and since one message can own hundreds of records, that guess cannot
be computed. This is where the unbounded scan actually comes from, and it is why
the nuke-and-reload path is *also* slow even though it paginates correctly: the
store still replays the full volume into the daemon, and only the daemon's
forwarding is gated.

### 3. A `repeated` field cannot enforce a page size

`ConversationPageTail.limit` claims:

> "a clamp makes that unrepresentable instead of merely discouraged"

That is false. A clamp rejects a number at runtime; the client can still send
5,000. And the response carries `repeated ConversationItem items`, which is
unbounded by construction regardless of what the request asked for.

---

## The contract

### Principles

1. **The client cannot name a position.** Not a seq, not an offset, not a cursor
   it authored or holds. "Give me everything" is not a request this contract can
   express — not because no caller sends it, but because there is no value that
   means it.
2. **The daemon owns the position**, persisted in the SSM, per reader per
   workspace. `NextPage` carries no position at all. Even an opaque cursor is a
   position the client holds.
3. **The page size is a property of the TYPE.** Ten discrete slots and no
   eleventh field. An over-large reply is unencodable, not merely non-compliant.
4. **The unit is the MESSAGE, never the record.** Ten records is not a page; it
   is a fragment, and a reader forced to keep asking until it holds ten messages
   is running the unbounded scan under a new name.
5. **No fence.** A fence was daemon state the client echoed back so the daemon
   could check the client against itself — the same category error as `from_seq`.
   The daemon owns the reader's position, so it already knows when a generation
   change invalidates it: it DROPS the position, the next `NextPageCmd` arrives
   with none and is REFUSED, and a client answers a refusal with `FirstPageCmd`.
   A page in flight across the transition is handled by `request_id`, which a
   client already uses to apply only the pages it awaits.

### On the webapp bounce, explicitly accepted

A frontend that bounces calls `FirstPageCmd` and starts from the bottom. Earlier
pages are re-reached by paging back. Scroll depth is NOT preserved across a
bounce, and no client-side persistence is added to preserve it. This is a
deliberate simplification, not an oversight.

---

## Frontend contract: `conversation-history.proto`

```proto
// conversation-history.proto — THE ONLY WAY a frontend obtains conversation it
// does not already hold, and deliberately a TWO-VERB one.
//
// Live delivery of NEW messages (ConversationDelta) is not pagination and is
// not governed here.

syntax = "proto3";

package agentshim.frontend.v1;

import "agentshim/frontend/v1/feed.proto";

option go_package = "agentrepl/proto/agentshim/frontend/v1;frontendv1";

// Ask for the MOST RECENT page, and reset this reader's position to it.
//
// The cold open and the whole recovery story: a client that bounced, rotated
// its seq space, or lost its place calls this and starts from the bottom.
message FirstPageCmd {
  // Which conversation. The daemon's position is per reader PER WORKSPACE, so
  // this is what selects the position being reset.
  string workspace = 1;
}

// Ask for the page IMMEDIATELY OLDER than the last one served to this reader.
//
// IT CARRIES NO POSITION, and that absence is the design. From a reader with no
// established position it is REFUSED rather than answered with the tail:
// defaulting would turn a client bug into a silent tail read, and "I have no
// position" already has its own verb.
message NextPageCmd {
  string workspace = 1;
}

// AT MOST TEN messages, oldest first.
//
// Ten discrete slots rather than a repeated field: a repeated field is
// unbounded by construction and the only enforcement available is a runtime
// check the producer must remember to apply. Here a producer holding an
// eleventh message has nowhere to put it.
//
// DENSITY IS THE PRODUCER'S INVARIANT. Slots fill from message_1 upward, and
// message_k set while message_(k-1) is empty is a bug. Making THAT structural
// needs a oneof of ten page-shaped messages and fifty-five fields; the ceiling
// is what matters and this achieves it, so the gap is documented rather than
// paid for.
//
// NESTED MESSAGES DO NOT COST A SLOT. A message contained by another travels
// inside its parent exactly as ConversationDelta carries it, so a client asking
// for a page cannot be surprised by its width.
message ConversationHistoryPage {
  string workspace = 1;
  // The requesting command's request_id, echoed so a client correlates this
  // page with the request it made rather than with whichever was most recent,
  // and so it can DISCARD a page it is no longer awaiting.
  string request_id = 2;

  // Messages OLDEST FIRST, as COMPLETE feed envelopes identical in shape to
  // ConversationDelta's — so a frontend renders a paged message with the same
  // code that renders a pushed one. Absent slots mean a short page, which at
  // the top of a conversation is the normal case.
  Message message_1 = 3;
  Message message_2 = 4;
  Message message_3 = 5;
  Message message_4 = 6;
  Message message_5 = 7;
  Message message_6 = 8;
  Message message_7 = 9;
  Message message_8 = 10;
  Message message_9 = 11;
  Message message_10 = 12;

  // WHETHER the conversation continues above this page — never WHERE.
  //
  // A oneof of MESSAGES rather than a bool or an enum: "there is more" and "we
  // reached the beginning" are the only two answers, and a shape that could
  // carry both or neither invites a client to invent a third.
  oneof continuation {
    // Older history remains. EMPTY on purpose: under this contract "there is
    // more" is a FACT the client acts on by calling NextPageCmd, not a handle
    // it stores. The cursor that used to live here is precisely the position
    // the client no longer holds.
    HistoryHasMore more = 13;
    // This page reaches the conversation's beginning; the client retires its
    // load-more affordance.
    HistoryAtStart start = 14;
  }

  // FIRST PAGES ONLY: the seq this page is current THROUGH, so the client
  // splices onto the live push stream gap-free BY CONSTRUCTION rather than by
  // timing. An OUTPUT the client never echoes back — it rides no request.
  // Zero on next pages, which are history and carry no live edge.
  uint64 live_join_seq = 15;
}

// Older history remains; call NextPageCmd.
message HistoryHasMore {}

// There is nothing older. A FACT the daemon established by reading to the floor.
message HistoryAtStart {}
```

Next free tag: **16**.

---

## Storage contract: `message-page.proto`

Shared by the store, the shim and the daemon — all three speak records, so one
shape serves every hop.

```proto
// message-page.proto — BOUNDED, BACKWARD-ANCHORED reads of durable history.
//
// # The unit is the MESSAGE, and that is the whole point
//
// "The last ten messages" means ten things the feed renders as rows. One of
// them can own hundreds of durable records. So a page bounded by RECORD COUNT
// is not a page: it is a fragment, and the reader must keep asking until it
// happens to hold ten messages, which is the unbounded scan relocated.
//
// The store therefore resolves ownership itself, selecting the ten most recent
// distinct top_level_message_id values below the anchor and returning every
// record they own. This is a deliberate, NARROW coupling: the store learns
// which record belongs to which message, and nothing else about how messages
// render.

syntax = "proto3";

package agentshim.core.v1;

option go_package = "agentrepl/proto/agentshim/core/v1;corev1";

// Ask for ONE page of messages, running BACKWARD from the anchor. Never a
// stream, never open-ended.
message MessagePageRequest {
  string request_id = 1;
  oneof anchor {
    // Anchor at the newest message held. The verb a cold open uses, and the
    // one the old vocabulary could not express: a reader that does not know
    // the head seq could not name it.
    MessagePageHead head = 2;
    // Continue below a page already received, using its last_page_seq
    // VERBATIM. This names a place the caller has DEMONSTRABLY BEEN, and walks
    // away from the history rather than into it.
    uint64 before_seq = 3;
  }
}

// Anchor at the newest message held. Empty: the head is a fact the serving side
// resolves, never a value the caller supplies.
message MessagePageHead {}

// AT MOST TEN messages and everything composing them, NEWEST FIRST.
message MessagePage {
  string request_id = 1;

  StoredMessage message_1 = 2;
  StoredMessage message_2 = 3;
  StoredMessage message_3 = 4;
  StoredMessage message_4 = 5;
  StoredMessage message_5 = 6;
  StoredMessage message_6 = 7;
  StoredMessage message_7 = 8;
  StoredMessage message_8 = 9;
  StoredMessage message_9 = 10;
  StoredMessage message_10 = 11;

  // The seq to anchor the NEXT request at: the oldest seq this page covers.
  // Returned so a caller never computes a position of its own — it copies back
  // a value the serving side minted.
  uint64 last_page_seq = 12;

  // WHETHER older history remains — never how much, never where.
  oneof boundary {
    // Older messages remain below this page.
    HistoryRemainsBelow more = 13;
    // This page reached the oldest RETAINED message. Distinct from the
    // conversation's beginning: retention is the serving side's own fact and it
    // says so, rather than letting a caller infer a beginning from a short page.
    HistoryAtRetainedFloor floor = 14;
  }
}

// One message and every durable record composing it, so a consumer receives a
// renderable unit WHOLE or not at all. A message split across pages would force
// the consumer to rejoin it, which is the correlation this shape removes.
message StoredMessage {
  // The message's own id — the value the store selects DISTINCT on.
  string message_id = 1;
  // Every durable record belonging to it, OLDEST FIRST.
  //
  // Deliberately `repeated` and deliberately UNBOUNDED. The ten-slot cap bounds
  // how many MESSAGES a page carries, which is the cost that matters. A message
  // legitimately owns hundreds of records, and truncating them would deliver a
  // lie about one message rather than fewer messages honestly.
  //
  // Records for messages NESTED inside this one are included here and carry
  // this message's id as their top_level_message_id — nesting is reconstructed
  // by the consumer, and never costs a page slot.
  repeated Record records = 2;
}

message HistoryRemainsBelow {}
message HistoryAtRetainedFloor {}
```

Next free tag: **15**.

---

## What the storage layer must provide

The page query the store runs, and the reason `top_level_message_id` is
denormalized onto every record rather than derived by walking parents:

```sql
SELECT DISTINCT top_level_message_id FROM records
  WHERE seq < :anchor
  ORDER BY seq DESC
  LIMIT 10
```

then fetch every record owned by those ten. One indexed pass, bounded read,
bounded reply.

### The three record categories, and the trap

**A — composes a message.** Owner knowable at write time from fields the
producer already carries (`ContentDelta.uuid` is the owning message; vendor tool
payloads name their issuing message).

**B — IS a message.** Task lifecycle, failure cards, clear/compact markers,
permission items. Owner is itself.

**C — NOT a message at all.** `session_started`/`session_ended`,
`turn_started`/`turn_ended`, `heartbeat_progress`, `message_latency`,
`turn_claim_bridge`, `query_lifecycle`, `account_usage_observation`,
`session_rewound`, `file_plane_diagnostic`. These render as nothing.

**Category C must carry NO ownership, and "unowned" must be structural rather
than an empty string.** If any of them acquires a `top_level_message_id` it
becomes a phantom page slot: `SELECT DISTINCT ... LIMIT 10` returns ten
"messages", several of which are turn boundaries, and the user sees a short page.
This is the highest-risk detail in the design and it fails silently.

---

## Migration: remove the old doors

`ResyncCmd.from_seq`, the unbounded `Subscribe`, `ReplayRequest`'s
zero-means-unbounded, and the old `ConversationPageCmd`/`ConversationPage` are
DELETED, not deprecated. Every remaining exploiter of the full-replay loophole
then stops compiling, which is a complete audit no manual search can match.

**Ordering constraint that is not negotiable:** the tail page must work before
`from_seq = 0` is removed. Removing it first trades a slow feed for an empty one.

---

## Open items

1. **Bubble rendering resolves to a value, not a shape.** Whether a message that
   represents detached work is a feed row or renders attached to its originating
   card changes only what the daemon writes into `top_level_message_id`, not the
   shape of any message. AGREED: such a message is a feed row — its
   `top_level_message_id` is its own id, and messages inside it name it as
   theirs. The values are persisted at write time, so changing this later means
   rewriting stored ownership.

2. **Page size fixed at ten**, replacing a runtime default of ~10 and a ceiling
   of ~50. If any surface needs more than ten, it surfaces as a compile error
   rather than a silent clamp.
