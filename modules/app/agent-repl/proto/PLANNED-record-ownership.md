# PLANNED: one vendor-agnostic vocabulary, and the vendor boundary at the edge

**Status: designed in conversation, NOT frozen, NOT implemented.** This file
records where the design stands, what was verified rather than assumed, and what
is still open. It exists so the reasoning is not re-derived.

It began as "why does pagination return nothing" and became an architecture
change, because the answer to the first question is a symptom of the second.

---

## Part 0 — The live defect this started from

Pagination is deployed and mechanically correct. Every page returns nothing.

```
session-controller: history page SERVED from the STORE'S BOUNDED PAGE
  ws="/Users/dodgecoates/.config/doom" anchor=first messages=0 records=0
  continuation=start
```

Measured against the live store:

| fact | value |
|---|---|
| rows in `event` | 2,890,952 |
| rows with `top_level_message_id` | **0** |
| newest rows, written after the deploy | also 0 |

Not a missing backfill. **Nothing populates ownership at all.**

### It fails silently, which is a second defect

```go
floor := m.replayFloorAt(workspace, sessionID, lastSeen, 0)
reachedStart := page.GetLastPageSeq() <= startSeq(floor)
```

An empty result trips that test, so "the store found no owned rows" becomes
`HistoryAtStart`. The webapp then correctly retires its load-more affordance,
because it was told the conversation begins here. **Any fix must make "no owned
rows below the anchor, in a store that has rows below the anchor" LOUD.**

---

## Part 1 — Verified, not assumed

1. **Two unrelated things are called "classifier".**
   - `sessioncontroller/classify.go` — shells out to `claude -p` to judge whether
     a queued prompt should interject. An LLM call. Irrelevant here.
   - `frontend/recordcategory.go` — a pure table mapping each `Event.payload` arm
     to a category. This is the relevant one.

2. **Ownership was never missing information.** It is already on the record:
   `ContentDelta.uuid` IS the owning message; a category-B record owns itself.
   Only the per-arm rule — which field to read, or none — is schema knowledge,
   and it lives in a hand-maintained Go map because that is where someone wrote
   it down.

3. **This repo already solved the same class of problem.** `slash-menu.proto:57`
   carries `extend google.protobuf.EnumValueOptions`, so each command declares
   its own literal and both languages read it off the descriptor.
   `sessioncommand.go` records why: three copies existed, nothing compared them,
   and a corrected literal left the frontend rendering the old spelling with
   every side's tests passing against its own copy.

4. **Tool cards are not records.** No tool arm exists on `Event.payload`. A tool
   call is a `ContentBlock` (`tool_use`, `tool_result`, `mcp_tool_use`, …) inside
   a message; its streaming arguments arrive as `ContentDelta.input_json`. So a
   tool card can never cost a page slot.

5. **`TranscriptLine` is fully supported today, on every end.**
   - PRODUCED: `handler/transcript.go:72` packs it into the vendor `Any`.
   - STORED: 663,351 of 2,890,952 rows are `TranscriptLine` vendor events.
   - CONSUMED: `translate.go:625` switches on it, `translate.go:704`
     `transcriptLineItems` turns it into `[]*frontendv1.Message`,
     `detachedsplit.go:220` type-asserts it.

6. **`vendor` means vendor-SPECIFIC, not unknown**, per its own comment:
   > Vendor extension payload (e.g. agentshim.data.v1.* messages). The daemon
   > treats it opaquely except for frontend translation; the store treats it
   > fully opaquely apart from dedup-key extraction.

7. **That escape clause is doing all the work.** The daemon is the LARGEST
   consumer of the vendor layer: **17 non-test files import `agentshim/data/v1`,
   with 168 `datav1.` references.** The store has 6, and they are dedup-key
   extraction exactly as documented. So the `Any` isolates exactly one component
   — the store — which is the one component that needs the ids.

8. **`LineEnvelope` is a census artifact, not a designed type.**
   `convert.go:493` `routeEnvelope` "assigns one top-level key to the shared
   LineEnvelope" by reflective canonical-name lookup, spilling unmatched keys
   into `extras`. It is a MIRROR of the on-disk key space: 38 fields because the
   JSONL has 38 top-level keys, `snake_session_id` commented "vestigial
   duplicate", and three fields RETYPED IN PLACE on reused numbers 30/32/34,
   each justified as "provably NEVER populated".

9. **The envelope reaches the daemon in full and dies there.** The daemon
   unpacks the whole `TranscriptLine` and reads `GetEnvelope()` at four sites.
   `recordEnvelopes` lifts SEVEN of thirty-eight fields into `RecordEnvelope` —
   a plain Go struct in no proto — and the rest end there. The code says it:
   "by the time the message is in the delta the envelope is gone."

10. **Curation is essentially PURE per record.** `CurateEvent(workspace, fence
    string, ev *corev1.Event)` and `conversationItemsFromVendor(a *anypb.Any, ev
    *corev1.Event)` are free functions with no receiver and no SSM access. So
    the conversion can run anywhere; moving it is a relocation, not a redesign.

11. **Fragments already bypass the store, by design.** `core.proto:13` —
    "EPHEMERAL class (ContentDelta, HeartbeatProgress) BYPASSES the store";
    `core.proto:17` — "Ephemeral events are never persisted and never replayed";
    `core.proto:68` names it the "delta bypass". The durable record of the same
    content is the file plane's whole `TranscriptLine`.

12. **The two producers write different things.** The shim persists lifecycle
    and control — `turnStarted`, `turnEnded`, `turnClaimBridge`, `degradedState`,
    session identity. The sidecar persists conversation content. They are not
    two implementations of one conversion.

13. **`logical_parent_uuid = 37` has ZERO readers.** It appears in
    `transcript.proto` and nowhere in the Go, annotated only
    `// compact_boundary lines`.

14. **Sidechain ownership is the one cross-record dependency.**
    `detachedsplit.go:179`:
    ```go
    key := env.SourceToolUseID
    if key == "" { key = "agent:" + env.AgentID }
    ```
    Turning that key into an owning message needs the record that SPAWNED the
    agent — a different record, usually in a different file. It is held in
    `detachedWorkStore`, whose own header calls it "ONE SESSION'S open work" —
    so it is per-SESSION state, not privileged daemon knowledge, and the sidecar
    already tails per session.

15. **`logical_parent_uuid` is UNWIRED, not dead — do not delete it.** Read from
    a real `compact_boundary` line on disk:
    ```
    type              = system
    subtype           = compact_boundary
    uuid              = 5e68a55d-03ec-4999-9380-920c80b54d2c
    parentUuid        = None
    logicalParentUuid = 60d447f4-c78e-49d0-ab38-fbabac5bd386
    ```
    `parent_uuid` is EMPTY at a compaction — the physical chain is cut — and
    `logical_parent_uuid` is the only pointer back across the cut. Deleting it
    would make pre-compaction history unreachable by any parent walk.

16. **The frontend does not know bookkeeping, with one exception that is itself
    a defect.** `TurnStarted`, `HeartbeatProgress` and `QueryLifecycle` appear in
    the webapp only in COMMENTS or as a frontend-declared error shape
    (`TelemetryRecordMissingQueryLifecycleSchema`). The exception is
    `AccountUsageObservation`, genuinely imported and used at
    `frontend-proto.ts:1035` as `usageAtStart?: AccountUsageObservation` — a raw
    measurement crossing to the client rather than a figure the daemon resolved,
    which is the figma→idl violation the contract already forbids.

---

## Part 2 — The architectural direction

**The vendor boundary moves to the edge. Nothing past the producers speaks
vendor.**

Today the vendor format is the daemon's working vocabulary, and the `Any`
protects only the store. Inverted:

- **shim** → converts the SDK stream to the agnostic vocabulary, writes to store
- **sidecar** → converts JSONL to the agnostic vocabulary, writes to store
- **store** → persists only that vocabulary, and can therefore index and group
  by message identity natively
- **daemon** → reads and resolves views; stops knowing vendor shapes entirely
- **webapp** → unchanged in kind

The producers' job becomes: *know the underlying SDK and convert to the
vendor-agnostic API*. That is the only place vendor knowledge is honest, because
it is the only place that talks to the vendor.

**The "dumb" protos stop crossing wires.** `data.v1` — the reflective JSON
mirror — becomes the producers' private parsing model rather than a wire
contract, or goes away.

**Lineage is resolved BEFORE persisting, not after.** The sidecar has the
envelope open in its hands, with `uuid`, `parent_uuid`, `is_sidechain` and
`agent_id` visible, before it seals anything. That is the last point where the
ids are visible and the record is not yet written. Everything downstream is
either blind by design or too late to help.

---

## Part 3 — The contract

```proto
// The durable record, whose FIRST question is no longer "what kind of thing is
// this" but "can we render it at all".
//
// The split is binary at the top for a reason: the page query selects the
// `message` arm and nothing else, so an entry we cannot render has no path into
// a page. That is not a rule the query applies — it is the only arm it can name.
message Event {
  // The transport and durability envelope, and all core needs to be once the
  // vendor tier stops crossing wires: a seq to order by, a plane to attribute,
  // a class to retain by, and a key to dedup on.
  string session_id = 1;
  uint64 seq = 2;
  Plane plane = 3;
  EventClass class = 4;
  int64 produced_at_ms = 6;
  string dedup_key = 7;

  // THE TEST THAT DECIDES THESE ARMS, in one question: if you PRINTED the
  // conversation to read it, would this appear?
  //
  //   yes                      -> message
  //   no, but we understand it -> bookkeeping
  //   we could not tell        -> unsupported
  //
  // Put another way: a MESSAGE is something the conversation CONSISTS OF, and
  // BOOKKEEPING is a fact ABOUT the conversation. Content versus evidence.
  //
  // The reverse readings are the useful ones. Something is NOT a message when
  // nothing in the feed would be missing had it never arrived. Something is NOT
  // bookkeeping when a reader scrolling back would notice a hole where it
  // should have been.
  oneof entry {
    // A record BELONGING to a conversation message — the thing the feed draws
    // and a page counts.
    //
    // It has an identity the user could point at, it accumulates over several
    // records that share that identity, and it is what "ten messages" counts.
    // `context_cleared` and `degraded_state` are messages by this test: a
    // reader must see WHERE the conversation was cut, and a failure card is a
    // thing the user reads and acts on.
    MessageEntry message = 40;

    // A fact ABOUT the session rather than a part of the conversation. We
    // understand it completely and nothing in the feed corresponds to it.
    //
    // It is not uncounted because it is unimportant — it drives the footer, the
    // SSM, accounting and the turn ledger. It is uncounted because it is not IN
    // the transcript: you cannot point at a turn boundary. `session_rewound`
    // and `turn_claim_bridge` are bookkeeping by this test — they explain an id
    // change and correlate a ledger, and neither leaves a hole when absent.
    //
    // Separated from `message` rather than filtered out of it, so a turn
    // boundary has no field capable of naming a message and therefore cannot
    // become a phantom page slot. Misfiling one is not a wrong value — it is an
    // unbuildable record.
    //
    // THE FRONTEND NEVER SEES THESE. Bookkeeping reaches the client only after
    // the daemon has resolved it into a view, which is what makes it evidence
    // rather than content.
    BookkeepingEntry bookkeeping = 41;

    // Something we could not place, preserved verbatim so the decision is
    // reversible. The printing test cannot be applied to it — that is what
    // makes it unsupported rather than either arm above.
    UnsupportedEntry unsupported = 42;
  }
}

// One record BELONGING to one message. Several of these share a message_id and
// fold, in seq order, into the single thing the feed draws.
//
// THE THREE IDS ARE THE SAME ON EVERY RECORD, whatever its payload does with
// the message. That is deliberate: "opens", "updates" and "composes" stop being
// three record SHAPES and become three payloads. An earlier draft split them
// into ComposingRecord and MessageRecord, which put the ids at two different
// levels and made `task_ended` — a record that closes a message rather than
// making one — sit in the arm named for identity.
message MessageEntry {
  // The message this record belongs to. Records sharing it fold together, so a
  // consumer needs no correlation pass to attach an update to what it updates.
  string message_id = 1;

  // The feed row that message belongs to, and the value a page groups by.
  //
  // EQUALS message_id when the message is itself a feed row; names the
  // containing message when nested, so a subagent and everything inside it
  // share one value and therefore one slot. Denormalized on purpose: it is
  // derivable by walking parents, and storing it is precisely why a page of ten
  // messages costs one indexed pass. It MUST equal the root of the parent
  // chain; a write that disagrees is corruption, not a variant.
  string top_level_message_id = 2;

  // The message immediately containing this one — ONE HOP, never the root.
  //
  // EMPTY means the message sits directly in the feed, in which case
  // top_level_message_id is its own id. Absence is the fact itself, not a
  // placeholder: a message whose parent could not be resolved is a producer
  // fault.
  //
  // AT A COMPACTION BOUNDARY the on-disk `parent_uuid` is empty and
  // `logical_parent_uuid` carries the only pointer back across the cut, so the
  // producer resolves this from the latter in that case. Reading only the
  // physical chain would make pre-compaction history unreachable.
  string parent_message_id = 3;

  oneof payload {
    // Records that OPEN the message they name.
    AgentEmission                agent                      = 10;
    ApiUserMessage               user_message               = 11;
    PermissionItem               permission                 = 12;
    FailureCardView              failure_card               = 13;
    ContextCleared               context_cleared            = 14;
    ContextCompacted             context_compacted          = 15;
    DaemonInterceptedCommandItem daemon_intercepted_command = 16;
    CompactionSummaryItem        compaction_summary         = 17;
    DetachedWork                 detached_work              = 18;

    // Records that UPDATE a message already opened. They carry the same
    // message_id, so attaching them needs no correlation and costs no slot.
    DetachedWorkProgress         detached_work_progress     = 19;
    DetachedWorkEnded            detached_work_ended        = 20;
  }
}

// An entry we do not render, held for posterity.
//
// SINGULAR because one Event carries one entry. The arms below are one concept
// — "unsupported" — split only because they imply different follow-ups.
//
// THIS ARM IS WHAT MAKES EAGER CONVERSION SAFE. Anything a producer could not
// map still lands durably and whole, so converting at the edge costs no
// fidelity: the decision stays reversible from stored data.
message UnsupportedEntry {
  oneof entry {
    // A fact specific to one vendor, understood but not carried into a
    // vendor-agnostic feed. The follow-up is a CONVERTER, if it turns out to be
    // portable after all.
    VendorSpecificEntry vendor_specific = 1;

    // A record we PARSED but do not MODEL, kept whole so a later schema can
    // reconstruct it with no loss. The follow-up is a MODEL.
    UnknownEntry unknown = 2;

    // A record that could not be PARSED at all — a failure rather than a gap.
    // It sits here because everything in this message shares one property: it
    // did not become a message, and it is preserved rather than dropped.
    UnparsedEntry unparsed = 3;
  }
}

// A fact about the session that renders as nothing.
//
// THERE IS NO MESSAGE ID ANYWHERE IN THIS MESSAGE. A page is ten messages, so a
// boundary that could name one would silently spend a slot and the user would
// see a short page with nothing logged. Here it has nowhere to put one.
//
// IT IS STILL RETRIEVED, just never COUNTED: the SSM, accounting and the turn
// ledger read these by SEQ RANGE, which is a different query with a different
// shape from a page.
message BookkeepingEntry {
  oneof kind {
    // Session boundaries. They configure the view and carry no conversation.
    SessionStarted session_started = 1;
    SessionEnded session_ended = 2;

    // Turn boundaries. Every turn produces both, so an owner here would make
    // every turn in the conversation a phantom slot.
    TurnStarted turn_started = 3;
    TurnEnded turn_ended = 4;

    // A liveness signal: it reports that something is alive, never what was said.
    HeartbeatProgress heartbeat_progress = 5;

    // A timing sample retained for analysis. It measures the conversation
    // rather than participating in it.
    MessageLatency message_latency = 6;

    // A runtime diagnostic about the READER, not the read. Consumers must never
    // render it as conversation material.
    FilePlaneDiagnostic file_plane_diagnostic = 7;

    // Correlation evidence for the durable turn ledger, deliberately NOT a
    // lifecycle boundary — it exists so a turn adopted from a previous daemon
    // can still be tied to its own history.
    TurnClaimBridge turn_claim_bridge = 8;

    // Lifecycle facts for one SDK query invocation: the machinery behind a
    // turn, not a participant in it.
    QueryLifecycle query_lifecycle = 9;

    // A subscription-usage measurement taken at a turn boundary.
    AccountUsageObservation account_usage_observation = 10;

    // Evidence explaining a vendor-session identity change, so a rotated id is
    // reconciled with the conversation it continues rather than appearing new.
    SessionRewound session_rewound = 11;
  }
}
```

### No fragment arm

Deltas are EPHEMERAL and bypass the store already (Part 1 finding 11). They
remain a live-transport concern between shim and daemon. The durable record of
the same content is the message the producer resolves from the completed line.

### The visible cuts stay messages

`context_cleared` and `context_compacted` are `Message` payload arms, not
bookkeeping: the feed must show WHERE a conversation was cut, rather than merely
behaving as though it were shorter.

---

## Part 4 — Implications per runtime

### shim (TypeScript)
- Gains: converting SDK stream events into the agnostic vocabulary.
- Keeps: the delta bypass for live typing, which never touches the store.
- Persists: lifecycle and control — `turnStarted`, `turnEnded`,
  `turnClaimBridge`, `degradedState`, session identity.
- Loses: nothing structurally; it already speaks the SDK.

### sidecar (Go)
- Gains: the largest new responsibility — converting JSONL lines into resolved
  `Message`s, including LINEAGE, while the envelope is still open.
- Gains: the per-session state needed for sidechain attribution (finding 14),
  which the daemon holds today.
- Keeps: `data.v1` as its private parsing model, plus `extras` for faithful
  capture.
- Emits: `UnsupportedEntry` whenever it cannot map something, so nothing is lost.

### store
- Gains: a vocabulary it can index. The page query becomes a real query over
  message identity rather than a scan of opaque blobs.
- Loses: nothing. It gets strictly more capable while its interface narrows.
- Note: it becomes SEMANTIC where it was a blob store. That is the coupling
  change this design accepts deliberately.

### daemon
- Loses: 17 files and 168 `datav1.` references. It stops parsing vendor shapes,
  stops owning `transcriptLineItems`, and stops being the place a JSONL line
  becomes a message.
- Keeps: view resolution — the figma→idl surface, the SSM, the turn ledger,
  progress, accounting.
- Net: it becomes what the architecture already claims it is.

### webapp
- Unchanged in kind. It already renders `Message`s and never saw `data.v1`.

---

## Part 5 — Settled

1. **No backfill.** The store is nuked instead, which is what makes a wire break
   affordable.
2. **The vendor boundary is the producers.** Nothing past them speaks vendor.
3. **One top-level cut**: `Message` | `BookkeepingEntry` | `UnsupportedEntry`.
4. **`vendor_specific`, `unknown` and `unparsed` share the unsupported arm**,
   because they mean one thing — did not become a message — and differ only in
   what the follow-up is.
5. **Bookkeeping carries no message id anywhere**, so a phantom page slot is
   unrepresentable rather than refused.
6. **No fragment arm.** Deltas already bypass the store.
7. **The shared vocabulary is not `frontend.v1`.** It is written by shim and
   sidecar, persisted by the store, and read by daemon and webapp — so it wants
   its own package (`agentshim.feed.v1` or similar), with `frontend.v1` keeping
   only view resolutions.
8. **Two converters is NOT a drift risk** (finding 12). Withdrawn as an
   objection.
9. **`ComposingRecord` and `MessageRecord` collapse into one `MessageEntry`.**
   Every record belonging to a message carries the same three ids; what it does
   with the message is a property of its payload.
10. **The package SPLITS rather than being renamed.**
    - `agentshim.conversation.v1` — the shared data model: `Message`, lineage,
      durability, the payload arms. Written by shim and sidecar, persisted by
      the store, read by daemon and webapp.
    - `agentshim.frontend.v1` KEEPS only genuine view resolutions — topbar,
      footer, sidebar, tokens-menu, gate-revival, merge.
11. **`logical_parent_uuid` STAYS**, and lineage resolution falls back to it
    when `parent_uuid` is empty at a compaction boundary (finding 15).
12. **`UnsupportedEntry`'s payloads are deliberately unspecified.** They exist
    for posterity and debugging; if one is ever needed, its shape is expanded
    then. Not a blocking question.

---

## Part 6 — Open

1. **Sidechain lineage at the producer.** The sidecar must resolve
   `SourceToolUseID` / `agent_id` to the owning message. It has the inputs — it
   tails both the main transcript and the subagent files — but does not
   correlate them today. Relocating `detachedWorkStore`'s per-session indexes is
   the work.
2. **Has any OTHER session-scoped correlation the same property?** The boundary
   is documented — construction lives in `internal/frontend` and is per-record,
   session routing lives in `sessioncontroller` — but `sessioncontroller` has
   not been swept exhaustively. Detached work may not be the only one.
3. **Does `data.v1` survive at all**, as the producers' private parsing model,
   or is it replaced by direct parsing?
4. **The `task_*` / `turn_*` naming hazard.** They differ by two letters, sit in
   adjacent arms, and mean OPPOSITE things for paging — a turn must never own a
   message, detached work always does. Proposed rename to `detached_work_*`,
   matching the `DetachedWork` payload arm that already exists. Cheapest now,
   while the wire is already breaking.
5. **`AccountUsageObservation` reaching the webapp** (finding 16). It should
   become a daemon-resolved figure, which is a small figma→idl fix that this
   wave makes natural.
6. **SEQUENCING.** How we get from here to there without a long window where
   nothing renders. Deferred by agreement until the requirements and
   architecture are settled.

---

## Part 7 — Out of scope, recorded so it is not lost

- **`session view identity rejected`** — 1253 occurrences for
  `explanation-engine` since the 09:05 restart, plus ~21 each for roughly a dozen
  dead session ids under `doom`. Something keeps publishing views for sessions
  that no longer exist. Undiagnosed.
- **`deploy-all.sh` fails a correct migration.** It waits 15s for `store.sock`;
  the schema 2→3 migration over a 4.3GB store with a 6.4GB WAL took nine minutes.
  It should wait on the migration, not a fixed timeout.
- **The codex backend** exists (`lisp/codex.el`) but is experimental and
  explicitly not a consideration.

---

## Design principles this is held to

- State enums are forbidden; any state is a `oneof` of dedicated messages.
- Every field and message carries a semantic comment explaining purpose,
  behaviour and motivation, never a restatement of its name.
- Absence renders absence: never a zero, a default, or a sentinel standing in
  for a value that did not arrive.
- A bound belongs in the shape of the type, never in a check applied to it.
- NEVER remove, weaken, or bypass error-handling coverage.
