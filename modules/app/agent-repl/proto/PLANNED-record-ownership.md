# PLANNED: record ownership, and the category as a structural fact

**Status: designed in conversation, NOT frozen, NOT implemented.** This file
records where the design stands, what was verified rather than assumed, and what
is still open. It exists so the reasoning is not re-derived.

It follows `PLANNED-message-pagination.md`, which is implemented and deployed,
and it fixes the reason that implementation returns nothing.

---

## Part 0 — The live defect this exists to fix

Pagination is deployed and mechanically correct. Every page returns nothing.

```
session-controller: history page SERVED from the STORE'S BOUNDED PAGE
  ws="/Users/dodgecoates/.config/doom" anchor=first messages=0 records=0
  continuation=start
```

Measured against the live store:

| fact | value |
|---|---|
| rows in `event` | 2,886,992 |
| rows with `top_level_message_id` | **0** |
| newest rows, written after the deploy | also 0 |

So this is not a missing backfill. **Nothing populates ownership at all**,
including records written right now.

### Why it fails silently, which is its own defect

`storepage.go` decides the continuation like this:

```go
floor := m.replayFloorAt(workspace, sessionID, lastSeen, 0)
reachedStart := page.GetLastPageSeq() <= startSeq(floor)
```

An empty result trips that test, so **"the store found no owned rows" becomes
`HistoryAtStart`**. The webapp then correctly retires its load-more affordance,
because it was told the conversation begins here. The user sees no history and
no way to ask for any, and nothing anywhere is logged as wrong.

The neighbouring conflation is carefully avoided — there is a whole comment on
why `HistoryAtRetainedFloor` must not become `HistoryAtStart` — and this one
was walked into anyway. **Any fix must make "no owned rows below the anchor,
in a store that has rows below the anchor" a LOUD failure.**

---

## Part 1 — What was verified, not assumed

Each of these was checked against the tree or the running system.

1. **There are two things called "classifier". Only one is relevant.**
   - `sessioncontroller/classify.go` — `CLIClassifier`, `spawnClassifier`. Shells
     out to `claude -p` to judge whether a prompt queued mid-turn should
     interject or hold. An LLM call. **Irrelevant here.**
   - `frontend/recordcategory.go` — a pure table mapping every arm of
     `Event.payload` to a category. **This is the one.**

2. **Ownership is not knowledge any process holds. It is already on the record.**
   - Category A: `ContentDelta.uuid` IS the owning message id.
   - Category B: the owner is the record's own id.
   - Category C: there is no owner.
   - What the table adds is only *which field to read, per arm* — a property of
     the SCHEMA, not of any process. It lives in the daemon because that is
     where someone wrote it down.

3. **This repo already solved this exact problem once, and recorded why.**
   `slash-menu.proto:57` carries `extend google.protobuf.EnumValueOptions`, so
   each `SessionCommand` value declares its own literal and both sides read it
   back off the descriptor. `sessioncommand.go` states the motivation: there
   used to be three copies — the daemon's table, the webapp's command list, the
   webapp's label table — and nothing compared them, so a corrected literal left
   the frontend rendering the old spelling with every side's tests passing.

4. **Tool cards are not records.** There is no tool arm on `Event.payload`.
   A tool call is a `ContentBlock` (`tool_use`, `tool_result`, `mcp_tool_use`,
   `server_tool_use`, and the result shapes) inside a message, and its streaming
   arguments arrive as `ContentDelta.input_json` with `tool_use_id` and
   `block_index`. So a tool card can never cost a page slot — which is exactly
   the contract's own arithmetic that one message "can own hundreds of records".

5. **The two planes emit different things, and only one is typed.**
   - STREAM plane (shim) → `ContentDelta`, typed.
   - FILE plane (sidecar) → `Event_Vendor`, an `Any`. The sidecar emits **no**
     `ContentDelta` at all.
   - `vendorEvent` (`handler/handler.go:34`) is the single packing site, called
     from `transcript.go:72` with a `datav1.TranscriptLine` and from
     `journal.go:43` with a `datav1.JournalRecord`.

6. **`vendor` means vendor-SPECIFIC, not unknown.** Its own comment:
   > Vendor extension payload (e.g. agentshim.data.v1.* messages). The daemon
   > treats it opaquely except for frontend translation; the store treats it
   > fully opaquely apart from dedup-key extraction.

7. **Unparsable content is ALREADY separated.** Parse and conversion failures
   become `Event_Unparsed` via `unparsedEvent`, at four call sites in `agent.go`
   and `journal.go`. Nothing unknown rides `vendor`; only content that converted
   successfully does.

8. **`UnknownRecord` is a modelling gap, not a vendor fact.** It carries the
   `discriminator`, which field it came from, and the raw `Struct`, with the
   stated intent: "A consumer that later learns the shape can reconstruct it
   from here with no loss." That is a record we parsed but have not modelled.

9. **`data.v1` is substantively neutral.** `LineEnvelope` is `uuid`,
   `parent_uuid`, `timestamp`, `is_sidechain`. `ContentBlock` is `text`,
   `thinking`, `tool_use`, `tool_result`, `image`. None of that is Claude's —
   it is the shape of an agent conversation. The genuinely CC-shaped residue is
   thin and sits in leaf fields: `version`, `entrypoint`, and the `Struct`
   escape hatches on `ApiAssistantMessage`.

10. **Nesting is real, so `message_id` and `top_level_message_id` are different
    facts.** `NewDurableChild` "INHERITED the parent's root", and the pagination
    contract states "NESTED MESSAGES DO NOT COST A SLOT". A subagent's own
    emissions carry their own `message_id` and the detached work's
    `top_level_message_id`.

11. **`TranscriptLine` carries no envelope of its own.** It is a pure oneof of
    15 line kinds; the `LineEnvelope` sits one level down, on each kind. So a
    producer reads `line.assistant.envelope.uuid`, never `line.uuid`.

---

## Part 2 — The settled shape

The category becomes the arm that is set, rather than a fact a table remembers.

```proto
// Event's payload, restructured so a record's CATEGORY is the arm that is set
// rather than a fact a separate table has to remember about it.
//
// The category was previously a hand-maintained Go map keyed by field number,
// with an init() descriptor walk to panic on an unclassified arm. That check
// existed because the mapping could drift. Under this shape it cannot: adding a
// kind means choosing which arm to add it under, and the compiler asks.
message Event {
  string session_id = 1;
  uint64 seq = 2;
  // ... the existing envelope fields are unchanged ...

  oneof record {
    // The record COMPOSES a message that exists independently of it, and names
    // both the message it composes and the feed row it pages under.
    ComposingRecord composing = 40;

    // The record IS a message, and therefore occupies exactly one page slot
    // unless it is nested inside another.
    MessageRecord message = 41;

    // The record renders as nothing. Separated so that it has no field capable
    // of naming a message, which is what keeps it out of a page.
    BookkeepingRecord bookkeeping = 42;

    // A vendor-specific fact core deliberately does not model.
    VendorRecord vendor = 43;
  }
}

// A record that composes a message: content that accumulates into something the
// feed draws, rather than something the feed draws on its own.
message ComposingRecord {
  // The FEED ROW this record pages under — the outermost ancestor, which for a
  // record composing a NESTED message is not the message it composes.
  //
  // This is the value `SELECT DISTINCT ... LIMIT 10` groups by, and it is what
  // makes nesting free: a subagent and everything inside it share one value and
  // therefore one slot. Never empty — a producer that cannot name the feed row
  // has not resolved its own record, and that is a fault, not a blank.
  string top_level_message_id = 1;

  oneof kind {
    // Streamed assistant content, composing the message named by its OWN `uuid`.
    //
    // `uuid` is retained inside ContentDelta rather than hoisted up here because
    // the two answer different questions: `uuid` says which message this delta
    // composes and may name a nested one, while top_level_message_id says which
    // feed row it pages under. Collapsing them would leave a subagent's deltas
    // unattachable to the message they actually belong to.
    ContentDelta content_delta = 2;

    // An update to detached work already open, accumulating into the message its
    // task id names. An update to a message is not itself a message, so it must
    // never mint a second one or spend a second slot.
    TaskProgress task_progress = 3;
  }
}

// A record that IS a message: the feed draws it, and it costs a page slot unless
// another message contains it.
message MessageRecord {
  // This record's own message id — the identity a consumer routes updates by.
  string message_id = 1;

  // The feed row this message belongs to. EQUALS message_id when the message is
  // itself a feed row, and names the containing message when it is nested — a
  // subagent's own emissions name the detached work that produced them.
  //
  // Denormalized on purpose: it is derivable by walking parents, and storing it
  // anyway is precisely why a page of ten messages costs one indexed pass. It
  // MUST equal the root of the parent chain; a write that disagrees is
  // corruption rather than a variant.
  string top_level_message_id = 2;

  // The message immediately containing this one — ONE HOP, never the root.
  //
  // EMPTY means this message sits directly in the feed, in which case
  // top_level_message_id is this message's own id. Absence is the fact itself,
  // not a placeholder: a message whose parent could not be resolved is a
  // producer fault.
  string parent_message_id = 3;

  oneof kind {
    // Opens detached work. Detached work is a FEED ROW naming itself, so that a
    // page of ten rows is ten bounded things rather than ten trees.
    TaskStarted task_started = 4;

    // Closes that same detached work, folded into the message task_started
    // opened rather than minting a second one beside it.
    TaskEnded task_ended = 5;

    // Becomes a failure card, which is a message whose owner is itself: the card
    // is the thing the user sees and acts on.
    DegradedState degraded_state = 6;

    // A clear marker. It is a message because the feed must show WHERE the
    // conversation was cut, not merely behave as though it were shorter.
    ContextCleared context_cleared = 7;

    // A compact marker, for the same reason: a compaction the user cannot see
    // is a conversation that appears to have lost content for no stated cause.
    ContextCompacted context_compacted = 8;

    // One durable conversation line, read from the file plane.
    //
    // TYPED rather than Any-wrapped, because its LineEnvelope already carries
    // uuid, parent_uuid and is_sidechain — precisely the three facts the page
    // query groups by. Leaving it opaque is why every page currently returns
    // zero messages. SEE PART 4: whether this arm lives here is OPEN.
    agentshim.data.v1.TranscriptLine transcript_line = 9;
  }
}

// A record that renders as nothing: a boundary, a measurement, or a diagnostic.
//
// THERE IS NO ID FIELD ANYWHERE IN THIS MESSAGE, and that absence is the entire
// point. A page is `SELECT DISTINCT top_level_message_id ... LIMIT 10`, so a
// boundary that acquired an owner would silently spend one of ten slots and the
// user would see a short page with nothing raised anywhere. Here a producer
// holding a turn boundary has nowhere to put a message id, so the phantom slot
// is unrepresentable rather than merely refused.
message BookkeepingRecord {
  oneof kind {
    // A session boundary. It configures the view and carries no conversation
    // content of its own.
    SessionStarted session_started = 1;

    // The closing session boundary, likewise carrying no conversation content.
    SessionEnded session_ended = 2;

    // A turn boundary. Every turn produces one, so an owner here would make
    // every turn in the conversation a phantom page slot.
    TurnStarted turn_started = 3;

    // The closing turn boundary, with the same slot hazard.
    TurnEnded turn_ended = 4;

    // A liveness signal, relayed as HeartbeatView so the footer can show work is
    // progressing. It reports that something is alive, never what was said.
    HeartbeatProgress heartbeat_progress = 5;

    // A conversion-failure diagnostic: a line that could not be parsed. The
    // daemon logs it and counts it toward backfill accounting. Giving it an
    // owner would spend a page slot on a parse error.
    UnparsedEvent unparsed = 6;

    // A timing sample retained for analysis on replay. It measures the
    // conversation rather than participating in it.
    MessageLatency message_latency = 7;

    // A sidecar runtime diagnostic written to sidecar.log. Consumers must never
    // render it as conversation material — it is about the reader, not the read.
    FilePlaneDiagnostic file_plane_diagnostic = 8;

    // Correlation evidence for the durable turn ledger, and deliberately NOT a
    // lifecycle boundary — let alone a message. It exists so a turn adopted from
    // a previous daemon can still be tied to its own history.
    TurnClaimBridge turn_claim_bridge = 9;

    // Facts about one SDK query() invocation: how it started, how it ended. The
    // query is the machinery behind a turn, not a participant in it.
    QueryLifecycle query_lifecycle = 10;

    // A subscription-usage measurement taken at a turn boundary. It is accounting
    // evidence, and the footer resolves it into a figure the user reads.
    AccountUsageObservation account_usage_observation = 11;

    // Durable correlation evidence explaining a vendor-session identity change,
    // so a rotated session id can be reconciled with the conversation it
    // continues rather than appearing as a new one.
    SessionRewound session_rewound = 12;

    // A record we PARSED but have not MODELLED, held verbatim so a later schema
    // can reconstruct it losslessly. It renders as nothing today, so it names no
    // message and cannot spend a page slot.
    //
    // It sits HERE rather than under vendor because it is not a vendor fact at
    // all — it is our own modelling gap, and filing it as vendor-specific would
    // hide that.
    UnknownRecord unknown = 13;
  }
}

// A vendor-specific fact core deliberately does not model.
//
// It carries NO lineage of any kind, and that absence is the decision: a fact
// core cannot model is a fact the webapp cannot render, so it must never occupy
// a page slot. Adding a vendor means writing its curator to emit typed records —
// not teaching this arm to page.
message VendorRecord {
  google.protobuf.Any payload = 1;
}
```

### What this shape buys

- The category is not read from anywhere. It IS the arm that is set.
- A bookkeeping record has no field for an id, so a phantom page slot is
  unrepresentable rather than refused.
- A composing record must name its feed row, so an unowned one is
  unrepresentable.
- `recordcategory.go` deletes entirely — the table, the descriptor walk, and its
  init() panic all become unnecessary, because the compiler asks the question.
- Nothing reads descriptors at runtime, in any language.

### What it costs

- It is a **wire break**: every payload tag moves, and every producer and
  consumer changes with it.
- The data cost is zero, because the store is being nuked (Part 3).

---

## Part 3 — Settled decisions

1. **No backfill.** Explicitly not wanted. The store is nuked instead, which is
   what makes a wire break affordable.
2. **Vendor-specific content never reaches the webapp, and therefore never
   pages.** `VendorRecord` carries no lineage at all.
3. **`UnknownRecord` moves to bookkeeping.** It is a modelling gap, not a vendor
   fact.
4. **`Event_Unparsed` stays exactly as it is.** The separation already exists
   and is already correct.
5. **The category is a oneof, not an enum option.** The option form (the
   `session_command_spec` pattern) would work and would be read off the
   descriptor by both languages, but the oneof makes the illegal state
   unrepresentable rather than merely detectable.

---

## Part 4 — OPEN, and blocking a freeze

1. **Does `core.v1` may depend on `data.v1`?**
   `core.proto` imports only `any`, `descriptor` and `struct` today. The
   `transcript_line` arm creates a new `core → data` edge. Gate I6
   (`check-durable-isolation.sh`) polices `durable.proto` on the FRONTEND push
   surface, not this edge — its stated harm is a renderer freezing durable
   shapes — so it does not obviously forbid this. **Unresolved: is the edge
   acceptable, or should the category envelope live in core with the transcript
   arms elsewhere?**

2. **Reuse `MessageLineage`, or restate its three fields?**
   `MessageRecord` now carries exactly the triple that `frontend/v1`'s
   `MessageLineage` defines. Reusing it keeps one definition of lineage;
   importing a frontend type into the durable layer inverts the intended
   layering.

3. **`parent_uuid` versus `logical_parent_uuid`.**
   `LineEnvelope` carries TWO parent pointers: `parent_uuid = 1` and
   `logical_parent_uuid = 37`, the latter on compact-boundary lines. Lineage
   resolution must state which wins, and when. **Not yet examined.**

4. **The `task_*` / `turn_*` naming hazard.**
   `task_started` and `turn_started` differ by two letters, sit in adjacent arms
   of the same restructured oneof, and mean opposite things for paging — one
   must never own a message, the other always does. Proposed rename to
   `detached_work_started` / `detached_work_ended`, matching what the frontend
   already calls the concept. Cheapest now, while the wire is already breaking.
   **Not yet accepted.**

5. **Where the producer resolves `top_level_message_id` for a nested line.**
   `is_sidechain` and `agent_id` are the signal, but the walk from a sidechain
   line to the detached-work message that owns it has not been specified.

---

## Part 5 — Out of scope, recorded so it is not lost

- **`session view identity rejected`** — 1253 occurrences for
  `explanation-engine` since the 09:05 restart, plus ~21 each for roughly a
  dozen dead session ids under `doom`. Something keeps publishing views for
  sessions that no longer exist; the webapp is right to drop them. Undiagnosed.
- **`deploy-all.sh` fails a correct migration.** It waits 15s for `store.sock`
  and gives up; the schema 2→3 migration over a 4.3GB store with a 6.4GB WAL
  took about nine minutes. A correct, necessary migration is reported as a
  deploy failure. It should wait on the migration, not on a fixed timeout.
- **The codex backend** exists (`lisp/codex.el`, verified against codex-cli
  0.144.1) but is experimental and explicitly not a consideration here.

---

## Design principles this is held to

- State enums are forbidden; any state is a `oneof` of dedicated messages.
- Every field and message carries a semantic comment explaining purpose,
  behaviour and motivation, never a restatement of its name.
- Absence renders absence: never a zero, a default, or a sentinel standing in
  for a value that did not arrive.
- A bound belongs in the shape of the type, never in a check applied to it.
- NEVER remove, weaken, or bypass error-handling coverage.
