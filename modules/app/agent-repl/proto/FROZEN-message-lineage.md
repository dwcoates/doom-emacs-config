# FROZEN CONTRACT: message lineage, and the retirement of "bubble"

**User pre-approved. This is the frozen contract. Every implementation agent
receives it byte-for-byte and may not deviate. A deviation discovered
mid-implementation is surfaced, never silently absorbed.**

---

## Part 1 — There is only Message

"Bubble" is a rendering word: it describes a rounded container in the webapp. It
must not exist in the daemon, the shim, the store, or on any wire. The webapp may
keep the word for its own renderer internals; nothing else may.

A bubble has no property that makes it not-a-message. It has identity,
provenance, containment, a timestamp, and content that accumulates — that is a
message. Its label, liveness and kind are PAYLOAD, exactly as a permission
card's or a failure card's payload is.

### What the separation was costing

1. **Two id spaces.** `AgentToolCall.spawned_bubble_id` exists solely to bridge
   message ids to bubble ids. One id space and the bridge disappears.
2. **Two containment hierarchies.** `AsyncBubble.parent_bubble_id` walks a tree
   containing only bubbles; message containment is a separate, implicit tree.
   The same object could be "top-level" in one and nested in the other, and
   nothing in the contract said which one meant containment.
3. **Two delivery paths.** `ConversationDelta` and `AsyncBubbleDelta` are two
   renderers, two routing tables and two ordering disciplines.
4. **Pagination is unanswerable.** "Ten messages" has no meaning while a bubble
   might or might not cost a slot.

### The rename

- `ConversationItem` → `Message`. Every `items` field naming them → `messages`.
  Prose in every proto follows; a half-translated contract is worse than an
  untranslated one, because then neither word reliably means anything.
- `AsyncBubble` and its anatomy (`AsyncBubbleUpdate`, `AsyncBubbleDelta`,
  `AsyncFold`, `AsyncOutputSpool`, `AsyncLiveness`, and the six `kind` arms)
  become Message payload rather than a parallel type with its own identity.
- Work whose output accumulates over time is a Message whose payload carries
  that accumulation. It is delivered on the ordinary message path.

---

## Part 2 — MessageLineage, embedded in every Message

```proto
// The two containment facts every message carries, in one message so no
// producer can supply half of them and no consumer can read them from two
// different shapes.
//
// SPAWN IS NOT CONTAINMENT. A message whose work was started by another
// message's tool call is NOT thereby contained by it: provenance lives on
// origin_tool_use_id, and lineage lives here. Conflating them is what made
// "is a top-level bubble a top-level message" unanswerable.
message MessageLineage {
  // The feed row this message ultimately belongs to — the ancestor whose own
  // parent is the feed itself.
  //
  // ALWAYS SET, including on a top-level message, where it equals that
  // message's own id. It is never empty and never inferred: a reader that had
  // to walk parent pointers to find the root would be performing the unbounded
  // traversal this field exists to remove, and a page query would stop being a
  // single indexed pass.
  //
  // DENORMALIZED ON PURPOSE. It is derivable by walking parent_message_id to
  // its end, and storing it anyway is the entire reason a page of ten messages
  // costs one query. The cost is that it can drift: it MUST equal the root of
  // the parent chain, and a write that disagrees is CORRUPTION, not a variant.
  string top_level_message_id = 1;

  // The message immediately containing this one — ONE HOP, never the root.
  //
  // EMPTY means this message sits directly in the feed, in which case
  // top_level_message_id is this message's own id. Absence is the fact itself,
  // not a placeholder for an unknown: a message whose parent could not be
  // resolved is a producer fault, never an empty pointer.
  string parent_message_id = 2;
}
```

### Rules

1. **Every Message embeds `MessageLineage`.** No exceptions, no per-kind
   variants, no message carrying only one of the two fields.
2. **`top_level_message_id` is never empty.** For a feed row it is
   self-referential. This is what lets a page query return exactly N feed rows
   with no null case and no walk.
3. **`parent_bubble_id` is DELETED**, subsumed by `parent_message_id`. Two
   pointers that must agree is a drift opportunity for no gain. After this there
   is ONE containment relation, so "a top-level bubble is a top-level message"
   stops being an invariant anyone maintains and becomes a tautology.
4. **`origin_tool_use_id` SURVIVES, unchanged and unrenamed.** It says which
   tool call started this work — provenance, useful for drawing an affinity line
   back to the originating card. It is NOT lineage and must never be read as
   containment.
5. **Detached work is a feed row.** A message representing work started by
   another message's tool call has `parent_message_id = ""` and
   `top_level_message_id = <its own id>`. Messages produced INSIDE it name it as
   their `top_level_message_id`.

### Why rule 5 is a value and not a shape

Whether such a message is a feed row or renders attached to its originating card
changes only what the daemon WRITES INTO these fields, never the shape of any
message. It is settled as "feed row" because it keeps a page's cost honest: ten
feed rows is ten bounded things, whereas ten messages each containing arbitrarily
many nested ones is ten unbounded things. These values are persisted at write
time, so this is settled now rather than discovered later.

---

## Part 3 — The three record categories

Every durable record falls in exactly one, and the third is the trap.

**A — composes a message.** Owner is knowable at write time from fields the
producer already carries: `ContentDelta.uuid` IS the owning message id; vendor
tool payloads name their issuing message.

**B — IS a message.** Task lifecycle, failure cards, clear/compact markers,
permission items. Its owner is itself.

**C — NOT a message at all.** `session_started`, `session_ended`, `turn_started`,
`turn_ended`, `heartbeat_progress`, `message_latency`, `turn_claim_bridge`,
`query_lifecycle`, `account_usage_observation`, `session_rewound`,
`file_plane_diagnostic`. These render as nothing.

**Category C carries NO lineage, and "unowned" MUST be structural rather than an
empty string that sorts into a query.** If any category-C record acquires a
`top_level_message_id`, it becomes a phantom feed row: a page query returns ten
"messages", several of which are turn boundaries, and the user sees a short page.
This fails silently and is the highest-risk detail in the contract.

Concretely: lineage is carried by the message-bearing arms, and a category-C
record has no lineage field to populate. Do not model it as an empty
`MessageLineage`.

---

## Part 4 — What is explicitly OUT of scope

- **Pagination.** The settled page contract is recorded at
  `proto/PLANNED-message-pagination.md` and is deliberately NOT implemented in
  this wave. It depends on this one and follows it.
- **The store's ownership COLUMNS and their query.** Persisting lineage in the
  store schema belongs to the pagination wave. This wave establishes the
  contract and the wire; the storage layer's denormalized columns come after.
- **Deleting `ResyncCmd.from_seq`, `Subscribe`'s unboundedness, or the old
  `ConversationPageCmd`/`ConversationPage`.** Those doors close in the
  pagination wave, after a bounded tail read demonstrably works. Closing them
  first trades a slow feed for an empty one.

---

## Design principles this contract is held to

- **State enums are forbidden.** Any state, phase, status or condition is a
  `oneof` of dedicated (possibly empty) messages. The set arm IS the state.
- **Every field and message carries a semantic comment** explaining purpose,
  behaviour and motivation — not a restatement of its name.
- **figma→idl.** The daemon RESOLVES what a component shows; the client renders
  VERBATIM and never derives. Consolidating identity does not conflict with
  this: figma→idl governs view resolution, not the data model's identity and
  containment.
- **Absence renders absence.** A value that did not arrive is reported as
  absent, never as a zero, a default, or a placeholder.
- **NEVER remove, weaken, or bypass error-handling coverage.** A refactor that
  changes how a failure manifests adapts the coverage rather than dropping it.
