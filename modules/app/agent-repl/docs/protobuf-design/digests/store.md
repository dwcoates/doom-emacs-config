# THE STORE — derived digest

DERIVED from figma-to-idl-redesign.md at 86fd2b543 — the canonical record WINS
on any conflict. Do not edit; regenerate.

Scope: every standing fact bearing on `store.v1`, the store's schema
architecture, its verbs, and the debts addressed to stage 5. Condensed to
DECISION / WHY / store-relevant consequence; superseded positions appear only
where the record keeps them visible. No proto text — the `.proto` files are
authoritative for shapes.

---

## 1. Standing policies that govern the store

### The store is NUKED, never migrated

DECISION. The store's contents are entirely nuked; no backfilling, hydrating
or schema migration is designed, written, or permitted. Where existing
contents are in the way, the store is DROPPED and recreated.

WHY. The durable store is the one artifact in the system that looks like it
must be migrated; a fresh implementer would otherwise preserve dead shapes or
spend the wave writing migrations the user explicitly does not want.

CONSEQUENCES. Every renaming, re-shaping and re-homing in the redesign may
break the durable schema freely. A durable-compatibility argument is NOT a
reason to keep a message, field or arm. The existing store implementation's
schema (`session_id`/`seq`/`top_level_message_id`) is superseded wholesale.
`CursorList.open_tasks`/`open_tasks_authoritative` died as an old-store
compatibility crutch, dead under this policy. The frozen-replay premise in the
deleted `durable.proto` is NOT void — it is an OPERATIONAL prescription, merely
not relevant during development; its constraints return once the schema ships.

### The shim, store and sidecar hold NO VARIABLE-SIZE STATE

DECISION. No stacks, queues or trees in shim/store/sidecar. Every observation
costs a CONSTANT number of single indexed lookups. A single parent-id lookup is
static; a variable number of lookups (walking lineage) is not. The DAEMON is
explicitly exempt — it is the one component allowed to hold state.

CONSEQUENCE FOR THE STORE, the user's ruling superseding the orchestrator's
flat-log leaning: "the store should be writing conversation.v1 to the
database." Shim and sidecar resolve each `conversation.v1` frame at write time
with a constant number of single lookups (tool return → its call by
`tool_use_id`; skill document → its call by `sourceToolUseID`; spawned agent →
its spawn; re-announced start → its original instant by unit id), and the store
holds the RESOLVED frames. Rows are conversation.v1 messages; no
record-granular vendor vocabulary is persisted. "The store persists whatever
streams carry" STANDS.

The shim/sidecar/store "should be very stupid": they say an agent EXISTS; the
daemon uses that to create whatever connections it sees fit.

### Ruling 1 — lineage is a DENORMALIZED ROOT

DECISION. Every row is stamped at insert with its top-level ancestor: one
lookup of the parent's stored root, then stamp. An inductive invariant,
constant per insert, so "everything spawned under X" is one indexed query. This
WITHDRAWS the earlier "KillTurn's transitive set is the one bounded exception"
(a lineage walk). VERIFIED at the type surface: the vendor names a task's
owning AGENT but never its owning TURN, so the stamp is required, not optional.

### Ruling 3 — `update` frames carry DELTAS

Consequence for the store: none structural. A progress or delta frame is an
ordinary upsert of its unit's row, which simply carries a fresher frame.
Accumulation lives in the daemon, where state is allowed.

### Audit of the settled design against the no-state principle

Already static: tool return → call; skill document → call; created agent →
spawn; re-announced start instant (Owed A); `AgentSuccess.answer` (last
response of the turn); the keep-alive rollback point; a page of an agent's
children (Owed H); one pending ask per agent.

---

## 2. The datalayer/protocol split

DECISION. `store.v1` owns the DATALAYER model (`StoreEntry`), shaped for
storage concerns — provenance, dedup, position, unconvertible material.
`conversation.v1` owns the PROTOCOL model, shaped for consumer concerns. The
SHIM owns the mapping, being the component that writes persistence and serves
the protocol: ONE mapping, one place, one author. `shim.v1.ExternalEntry`
ceases to exist as a shared half.

WHY. The orchestrator's objection (that the split already existed, and that a
protocol respelling would be a third spelling) COLLAPSED on the fact: the old
`store.v1.Entry` embedded `ExternalEntry` and `EntryBatch` carried it, so the
write path handed the store both halves — `ExternalEntry` was the SHARED half,
written AND served. The user's position was correct.

ACCEPTED COSTS, stated. A hand-maintained mapping has NO COMPILER to detect
divergence; the mitigation owed to the wave is tests that FAIL when a field is
added on one side and not mapped. The guarantee given up is the old design's
"producing the daemon's view is a field access, so no mapping function can
forget a field."

GRANULARITY COLLISION this fixed. One word "message" carried three
granularities — a RECORD, a RENDERABLE ROW (what a page counts), and a TURN —
with the same type naming different ones.

---

## 3. `StoreEntry` — the storage envelope

DECISION (the user's own sketch). `StoreEntry` carries the plane, a `write_id`,
an opaque `upsert_key`, and a oneof selecting either a `StoreAgentUpdate` or a
`conversation.v1.SessionUpdate`. `StoreAgentUpdate` carries `top_level` plus a
oneof over: a serveable page line (which NAMES its book agent), the unserved
item, a bash run wrapper, or a workflow run wrapper. `Entry`, `InternalEntry`
and the `shim.v1.ExternalEntry` import are DELETED. `Plane` keeps its two arms.
The write batch carries `StoreEntry`.

PAGEABILITY IS DECIDED BY THE PRODUCER. A page is the contiguous items of ONE
book; non-item frames (a bash update) must be structurally unable to appear in
a page. So a page line NAMES its book agent; unserveable material has no book;
run frames are not page lines at all.

NO PARENT COLUMN ON THE ENVELOPE. The agent is read from `AgentFrame.agent_id`
or `AgentPrompt.agent`; a `SessionUpdate` row is the main agent's. The earlier
envelope `parent` duplicated the frame's own field — the user's catch that
produced `AgentPrompt` as the one form of a delivered prompt (TurnId + AgentId
+ UserSaid), returned by the delivering rpc, persisted by the store, replayed
by history. The shim must resolve the recipient agent before answering
StartTurn.

THE `parent_item`/ANCHOR CONSTRUCTION DISSOLVED when the user ruled each block
its own feed item. The earlier two-key pagination design (AGENT for lineage,
PARENT ITEM for assembly, anchors-then-children) is therefore superseded as a
store construction; what survives is the one-unit-per-row rule, which matches
both the feed's "each block is its own feed item" and the DOM.

### `upsert_key`, the user's rule

One opaque PRODUCER-MINTED key. The store holds one row per key and a write
supersedes it whole. "The store should have a single place it looks for a given
property, never multiple places for a given column" — the mapping from a
TurnId, a unit id or a run id onto the key is the shim's and the sidecar's,
NEVER the store's. Upserts are universal; what varies is only which identity
the key was minted from, under "one row per drawn subject".

### `top_level`, the user's taxonomy

The nearest NON-SYNC ancestor: the turn's main agent or a detached-work agent,
never a sync subagent — equivalently, which live stream carried the work.
Denormalized at insert by the one-lookup induction: top_level(X) = X when X is
main-or-detached, else the parent's stored top_level. It makes "kill a detached
subagent's whole subtree" and "attribute a sync subagent's work to its stream"
one indexed query each. It is documented UNSET when unresolvable (an unparsed
record may name no agent), and it carries the explicit `optional` keyword.

`top_level` is NEVER used for paging — only for kill scope and session scope.

### The three lineage keys

1. `top_level` — as above.
2. The PARENT AGENT — the immediate agent running the thing, sync subagents
   included; read from the frame (`AgentFrame.agent_id`, `AgentPrompt.agent`),
   indexed, never restated on the envelope. It is THE pagination key, so a sync
   subagent is exactly ONE item in its parent's page (its spawn unit) while its
   own constituents carry it as their parent agent.
3. `parent_item` — the within-response anchor grouping (dissolved per above).

An earlier `top_level`-as-main-agent column is SUPERSEDED: the main agent is
constant per logical-session scope (Owed H) and names no column.

OPEN: whether detachable spawn rows also carry a TURN stamp so `KillTurn`'s
transitive refusal list is one query, or the daemon resolves it from its own
state.

### A PAGE IS "IMMEDIATE CHILDREN OF X"

A history page is the rows whose IMMEDIATE parent is the addressed container,
newest first. A unit's later frames (a bash update, a tool return) are not
children of anything — they are upserts of the same unit — so the tree is agent
→ units, and NOTHING has a tool call as parent. The one container that is not
an agent is a WORKFLOW RUN, whose agents the script created, so the stored
parent is a oneof of agent or workflow-run while `top_level` stays an AgentId.
SETTLED: "workflows aren't loaded by page, the SUBAGENTS WITHIN are" — reads
stay addressed by AgentId; the workflow-run parent arm exists only so those
rows have a non-nil parent for scope, and no query pages by it.

PAGINATION IS FOR AGENTS AND NOTHING ELSE. The test is IDENTITY: an item has
one, can be upserted, and can contain other things. A bash line has none — line
400 cannot be addressed or upserted. Same for read lines, grep matches and
patch hunks: positional, not identified.

---

## 4. The unserved oneof

DECISION. `StoreAgentUpdate`'s unserveable arm is replaced by a
`StoreUnservedItem` oneof whose arms are: KEEPALIVE (a real store agent item
that must never be served), vendor-specific residue, unknown residue, and
unparsed residue. THE ARM IS *WHY* it cannot be served — no book, or
unconvertible. The residue bodies carry over verbatim under `Store*` names;
the `UnsupportedEntry`-era wrappers stay dead.

CONSEQUENCE. The residue rides the same envelope as everything else — one
`upsert_key` space, one write path — and the old separate `unconverted` table's
reason to exist goes with it.

STILL HOMELESS, flagged not landed: the old `source_record` kept-whole field (a
faithful conversion that was nonetheless LESS than the source). The user has
not said where or whether it returns.

RELATED EXEMPTION (SESS-13). The undocumented `mode` disk line (always
"normal", 2,669 observed, meaning unknown) is exempt AS A RECORD: the store's
unparsed-residue arm keeps the raw line and nothing else ever sees it.

---

## 5. Keep-alives — the store's exclusion obligation (Owed G)

DECISION. Keep-alive turns are ENTIRELY INSIDE THE SHIM: the daemon never
submits one, nothing keep-alive-shaped is on the daemon-facing wire, and
`PROMPT_ORIGIN_CACHE_KEEP_ALIVE` was retired. The shim determines the
keep-alive prompt text under the hood.

REQUIREMENTS ON THE STORE, recorded as owed (vetting register, Owed G):

1. Keep-alive turns must be FIRST-CLASS IN THE STORE AS NEVER-SERVED — indexed
   so no page returns them and no activity is routed to the daemon. This is the
   `keepalive` arm of the unserved oneof.
2. A real prompt must ROLL BACK context to just after the LAST REAL PROMPT, so
   the next turn does not build on keep-alive context. `SessionRewound` +
   `KeepAliveDiscard` already claim this; its reliability is a vetting item.
   The rollback point is a static single-lookup by the no-state audit.
3. Keep-alives are invisible on the CONTROL plane and visible on the RECORD
   plane: they make real API calls, so their cost lands in accounting whether
   or not anything announces them, and discarded turns are marked superseded —
   "never deleted, only excluded from replay". A paged read must handle
   superseded turns.

The keep-alive YIELD obligation (discarding trailing keep-alive turns before a
real prompt) is the GUARANTOR of the one-submitter invariant, not an
optimization.

---

## 6. Logical-session scoping (Owed H)

DECISION. The store scopes by the LOGICAL SESSION / main agent id, with the
vendor session id as a MUTABLE ATTRIBUTE. The parent column is never nil. "N
most recent children of agent X" is an indexed query on the agent column, not
the old ingest-time feed-row walk.

WHY. If the main agent's id were the vendor session id, a rotation mid-page
would split the agent's history. The two identities are DECOUPLED: the main
agent id is OURS — shim-minted on the first fresh start, STORE-PERSISTED,
reported unchanged on every later start — while identity rotation changes only
the vendor handle the shim resumes by. Shim-minted rather than daemon-minted so
a fresh daemon resuming an old store has one authority.

THE MAIN AGENT IS AN AGENT, and nil dies: rather than "parent nil means top
level", the main agent has an AgentId like any other.

LATER REFINEMENT. `SessionStarted.main_agent_id` was SUPERSEDED when the agent
consolidation landed ("main agent" leaves the API); the store still scopes by
the logical session INTERNALLY, and no consumer ever sees the concept. Owed H
is unchanged by that.

---

## 7. Stage 5 schema architecture — four tables

SETTLED with the user as architecture guidance for the store implementation;
the `store.v1` wire contract is unchanged by it. Four tables, each the ONE
canonical home of one kind of fact:

- `agent` — one row per AgentId, MAIN AGENT INCLUDED; THE source for agent
  metadata: `spawned_by` (an agent, a workflow, or nothing for main), the
  unpacked subagent-start fields, `started_at`, `ended_at` (NULL = live). It
  carries `spawned_by_agent` XOR `spawned_by_workflow`, so "agents of run W" is
  one indexed query.
- `workflow` — one row per run, keyed by the announced handle; `spawned_by` +
  origin unit, the unpacked workflow-start fields, the terminal once ended. THE
  SUBAGENT LEVEL IS NEVER STORED: it is the JOIN (agents whose `spawned_by` is
  the run, with their liveness), so agent liveness has exactly one home. This
  supersedes the earlier "the store's workflow row shrinks to start + latest
  level + terminal".
- `entry` — the page lines: a QUERYABLE SPINE (`upsert_key` primary key,
  `book_agent_id` indexed and NULL for unserveable, `write_id` unique, plane,
  first-insert position) around a SERIALIZED frame the store never opens.
- `detached_work` — one row per detached non-agent run (bash today): the
  handle, kind, origin unit, owner agent, unpacked latest state, `ended_at`.

### The columns-vs-blob line, the user's rule

"Not ALL shapes need to map … it's the queryable and joinable stuff." `agent`,
`workflow` and `detached_work` are UNPACKED TO COLUMNS (the store filters and
joins on them); `entry`'s frame stays SERIALIZED — the activity vocabulary is
content, and unpacking it would put every `conversation.v1` churn into DDL and
two mapping directions for nothing the store ever queries. Mapping tests per
the persistence-model principle guard the unpacked three.

### The routing rule — every write lands in exactly ONE table

Decided by the wire arm of `AgentFrame.result`:

- `update` → the `entry` table, as a page line of the agent's book.
- `success` / `failure` → BOTH `entry` (the stop notice has no other source)
  AND the agent row's terminal columns, in ONE TRANSACTION.
- `detached_work` → the lifecycle table for its kind (agent, workflow,
  detached_work), NEVER a page line — the spawning call is already one.

The workflow arms route with no shape change: start → workflow row; the update
level → AGENT rows upserted (spawned-by-workflow, spawn columns, liveness);
terminal → workflow terminal columns. The level is stored nowhere and IS the
join.

### Other schema settlements

- Prompts and frames share ONE entry table. One position space is what makes
  "everything after the last real prompt" — the rollback — a range query.
  `turn_id` is a nullable column or a 1:1 side table; a prompts "table" is a
  PARTIAL INDEX, never a second position space.
- The DEDICATED TABLES ARE PRIMARY for their facts. The earlier
  projections-rebuildable-from-`entry` idea is SUPERSEDED: verbs read tables,
  `entry` is canonical for content.
- Every foreign key is stamped at insert with ONE lookup, per the no-state
  principle.
- The store must be INDEXABLE ON A UNIT'S ID. Today's `entry` is keyed
  (session_id, seq) with a dedup index on `write_id` and two indexes on the
  feed-row-owner column, so a lookup by unit id would be a SCAN. Under the
  protocol model the unit's id becomes the record's own identity, so
  `StoreEntry` must carry it as an INDEXED column. Stage 5 owns that.
- `seq` stays the STORE'S OWN ADDRESSING and never reaches the daemon-facing
  wire ("a position is the store's addressing, not a fact about a
  conversation").

### What the verbs become against this schema

`GetWorkflow` = one workflow row + the agent join. `GetLiveWork` = the two
`ended_at IS NULL` scans. The page verbs = the entry spine.

---

## 8. The store's file model and package shape

- The store is a CONNECT SERVICE (`service ShimStore`), callers the SHIM and
  the SIDECAR only — the store earned its process boundary by having TWO
  producer processes (contrast the daemon's WSM state, which has one and
  therefore stays an in-process SQLite library).
- FILE MODEL, final: `service.proto`, one `endpoint_*.proto` per rpc, and
  `store.proto` for datatypes. The cross-endpoint page vocabulary (the item
  pointer, the line-at wrapper, the session token, the page, the more/floor
  arms) lives in `store.proto` beside the record, the write batch and the
  cursor.
- SUPERSEDED, kept visible: an intermediate fold that merged the three read
  endpoint files into a single `read.proto` (and, before that, the fold of
  `write`/`entry`/`unsupported` into one `store.proto`). The standard service
  file model above supersedes both. No shape changed in any of it.
- `cursor.proto` FOLDED INTO the write concern: the cursor state, query and
  list live with "how a producer writes and resumes"; the batch already
  embedded the cursor and nothing else imported the file.
- DELETED with that fold: `OpenTaskState` (a timestamp that could not name
  WHICH task — live-work recovery reads the run rows instead) and the
  open-tasks/authoritative pair. `CursorQuery.file_id` went `optional`
  (empty-means-all was a sentinel).
- Stage 5's original per-file walk order was `write` → `entry` → `cursor` →
  `unsupported`, top-down by containment.

### The cursor, settled at high level first

The cursor is invisible machinery whose UX is "after any crash or deploy,
history has no gaps and no repeated messages". It rides the BATCH, not the
entry: one read position yields many entries, it is a FILE BOOKMARK rather than
a conversation fact, and stream-plane writes have no file to be positioned in.
`EntryBatch.cursor_advance` carries the explicit `optional` keyword.

---

## 9. The store's verbs

### `WriteBatch`

Request carries the producer and the entry batch; the response is the canonical
success/failure pair. `StoreEntryWrite` is DELETED — under Connect the rpc IS
the envelope, and a separate carrier would be a second spelling.

SEMANTICS. Success means DURABLE — records plus the cursor advance, ONE
TRANSACTION — with replay absorption via `write_id` documented at the same arm.
Failure means NOTHING COMMITTED, so the producer's spill holds and replays.

WHY IT MATTERS. The old UDS protocol acked nothing: a producer learned failure
only by connection death. The success arm is what lets the shim's spill retire
batches on acknowledgment.

RENAME CONSIDERED AND DECLINED. `WriteBatch` → `WriteSidecarBatch` was
withdrawn: the shim also writes batches (stream-plane facts, spill replays);
only `cursor_advance` is sidecar-specific and already states its absence.

### `OpenAgentSession` + `WatchAgentSession` — open answers "where am I", watch is a pure tail

DECISION (the user's factoring and names). The OPEN takes the agent, a page
size and an optional caller high-water pointer, and answers with the page (each
line carrying its pointer, with a more/floor boundary) plus an opaque WATCH
TOKEN. The WATCH takes that token echoed and is a STANDING stream of one frame
per written line, UPSERTS INCLUDED.

WHY. Pages and watching are DECOUPLED: the open is a bounded unary answer, the
watch a pure tail addressed by an opaque STORE-MINTED token — "a hash of the
actual session identifier, so the client MUST call open to subsequently watch"
— which also PINS THE TAIL to begin exactly after the page's newest item.

`known_through` is the CALLER'S OWN high-water mark: UNSET = repaint (a
reopened historical workspace paints the first page whole); SET = catch-up
after a shim bounce (the page carries only newer items; a gap wider than the
page size is walked older via the page verb until the caller meets its own
mark). THE STORE DELIBERATELY TRACKS NOTHING about what it previously served.
Every streamed line carries its pointer so the caller always holds a current
mark.

REOPENED BY NAME BY THIS PATTERN, and both later discharged: (1) the shim
boundary got the same open/watch split — realized as `WatchAgent`, whose FIRST
FRAME is the opening page and whose tail is one pointered entry per write, with
`ReadHistory` going next-only; (2) the `agentrepl.v1` feed surface got it at
the frontend remediation pass — `OpenFeed` mints a `FeedWatchToken` pinning the
tail exactly after the answered page, and `WatchFeed` became token-addressed
and tail-only.

### `ReadAgentPage`

RENAMED from `ReadPage` — "we only paginate on agents, so names carry
AgentPage, not Page"; the rpc, its endpoint file and its whole message family
renamed, a pure rename.

The request's position collapsed to a REQUIRED `after` pointer: there is no
first-page request semantics anywhere, because the first page is always the
OPEN's answer.

PAGINATION SPEC (the user's). Page size rides the REQUEST so it can vary across
calls of one walk; `next` carries ONLY the pointer to the previous page's last
item, which the previous response's `more` arm served — no memory of the
previous page's size anywhere.

`StoreItemPointer` is OPAQUE and STORE-MINTED, and STABLE ACROSS UPSERTS
because ORDER IS BY THE UNIT'S FIRST INSERT, never its last write — a unit
settling mid-walk cannot teleport across a continuation.

### `GetSidecarCursors`

Request/response pair under a RECOVERY section on the service, with the
canonical success/failure outcome. Empty success is documented as the
FRESH-STORE answer.

### `GetWorkflow`

Request is the detached-work handle. Success carries the workflow start (from
the row's columns) plus a standing oneof: LIVE, carrying the DERIVED level, or
ENDED, EMBEDDING the stream's terminal message rather than respelling it. The
level is computed AT SERVE TIME from the agent table and stored nowhere. The
shim's own `GetWorkflow` serves from this, adding only the watch token.
`ended` deliberately carries no level; a caller wanting a dead run's spawn list
is a later additive arm if ever needed.

### `GetLiveWork` — the OPEN-OBLIGATIONS verb

Empty request; success carries live agents, live workflows and live detached
work — IDS ONLY, from the three tables' non-terminal rows.

THE SEMANTICS the user probed to settlement: "live" is a claim about the
RECORD, not the world — "a start was written and no terminal ever was" — so it
CANNOT GO STALE; a five-second bounce and a two-week-old workspace run the
identical procedure.

THE SHIM (never the sidecar, which is a copier whose only recovery is cursors)
calls it once at session start and resolves every item: re-adopt what the
revived vendor process actually has, and WRITE THE CLOSING TERMINAL for what
did not survive (a dual write that closes the record and puts the stop notice
in the feed). THE INVARIANT: every started thing eventually gets a terminal
row, by observation or by reconciliation. It is also the ONLY producer of
resume-time live work, since the vendor's level is per-process and empty at
startup.

Deleting the verb was weighed: it would make restarted background work
invisible-but-running and dead work spin forever.

### The read inventory as originally opened

Names settled in conversation when the first read verb landed: GetItem (by
`upsert_key`), GetRun, GetLiveWork, GetLastPrompt (the keep-alive rollback
point), GetCursors, GetSessionFacts. The old UDS Subscribe surface is
superseded. What actually landed is the set above; the rest is reference.

---

## 10. Identity facts the store depends on

- FOUR IDENTIFIER SPACES, not interchangeable: the vendor's `agent_id` (WHICH
  AGENT INSTANCE — its own space, proven by `parent_agent_id` existing because
  depth > 1 cannot be resolved from call ids), the vendor's `tool_use_id`
  (WHICH TOOL CALL — for a Task call this is THE SPAWN, not the agent),
  `activity_id` (ours — WHICH UNIT OF WORK, shim-minted, one per unit for its
  whole life, NAMES WORK NEVER AN AGENT), and `TurnId` (ours — WHICH TURN,
  daemon-minted).
- STILL OPEN, OWED TO STAGE 5: whether `activity_id` and the store's
  per-record identity are the same value. Under the protocol model the unit's
  id IS the record's identity and the no-respell rule says a typed identity is
  imported rather than restated, so they SHOULD coincide — but the old store's
  `top_level_message_id` is a THIRD and coarser thing (the feed-row owner
  column) and must not be conflated with either.
- `DetachedWorkId` STAYS as the uniform CONNECTION token (a retraction of an
  earlier in-stage verdict). The store's run wrappers keep their TYPED
  identities — bash keyed by its activity id, workflow by its agent id —
  BECAUSE THE STORE JOINS ON IDENTITY; the shim owns the handle↔identity
  mapping, one lookup.
- The re-announced `start` INSTANT is RECOVERED FROM THE STORE, not held in
  memory (the user's ruling, improving an orchestrator note): the store has it
  and must be indexable on the unit's id. The path this serves is SHIM RESTART
  — for an ordinary detach the shim still holds the instant.
- The prompt's id is the DAEMON'S: minted at submission, ADOPTED by the shim on
  delivery, written under it, and returned by history. The vendor's own
  uuid/promptId stay in the store's internal half; vendor uuids never cross the
  contract.

---

## 11. Consequences landing on the store from other stages

- `state.v1` IS DELETED and its two stored uses — the token-utilization and
  turn-accounting BLOB ROWS — LOSE THEIR PRODUCER: usage rides the store's
  frames and aggregates are DERIVED ON READ. The daemon's durable state needs
  no wire shape at all, being internal DDL under the SAME columns-vs-blob rule
  as the store.
- What dies with today's `state.db`, with the store as replacement for several:
  the turn ledger → open watch streams + store entries; token evidence → store
  frames; the compaction gate and reader positions → client-held pointers and
  THE SHIM'S STORE READS.
- The WSM's `held_prompt` table uses the same columns-vs-blob line: the user's
  said content as the one BLOB, classification and hold reason as columns.
- The topbar/footer accounting has no store schema implication: usage rides the
  `AgentActivity` envelope with the ONE-UNIT-PER-RESPONSE rule (the unit for
  the response's FIRST content block carries it, every other unit leaves it
  unset), so a consumer summing units does not over-count.
- The SIDECAR is a SECOND PRODUCER needing its own verification pass; it must
  read each workflow agent's meta file alongside its transcript.
- Money is deliberately NOT represented in the API at all (RunCost deleted), so
  no cost column exists anywhere.
- Reconciliation: the store subsystem reconciled GREEN in an isolated worktree
  and merged (all packages race-checked); `storev1connect` handler interfaces
  now generate, because `protoc-gen-go` emits message types only. The store's
  dead-code inventory, integration replacement specs and reconciliation gotchas
  are seeded into `docs/implementation/`.
- THE TEST RULE for reconciliation: any test referencing a DELETED symbol, or a
  RESPELLED one, is DELETED, never adapted; pure renames adapt mechanically.
  Replacement coverage is prescribed for INTEGRATION and E2E only — unit
  coverage falls out of the proto→code mapping convention.
- IMPLEMENTATION INVARIANTS the store inherits verbatim: unset non-optional
  fields are ILLEGAL everywhere (a request carrying one errors to the producer
  at once; a consumer receiving one on a response or stream push raises
  loudly); one base implementation function per message where validation lives
  once; a dedicated testable function per non-primitive use site; debug logging
  on every logical branch, warnings remediated to zero.
