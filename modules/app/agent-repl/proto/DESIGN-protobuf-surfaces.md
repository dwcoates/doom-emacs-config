# DESIGN: the agent-repl protobuf surfaces

## The five surfaces

Every message belongs to exactly one. **The package boundary IS the surface
boundary** — nothing straddles, so which surface a message is on is a fact a
compiler can check rather than a convention a reviewer has to hold.

| package | holds | importable by |
|---|---|---|
| **agentshim** | shim-side internals: which plane observed a record, the store's write identity, anything a producer could not convert | shim, sidecar, store ONLY |
| **conversation** | the message model the shim generates and the daemon routes to the webapp — `MessageEntry`, its payloads, the content model, `TokenUsage`. Nothing about sessions, turns or machinery | everyone |
| **protocol** | what traverses the daemon↔shim boundary, and ONLY that boundary, in both directions: handshakes, commands, receipts, health, replay and page requests, and the delivery envelopes that carry conversation records with their position | shim, daemon |
| **frontend** | what reaches a frontend client. COMPOSED of `conversation` messages the daemon forwards, and ALSO of novel messages the daemon synthesizes — topbar, sidebar, footer, and the rest | daemon, webapp |
| **state** | daemon-internal only: what no other service uses, including the schema the daemon marshals into its own SQLite store | daemon |

**A package is named for its purpose, not its owner.** `state` holds state and
the daemon owns it; `protocol` describes a wire both ends speak. Naming either
for the daemon would have said who it belongs to while leaving what it holds to
be inferred — and in `protocol`'s case would have been actively wrong, since the
shim produces on it too.

`BookkeepingEntry` is `protocol`, not `conversation`, and this is the routing
test doing real work. Bookkeeping is produced by the shim and consumed by the
daemon, and it STOPS there — a client sees it only after the daemon has resolved
it into a view. It never reaches the webapp, so it is not part of what routes
through.

`ExternalEntry` is `protocol` for the same reason: it is the WIRE envelope, and
it carries either a `conversation.MessageEntry` or a `protocol.BookkeepingEntry`.
It does not collapse when bookkeeping leaves — the store persists bookkeeping
too, so a stored record must still be able to hold one.

Every package keeps a `v1` suffix, and the five are ROOT namespaces —
`agentshim.v1`, `conversation.v1`, `protocol.v1`, `frontend.v1`, `state.v1`. There
is no umbrella prefix: `agentshim` is the name of the surface holding
shim-specific internals, so it cannot also be the namespace everything else
hangs under.

## Collecting gaps rather than fixing them

While the model is being implemented, two situations — and only these two —
indicate a POTENTIAL gap in the new schema:

1. The proto build breaks after something is removed from the old packages.
2. An implementing subagent finds existing code parsing data into an old proto
   shape that has no counterpart in the new one.

In neither case is a change made. The situation is NOTED and collected. Only
once the implementation attempt is complete are the collected cases taken up
together, as one remediation conversation.

**Why this lives here**: a gap found mid-implementation is evidence, not a
verdict — it may be a real omission, or bad modeling, an abstraction leak, or a
dead feature, and that judgment needs the whole set in view. Letting each
implementing agent patch the schema where it happens to trip is how six agents
produce six unilateral contract changes nobody reconciled.

Store↔sidecar traffic — the cursor messages — is `agentshim`, not `protocol`:
it never crosses the daemon boundary, which is the only boundary `protocol`
describes.

`state` also lets `check-durable-isolation.sh` be deleted: that gate exists only
because the daemon's persistence layer currently sits inside `frontend/v1` while
being definitionally not frontend. Once the package boundary says so, the gate
is enforcing something the compiler already enforces.

## What decides which surface a message is on

**Who produces it, and where is it routed.** Never "what is it about".

Subject is a judgment call, so a message whose subject was ambiguous never got a
home at all — and a missing message does not fail a compile, it simply is not
there, until a consumer reaches for it. That is the mechanism behind every
message this refactor lost.

Routing has one answer per message and is decidable by inspection. Worked
example: `QueryLifecycle` is about the SDK query object rather than about the
conversation, which made it homeless under a subject test. It is produced by the
shim and routed to the daemon, so it is `protocol`, and there is nothing further
to decide.

## The dependency rule

`conversation` **imports nothing**. It is the leaf, which is what makes it
shareable: `agentshim` imports it (a stored record contains one), `protocol`
imports it (an envelope carries one), `frontend` imports it (a client renders
one).

So **a shared type lives in the most upstream package that needs it**, and there
is no vocabulary package. `TokenUsage` is used by `conversation.AgentSaid` and by
`frontend`'s accounting views; it lives in `conversation` and frontend imports
it.

## Passthrough and synthesis

The flow is **shim → daemon → webapp**. A record appearing at two surfaces is it
MOVING, not it being ambiguous. The webapp renders shim-produced information
because the webapp is downstream of the shim.

The only thing that would be a real overlap is shim INTERNAL surface reaching
the daemon, and that is structurally prevented: separate package, separate Go
import path, separate TypeScript module, plus `check-conversation-isolation.sh`
wired into `codegen-gate` so no route to bindings can emit for a violation.

**The daemon does both.** It FORWARDS conversation records — `MessageEntry` and
its payloads reach the webapp as the producer wrote them. And it SYNTHESIZES:
bookkeeping records are coalesced into the sidebar, the footer, the topbar, the
`/status` rows, failure cards and turn accounting, none of which any producer
could compute.

So `frontend` is composed of both, and the line between them is whether the fact
was already known to the producer.

### Two consequences that were recorded as accepted, and did not hold

Both were written here as settled before anything implemented against them. The
daemon wave then checked them against the frozen schema and neither survives in
the form recorded. Kept visible rather than quietly rewritten, because the error
in both is the same one: a consequence was derived from the passthrough PRINCIPLE
without checking whether the schema actually admits it.

**1. `translate.go` goes — WRONG AS STATED.** The claim was that translation is
now ceremony, since producers convert at the edge and no vendor shape is left to
neutralize. But `frontend.v1.Message`'s payload oneof has NO arm that can carry a
`conversation.v1.MessageEntry`. Its arms are `AgentEmission`, `UserContent`,
`FailureCardView`, `DaemonInterceptedCommandItem`, `DetachedWork` and
`CompactionSummaryItem`. So the daemon must still re-encode `MessageEntry`
payloads into `AgentEmission`.

What is true is narrower: **translation stopped being VENDOR translation.** It
did not stop existing. Whether the feed should gain a `MessageEntry` arm — making
passthrough literal — or keep re-encoding is an open question, not a settled one.

**2. The feed envelope shrinks — NOT IMPLEMENTED, AND PARTLY WRONG.**
`feed.proto` still declares `lineage`, `source` and the `durability` oneof. The
shrink was recorded here as landed and was never applied anywhere.

It is also not wholly desirable. `lineage` and `source` do restate what
`MessageEntry` carries. **`durability` does not, and cannot.** There is no
`conversation.v1` counterpart, because every `MessageEntry` exists for the reason
that a producer wrote it — ephemerality is a fact about the FEED, not about the
record. Drop it and a page missing an ephemeral card becomes indistinguishable
from a page that lost a durable one, which is data loss reported as normal.

## Implementation and integration status

**The proto tree is restructured onto the five surfaces; no consumer has been
migrated.** `daemon/`, `webapp/` and `agent-shim/` are mid-rewrite and do not
build against it, which is the expected steady state until the gaps below are
reconciled.

**Wave two is in flight**, across six scopes: the shim, the sidecar, the store,
the wire layer, the daemon and the webapp.

A first wave was dispatched earlier and four of the six correctly stopped without
writing code. The reason, in one line: the contract specified what a record IS
and never specified how one MOVES. `StoreWrite` still carried the retired
`EventBatch`, nothing imported the stored record type, and the delivery envelope
was generated and referenced by nothing. Kept here because it is the cheapest
lesson in this document — a record contract with no transport reads as complete
right up until someone tries to send one.

**The wave stops when all six report**, for one joint review of the collected
gaps. No structural review, no e2e run and no remediation dispatch happens before
that review, because remediation is a judgment about the whole set — which is the
entire point of the gap protocol above.

Every agent in the wave carries the same standing rules: no protobuf edits for
any reason, scope-exclusive, every other system expected broken as the steady
state, unit tests only, no backward compatibility, and error-handling coverage
adapted rather than dropped when a failure changes shape.

---

# COLLECTED GAPS — awaiting one reconciliation conversation

Found while restructuring the tree. **Nothing here has been fixed**, per the gap
protocol above: each is evidence, and whether it is a real omission, bad
modeling, an abstraction leak or a dead feature needs the whole set in view.

Every retired field below is reserved by NUMBER and by NAME.

## A. A name collision forced the one unilateral change in the pass

`BookkeepingEntry` declares `Heartbeat`, and `core.proto` declared a `Heartbeat`
too — the UDS keepalive. Moving bookkeeping onto `protocol.v1` put both in one
package and protoc refuses the tree until one name moves.

The keepalive was renamed `ConnectionHeartbeat`, because the bookkeeping spelling
is frozen (`FROZEN-conversation-v1.md`). **Which of the two should keep the plain
name is a contract decision, not a build fix.** It is the only schema change in
this pass that was not mandated.

`SessionEnded` and `TurnEnded` collided the same way and needed no ruling: both
core spellings were on the retirement list, and the bookkeeping ones carry
strictly more.

## B. Retired records whose consumers have no replacement

1. **`PermissionResponse.decision`** (field 2). `PermissionDecision` retired with
   the record layer, leaving a response that can carry an edited input and a
   denial sentence but cannot state the allow/deny verdict — the one thing the
   message exists to say.

2. **`Message.permission`** (feed.proto, field 30). Carried
   `PermissionItem`, the daemon-composed request-plus-resolution a client renders
   and answers. A permission request is a MESSAGE by the feed's own test, so its
   absence is a hole in the feed, not a tidy-up.

3. **`Message.context_cleared` / `context_compacted`** (fields 32, 33). A context
   cut is a message by `conversation.v1`'s test — a reader scrolling back must
   see WHERE the conversation was cut. No arm names one on any surface now.
   `slash-menu.proto`'s `DaemonInterceptedCommandItem` still documents itself as
   "the invocation, not the outcome", and the outcome half no longer exists.

4. **`TypingDelta.delta`** (field 3). Carried `ContentDelta` — the live typing
   preview the whole message exists to relay. `TypingDelta` now carries a
   workspace and a fence and no content.

5. **`HeartbeatView.progress`** (field 3). Carried `HeartbeatProgress`
   (`tool_use_id`, `elapsed_seconds`). Tool progress is a fact ABOUT a turn, so
   `BookkeepingEntry` is its home — but bookkeeping never reaches a client and no
   resolved frontend spelling exists, so the view has nothing to tick.

6. **`OpenTaskState.started`** (field 1). Carried the `TaskStarted` `Event` that
   opened the task. The message can now say when a task was last active but not
   WHICH task it is, which makes the sidecar's open-task recovery underivable.

7. **`StoreWriteAck` has no replacement.** `StoreWrite` became `agentshim.v1`'s
   `StoreEntryWrite`, but nothing acks it — so `accepted`, `deduped`, `last_seq`
   and the batch-rejected `error` have nowhere to be reported. A producer cannot
   currently learn that its write landed, which is what `write_id`'s
   replay-idempotency contract depends on.

8. **`BackfillState.FAILED` has no evidence to resolve from.** It resolved off
   `core.v1.UnparsedEvent`, which was durable and reached the daemon.
   Unconvertible records are now `agentshim.v1`'s unsupported arm, which
   deliberately never leaves the shim.

## C. Layering observations, no build impact

9. **`frontend.v1` imports `protocol.v1`, which the dependency rule does not
   sanction.** The rule says `conversation` is the leaf that
   `agentshim`/`protocol`/`frontend` all import; it says nothing about frontend
   importing protocol. It does, for `PromptOrigin` (prompt-queue, footer),
   `InterruptOutcome` (footer), `QueryRuntimeIdentity` (durable) and four query
   failure arms (errors). Each is a shim-wire type reaching a client directly.

10. **`state.v1` imports `protocol.v1` and `conversation.v1`.** Consistent with
    "daemon-internal, depends on everything", but worth stating: the frozen
    persistence layer now names types on two other surfaces, so a change to
    either is a durable-replay concern.

11. **The store↔sidecar cursor messages are in `protocol.v1`, and this document
    says they should be `agentshim.v1`** — "it never crosses the daemon boundary,
    which is the only boundary `protocol` describes". `CursorState`,
    `CursorQuery`, `CursorList` and `OpenTaskState` moved with the rest of
    `core.proto` because the restructure was specified as a directory move. They
    are misfiled by this document's own routing test.

12. **`conversation.v1` imports `google/protobuf/struct.proto`** (content.proto),
    so "imports nothing" is true of the five surfaces but not literally true. A
    well-known type creates no surface coupling; noted only so the rule is not
    read as violated.

## D. Orphaned but retained

13. **`TaskKind`, `TerminalStatus` and `SessionSource` have no user left in the
    schema.** Their only referents were the retired task and session payloads.
    Kept because master still names all three, per the delete-only-what-nothing-
    references rule — but nothing in the new model produces or consumes them.

14. **`TurnClaimBridge`, `SessionRewound`, `KeepAliveDiscard`, `QueryLifecycle`
    and the whole query-lifecycle subtree have no carrier.** They were reachable
    only through `Event.payload`. They remain declared on `protocol.v1` and every
    one is referenced on master, but no envelope on the new model carries them, so
    a producer has no way to send one.

---

# LANDED CHANGES

What changed, why, and which consequences were accepted as costs.

## The schema splits into the five surfaces

**What changed.** The tree became five ROOT namespaces — `conversation/v1`,
`protocol/v1`, `agentshim/v1`, `frontend/v1`, `state/v1` — with no umbrella
prefix. `BookkeepingEntry` and `ExternalEntry` moved to `protocol`; the durable
persistence layer moved to `state`; the shim-side record surface became
`agentshim`. Twenty-four retired types were deleted outright. The
durable-isolation gate was deleted with them.

**Why.** Membership was previously decided by subject, which is a judgment call,
so an ambiguous message got no home at all — and a missing message does not fail
a compile, it is simply absent until a consumer reaches for it. Routing has one
answer per message and is decidable by inspection. Making the package boundary
the surface boundary turns that answer into something a compiler checks.

`check-durable-isolation.sh` could go because it existed only to enforce that the
daemon's persistence layer, which sat inside `frontend/v1`, was not treated as
frontend. Once the package says `state`, the gate enforces what the compiler
already does.

**The delete-by-default pass found nothing to delete.** Every one of the ~79
top-level types in the old `core.proto` is referenced on master — verified across
Go and TypeScript including enum-value constants and oneof wrapper types, which a
word-boundary grep misses. So the "delete because nothing references it" bucket
is empty, and everything removed came off the known-retired list instead. Worth
recording because the delete-first instinct was correct as a posture and produced
zero deletions on its own evidence.

**Accepted cost.** `frontend` imports `protocol` — for `PromptOrigin`,
`InterruptOutcome`, `QueryRuntimeIdentity` and four query-failure arms. The
dependency rule sanctions `conversation` as the shared leaf and says nothing
about this edge, so each of those is a shim-wire type reaching a client directly.
Recorded as gap 9 rather than resolved, because whether a client should see a
shim-wire type is the same question the gap protocol exists to batch.

## Store↔sidecar cursor traffic moves to `agentshim`

**What changed.** `CursorState`, `CursorQuery`, `CursorList` and `OpenTaskState`
moved from `protocol/v1/core.proto` to `agentshim/v1/cursor.proto`, unchanged in
content.

**Why.** `protocol` describes the daemon↔shim boundary and only that boundary.
These four never cross it — they are how the sidecar recovers its file cursors
from the store. This was already the document's ruling; the restructure landed
them in `protocol` only because it was specified as a directory move, and gap 11
recorded the contradiction.

Applied rather than batched with the other gaps because it is not an open
question: the routing test gives one answer, and the evidence confirmed the move
costs nothing. No `daemon/` or `webapp/` source references any of the four, and
the sole cross-package proto reference is `agentshim`'s own `EntryBatch`, so the
move REMOVES an import rather than inverting one.

## `QueryRuntimeIdentity` stops restating process-fixed facts

**What changed.** Removed `claude_code_version`, `auth_source` and
`subscription_type`, reserving both numbers and names. Everything that can
differ between two `query()` calls stays, including all five
`EvidenceFingerprint` fields.

**Why.** `SessionBegan` already states the CLI version, auth source and
fast-mode posture once per session. Two spellings of one fact, with nothing
comparing them, diverge silently — a wrong `/status` panel or a wrong cost
attribution with no error anywhere.

The cut was made per FIELD, because the blanket version was wrong: `/fast`
toggles mid-session and a model switch changes `effective_model`, so those vary
per query and stayed. Verified against the shim's emission code, which corrected
one of the three — `subscription_type` was never populated at all (hardcoded
empty) and its real home is `AccountUsageObservation`. Dead surface, not
duplicated surface.

**Accepted cost.** `claude_code_version` and the auth source now require a join
against `SessionBegan` rather than sitting on the evidence record, which weakens
it as a self-contained forensic artifact. Accepted because both are constant for
a shim process, so the join has exactly one answer, and because the fingerprints
— what the record exists for — remain per-query and self-contained.

# WAVE-TWO REMEDIATION DECISIONS

Settled one at a time against `GAPS-wave-two.md`. Each lands in the canonical
`.proto` files as it settles, not at the end.

## The sidecar's own diagnostics stay durable

**Decided.** Gap 5 is closed as "no change". The sidecar keeps writing its
store-link outage report through the store, as a `ProducerDiagnostic`.

**Why.** The three alternatives all cost more than the problem. Giving the
sidecar a daemon-facing wire is real architecture for one message; reviving the
store's publish-without-persist path rebuilds the very thing the ephemeral class
was retired to remove; routing the sidecar through the shim inverts the
topology, because the sidecar is a launchd-managed SINGLETON that reads
transcripts for sessions with no live shim at all, and its cursor contract is
with the store rather than with any session.

The sidecar's own total-ingestion mandate also already says everything it reads
lands in the store, so a record about its read loop is not obviously out of
place there.

**Accepted cost.** An ingestion outage becomes a durable row in conversation
history, and workspace health never learns the component was down —
`DegradedState`'s fault WINDOW cannot be opened by a producer that can only
report after the fact.

**Correction this supersedes.** Gap 5 was filed as "ephemerality has no
write-side expression", which was wrong and would have led to the wrong fix. The
audit found no ephemeral record reaching the store: the shim routes every one
straight to the daemon (`uds-session.ts:2451`) and returns before any write
path. Ephemerality needs no write-side expression, because by definition nothing
ephemeral is ever written. The real finding was narrower — the sidecar has no
live channel at all.

## A record may be partially convertible

**Decided.** `InternalEntry` gains `google.protobuf.Struct source_record`, set
when converting a record to its external half dropped structure.

**Why.** `Entry` could carry a rendering OR a verbatim record, never both, so a
record that renders fine and is ALSO more than its rendering was durably less
than what was on disk. `journal.go:50` is the worked case: a journal object
becomes the string `"result build: ok\n"` and every other key is gone. That
breaks the sidecar's total-ingestion mandate, whose old escape hatch was a raw
Struct.

It is safe in the INTERNAL half specifically: vendor-shaped material there is
structurally unreachable from the daemon, which is exactly what makes eager
conversion at the edge a reversible bet rather than a lossy one. `unconverted`
keeps its own job — records with nothing renderable at all.

## The vendor's final response usage gets a carrier again

**Decided.** `TokenUsage` gains `output_thinking_tokens`; `BookkeepingEntry`
gains a `ResponseUsageCorrected` arm keyed by `api_message_id`.

**Why.** `message_start` carries a usage SNAPSHOT — final input and cache
counters, interim output. The FINAL `output_tokens` arrive on `message_delta`
and nowhere else the daemon can see. `TokenUsage` rides `AgentSaid`, which is
file-plane, so the stream plane observes the correct number with no field to put
it in. One measured turn summed 563 against the result's 4407, with input
reconciling exactly.

This regression is a REPEAT: commit `1bdeacec5` found and fixed it once, adding
`vendor_usage` and `api_message_id` to the retired `MessageDeltaEvent`. The
restructure removed that carrier and replaced it with nothing.

It is a separate RECORD rather than a field on the response because the two
facts come from different planes at different times — a producer that could only
state usage on the response would have to withhold the response until the turn
ended, or restate it, and neither is available to it.

`output_thinking_tokens` is included because the old narrow projection could not
express `output_tokens_details`, and the earlier fix recorded narrowing as the
defect rather than the fix.

## The store↔sidecar health traffic stays in `protocol.v1` for now

**Not decided — explained and left.** `ConnectionHeartbeat`, `HealthCheck` and
`HealthStatus` are misfiled by this design's own routing test: they never cross
the daemon boundary, which is exactly why the cursor messages moved to
`agentshim`.

They are NOT moved, because the case is not as clean as the cursors'. The shim's
UDS server ANSWERS `HealthCheck` (`server.ts:730`) and
`daemon/internal/shimclient/bringupgate_test.go` references it, so a daemon→shim
probe was clearly intended even though no production daemon code sends one
today. Moving them would foreclose that. Resolve the bring-up gate's intent
first.

## The duplication carve-out is INTRA-namespace only

**The rule.** A shared datastructure is IMPORTED from the namespace that owns
it. It is never re-declared in the consuming namespace, in any form — not as a
parallel message, not as a parallel oneof, not as a loose scalar standing in for
a typed identity.

**What the figma→idl carve-out actually licenses.** "Duplicate, don't share"
says that when TWO UI COMPONENTS render the same fact, each component's message
carries its own resolved copy rather than the two reading one shared message and
deriving. That is a statement about component messages WITHIN `frontend.v1`, and
only about them. It says NOTHING about reusing types from another namespace, and
it never licensed `frontend.v1` re-spelling `conversation.v1`.

**Why the distinction is the whole point.** The carve-out's cost is bounded: a
duplicated RESOLVED VALUE is re-resolved on the next publish, so a stale copy
self-corrects and authority stays with the resolver. A duplicated TYPE has no
such property — it is a second definition of the same idea, maintained by hand,
and it diverges silently the moment one side gains an arm. `conversation.v1`'s
detached-work outcome has four arms; `frontend.v1` re-spelled it with three and
`TaskStatus` with six. Nothing failed to compile.

**A raw scalar is a re-spelling too.** A `string message_id` in `frontend.v1` is
a `conversation.v1` identity with its type removed. Message ids are minted by
the producing plane and belong to `conversation.v1`; a frontend field that holds
one as a bare string can be assigned any string at all, which is exactly the
class of error the surfaces exist to make unrepresentable.

The inventory of current violations is in `RESPELLINGS.md`.

## The daemon's plane checks are vestigial and delete — VERIFIED

**Decided.** All 17 daemon references to `Plane` are removed. Gap 12 is closed:
there is no load-bearing use, which reverses the reading recorded in
`GAPS-wave-two.md`.

**What the four deciding sites were for.** `turnboundary.go:146`,
`turnlifecycle.go:192` and `turnclaims.go:201` all enforce one rule — only the
STREAM plane may move turn state — because the file plane would later re-read
the same boundary from the transcript and the turn would be counted twice.
`events.go:426` is the inverse self-consistency check on a file-plane
diagnostic. The other 13 references are `logf` format arguments.

**Why they are now vestigial.** The twin no longer exists. Verified: the
sidecar and the store produce NO `TurnBegan`/`TurnEnded` on any path — only the
shim does (`shim/src/proto/convert.ts:27,58`). There is no second producer left
to guard against, which is the same fact that justified removing `dedup_key`.
`events.go:426` is dead twice over, because `FilePlaneDiagnostic` is itself
retired.

**Why this is not "move the dedup down a layer".** There is no dedup left to
site anywhere. The planes now divide cleanly — the shim owns lifecycle and
cannot write conversation content at all, the file plane owns content — so the
duplicate the daemon was refusing cannot be produced.

## Re-spelling, defined — and the extraction that replaces it

**The term.** RE-SPELLING is declaring, in namespace B, a type whose semantic
content is already declared in namespace A, instead of importing A's.

Four forms, one defect:

- **whole-message** — B re-declares ALL of A's constituents.
- **partial** — B re-declares SOME of them.
- **vocabulary** — B re-declares A's oneof arms.
- **identity** — B holds A's typed identity as a bare scalar.

The test: *could a change to A's meaning leave B compiling and wrong?* If yes,
it is a re-spelling.

**Two remedies, chosen only by how much the consumer needs.**

- Needs the WHOLE semantic content → import A's message. No split. **This is
  the common case** — most re-spellings here are not partial-need at all. The
  consumer needed the same semantics and wrote them out again.
- Needs a STRICT SEMANTIC SUBSET → the PROVIDER extracts a narrower message and
  the consumer imports that. The consumer never writes its own copy.

**The split happens at the PROVIDER, and the provider is not the namespace
holder.** This resembles the Interface Segregation Principle, but ISP assumes
the provider DECLARES the interface it serves, and that assumption fails here.
Our namespaces are drawn by SURFACE — who talks to whom — not by who populates.
`frontend.v1` is named for its CONSUMER; the webapp only reads it, and the
daemon populates it. So "split at the provider" does NOT mean "split where the
message is declared". It means split where the fact ORIGINATES, which for every
conversation fact is `conversation.v1`.

A consumer-named namespace is never the place to declare a new spelling of an
upstream fact, because nothing on the consuming side produces one.

**The guard against over-segregation**, which is ISP's own failure mode: split
on a strict SEMANTIC subset, never on a RENDERING subset. A component that
displays three of five fields still MEANS all five — that is a renderer using
part of what it was given, not a case for extraction.

**Why the distinction is self-enforcing rather than a matter of taste.** A
duplicated resolved VALUE is re-resolved on the next publish, so a stale copy
self-corrects and authority stays with the resolver. A duplicated TYPE has no
such property: it is a second definition maintained by hand, and it diverges
silently the moment one side gains an arm. Both divergences in this tree — an
outcome vocabulary at three different sizes, and `workflow` against `journal` —
happened with nothing failing to compile.

## Depth is not synthesis

Stated explicitly, because the corrected carve-out is otherwise misread as "a
component's data must sit at the top level of its own message" — and acting on
that misreading flattens an embedded import straight back into a re-spelling.

**What matters is not how deep a field is, but HOW MANY MESSAGES a component
needs.** One message, at any depth, is fine. Two or more SIBLING messages is
synthesis, and synthesis is what the rule forbids.

In practice the question barely arises, because the component tree and the
message tree have the same shape. Each component is handed one field and passes
one of ITS OWN fields to each child: `encompassing(main.encomp)` calls
`smaller(encomp.smaller_field)`. Every call site is depth-1 relative to what
that component received. Nobody writes `smaller(main.encomp.smaller_field)`.

## `state.v1`'s token vocabulary is NOT a re-spelling

**Decided.** `state.v1.TokenUsageTotals`, `state.v1.TokenCacheCreation`,
`state.v1.VendorTokenUsage` and `state.v1.TokenOutputDetails` stay as they are,
alongside `conversation.v1.TokenUsage`, `conversation.v1.TokenCacheHits` and
`conversation.v1.TokenCacheMisses`. `RESPELLINGS.md` §5 closes as no-change.

**Why.** They are two different things, not one thing written twice. `state.v1`
is the DURABLE layer and sits deliberately beneath the UI mapping, carrying its
own compatibility constraints — `state.v1.TokenUtilization` and
`state.v1.TurnAccounting` moved there field-for-field identical precisely so
replay stays byte-safe. A vendor-FAITHFUL durable spelling and a
vendor-AGNOSTIC conversation spelling answer different questions: one preserves
what the vendor said for forensics and replay, the other models what the system
means. Collapsing them would make the durable layer inherit the conversation
model's deliberate lossiness.

## What is native to `frontend.v1.DetachedWork`, and the test that decides it

**The test, and it is the general one.** A field is native to `frontend.v1` if
and only if the DAEMON must SYNTHESIZE it — that is, it is not part of the
`conversation.v1.MessageEntry` the shim and sidecar populate, and exists only
because the daemon derived it from its own bookkeeping. Everything else is a
`conversation.v1` fact and is IMPORTED.

This test is not specific to detached work. It decides every `frontend.v1`
membership question: produced upstream → import; synthesized by the daemon →
declare it here.

**Native — the daemon synthesizes these:**

- `frontend.v1.DetachedWorkFold` — the fold's accounting and cap. The daemon's
  own bookkeeping about how much it chose to show; no producer states it.
- `frontend.v1.DetachedWorkLiveness`, with `frontend.v1.DetachedWorkLive` and
  `frontend.v1.DetachedWorkSettled` — resolved from whether an end record has
  arrived. A client deriving it would be the second authority figma→idl exists
  to remove.

**Imported — these are `conversation.v1` facts:**

- The kind arms. `conversation.v1.DetachedWorkKind` already states them, and the
  producer is what knows the kind.
- The outcome arms, against `conversation.v1.DetachedWorkEnded`'s
  `DetachedSucceeded`/`DetachedFailed`/`DetachedCancelled`/`DetachedLost`.

**Two the test surfaces as NEW questions rather than settling:**

- `frontend.v1.DetachedWorkShellExit` — an exit code is OBSERVED by the
  producer, not synthesized by the daemon, so by this test it is a
  `conversation.v1` fact. It has no home there:
  `conversation.v1.DetachedWorkEnded` carries an outcome oneof and no exit
  status. A new gap, found by applying the rule rather than by an agent.
- `frontend.v1.DetachedWorkJournalRow` — structured label/detail/status rows
  against `conversation.v1.DetachedWorkProgressed.output`, which is a plain
  string. If the daemon is PARSING that string back into rows, it is deriving,
  and the structure should reach it intact instead — which is what
  `agentshim.v1.InternalEntry.source_record` now makes possible.
