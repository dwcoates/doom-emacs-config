# DESIGN: the agent-repl protobuf surfaces

## The six surfaces

Every message belongs to exactly one. **The package boundary IS the surface
boundary** — nothing straddles, so which surface a message is on is a fact a
compiler can check rather than a convention a reviewer has to hold. All six are
ROOT namespaces with a `v1` suffix and no umbrella prefix, one directory each
under `proto/src/`.

| package | directory | holds | importable by |
|---|---|---|---|
| **conversation.v1** | `conversation/v1/` | the message model — `MessageEntry`, `MessagePayload`, the content model, `TokenUsage`. Imports nothing; the leaf every other surface shares | everyone |
| **frontend.v1** | `frontend/v1/` | the UI components the webapp renders — `feed`, `topbar`, `sidebar`, `footer`, and the commands those components send | daemon, webapp |
| **agentrepl.v1** | `agentrepl/v1/` | the agent-repl API surface — `service AgentRepl`, the frame envelope a subscriber receives, the connect snapshot, and the acks | daemon, webapp |
| **shim.v1** | `shim/v1/` | what traverses the daemon↔shim boundary and only that boundary: handshakes, commands, receipts, health, replay and page requests, `BookkeepingEntry`, and the `ExternalEntry`/`EntryDelivery`/`MessagePage` read half | shim, daemon |
| **store.v1** | `store/v1/` | the producer-side internal half: which plane observed a record, the store's write identity, anything a producer could not convert | shim, sidecar, store ONLY |
| **state.v1** | `state/v1/` | daemon-internal only, including the frozen schema the daemon marshals into its own SQLite store | daemon |

`BookkeepingEntry` is `shim.v1`, not `conversation.v1`. Bookkeeping is produced
by the shim and consumed by the daemon, and it STOPS there — a client sees it
only after the daemon has resolved it into a view.

`ExternalEntry` is `shim.v1` for the same reason: it is the WIRE envelope, and
it carries either a `conversation.v1.MessageEntry` or a
`shim.v1.BookkeepingEntry`. It does not collapse now that bookkeeping sits
beside it, because the store persists bookkeeping too, so a stored record must
still be able to hold one.

## The package names encode OWNERSHIP, not routing

**Decided, and this supersedes the routing test the sections below were written
under.** The old scheme filed a message by who PRODUCED it and where it was
ROUTED. The new one files it by what OWNS it. Routing was a good rule for
deciding a hard case and a bad name for a package: it produced `protocol`, a
name that describes every one of these six, and `agentshim`, a name that
describes the process rather than the surface.

**There are two kinds of package, and one exception.**

BOUNDARY packages own envelopes and verbs — `agentrepl`, `shim`, `store`. Each
is named for the boundary it describes, and what it holds is the vocabulary for
crossing that boundary.

MODEL packages own the payloads that ride those envelopes — `frontend`,
`conversation`. Neither describes a wire; both describe a thing that travels on
one.

`state.v1` is neither, and that is why it is alone in carrying frozen
field-number constraints. It is not a boundary between two parties and not a
model anyone shares: it is the daemon's private persistence across its own
restarts. The other party is the daemon a week from now, which cannot be asked
to upgrade in lockstep, so a field number there is permanent in a way no wire
field is.

**The test for `frontend` versus `agentrepl` is: is it DRAWN, or is it CALLED?**
What the webapp renders is `frontend`. What it calls is `agentrepl`. That
question has one answer per message and does not require knowing who produced
it — a `TopbarView` is drawn whether the daemon synthesized it or forwarded it,
and `Subscribe` is called whether Emacs or the webapp is calling.

**`agentshim` → `store`**, because all four of its files were store surface and
each said so in its own header. `cursor.proto` is store↔sidecar traffic;
`write.proto` is how a producer hands a record to the store; `entry.proto` is
the stored record; `unsupported.proto` states outright that the daemon must
never import it. The name `agentshim` never described the content — it described
the repository the code happened to live in, which is how it also ended up
looking like the namespace everything else should hang under.

**`protocol` → `shim`**, for the daemon↔shim boundary it actually holds. The old
name was defensible on its own and indefensible in a set of six, all of which
are protocols.

**`gen/` and `go_package` deliberately did NOT move.** The Go import path is
still `agentrepl/proto/<pkg>/v1`, so roughly 300 consumer files across the
daemon, shim, sidecar, store and webapp need no edits at all. Only the SOURCE
location moved, into `proto/src/<pkg>/v1/`. That is the whole reason a rename of
this size was affordable: the thing every consumer names is the generated import
path, and it was left alone.

**Two placements are known to be imperfect, and are open questions rather than
settled.**

`shim.v1` holds `bookkeeping.proto`, which is session EVIDENCE — a model, not a
boundary. It is there because the shim produces it and the daemon consumes it,
which is the old routing test still doing the deciding. Under an ownership test
it has a weaker claim to a boundary package, and no better home has been argued
for yet.

`shim.v1` also holds `ConnectionHeartbeat`, `HealthCheck` and `HealthStatus`,
which ride EVERY UDS hop — including store↔sidecar, where no shim is involved at
all. A message that crosses a boundary the package is not named for is exactly
the straddle this scheme exists to make impossible, and it is stated here rather
than left for someone to discover.

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

## Import the ENCOMPASSING message, not its constituents

**Decided.** When a consumer needs a fact another namespace owns, it embeds the
message that CONTAINS that fact, not the constituent the fact sits in. Dropping
to a constituent is reserved for a genuine strict-subset need, and that need
triggers provider-side extraction rather than a partial import.

**Why the distinction bites.** `conversation.v1.DetachedWorkKind` is referenced
from exactly one place, `conversation.v1.DetachedWorkStarted.kind`. Reaching for
the kind alone reads as reuse, but `frontend.v1.DetachedWork` re-spells all
THREE of that message's fields — `origin_tool_call_id`, `label` and `kind` — so
importing only the kind would fix a third of the duplication and leave the rest
looking intentional. Only `workspace` is genuinely native there.

So `frontend.v1.DetachedWork` embeds `conversation.v1.DetachedWorkStarted`
whole, and `frontend.v1.DetachedWorkSettled` embeds
`conversation.v1.DetachedWorkEnded` whole rather than re-declaring
`frontend.v1.DetachedWorkOutcomeDone`/`Error`/`Killed`.

**This corrects an inconsistency in the earlier record**, which specified a
whole-message embed for the outcome and an arm-level import for the kind. There
was no principle behind the difference.

**The schema had already written down the failure mode it was about to repeat.**
`conversation.v1.DetachedWorkKind`'s comment reads: "SIX ARMS, matching what the
daemon already resolves (frontend.v1's DetachedWork). An earlier draft had four
and silently dropped `merge` and `skill`." The author saw the duplicate and took
MATCHING IT BY HAND as the remedy — a remedy that had already failed once, at
four arms, before anyone noticed. That is the whole argument for the rule in one
comment.

## Pagination is scoped, and any container can be paged

**Decided.** A page is a page OF A CONTAINER. The feed is the container with no
parent, so a nested subagent pages with the same message, the same cursor and
the same renderer as the top-level conversation.

`ConversationPage` gains a `PageScope` — a oneof of `PageScopeFeed` and
`PageScopeInside { container_message_id }` — and the response states the
ancestors it resolved.

**What this replaces, and why the replacement was necessary.** The tree
conflated "a message" with "a feed row", and everything below followed from it:

```
  "a message == a feed row"
        -> contained records cannot be records (they would flood the feed)
        -> so they become repeated frontend.v1.AgentEmission
        -> so they cannot be paged (a repeated field ships whole or not at all)
        -> so they must be capped (frontend.v1.DetachedWorkFold)
        -> so capped content has NO FETCH PATH and is unreachable
```

Verified: `frontend.v1.DetachedWorkFold` is a TAIL cap (`dropped_before`,
`tail_cap`), and there is no command anywhere to request the dropped entries —
zero `DetachedWork` references in `commands.proto` or `conversation-page.proto`.
A subagent that ran long shows its last N emissions and a notice naming how many
thousand the user may not look at. They are in the store and unreachable.

The conflation is contradicted by the schema itself: `conversation.v1.MessageParentInside`
exists precisely so a message need not be a feed row.

**Where the conflation actually lives**, which is further upstream than it first
appeared — `conversation.v1.MessageEntry.top_level_message_id`:

> EQUALS `message_id` when the message is itself a feed row; names the
> containing message when nested, so a subagent and everything inside it share
> one value and therefore ONE PAGE SLOT.

So it is a `conversation.v1` decision, not a `frontend.v1` one, and
`frontend.v1`'s `emissions` and `fold` are its downstream symptoms.

**Why the path is NOT encoded in the request.** An earlier sketch carried
`repeated string message_id_path_to_parent`. It is not needed, for two reasons
that are both stronger than a path:

- The response ECHOES its scope, alongside the `request_id` echo that already
  exists for exactly this ("a client with a load-more in flight and a cold open
  still settling has two pages coming").
- Every returned `conversation.v1.MessageEntry` carries its own `parent`, so the
  client VERIFIES rather than trusts: every record in the page must name the
  container that was asked for. A path proves what was ASKED; the parent on each
  record proves what was RECEIVED.

A path would also be a second statement of the parent chain, and the schema
already took that bet once — `top_level_message_id` is denormalized with an
explicit "a write that disagrees is corruption, not a variant". One such
denormalization is worth its cost; two is a second place for the same
disagreement.

**What the path was protecting, and where it moved.** A cold open deep into a
nested container holds no id map and needs the ANCESTORS to render surrounding
context. That is the daemon ANSWERING with the chain, not the client asserting
it: `ConversationPage.ancestor_message_ids`, outermost first, empty for the
feed. It cannot disagree with the parent chain because the resolver derived it
from that chain in the same pass.

**Follow-up this requires and does not itself deliver.** The store has NO PARENT
INDEX — `parent` lives inside the opaque `payload` BLOB, and
`entry_message_owner` indexes `top_level_message_id`, which is the same value
for a subagent and a subagent inside it. Paging an arbitrary nested container is
a scan until `entry` carries an indexed parent column.

## The output spool's gap detection is dropped

**Decided, and it is a deliberate removal of error-handling coverage, signed
off.** `frontend.v1.DetachedWorkOutputSpool.through_offset` and
`DetachedWorkOutputAppend.from_offset` go.

**What they did.** `from_offset` had to equal the spool's `through_offset`, and
a mismatch was the only thing that could tell a LOST chunk from a QUIET one; the
webapp's sole use was to trigger a resync.

**What is lost.** A dropped chunk renders a shell transcript with a silent
hole — it looks complete and is not. The durable record is intact, so a reload
repairs it; the user simply has no signal that they should reload.

**Why accepted.** It is display fidelity on a local hop, not data loss. And the
coverage was already lopsided: `frontend.v1.ConversationDelta`, `TypingDelta`
and `DetachedWorkDelta` have NO gap detection at all, so one stream of four was
protected while implying the others were safe. `frontend.v1.FrontendFrame`
carries no sequence number; the ten `fence` fields are staleness tokens, which
answer "is this from an old generation", not "did something go missing".

**The cheaper way back, if it is ever wanted.** One `seq` on
`frontend.v1.FrontendFrame` covers all four delta streams for less than what
this deletes. Not built now.

**What dropping it unlocks.** With the cursor gone the spool is just bytes, so
`conversation.v1.DetachedWorkProgressed.output` becomes a genuine counterpart
and both output TODOs close.

## The store indexes `parent`, and the database is NUKED rather than migrated

**Decided.** `entry` gains a `parent_message_id` column, extracted at ingest,
and an index `entry(session_id, parent_message_id, seq)` mirroring
`entry_message_owner`. Without it `frontend.v1.PageScopeInside` is a scan:
`parent` lives inside the opaque `payload` BLOB, and the owning feed row cannot
substitute because a subagent and a subagent inside IT share one value.

**Superseded in part — see "Lineage is one optional field, and the root moves
into the store" below.** That section was written when a record carried BOTH its
immediate parent and its root on the wire, so this column was merely the second
of two extractions. The root has since left the wire entirely. The column and
the index decided here are unchanged and still wanted; what changed is their
company. `parent_message_id` is now the ONLY lineage value extracted from the
record, and the owner column feeding `entry_message_owner` is DERIVED by walking
that parent chain at ingest rather than read off the record. The nuke-rather-
than-migrate posture below covers that change too.

**NO MIGRATION AND NO BACKFILL.** The store database is NUKED as needed and is
to be regarded as EMPTY. Nothing is written to carry old rows forward.

This holds for EVERY store schema change in this stage of the project, not just
this one — but it is a DEVELOPMENT-STAGE posture rather than a permanent
property of the store. Nobody currently cares what is in that database, so
migrating it buys complexity for nothing. The day its contents matter to
somebody, this is the decision that has to be revisited first.

**Every implementation agent must be told this explicitly.** A half-migrated or
stale database filled with rows written under a retired schema produces
confusing, plausible-looking wrong data, and an agent that assumes the database
must be preserved will invent migration and compatibility work that is not
wanted and will not be reviewed. The absence of a migration is a DECISION, not
an oversight to be helpfully corrected.

**Sequencing.** The column and index are part of the implementation fan-out, not
of this schema wave. They are inert until `claude-repld` can serve a scoped page.

## `frontend.v1.TypingDelta` is deleted; live typing is a `conversation.v1` record

**Decided.** `frontend.v1.TypingDelta` goes, along with its arm on
`frontend.v1.FrontendFrame`. The live typing preview is
`conversation.v1.ContentArriving`, which the shim already produces and hands
straight to the daemon without touching the store.

**Why it costs nothing today.** `TypingDelta` currently carries NO CONTENT. Its
`delta` field is reserved with "NOTHING REPLACES IT", so the message is an
envelope around nothing. `claude-repld.internal.frontend.translate.go:119` still
sets `TypingDelta.Delta` — a field that does not exist — which is the daemon
being written against a pre-reshape schema rather than a working path.

**The other half is equally dead.** `conversation.v1.ContentArriving` has ZERO
non-test references in `daemon/`: no producer, no consumer. So live typing is
broken from BOTH ends right now, and deleting the frontend spelling removes an
empty box rather than a working feature.

**What restores it, and what that depends on.** `ContentArriving` is a
`conversation.v1.MessageEntry` payload arm, so it reaches a frontend the moment
the delivery channels carry `MessageEntry` — the same open question as
`frontend.v1.Message`'s payload shape. Until that is settled there is no path
for it, which is a statement of the sequencing rather than a reason to keep an
empty message around.

**Sequencing.** Queued behind the reservations strip, which is rewriting every
proto file including `feed.proto`.

## `frontend.v1` stops flattening `conversation.v1.AgentSaid`; it carries it whole and stamps alongside

**Decided.** `frontend.v1.AgentEmission` currently EXPLODES one assistant
response into several emissions — a response, a thinking block, a tool call —
so that each fragment can carry its own daemon-resolved stamp. That flattening
is the ROOT CAUSE of the agent-output re-spellings: every fragment needs a
frontend type to be a fragment OF, and each of those types is a second spelling
of something `conversation.v1` already says.

It stops. `frontend.v1.AgentResponse` embeds `conversation.v1.AgentSaid`
VERBATIM, nothing stripped, and the daemon's resolved facts ride ALONGSIDE it
keyed by a value the content already carries.

```proto
message AgentResponse {
  conversation.v1.AgentSaid said = 1;
  ResponseUsageStamp usage_stamp = 2;
  repeated ToolCallVerdict verdicts = 3;
}

message ToolCallVerdict {
  string tool_use_id = 1;
  string spawned_message_id = 2;
}
```

**Deleted outright:** `frontend.v1.AgentThinking`, `frontend.v1.AgentToolCall`,
`frontend.v1.AgentToolResult`, `frontend.v1.SkillBodyItem`.

**The flattening was already losing data, in three places.**

- `frontend.v1.AgentToolResult` wrapped `conversation.v1.ToolResultContent`
  instead of the encompassing `conversation.v1.ToolReturned`, so
  `ToolReturned.is_error` had NO counterpart on the frontend surface. A tool
  that failed rendered identically to one that succeeded.
- `frontend.v1.AgentResponse` wrapped `conversation.v1.AgentContent` instead of
  `conversation.v1.AgentSaid`, dropping `stop_reason` entirely and relocating
  `model` into `ResponseUsageStamp`.
- `frontend.v1.AgentThinking` carried `api_message_id` and `block_index` — pure
  RE-DERIVATIONS of position, needed only to rebuild what the stripping
  destroyed. A thinking block's position IS its index in
  `AgentSaid.content`; stripping it out and then re-stating where it came from
  is the schema paying twice for a fact it started with.

**It also makes the live preview coherent.**
`conversation.v1.ContentArriving` keys on `block_index` within the settled
message's content. Under the flattening, the settled content a preview
reconciles against had been mutated out from under it. Carrying `AgentSaid`
whole means preview and settled block index the SAME array.

**`SkillBodyItem` dies on its own evidence.** It correlates by `tool_use_id`,
which `conversation.v1.SkillBodyResolved`'s own comment identifies as the
SUPERSEDED mechanism — the record now carries the skill message's `message_id`
directly, "which is what makes the old daemon-side correlation unnecessary".

**What this costs, stated plainly.** The webapp renderer must walk
`AgentSaid.content` and draw each block — prose, reasoning collapsed, tool
calls — rather than receiving them pre-separated. That is a REAL behavior
change on the client, and it is the price of the frontend surface no longer
holding a second, lossier copy of the agent's own words. Walking a content
array to render it is rendering, not deriving: nothing about the message's
meaning is being reconstructed, only drawn.

## `conversation.v1.MessagePayload` is extracted so the payload set can be imported

**Decided.** `conversation.v1.MessageEntry.payload` moves into its own message,
`conversation.v1.MessagePayload`, which `MessageEntry` then embeds.

**Because a `oneof` is not a type.** `frontend.v1` cannot import
`MessageEntry.payload` — proto3 offers no way to name it. The ONLY two options
available to the author were to import `MessageEntry` whole (inheriting
identity and ordering stamps a frontend wrapper wants to set itself) or to
write the arms out again. They wrote the arms out again, and that is the
mechanical origin of `frontend.v1.AgentEmission`.

Extracting the oneof gives the third option that should always have existed:
ONE canonical payload set, embedded by `MessageEntry` for durable records and
importable by any surface that needs the same vocabulary with different stamps.

This is the EXTRACT-rather-than-re-spell heuristic applied literally, and it is
pure enablement — `MessageEntry` keeps exactly its current semantics, and the
extraction commits to nothing about what the delivery channels carry.

## Three removals from `frontend.v1`: two dead messages and the durability class

**Decided.** `frontend.v1.RosterNotice`, `frontend.v1.DetachedWorkOutputSpool`,
and `frontend.v1.Message.durability` — with `MessageDurable` and
`MessageEphemeral` — are deleted. No reservations; the payload arms renumber
contiguously, as everywhere in this tree.

**`RosterNotice` was dead outright.** No field in any proto carried it, and no
source in `daemon/` or `webapp/` named it. The only matches were stale compiled
binaries. The rail's notice line was designed and never wired; nothing regresses
because nothing ever consumed one.

**`DetachedWorkOutputSpool` was orphaned and already inert.** No field carried
it either, so it had no path to any client. Removing it changes NO behavior:
there was no embedding to break. Its live sibling
`frontend.v1.DetachedWorkOutputAppend` stays — it is arms 4 and 5 of
`DetachedWorkUpdate` and carries real traffic.

**`conversation.v1.DetachedWorkProgressed.output` is the spool's counterpart.**
That was already the consequence recorded under "The output spool's gap
detection is dropped": with the cursor gone the spool was just bytes, and bytes
the producer already writes. This deletion is that decision finishing.

The spool's comment held one thing the append's did not — the enumeration of the
four delta streams a per-frame `seq` would cover (`ConversationDelta`,
`TypingDelta`, `DetachedWorkDelta`, and the output stream). That sentence moved
onto `DetachedWorkOutputAppend` rather than being lost.

### A frontend has no conception of durability

**The field was write-only.** `daemon/internal/frontend/durability.go` sets it.
Nothing reads it. The webapp's decoder assigns `frame.durability` and no renderer,
router, or state transition ever consults the result; Emacs never looks at all.
It computed a fact, put it on the wire, and no client used it for anything.

**Its stated justification describes something no client does.** The comment
argued that without the class, "no durable record exists for this message" is
indistinguishable from "the record was not found", so a page missing an
ephemeral card looks like a page that LOST a durable one. That check is real —
and it is not a client's to make. The daemon owns the store and owns pagination:
it is the only party that knows what a page was supposed to contain, and the
only one positioned to notice a durable message the store cannot produce. A
client holds neither side of the comparison, which is why none was ever written.

**The daemon keeps knowing.** Durability does not stop existing; it stops being
a wire fact. The classification, and the loud failure when a durable message is
missing from the store, remain daemon-internal — where the evidence already is.

**What this costs.** `daemon/internal/frontend/durability.go` exists solely to
compute the deleted field, and the webapp decoder's two loud checks against it
(both arms set; an ephemeral message naming a parent) lose the input they
validate. Those are consumer-side consequences of a contract change, resolved in
the implementation wave and not in the contract commit.

### The ephemeral lineage constraints survive as daemon-side construction invariants

They were stated on `MessageEphemeral`, and they are not wire facts — they are
rules about how the daemon may build a message. Carried over unchanged in
substance:

- An ephemeral message is ALWAYS a feed row: empty `parent_message_id`, and
  `top_level_message_id` equal to its own id.
- It may NOT be a parent. A durable child naming an ephemeral root would be
  unreachable by any store query, since a store record can only name ids that
  exist in the store.
- It may NOT name a durable parent. Otherwise an ephemeral card attaches itself
  into a paged conversation it will simply vanish from, leaving a hole where a
  reader has every reason to expect a message.

**They are refused at construction**, so a violating message cannot be built and
then noticed; a check applied afterwards is a check something can skip. That was
true when the class was on the wire and it is true now — the enforcement point
never moved, only the field it was described next to.

**Membership is unchanged and is decided by whether Claude ever saw the thing**,
never by who minted the id. A slash command the daemon answers alone never
reaches the CLI, so the CLI writes nothing; a `system/local_command` record is
ruled out of the durable set; a failure card the daemon synthesized describes a
session that failed to start, so there is no transcript for it to live in.
Conversely a daemon-MINTED id over a real `TaskStarted` is durable, because the
record exists.

**`ConversationSource` and `Message.source` are untouched.** Provenance is a
different question — who drove a message, not whether a record exists for it —
and it stays on the wire.

## `ContentArriving` drops `arguments_json`; tool arguments arrive typed and whole

**Decided.** The `fragment` oneof loses its third arm. `ContentArriving` streams
prose — `text` and `thinking` — and nothing else. Tool arguments reach a client
exactly once, complete, as `conversation.v1.ToolCallBlock.arguments`.

**The settled form was already typed, and the preview arm contradicted it.**
`ToolCallBlock.arguments` is a `google.protobuf.Struct` for a stated reason: a
client renders fields, and re-parsing a string to find them is a second parser
that can disagree with the first. `arguments_json` was that string, on the same
content, from the same producer. One surface cannot both forbid a string of
arguments and ship one.

**It was a vendor leak.** `arguments_json` carried the Claude SDK's
`input_json_delta` framing intact into `conversation.v1` — the vendor's chunking,
the vendor's partial-JSON convention, renamed but not converted. That is what
`11d22f86c` deleted `data.v1` to stop: vendor knowledge reaching the daemon and
the webapp, which the architecture forbids. A producer that emits partial vendor
JSON has not converted at the boundary; it has moved the boundary.

**The second parser was not hypothetical — it shipped.**
`webapp/src/streaming.ts:352` accumulates `item.inputJson += delta.delta`, and
`webapp/src/catalogue.ts:148` re-stringifies the settled input to match it. That
is precisely the defect `ToolCallBlock`'s own comment names, built twice in one
client.

**And a renderer could never draw from a fragment anyway.** `{"file_pa` is not a
field, a path, or a value. Nothing can be shown until the last chunk lands, so
streaming the chunks bought an accumulator, an ownership rule, and a validation
branch, in exchange for no picture.

**What this costs, stated plainly.** The live "watch the agent compose a tool
call" effect goes away. A tool card now appears with its arguments already
filled rather than filling in. That is accepted, not overlooked: the effect was
the only thing the arm delivered, and it was delivered by a consumer
reconstructing JSON the producer had already parsed once.

**The alternative that was rejected.** The shim could buffer fragments and emit
progressively-complete `Struct` snapshots — typed the whole way, animating as
fields settle. It was rejected because JSON does not arrive field-by-field: a
snapshot can only be emitted when the buffer happens to parse, so the emissions
are lumpy and arrive in bursts unrelated to how a reader scans a card. That is
machinery, a partial-parse loop, and a re-send policy, spent on an animation
worth little.

## Lineage is one optional field, and the root moves into the store

**Decided.** `conversation.v1.MessageEntry` states containment with a single
`optional string parent_message_id`. Unset means the message sits directly in
the feed; empty is invalid, not a third answer. `top_level_message_id` is gone
from the wire, and so are `MessageParent`, `MessageParentRoot` and
`MessageParentInside` — the oneof that used to spell "root or inside" is now the
presence or absence of one field.

**`frontend.v1.MessageLineage` is deleted.** Reduced to a single optional field
it was a pure wrapper around what `conversation.v1` spells as a bare field, which
is the same shape `AgentToolResult`, `SkillBodyItem` and
`DetachedWorkSkillBodyResolved` were deleted for in this pass. The field moves
onto `frontend.v1.Message` as `optional string parent_message_id = 5`, taking the
tag `lineage` held. The argument that came with it survives verbatim in
substance: containment sits in the PACKAGING half because it is a fact about the
message regardless of what the message is, and non-message records —
`shim.v1.Envelope` payloads — must never gain the field. Under the new spelling
that prohibition is SHARPER, not weaker: unset is precisely the spelling for
"sits directly in the feed", so a parent pointer on an Envelope would make every
turn boundary and heartbeat a top-level feed row.

**The denormalization moved into the store.** The store still resolves page
ownership itself and a page is still bounded by MESSAGES, not records — a page
bounded by record count is a fragment, and the reader asking again until it holds
ten messages is the unbounded scan relocated. What changed is where the owning
feed row comes from. The record states only its immediate parent; the store walks
that chain ONCE at ingest and keeps the resolved feed row as a column of its own,
indexed alongside seq. The page query still selects distinct values of that
column below the anchor. One indexed pass, unchanged. The denormalization was
never wrong — it was in the wrong place, on a wire where every consumer had to
carry it, rather than in the store where the index already lives.

**What this costs, stated plainly.** Two things.

The store now walks the parent chain once per record at write. That is work the
producer used to do, moved; it is bounded by nesting depth and paid on ingest
rather than per page, which is the trade that keeps reads flat. A store that
receives a child before its parent must handle that ordering itself — the wire no
longer hands it a root it can trust without the parent being present.

And a record no longer carries its own root, so nothing on the wire can be
cross-checked against it. The old `MessageLineage` comment called a write whose
root disagreed with its parent chain "CORRUPTION, not a variant", and the daemon
audits for exactly that. Under the new model there is no second copy to disagree
with, which removes the drift risk and removes the detection along with it. The
class of bug those checks caught cannot occur; the class where the single parent
pointer itself is simply wrong is now undetectable at the boundary, because there
is nothing to compare it to. That is accepted: one copy that can be wrong is
strictly better than two copies that can disagree, but it is not free, and it is
not the same as being checked.

**Stranded, not compensated for.** `daemon/internal/frontend/lineage.go` audits
the two-field invariant and three of its four defect classes no longer exist.
`daemon/internal/frontend/detachedwork.go:240` (`FeedRowLineage`) constructs the
deleted message. Neither is touched here — this wave is proto and docs — and
neither is deleted quietly during implementation without saying so. See the
report accompanying this change for the full list.

## `agentrepl.v1` is a Connect service, and the command oneof is gone

**Decided.** `service AgentRepl` in `agentrepl/v1/service.proto` replaces the
`FrontendCommand` message: 33 unary methods, one per arm the oneof used to
carry, plus one server-streaming `Subscribe`. The oneof is DELETED, not kept
alongside — the service is the method table, and two spellings of one thing is
the duplication this pass has spent itself removing.

**The verbs get their own file.** `service.proto` holds the service and
`SubscribeRequest`; `frame.proto` keeps `FrontendFrame`, `StateSnapshot` and
the acks. Same package, so nothing about the surface boundary changed — the
split is for readers, who look for verbs in one place and payloads in another,
and it keeps `frame.proto` from becoming the file where everything is.

**The oneof was already a method table, badly.** Thirty-five arms dispatched by
a type switch in `daemon/internal/frontend/commands.go`, with a `default:` at
`:305` returning "the command oneof was empty or unrecognized". That default is
what an unimplemented method costs today: a runtime refusal a client discovers
in production. Under a service an unimplemented method does not compile. The
same trade appears on the response side — one `CommandAck` answered for 33
different outcomes, and four of its nine fields were commented "Present only
for X", which is a type check written as prose.

**Connect, not gRPC, and the reason is the clients.** The webapp runs inside an
Emacs xwidget WebKit view, which cannot speak gRPC — no trailers, no HTTP/2
frame access from JS. Emacs is the OTHER first-class client, and elisp has no
gRPC either. Connect is HTTP/1.1-compatible and its unary wire format is an
ordinary POST with a protojson body, which is exactly what
`lisp/frontend-uds.el` already builds by hand. The repo also already generates
TS with `@bufbuild/protoc-gen-es`, and Connect-ES is the same toolchain family,
so the implementation wave adds a plugin rather than a second codegen story.

### The envelope fields move onto the commands

**Decided: option (a).** `request_id` on every request; `workspace` on the
workspace-addressed requests only. Not per-method `*Request` wrappers.

**The wrappers were rejected because they double the vocabulary.** Thirty-three
near-identical two-field messages, each meaningful only when paired with the
command inside it, is the `FrontendCommand`/`CommandAck` duplication rebuilt at
a smaller scale. Flattened, the command message IS the request: `MergeWorkspaceCmd`
handed to a log line or a durable inbox says everything about itself, and there
is exactly one name for the thing a caller sends.

**`request_id` is NOT made redundant by the call/response pairing, and deleting
it would have broken three separate things.** The ack answers the call, but a
command's EFFECT arrives later on the push stream — `ConversationHistoryPage`,
`DaemonHealthView` and `SessionHealthView` all carry the id back so a client can
match an effect to the request that caused it. It is also the DURABLE TURN KEY:
`daemon/internal/statedb/promptreceipt.go:293` refuses a turn claim with no
request id, and `sessioncontroller/promptdispatch.go:513` refuses a prompt with
none, because the id becomes the shim's `turn_id`. And the alternative —
daemon-minted ids returned on the ack — loses a race the client-minted scheme
does not have: on Connect the ack and the stream are different connections, so a
push can beat its own ack, and a client that did not mint the id has nothing to
file the early push under.

**`workspace` is where the flattening actually pays.** A shared envelope handed
one to every command whether or not it meant anything, and the schema recorded
the damage in prose: `PauseMergeQueueCmd`'s comment said "the command's
workspace is ignored". A field the receiver ignores is a field a caller can be
wrong about. Flattened, workspace-addressing is a fact of the message —
`ShutdownCmd`, `ScheduleShutdownCmd`, `CancelScheduledShutdownCmd`,
`PauseMergeQueueCmd`, `ResumeMergeQueueCmd`, `EvictMergeCmd`,
`CreateWorkspaceCmd`, `WorkspaceMaterializedCmd`, `HostActionCompletedCmd`,
`DeleteSessionCmd`, `DaemonHealthCmd` and `PublishWorkspaceRosterCmd` simply
have nowhere to put one. `CreateSessionCmd` keeps `cwd` and gains no second
`workspace`, because the daemon's workspace key IS the session's absolute cwd
and two spellings of one path is two things a caller can disagree with itself
about.

**What this costs, stated plainly.** Three things.

There is no compiler check that a NEW command remembers `request_id`. A wrapper
would have enforced it structurally; thirty-three hand-written fields enforce it
by convention. That is the price of not doubling the vocabulary, and it is paid
knowingly.

The commands that were EMPTY are no longer empty. `CloseWorkspaceCmd`,
`RestartSessionCmd`, `HibernateWorkspaceCmd`, `CancelDetachedAgentsCmd`,
`PauseMergeQueueCmd`, `ResumeMergeQueueCmd` and `DaemonHealthCmd` each had `{}`
as their whole body precisely because the envelope carried their meaning. Their
comments said so, and those comments are rewritten rather than deleted — the
argument (the session is the workspace; the command has nothing else to say)
survives, pointing at a field instead of at an envelope.

And the `frontend.v1` command messages now carry API-surface fields.
`SubmitPromptCmd` lives in the package of UI components the webapp renders, and
it gained `request_id`. The precedent was already there — `FirstPageCmd` and
`NextPageCmd` carried their own `workspace` before this change — and the routing
test still puts them in `frontend.v1`, since a frontend produces them and the
daemon consumes them. But the surface line is blurrier than it was, and that is
a real cost rather than a technicality.

### `client_id`, which the transport used to provide for free

**New, and load-bearing.** `SubscribeRequest.client_id`, repeated on `ResyncCmd`,
`FirstPageCmd`, `NextPageCmd`, `DaemonHealthCmd` and `SessionHealthCmd`.

Under the WebSocket, one socket carried the commands AND the pushes, so "which
reader is this" was the socket itself and no field had to say it —
`daemon/internal/frontend/commands.go:351` reads a connection-minted reader
identity out of the request context and REFUSES a paging call that arrives
without one. A service splits the two: a `NextPage` call is its own HTTP request
with no inherent relationship to any stream. Without a reader name, "the daemon
holds the reader's place" becomes unimplementable and that refusal at `:351`
becomes unreachable — which is exactly the kind of silent weakening this
document exists to refuse. The rule is one sentence: **a command whose effect
arrives on the push stream rather than in its own ack names the stream it should
arrive on.** Those five commands, and no others.

It is CLIENT-minted, like `request_id`, for the same race.

### The push channel stays ONE ordered stream

**Decided.** `rpc Subscribe(SubscribeRequest) returns (stream FrontendFrame)`,
and `StateSnapshot` is the first message ON that stream — not a unary call.

**One stream is the contract, not a convenience.** Everything a client holds is
built by applying a snapshot and then the deltas that follow it, and "follow" is
only meaningful within one ordered channel. Per-topic streams would let a
`WorkspaceState` revision be applied before the snapshot that supersedes it, a
`TypingCut` retire a preview whose opening frame had not landed, and a
`ConversationDelta` arrive against a workspace the client has never heard of.
Nothing on this protocol carries a global sequence a client could repair that
ordering with, and adding one would be rebuilding the stream inside every
client.

**The snapshot is not a separate call, and the batching is why it cannot be.**
`StateSnapshot` may split its `workspaces` across several frames delivered
back-to-back, with `workspace_total` stating what a complete view is; a client
holds a partial view until it has applied that many. That is a multi-frame,
ordered delivery with a completeness rule — it is a stream head, not a response
body. A unary `GetSnapshot` would also race the deltas it is meant to seed:
pushes begin the instant the subscription opens, and nothing relates the two
channels. Snapshot-then-deltas on one connection is the whole recovery story.

**The `command_ack` arm is DELETED from `FrontendFrame`.** An ack is now the
return value of the call that produced it, so it travels on that call and cannot
reach a client that did not make it. What still arrives on the stream is a
command's effect, which is a different thing.

**Host-versus-GUI stays in the LISTENER, and `SubscribeRequest` states no role.**
Several `StateSnapshot` fields and several `FrontendFrame` arms are host surface
that `frontend.Server` strips from GUI clients. The distinction is currently the
transport itself — the host reaches the daemon over its private UDS, a GUI client
over TCP — and Connect runs over both. A role field would convert an unreachable
socket into a string anyone may send, which is strictly weaker than what exists.

### Failures stay typed messages, never status codes

**Unchanged, and deliberately restated.** Every unary method returns a message on
refusal. `CommandAck.failure` is a `FailureKind` with ~71 arms, and both
frontends RENDER a refusal — the webapp as a failure card, Emacs as a classified
account. A Connect error code is a number with a string stapled to it, which is
what `CommandAck.error` was before the classifier replaced it. Converting
failures to status codes would undo that change and route every refusal back
through an `err.Error()` funnel.

`InterruptResponse` is the sharpest case: the confirmation CHALLENGE is neither
success nor failure — the command was understood and deliberately not performed
— and there is no status code that means that.

### Four per-method responses, and only four

`Interrupt`, `SetModel`, `CreateSession` and `CancelDetachedAgents` return
`InterruptResponse`, `SetModelResponse`, `CreateSessionResponse` and
`CancelDetachedAgentsResponse`; the other 29 return `CommandAck`.

Those four are exactly the fields `CommandAck` carried under a "Present only for
X" comment — `interrupt_confirm_required`, `selected_model`,
`observed_claude_session_id`, `detached_cancel`. Each was meaningless on 32
other commands, and the method signature now says what the comment used to. No
response was invented that carries nothing beyond the ack: `SubmitPrompt`
returns a plain `CommandAck`, as does every other method with nothing extra to
say.

**`DaemonHealth` and `SessionHealth` deliberately did NOT get per-method
responses**, even though `DaemonHealthView` and `SessionHealthView` look like
the obvious return types. Returning them would convert an asynchronous probe
into a blocking call, and both views' own comments state the invariant that the
ack is only a receipt and the VIEW is what Emacs waits on. Changing a probe's
concurrency is an implementation decision, not a contract cleanup, so the views
stay pushes and the commands carry `client_id`.

### What the implementation wave must change

The contract is now ahead of every implementation, on purpose. The toolchain is
untouched in this pass — no Connect plugins, no new dependencies, no `go.mod`
or `package.json` edits — so `make validate` still emits messages only and the
service block is parsed and dropped. Adding `protoc-gen-connect-go` and
`@connectrpc/protoc-gen-connect-es` to the Makefile is the wave's first step,
not this one's.

**Codegen.** `agentrepl/v1/service.proto` is added to the Makefile's `PROTOS`
list, and plain `protoc` emits its messages and silently drops the service
block — which is exactly the intended state until the plugins land.

**Daemon.** The type switch and `CommandHandler` interface in
`daemon/internal/frontend/commands.go:14-308` become a generated service
implementation, and its `default:` arm at `:305` disappears along with the
class of bug it caught. The two "this frame must be a `FrontendCommand`"
decoders — `daemon/internal/frontend/server.go:2336` and
`daemon/internal/server/server.go:2030` — lose their subject; Connect does the
routing and the decoding. The workspace canonicalization choke point
(`daemon/internal/frontend/workspacekey.go`, called once at
`server.go:1612`) currently rewrites one envelope field for every command; it
must become a per-method step, and `checkWorkspaceKey`
(`daemon/internal/server/frontendcmd.go:796`, eight call sites) must keep
refusing a display name where a key belongs. The lane keying at
`daemon/internal/frontend/lanes.go:114` reads the envelope's workspace to pick a
serialization domain and needs the same treatment. The reader identity at
`commands.go:351` moves from connection context to `client_id`.

**Webapp.** `webapp/src/frontend-command.ts:395-498` — the exhaustive
`encodeBody` switch — and `webapp/src/proto-names.ts:131-152`, the arm-name
table checked at compile time against the generated descriptor, are both
replaced by generated method stubs. `command-dispatch.ts`'s pending-request map
and `onAck` correlation stay, because the effect-on-the-stream correlation they
exist for stays.

**Emacs.** `lisp/frontend-uds.el` hand-builds the envelope
(`:1412`), holds a runtime allowlist of sendable arms (`:336-401`, enforced at
`:1402`), and hard-fails an ack with no `request_id` (`:2016`). Under Connect
the allowlist becomes a URL path per method, and the UDS newline framing at
`server.go:2215` becomes HTTP over the same socket. The ack-correlation hard
failure must survive: it is the only place either frontend treats a missing
correlation id as a protocol violation.

**Stranded, not compensated for.** Every check listed above still compiles
against a `FrontendCommand` that no longer exists in the schema, and none of it
is touched here — this wave is proto and docs. The full list, with `file:line`,
is in the report accompanying this change.

## Every endpoint owns its request and response, one file each

This SUPERSEDES "Four per-method responses, and only four" above. That section
recorded a service whose 33 unary methods shared one `CommandAck` and whose
signatures reached into `frontend.v1` for their requests. Both are gone.

### A request type is `agentrepl`, never `frontend` — the drawn/called test

`frontend.v1` holds what the webapp RENDERS. `agentrepl.v1` holds what it
CALLS. A `QueueForceCmd` was never drawn by anybody: it is the wire form of a
click, addressed to a method on this service, and it reached `frontend.v1` only
because the component that sends it is drawn there. That is proximity, not
ownership, and it is exactly the "what is it about" test the surface table
already rejects for every other message.

The evidence it was wrong: `service.proto` could not state its own API without
importing four `frontend.v1` files, so a daemon that wanted only the method
table pulled in the whole feed, footer, sidebar and topbar component model with
it. A request type that lives on the surface it is sent TO needs no qualifier,
and none of the 34 signatures carries one now.

Thirteen messages moved: `SubmitPromptCmd`, `InterruptCmd`,
`PermissionAnswerCmd`, `QueueForceCmd`, `QueueAcceptCmd`, `QueueCancelCmd`,
`DaemonHealthCmd`, `SessionHealthCmd`, `SetModelCmd`,
`PublishWorkspaceRosterCmd`, `CancelDetachedAgentsCmd`, `FirstPageCmd`,
`NextPageCmd`. Each was checked first for a reference from inside `frontend.v1`
— a view embedding a command would have forced a `frontend.v1` →
`agentrepl.v1` import and inverted the dependency — and none had one. Every
reference was prose.

The import direction that remains is the one that was always there:
`agentrepl.v1` reads `frontend.v1`'s component types where a request genuinely
carries one (`PublishWorkspaceRosterRequest.roster`,
`FirstPageRequest.scope`), and `frontend.v1` reads `agentrepl.v1.shared` for
the failure vocabulary. Nothing new points the wrong way.

### Every method gets its own response, and none of them is `CommandAck`

Sharing `CommandAck` gave 33 methods ONE response type. The cost is not
hypothetical and the previous section paid it twice: `CommandAck` accumulated
`interrupt_confirm_required`, `selected_model`, `observed_claude_session_id`
and `detached_cancel`, each meaningless on 32 other commands, each documented
with a "Present only for X" comment doing the work a signature should do. The
four per-method responses that replaced those fields fixed four instances of
the problem and left the mechanism intact — the 29th method that needs to say
something still had nowhere to say it but the shared ack.

So every method now returns `<Method>Response`. Where a method genuinely has
nothing to add, its response WRAPS `CommandAck` in one field rather than being
`CommandAck`. That wrapper is the whole point: it is a place to put the next
field without offering it to anyone else. `InterruptResponse`,
`SetModelResponse`, `CreateSessionResponse` and `CancelDetachedAgentsResponse`
fold into the scheme unchanged rather than being duplicated beside it.

`DaemonHealth` and `SessionHealth` still do NOT return their views, for the
reason the superseded section gives: returning `DaemonHealthView` would convert
an asynchronous probe into a blocking call. Their responses wrap the ack, and
the verdict is still pushed.

### One file per endpoint, and the `endpoint_` prefix is the grouping

`<Method>Request` and `<Method>Response` live together in
`agentrepl/v1/endpoint_<snake_case_method>.proto` — `ForceQueueEntry` in
`endpoint_force_queue_entry.proto`, and nothing else in it. The whole of what
one method takes and returns is then one file, findable from the method name
without a search, and a change to one endpoint touches one file instead of
appending to a 2000-line `shared.proto` that four unrelated surfaces also read.

**They cannot live in a separate directory.** The Makefile generates with
`--go_opt=paths=source_relative`, so a file's OUTPUT path is its source path:
every file declaring `package agentrepl.v1` must sit in one directory or the
Go bindings for one proto package land in two, which does not compile. A
`src/endpoints/` tree would emit `gen/go/endpoints/...` while the rest of
`agentrepl.v1` emits `gen/go/agentrepl/v1/...`, and the two halves could not
refer to each other. The `endpoint_` PREFIX is what a directory would have
been: it sorts the 34 files together, and `ls src/agentrepl/v1` reads as the
method table.

### `Subscribe` streams a wrapper, and the wrapper is not free

`Subscribe` returns `stream SubscribeResponse`, a message whose single field is
a `FrontendFrame`. The alternative was renaming `FrontendFrame` itself, which
applies the rule with no wrapper at all and no per-message cost.

The wrapper was chosen to PRESERVE THE NAME. `FrontendFrame` is the daemon's,
the webapp's and the elisp frontend's central vocabulary word for "a thing
pushed at a client"; retiring it renames a concept across three
implementations to satisfy a naming convention on one method.

**State the cost plainly: it is paid per frame, on the hot path.** This is the
channel every conversation delta, every typing preview and every state
revision travels on. Each one now carries an extra length-delimited nesting
level — a tag, a length, and one more allocation in every generated decoder.
Nothing else on this surface pays a per-message price for a naming rule, and
the rename would have cost nothing at runtime. If the price is ever measured
and disliked, the remedy is the rename, not a second unwrapped stream. The
judgement recorded here is that a name three systems already speak is worth
more than the bytes, but it is close, and the rename is the defensible other
answer.

## Every endpoint answers with a success or an error, and the failure vocabulary splits by drawn and called

The previous section gave every method its own response type and then put the
same thing inside all of them. `<Method>Response` wrapped `CommandAck`: an `ok`
bit, a free-text `error`, a classified `failure`, a card reference, and four
per-method fields meaningless on the other 32 methods. The wrapper was
described as "a place to put the next field without offering it to anyone
else", and it was — but the field everyone still shared was the ANSWER itself.

Every response is now a two-arm oneof.

```proto
message ForceQueueEntryResponse {
  string request_id = 1;
  oneof response {
    ForceQueueEntrySuccess success = 2;
    ForceQueueEntryError   error   = 3;
  }
}
```

**What this makes unrepresentable is the point.** `CommandAck` could carry
`ok=false` with `failure` unset, which the interrupt challenge actually used
and which had to be documented as "the one non-failure refusal" — a refusal
that is not a refusal. It could carry `ok=true` with a `failure` set. It could
carry `ok=true` with a `selected_model` on a method that has nothing to do with
models. Three states nothing meant, reachable by any producer, and every
consumer had to decide independently what to do with them. None of the three
exists now.

### Errors are structured and IN BAND

A failure caused by a request travels in that request's response and nowhere
else. Not as a transport status — a status code is a number with a string
stapled to it, and what a frontend draws is a classified account with typed
evidence. Not on the push stream, which no caller can correlate to the call it
answers.

### The async boundary, which is NOT a loophole

The rule above is about failures that ANSWER something. A failure that happens
on its own — a session dying mid-turn, a rate limit, a shim crash — is nobody's
answer, and there is no request whose response could carry it. Those stay on
the push stream as `frontend.v1.FailureCardView`, delivered inside
`agentrepl.v1.SubscribeResponse`.

This distinction is load-bearing and easy to lose. "No out-of-band errors",
over-applied, deletes the failure card: the thing that tells a user their
session died while they were reading. The test is not "is this an error", it is
"is this an ANSWER". A shim that crashed answered no question anyone asked.

### The `FailureKind` split, and the inverted import it removes

`agentrepl.v1.FailureKind` served both roles from one 62-arm oneof, and the
evidence that it must not is in the imports. `src/frontend/v1/feed.proto` line
20 imported `agentrepl/v1/shared.proto` so that
`frontend.v1.FailureCardView.kind` could be an `agentrepl.v1.FailureKind` — a
MODEL package importing a BOUNDARY package, backwards under the ownership
scheme this document establishes. The model surface reached into the API
surface because the API surface owned the word for "what went wrong".

The vocabulary splits by the drawn/called test:

- **Drawn** — arms the feed renders because nothing asked: the whole vendor
  family, `FailureShimDegraded`, `FailureSessionShimDied`,
  `FailureQueryTermination`, `FailureTurnUndriven`, the keep-alive windows, the
  session-lifecycle endings, and the six client-local arms a frontend mints
  about its own machinery. These are `frontend.v1.FailureKind`, in the new
  `frontend/v1/failure.proto`.
- **Called** — arms that answer a command: `FailurePromptRefusedByMergeState`,
  `FailureQueueEntryUninterruptibleTurn`, `FailureSessionHibernated`,
  `FailureReconnectSuperseded`, `FailureConversationUnresumable`,
  `FailureInterruptUndelivered`, the shim-delivery family, and the rest. These
  stay in `agentrepl.v1` and become the arms the per-method error oneofs draw
  from.

**`src/frontend/v1/feed.proto` now imports no `agentrepl/v1/*` at all.** That
required moving one more thing: `SessionCommand`, which feed.proto also named.
It goes to `conversation.v1`, where feed.proto's own comment had already
claimed it lived, and where the argument that moved it out of `frontend.v1` in
the first place — a stored conversation record cannot depend on the daemon's
resolved output surface — is satisfied rather than reversed. A vocabulary that
the feed, the footer, the daemon's recognizer and an agentrepl refusal all read
belongs in the leaf every surface may import.

### Nothing is duplicated across the split

A few failures are genuinely BOTH an event and an answer. An SDK query that
terminated is drawn when it happens mid-turn and answers a `CreateSession` when
it happens during bring-up; `SessionResumeFailure`'s own `attempt` oneof has
said so all along, with a `create` arm and an `automatic_restore` arm.
`FailureInternalUnclassified` is the daemon's one classifier funnel, and the
daemon runs that funnel over both paths.

They are not duplicated. The EVIDENCE message is declared once, in
`frontend.v1`, and named from both sides; the ARM is what says whether an
instance is an answer or an event. `agentrepl.v1` may import `frontend.v1`, and
the reverse is exactly what the split removed, so the direction works out. The
split is of the KIND ONEOF, not of the vocabulary.

### Request modes are arms, because a bool can lie

`agentrepl.v1.InterruptRequest` carried `bool confirm_agents = 3`. Any client
could set it true without ever having shown a user a question — the wire could
not distinguish "the user deliberately agreed to stop working subagents" from
"this client wanted the gate to go away". Confirming is an ACT, and an act gets
an arm:

```proto
oneof intent {
  agentrepl.v1.InterruptUnconfirmed interrupt = 3;
  agentrepl.v1.InterruptConfirmed   confirm_interrupt = 4;
}
```

A caller that sends `confirm_interrupt` has sent a DIFFERENT REQUEST, not the
same request with an assertion stapled to it.

The same test was applied across the surface. Converted:
`AnswerPermissionRequest`'s `bool allow` plus its two orphan fields, which made
a denial-carrying-edited-input and an allow-carrying-a-denial-message
representable; `MergeWorkspaceRequest`'s `conflict_resolved_continue`, whose
two values are two different commands the daemon branches on before anything
else it does; `HostActionCompletedRequest`'s `ok` plus `error`, where one
outcome retires a durable record and the other preserves it.
`AnswerMergeDequeueRequest` and `ReviveSessionRequest` needed no conversion —
they were already the shape the rule generalizes from.

Deliberately NOT converted, and each is a judgement worth stating.
`ShutdownRequest.stop_shims` and `fake` state a PROPERTY of what is being asked
for, not an act the sender may not have performed; both values are ordinary
requests the daemon performs as asked. `allow_ungated` IS a consent flag and is
exactly the shape that can lie, but the act it attests to is the caller's own
rather than a prior round-trip, and what makes it safe is that it is required
and refused when absent. `CreateSessionRequest.resume_mode` stays an enum: its
retired tag 2 must remain REFUSABLE, and a oneof cannot refuse a field that
simply vanishes on decode.

### "Confirm first" is a SUCCESS

The interrupt confirmation challenge was returned as `ok=false` with `failure`
unset, and the daemon's own comment called it "THE ONE NON-FAILURE REFUSAL".
The command was understood, correctly processed, and deliberately not
performed. That is not an error. The answer is a question.

```proto
message agentrepl.v1.InterruptSuccess {
  oneof outcome {
    agentrepl.v1.InterruptStopped         stopped = 1;
    agentrepl.v1.InterruptConfirmRequired confirmation_required = 2;
  }
}
```

`InterruptConfirmRequired` moves out of `frame.proto` with it. It was declared
beside the frames because it began life as a push arm; it answers one call and
no other now, so it lives with the call it answers, and its `FrontendFrame` arm
is gone.

The same reading gives `CreateSessionSuccess` two arms —
`CreateSessionEstablished` and `CreateSessionHibernated` — because a create
that met the revival gate has NO SHIM, and reporting it as a bare success is
how a create came to claim a healthy shim for a session that had none.

### Per-method errors, DERIVED not invented

`<Method>Error`'s arms are the failures that method can actually produce, read
off the daemon: the create-over-wire refusal and the nil-retainer in
`daemon/internal/frontend/commands.go`; the unwired-dependency family, the
`checkWorkspaceKey` sites and the semantic gates in
`daemon/internal/server/frontendcmd.go`; the nine `CreateSessionCmd` rules in
`daemon/internal/server/createestablish.go`.

Most of those refusals had no wire spelling at all. They were `fmt.Errorf` text
that reached Emacs as an echo and the webapp as nothing, which is why the
`Refusal*` prefix exists beside the older `Failure*` one: the prefix records
whether a frontend already had a renderer for it.

**No method gets a catch-all to fill its oneof.** Where a handler genuinely has
an unclassified funnel, the arm names it, because the daemon really does emit
`internal.unclassified` and hiding that would be a worse lie than stating it.
Where a method has one derived refusal, it gets one — `DeleteSession` and
`Shutdown` say so out loud rather than padding. Three methods —
`PauseMergeQueue`, `ResumeMergeQueue` and `EvictMerge` — have NO daemon handler
at all, so there was nothing to derive from; the first two carry only the
funnel and say why, and `EvictMerge` carries exactly the two refusals its own
normative text has always promised and nothing more.

### The cost: a large webapp change

Every response site in the webapp must switch on a oneof. There is no longer an
`ack.ok` to test, no `ack.error` to render, and no shared `CommandAck` type to
write one handler against — each of the 34 methods has its own success and its
own closed error set, and the code that reads them has to know which. That is
the price of making an unhandled refusal a compile error instead of a
`default:` nobody wrote.

The elisp frontend pays a smaller version of the same bill, and the daemon pays
the largest one: its handlers return `error` today and the classifier turns
every one of them into a `FailureKind`, so the funnel has to become a
per-method arm selection. None of that is done here — this is the contract
change, and the systems that produce and consume it follow.

## The `service.proto` redesign walks a settled iteration sequence

**Decided.** The redesign that deletes `FrontendFrame` and `StateSnapshot` is
walked in four stages, highest abstraction first, and a later stage opens only
after every earlier one is settled:

1. **Transport shape** — whether server-initiated data arrives on one
   multiplexed stream or on one stream per component.
2. **Endpoint inventory** — every method by name and one-line purpose, with no
   shapes attached. This is where each of the outbound god-message's arms is
   classified as an answer to an existing call, a push endpoint of its own, or
   homeless, and where the connect snapshot's fields are ruled on one at a
   time.
3. **Cross-endpoint conventions** — the rules binding every endpoint at once.
4. **Per-endpoint request and response shapes**, one endpoint at a time.

The user accepted the default order unamended.

**Transport goes first because it decides whether the rest is coherent.**
Deleting the outbound god-message is only a real deletion under one of the two
transport answers; under the other it is a rename, because a single multiplexed
stream still needs one message enumerating everything that can travel on it.

**STAGE 1 EXPLICITLY REOPENS A DECISION THIS DOCUMENT ALREADY RECORDS AS
SETTLED** — see "The push channel stays ONE ordered stream" above. Reopening it
reopens every decision recorded downstream of it, including "`Subscribe`
streams a wrapper, and the wrapper is not free" and the connect snapshot's
multi-frame batching rule. Nothing downstream of the transport decision is kept
merely because it was written down before.

**Recorded so the rejected alternative is not re-proposed in good faith.** The
per-topic-stream design was already argued and refused, for three concrete
ordering failures: a workspace revision applied before the snapshot that
supersedes it, a typing cut retiring a preview whose opening frame never
landed, and a conversation delta arriving against a workspace the client has
never heard of. Any reopening must answer those three by construction or it
loses to them again. The orchestrator re-proposed exactly this alternative in
this session before reading the recorded refusal, which is precisely the
failure the "record why the alternatives lost" rule exists to prevent.

## The one multiplexed push stream is REVERSED: one stream per component

**Decided, and it overturns "The push channel stays ONE ordered stream" above.**
That decision stands retracted. `Subscribe`, `SubscribeRequest`,
`SubscribeResponse`, `FrontendFrame` and `StateSnapshot` are deleted outright,
and `frame.proto` and `endpoint_subscribe.proto` are deleted with them. Server
-initiated data will arrive on one server-streaming endpoint per UI component,
each in its own `endpoint_` file, and the component's stream is the only place
that component's data travels.

**WHY, in the user's terms.** `FrontendFrame` is a god-message and a
god-message should be straight up deleted, not relocated. Every message belongs
in the endpoint file of the endpoint that carries it, with only genuinely small
reusable messages living anywhere shared. The connect snapshot was called
horrifically disorganized, and it is: seventeen fields of unrelated shape, four
of them documented as host surface that a runtime filter strips before a GUI
client sees them.

**What the reversal buys, structurally.** Host-only data is stripped today by
`frontend.Server` at runtime, per-field, on a shared envelope — a correlation,
not a guarantee, and a missed strip leaks. One stream per component makes that
authorization a property of the METHOD: a GUI client does not call the host
component's Watch endpoint, so there is no field to scrub and a missed scrub
stops being representable.

**What it costs, ACCEPTED BY THE USER as the price.** The previous decision
refused per-topic streams for three concrete ordering failures. Two of them
dissolve, and the third does not:

- A workspace revision applied before the snapshot that supersedes it —
  DISSOLVES. Both are elements of the same component's stream, which is
  totally ordered.
- A typing cut retiring a preview whose opening never landed — DOES NOT
  DISSOLVE under this answer. Typing and conversation are two components and
  therefore two streams, but one ordering domain. The partition chosen is by
  COMPONENT, not by ordering domain, so every such pair needs its own fencing
  answer rather than getting one for free.
- A conversation delta naming a workspace the client has never heard of —
  DOES NOT DISSOLVE. Workspace identity and conversation content are different
  domains under any partition. One stream guaranteed this by position; N
  streams must answer it at every cross-stream reference.

The user chose the component partition knowing both surviving failures. They
are not deferred — they are the substance of a cross-endpoint convention that
the conventions stage must settle, and no component's stream is designed as
though its references were free.

**Two things left deliberately dangling on disk.** The `Resync`, `FirstPage`
and `NextPage` docstrings still describe their answers arriving on "the
Subscribe stream", which no longer exists. Those three are exactly the
out-of-band answers the redesign exists to reclassify, so the references are
kept visible as open questions rather than patched into plausible-sounding
prose. Separately, `SubscribeRequest.client_id` carried the definition of a
reader's identity as "stable for the life of this stream"; with N streams that
phrase has no referent, while the `client_id` FIELD survives independently on
the three requests above. What a reader's identity is scoped to is a
conventions-stage question and is not answered here.

**The build is broken by this change and that is intended.** The consuming
systems are rewritten by the implementation fan-out; blocking a settled
structural decision on downstream compilation is how a design document and the
schema on disk come to disagree.
