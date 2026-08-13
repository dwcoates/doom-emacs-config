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

### Two consequences, accepted

**1. `daemon/internal/frontend/translate.go` goes.** It existed to neutralize a
VENDOR shape before a client could see it. The producers now convert at the
edge, so there is no vendor shape left to neutralize and the layer is ceremony —
decoding one neutral shape and re-encoding it as another. Synthesis stays;
translation does not.

**2. `frontend`'s feed envelope shrinks.** `Message` carries
`MessageLineage{top_level_message_id, parent_message_id}`, a durability oneof and
`ConversationSource`; `MessageEntry` already carries `message_id`,
`top_level_message_id`, `parent` and `author`. Under passthrough that envelope is
a second spelling of the same facts, and it reintroduces the empty-string
ambiguity `MessageParent` was made a oneof to remove. What survives is only what
the daemon genuinely adds.

## Implementation and integration status

**The proto tree is restructured onto the five surfaces; no consumer has been
migrated.** `daemon/`, `webapp/` and `agent-shim/` are mid-rewrite and do not
build against it, which is the expected steady state until the gaps below are
reconciled.

A first implementation wave was dispatched across six subsystems and four of the
six correctly stopped without writing code. The reason, in one line: the contract
specified what a record IS and never specified how one MOVES. `StoreWrite` still
carried the retired `EventBatch`, nothing imported the stored record type, and
the delivery envelope was generated and referenced by nothing. Recorded here so
nothing is dispatched again before the model is whole.

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
