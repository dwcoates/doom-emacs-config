# DESIGN: the agent-repl protobuf surfaces

## The five surfaces

Every message belongs to exactly one. **The package boundary IS the surface
boundary** — nothing straddles, so which surface a message is on is a fact a
compiler can check rather than a convention a reviewer has to hold.

| package | holds | importable by |
|---|---|---|
| **agentshim** | shim-side internals: which plane observed a record, the store's write identity, anything a producer could not convert | shim, sidecar, store ONLY |
| **conversation** | the shared conversation model — `MessageEntry`, `BookkeepingEntry` and their payloads | everyone |
| **protocol** | what traverses the daemon↔shim boundary, and ONLY that boundary, in both directions: handshakes, commands, receipts, health, replay and page requests, and the delivery envelopes that carry conversation records with their position | shim, daemon |
| **frontend** | what reaches a frontend client: daemon-synthesized views, and the conversation records the daemon forwards | daemon, webapp |
| **state** | daemon-internal only: what no other service uses, including the schema the daemon marshals into its own SQLite store | daemon |

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

**Not started.** A first implementation wave was dispatched across six
subsystems and four of the six correctly stopped without writing code.

The reason, in one line: the contract specified what a record IS and never
specified how one MOVES. `StoreWrite` still carried the retired `EventBatch`,
nothing imported the stored record type, and the delivery envelope was generated
and referenced by nothing. Recorded here so nothing is dispatched again before
the model is whole.

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
