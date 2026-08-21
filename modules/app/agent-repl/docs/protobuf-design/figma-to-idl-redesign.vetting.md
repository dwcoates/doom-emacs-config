# POST-DESIGN VETTING: the figma→idl redesign of the agent-repl contract

Companion to `figma-to-idl-redesign.md`. That document is the DECISION RECORD;
this one is the list of INVESTIGATIONS OWED once every protobuf has landed.

## What belongs here

An item belongs here when a landed decision RESTS ON AN ASSUMPTION about a
system outside this contract that was not verified at the time it was made,
and where verifying it earlier would have blocked the design for no good
reason. The design proceeds on the assumption; this document records the debt.

An item does NOT belong under **Items** when it is an implementation task, a
known gap with a decided answer, or a question the design record already
settles.

A KNOWN GAP WITH A DECIDED ANSWER that a LATER STAGE must honour is different
from both, and it is tracked separately under **Owed to later stages** below.
Such an item needs no investigation — the answer is settled — but it will be
silently lost if the stage that must act on it never learns anything depends on
it. The two lists are kept apart because they are discharged differently: an
item under **Items** is VERIFIED, an item under **Owed** is IMPLEMENTED.

Each item states, at minimum:

- **The assumption** the contract is resting on.
- **What is affected** if it turns out false — which messages, fields or arms.
- **How to verify it** — the concrete method, so the check is not re-derived.
- **Status** — OPEN, or the finding and the date it was settled.

## Items

### 1. Every conversation.v1 message maps to a real route in the SDK OR THE SIDECAR, in both directions

**The assumption.** Each message and arm in `conversation.v1` corresponds to
an actual route in ONE OF TWO PRODUCERS — that (a) actually carries the
information the protobuf message claims, and (b) has no information we failed
to represent.

THE TWO PRODUCERS, and the verification differs for each:

- THE VENDOR SDK (`@anthropic-ai/claude-agent-sdk` 0.3.220) — a message
  subtype, a tool output type, a control response. The shapes were derived
  from its TYPE SURFACE (`sdk.d.ts`, `sdk-tools.d.ts`), which states what a
  field IS but not whether a producer in our configuration actually sets it.
- THE SIDECAR (`agent-shim/claude/shim-sidecar`) — facts NO SDK route carries
  at all, which the sidecar observes out of band by tailing files the agent
  binary writes. A backgrounded shell's output spool is the clearest case: the
  SDK has no output-retrieval route for a background task, so every byte of
  detached shell output on this contract comes from
  `internal/handler/shell.go` reading the spool file and its `EXIT=<code>`
  terminator. Workflow journal steps arrive the same way
  (`internal/convert/journal.go`).

THE SIDECAR HALF IS THE MORE FRAGILE HALF, and it is the reason this item
names two producers rather than one. A vendor route breaking is a vendor
change we would meet at the type surface on the next upgrade. A sidecar route
breaking is a FILE FORMAT OR PATH we do not own changing underneath us, with
no type to fail against and no compiler to say so — the contract keeps
claiming the field and the field silently stops arriving.

**What is affected.** Potentially every arm of `TurnProgress` and every
`TurnAgent*` body, plus the `Turn*` detached-work family. Concretely, the arms
whose bodies were read off a type with no captured transcript to confirm them:
the bash family, the read/write/edit patch families, grep and glob, the task
tracker's six status arms, and the thinking/response usage placement.

**How to verify it.** Two passes, in this order, because the second is
meaningless without the first.

1. FORWARD — for each landed message, name the route it is filled from AND
   WHICH PRODUCER owns that route, then confirm the route fires and that every
   field the message declares is populated. For an SDK route the evidence is a
   REAL captured transcript, never the type surface and never the shim's own
   fake (see item 5). For a SIDECAR route the evidence is a real file of the
   kind the handler tails, since there is no transcript to capture. A field no
   producer ever sets is deleted or documented as conditional, never left
   implying a guarantee.
2. REVERSE — enumerate every SDK message subtype and tool output type, AND
   every file the sidecar tails, and confirm each is either represented in
   `conversation.v1`, deliberately dropped with the reason recorded, or
   genuinely out of scope. This is the direction that finds SILENT OMISSIONS,
   and it is the one that cannot be done by reading our own protos.

**Status.** OPEN. Blocked on nothing; runs once the last arm body lands.

### 2. Whether a shell's detachment announces itself the way a subagent's does

**The assumption.** A backgrounded shell enters the vendor's live-background
set and is announced the same way a spawned subagent is, so one detection path
in the shim serves both.

**What is affected.** Nothing in the contract — the frames are identical
either way. It decides only whether the shim reads the task set or the tool
output to detect detachment, so it is recorded here rather than left to be
rediscovered as a surprise.

**How to verify it.** Capture a transcript of a `run_in_background` shell call
and compare its frame sequence against the detached-subagent sequence already
captured. The type surface cannot answer it.

**Status.** OPEN.

### 3. Whether `stop_reason` distinguishes an intermediate response from a final one

**The assumption.** It does not, and finality is therefore not a wire fact —
so the turn's own conclusion names the answering response rather than a
finality field on the response itself.

**What is affected.** If `stop_reason` DOES discriminate, a producer could
state finality directly and the turn-level naming becomes redundant.

**How to verify it.** Every assistant message in our own fixture reports
`end_turn`, INCLUDING six that carry `tool_use` blocks — which is either an
unfaithful fixture or the agent binary normalizing the value. A real captured
transcript settles which.

**Status.** OPEN.

### 4. Whether every vendor API failure seen live also lands as a transcript record

**The assumption.** It does, which is why the vendor's API-failure classes
were removed from `FailureKind` and reach the feed as an
`ApiRequestFailed` record instead.

**What is affected.** If some live API failures never become records, the feed
would MISS them entirely and that deletion reopens.

**How to verify it.** Compare the daemon's `errclass` construction sites
against the records the store actually holds for a session that hit real API
failures.

**Status.** OPEN.

### 5. The shim's fake SDK is hand-written, so it can only ever confirm our own reading

**The assumption.** `agent-shim/claude/shim/src/fake-query.ts` — the shim's
stand-in for the SDK's `query()`, which yields a scripted sequence of
`SDKMessage` objects so tests can drive the pipeline without spawning the
agent binary — faithfully imitates what the real SDK emits.

THIS ASSUMPTION IS STRUCTURALLY UNVERIFIABLE FROM THE FAKE ITSELF. It was
written by us, so wherever our reading of the SDK is wrong the fake is wrong
in the same direction and agrees with us. It is evidence of INTERNAL
CONSISTENCY and never of vendor behavior, and any design claim resting on it
alone is resting on our own prior belief.

**What is affected.** Every decision whose only evidence was the fake. Two are
known: that a spawn is announced DURING the turn rather than at its end (which
is load-bearing — it is why the turn must be a stream at all), and the reading
of `stop_reason` in item 3. There may be others; enumerating them is part of
this item.

**What is also affected by what the fake DOES NOT contain.** It scripts a
detached SUBAGENT turn and no detached SHELL turn at all, so it is silent on
the frame order for a backgrounded shell — which is item 2.

**How to verify it, and this one is solvable rather than merely checkable.**
Capture real transcripts from the actual agent binary and DIFF them against
what the fake yields for the same scenarios. Every divergence is either a bug
in the fake or a wrong belief in the contract, and both are worth finding. The
durable fix is to build the fake's scripts FROM captured transcripts rather
than by hand, so it stops being able to agree with us by construction; that is
a test-infrastructure change, and it is the reason this item is recorded as
solvable rather than as a standing limitation.

**Status.** OPEN.

## Owed to later stages

Settled requirements a later stage must honour. Nothing here needs
investigating; each needs DOING, by a stage that would otherwise have no reason
to know it matters.

### A. `StoreEntry` must index a unit's OWN identity (owed to stage 5, `store.v1`)

**What is required.** The datalayer model must carry a unit's own identity as an
INDEXED column, so a record can be looked up by the unit it belongs to rather
than only by position or by feed-row owner.

**Why, and who depends on it.** `start` is emitted per STREAM rather than per
unit, so work that detaches is announced twice and the second announcement must
repeat the ORIGINAL instant — otherwise a drawn clock resets when work merely
moves. For an ordinary detach the shim still holds that instant. After a SHIM
RESTART it does not, and still-live background work must be re-announced anyway
(the path `ShimHello.live_task_set` exists for), so the instant is read back out
of the store.

**What is in the way.** The store is not indexable that way today. `entry` is
keyed `PRIMARY KEY (session_id, seq)`, with a dedup index on `write_id` and two
indexes on `top_level_message_id` — the FEED-ROW OWNER column derived by the
ingest-time parent walk, NOT the unit's own identity
(`shim-store/internal/db/db.go:156-172`). A lookup by unit id would be a scan.

**Why it is recorded rather than left to stage 5's judgment.** Under model 2 the
unit's id becomes the record's own identity, so the column is natural and stage 5
might well add it unprompted — but nothing in the schema says a RESTART PATH
depends on it, so a stage 5 that omitted the index would look correct and break
re-announcement only after a shim bounce, which is exactly the failure nobody
reproduces on purpose.

**Status.** OPEN.

### B. The shim must set `forwardSubagentText` (owed to the implementation wave)

**What is required.** The shim's query options must enable subagent text
forwarding. `agent-shim/claude/shim/src/main.ts:314` sets
`includePartialMessages: true` and does NOT set `forwardSubagentText`.

**Why, and who depends on it.** The vendor's default forwards only a subagent's
`tool_use` and `tool_result` blocks — documented as "enough for a heartbeat
counter". A subagent's PROSE AND REASONING are not forwarded at all unless the
option is set. The protocol model represents a subagent's activity with the same
vocabulary as any agent's, including responses and thinking, so without this
option those arms have NO PRODUCER and a nested transcript cannot be drawn — the
contract would declare facts nothing ever fills.

**Why it is recorded rather than left to the wave to notice.** Nothing in the
schema mentions a producer option, and the failure is SILENT AND PARTIAL: nested
tool calls would appear correctly while nested prose simply never arrived, which
reads as "the subagent did not say anything" rather than as a misconfiguration.

**A cost to weigh at the wave, not a reason to skip it.** Forwarding a full
subagent conversation is strictly more traffic and more records than the default,
and a session with many subagents pays it. The alternative is not drawing nested
transcripts at all.

**Status.** OPEN.

### 6. Whether a question's free-text answer and its selection note are two distinct facts

**The assumption.** The producer's question output carries two separate
text-bearing fields — one that appears to be the free-text escape a user types
instead of picking an option, and one described as notes the user added to their
selection — and they are genuinely different facts rather than two spellings of
one.

**What is affected.** `AgentQuestionAnswer.free_text` and
`AgentQuestionAnswer.note`. If they are the same fact, one field is a
respelling and goes. If the substitute-for-a-choice field turns out to be
something else entirely, `free_text` has no producer and the drawn card's
free-text escape has nowhere to land.

**How to verify it.** Pose a question through the real harness, answer it in
three ways — pick an option only, type free text only, and pick an option while
adding a remark — and read which fields the tool output carries in each case.
The type surface cannot distinguish them.

**Status.** OPEN.

### C. The topbar needs an alert kind for unmodeled tool calls (owed to a `frontend.v1` increment)

**What is required.** `TopbarWarningStrip` needs a warning kind for "unmodeled
work occurred", with a dropdown treatment that renders an ABBREVIATED, LEGIBLE
account of the call — never a dump of its untyped arguments.

**Why, and the two constraints that pull against each other.** An unmodeled tool
call must reach the user so a blind spot does not grow unnoticed, but it is NOT
an error — the tool very likely ran correctly and only this contract's coverage
is at fault. So it cannot be drawn as a failure, and it cannot be drawn as
nothing. The purpose of surfacing it is remediation: someone decides whether the
tool deserves a modelled arm, which requires a legible summary rather than raw
structure.

**An idea recorded as an idea, not a decision.** Classify the arguments'
structure programmatically and, when the structure is novel to the daemon's state
manager, run a small fast model over it to produce a plain-English summary for
rendering. This was raised explicitly as a possibility rather than a ruling. The
REQUIREMENT it addresses is settled; this particular means of meeting it is not.

**Status.** OPEN.

### D. The thinking-token estimate needs its own representation (owed to a `conversation.v1` increment)

**What is required.** A type for the thinking figure a footer draws, distinct
from `TokenUsage`.

**Why.** `usage` contains NO thinking-token field. The figure comes from a
separate channel (`system/thinking_tokens` -> `estimated_tokens`,
`estimated_tokens_delta`) and is an ESTIMATE, not a billed amount. Borrowing
`TokenUsage` for it would invite a consumer to add an estimate into a bill.
Removing the misattributed field from the thinking arm left the figure with no
representation at all, so the footer's thinking cell currently has no producer.

**Also unmodelled.** `usage.output_tokens_details` appears in real transcripts
and `TokenUsage` does not carry it.

**Status.** OPEN.
