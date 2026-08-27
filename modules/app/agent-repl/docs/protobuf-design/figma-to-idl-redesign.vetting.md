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

**Status.** RUN (2026-08-22/24), results persisted at
`docs/protobuf-design/audit/` — five reports (A: SDK stream messages, B:
per-tool I/O types, C: real JSONL corpus 641k records, D: sidecar file
sources, E: control surface), each a per-field forward/reverse
classification. FINDINGS NOT YET JUDGED: the unsupported/non-static lists
await the design conversation; each judged finding re-enters the sketch loop
per the register's rules.

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

### 7. Whether the MAIN agent accepts a prompt mid-turn at its next tool round, as a subagent does

**The assumption.** `AgentInput.prompt` is one arm for the main agent and a
detached subagent alike, and the shim resolves per kind how it lands. For a
LIVE subagent the landing is OBSERVED (123 real results: "queued for delivery
to X at its next tool round"). For the main agent it is NOT: the interrupt
docs (`sdk.d.ts:3487`) say a mid-turn main-thread prompt is enqueued and
"coalesced into one turn" the drain loop starts AFTER the current one, while
`SDKUserMessage.priority: 'now' | 'next' | 'later'` (`sdk.d.ts:4592`) exists
UNDOCUMENTED and reads like a next-tool-round injection.

**What is affected.** Nothing in the schema — the arm stands either way. It
decides what "deliver now" to a busy main agent MEANS in the shim: a steer at
the next tool round (if `priority: 'now'` does that), or the only alternative,
a hard interrupt followed by a resubmit. The daemon's classifier today knows
only the latter (`interject`).

**How to verify it.** Send a main-thread user message with `priority: 'now'`
while a turn with several tool calls is running, and observe in the transcript
whether it lands between tool calls of THAT turn or opens a new one. Repeat
with `'next'` and `'later'`.

**Status.** OPEN.

### 8. Whether the SDK can initialize a session from a transcript the shim wrote (the compaction remediation)

**The assumption.** `SessionColdCompact` is implementable: the shim runs a
throwaway session that summarizes the transcript, writes a compacted
transcript, and initializes the REAL session from it, preserving the vendor
session identity.

**What is affected.** The `compact` arm of `SessionColdRemediation`. If no
route exists, the arm either dissolves into the vendor's own `/compact`
(losing the model and scope choices) or requires a different mechanism.

**How to verify it.** Three candidates at the type surface: the `@alpha`
`sessionStore` option, `forkSession`, and writing the JSONL the agent binary
reads on `resume` (including a `compact_boundary` record in the vendor's own
format, `sdk.d.ts:2945`). Try each against a probe session and confirm the
resumed session's first usage reflects the compacted size.

**Status.** OPEN.

### 9. Whether the vendor's background tasks survive the query closing

**The assumption.** CloseSession refuses while detached work is live because
killing the query may kill the work: a backgrounded shell and a subagent are
presumed children of the agent binary.

**What is affected.** `CloseSessionRequest.force` semantics and whether a
non-forced close with live work must refuse. If tasks survive the binary,
the next OpenSession reports them via `SessionOpened.live_work` and a close
could be legal without force.

**How to verify it.** Background a long shell via the SDK, end the query
cleanly, and observe whether the process and its spool file continue; then
resume the session and check `background_tasks_changed` membership.

**Status.** OPEN.

### 10. Whether backgrounding a FOREGROUND agent (Ctrl+B) fires a `task_started` edge

**The assumption.** The shim detects every entry into the live-background
set from the `task_started` EDGE alone (principle #3 forbids diffing the
level). The level's doc comment lists "a foreground agent being backgrounded"
as a membership change; `SDKTaskStartedMessage`'s doc does not say it fires
for that transition.

**What is affected.** `AgentDetachedWork`'s `detached` origin for a Ctrl-B'd
agent: if no edge fires, the shim must take that one transition from
`SDKTaskUpdatedMessage` or from the relayed level, and the frame's producer
note changes; the shape does not.

**How to verify it.** Background a running foreground subagent via
`backgroundTasks(toolUseId)` and record which system messages arrive, in
order.

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

**Status.** PARTLY SETTLED (2026-08-22): `usage.output_tokens_details.thinking_tokens` IS present in real transcripts (324 non-zero occurrences), so `TokenUsage.output_thinking_tokens` has a billed producer and the "no thinking field" claim is retracted. What remains owed is a type for the live ESTIMATE channel, if a surface draws it.

### E. A workflow's per-agent transcripts are never ingested (owed to the implementation wave)

**What is required.** The sidecar must discover and tail
`projects/<project>/<session>/subagents/workflows/wf_<id>/agent-<id>.jsonl`, so a
workflow's constituent agents reach the store as records like any other
subagent's.

**What is in the way.** Discovery globs only the run's JOURNAL
(`internal/discover/discover.go:86` matches `workflows/wf_*/journal.jsonl`). The
per-agent transcripts sit in the SAME directory — seven of them in the run
inspected, each with a `.meta.json` beside it — and match no discovery pattern,
so they are never read. Ordinary subagent transcripts ARE ingested
(`discover.go:132`, `KindAgentTranscript`), which is what makes the omission easy
to miss: the mechanism exists and is simply not pointed at this directory.

**Why it matters under the flat subagent model.** A workflow's agents get a
creation record and an agent identity, so the frontend draws a bubble for each
and can ask for its items by identity. There would be NOTHING TO SERVE. Worse,
the journal IS ingested but only as flattened prose lines — its own converter
states "THE RENDERING IS LOSSY AND THAT IS A KNOWN COST" — so the run appears to
have been recorded while the actual work was discarded.

**The general form of the requirement, stated because it outlives this one bug.**
Anything a bubble can be drawn for must be RESOLVABLE FROM THE STORE, not from a
file that happens to still exist. A historical bubble is expanded days later,
after the session that produced it has ended, and the only durable answer is the
store.

**MEASURED, so the scope is not guessed.** 1,168 ordinary subagent transcripts
sit at `subagents/agent-<id>.jsonl` and ARE ingested. 93 sit at
`subagents/workflows/wf_<id>/agent-<id>.jsonl` and are NOT. Identical file shape,
identical meaning; the discovery pattern simply stops one directory level short.

**WHY THIS IS NOW LOAD-BEARING RATHER THAN DESIRABLE.** The settled model makes a
workflow's agents announce themselves through the journal's `started` records,
which the sidecar already reads — so the frontend WILL draw a container per
workflow agent. The contents of those containers come from the per-agent
transcripts. Unfixed, every one of them opens onto nothing, and the failure looks
like empty bubbles rather than like missing ingestion.

**Status.** OPEN.

### F. Four files still name `conversation.v1.MessageId` and must be repointed (owed to stages 2 and 3)

**What is required.** `message.proto` is DELETED. Four files still declare fields
of its `MessageId` type and must be repointed as their stages are re-walked:

- `agentrepl/v1/endpoint_get_feed_page.proto` — the parent container arm.
- `agentrepl/v1/endpoint_interrupt.proto` — the detached target.
- `frontend/v1/feed.proto` — five fields: the row's identity, its parent, a
  breadcrumb target, and a bubble reference.
- `frontend/v1/footer.proto` — an expanded row's jump target.

**What each becomes.** Every one of them is a ROW or TARGET identity, which is
the granularity collision the stage-1 reversion flagged by name: one type was
serving both a record's identity and a renderable row's. Under the settled model
those are `AgentActivityId` where the target is a unit of work and `AgentId`
where it is an agent's container.

**One more reference, already dying.** `shim/v1/external.proto` declares a
`MessageEntry` field. That message is superseded by the datalayer/protocol split
and goes with it rather than being repointed. Remaining mentions elsewhere are
comment text only.

**Status.** OPEN.

### G. Keep-alive turns are FIRST-CLASS in the store as never-served, and context is rolled back before the next real prompt (owed to stage 5 and the implementation wave)

**What is required.** The shim's cache keep-alive is invisible to the daemon's
API — no request arm, no rpc, no `PromptOrigin` value. Three things must
therefore hold without the daemon's help: (1) the store indexes keep-alive
turns so that no page ever returns them and no activity from them is routed to
the daemon; (2) before a real prompt is submitted, the shim ROLLS BACK the
vendor's context to just after the last real prompt, so real turns never build
on keep-alive context; (3) the keep-alive prompt text is the shim's own.

**Why, and who depends on it.** The daemon believes the main thread is idle
between real turns. A keep-alive that leaked into a page, a feed, or an
accounting sum would be a prompt nobody submitted; a keep-alive left in
context would change the model's answers to real prompts.

**What exists today.** `SessionRewound` and `KeepAliveDiscard.dropped_turn_ids`
already claim the rollback and the exclusion. Whether the rewind is RELIABLE
against the vendor's actual transcript is unverified — that is the vetting
half of this item; the store index is the stage-5 half.

**Status.** OPEN.

### H. The store scopes by the LOGICAL session, never nils a parent, and indexes children by agent (owed to stage 5, `store.v1`)

**What is required.** (1) The store's session scope is the logical session
(keyed by `main_agent_id`), with `vendor_session_id` a mutable attribute a
rotation updates — today it scopes seq, dedup and fan-out by the vendor id.
(2) Every record's parent is an `AgentId`, never nil: the main agent's
children carry the main agent's id. (3) "The N most recent children of agent
X, newest first, from a continuation" is one indexed query on the parent
column, replacing the ingest-time feed-row-owner walk. (4) Keep-alive turns
are never returned by it (Owed G).

**Why, and who depends on it.** `ReadHistory` is addressed by agent and
promises a rotation never splits a page; `SessionStarted.main_agent_id`
promises the id is stable across starts; both are store facts.

**Status.** OPEN.

### 11. The cost/usage panels' one structured source is an EXPERIMENTAL control method

**The assumption.** The vendor's structured session-usage answer (the
control channel's get-usage response: session cost, rate-limit windows) —
the only structured producer for CostPanelView and UsagePanelView — remains
available, despite the SDK naming it experimental
(`usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET`).

**What is affected.** CostPanelView and UsagePanelView: if the method is
removed or reshaped, the daemon's fill for both panels breaks; the panel
SHAPES survive (label/value rows), only the fill strategy changes (e.g.
deriving cost rows from per-turn result usage instead).

**How to verify it.** At the implementation wave: pin the SDK version, call
the method against the pinned version, and add a shim test that fails
loudly when the method disappears on an upgrade.

**Status.** OPEN.
