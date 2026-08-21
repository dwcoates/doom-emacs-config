# POST-DESIGN VETTING: the figma→idl redesign of the agent-repl contract

Companion to `figma-to-idl-redesign.md`. That document is the DECISION RECORD;
this one is the list of INVESTIGATIONS OWED once every protobuf has landed.

## What belongs here

An item belongs here when a landed decision RESTS ON AN ASSUMPTION about a
system outside this contract that was not verified at the time it was made,
and where verifying it earlier would have blocked the design for no good
reason. The design proceeds on the assumption; this document records the debt.

An item does NOT belong here when it is an implementation task, a known gap
with a decided answer, or a question the design record already settles. Those
live in the record or in the wave's own work.

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
