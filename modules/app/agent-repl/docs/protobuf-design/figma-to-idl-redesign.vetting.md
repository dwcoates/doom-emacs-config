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

### 1. Every conversation.v1 message maps to a real SDK route, in both directions

**The assumption.** Each message and arm in `conversation.v1` corresponds to
an actual route in the Claude Agent SDK — a message subtype, a tool output
type, a control response — that (a) actually carries the information the
protobuf message claims, and (b) has no information we failed to represent.
The shapes were derived from the SDK's TYPE SURFACE
(`@anthropic-ai/claude-agent-sdk` 0.3.220 `sdk.d.ts` and `sdk-tools.d.ts`),
which states what a field IS but not always whether a producer in our
configuration actually sets it.

**What is affected.** Potentially every arm of `TurnProgress` and every
`TurnAgent*` body, plus the `Turn*` detached-work family. Concretely, the arms
whose bodies were read off a type with no captured transcript to confirm them:
the bash family, the read/write/edit patch families, grep and glob, the task
tracker's six status arms, and the thinking/response usage placement.

**How to verify it.** Two passes, in this order, because the second is
meaningless without the first.

1. FORWARD — for each landed message, name the SDK route it is filled from,
   and confirm from a REAL captured transcript (not the type surface, and not
   the shim's fixture) that the route fires and that every field the message
   declares is populated. A field the producer never sets is either deleted or
   documented as conditional, never left implying a guarantee.
2. REVERSE — enumerate every SDK message subtype and tool output type, and
   confirm each is either represented in `conversation.v1`, deliberately
   dropped with the reason recorded, or genuinely out of scope. This is the
   direction that finds SILENT OMISSIONS, and it is the one that cannot be
   done by reading our own protos.

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
