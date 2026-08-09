# figma→idl draft — unhooked review artifact

This directory is the **materialized form of `../SKETCH-figma-idl.md`**: the
whole `agentshim.frontend.v1` surface split into per-component files, written
as it is intended for production so the split can be reviewed as files rather
than as a table.

**Nothing here is live and nothing here is built.**
`../agentshim/frontend/v1/frontend.proto` remains the sole wire contract; the
`Makefile` and every build script are untouched, no bindings are generated
from these files, and no code imports them. Deleting this directory changes
nothing.

## What the files are

- **The `.proto` files carry no process narration.** A comment in a `.proto`
  explains what the message or field IS, what it means, when it is present and
  who resolves it — nothing about how the draft came to look this way. There
  are no `NEW`/`RELOCATED`/`RESTRUCTURED` markers, no round numbers, no
  "replaces X in the client", no review-artifact banners. The files must read
  timelessly to a maintainer who never saw this review.
  - **The record of what changed lives HERE**, in the round sections below,
    and in `../SKETCH-figma-idl.md`. That is the whole reason those sections
    are as detailed as they are.
- **Relocated declarations are byte-verbatim copies** of the current
  `frontend.proto`: comments, field numbers, `reserved` statements and
  `[deprecated = true]` markers all identical, no rewording and no
  renumbering. Free-floating section banners travel with their subject matter,
  also verbatim.
  - This means a handful of relocated comments still carry `frontend.proto`'s
    OWN historical notes (step/stage references, "replaces Emacs POST
    /sessions", and similar). Those came in with the text and were left
    untouched, because rewriting them would break the byte-verbatim property
    that makes the split reviewable as a pure move. Cleaning them is a change
    to `frontend.proto` itself, not to this artifact.
- **New declarations are verbatim from the sketch**, placed in the file the
  sketch's manifest names.
- Cross-file references import siblings by their **intended production path**
  (`agentshim/frontend/v1/<file>.proto`); references into `core.v1`,
  `data.v1`, and `google.protobuf` keep the imports they already had.

## Round 2 changes (async / AgentEmission)

Round 1 split `frontend.proto` into component files and added a handful of
resolved views. Round 2 changes a **shape**, and it is the shape the async
surface hangs off. Exactly this changed:

1. **`agent-emission.proto` is new.** It holds `AgentEmission`, a closed
   `oneof` of everything an agent can produce in a conversation.
   - Every arm is extracted from an existing round-1 `ConversationItem`
     agent-output arm; nothing agent-produced is invented.
   - The arm payload messages live in the component files that draw them, so
     the message and its picture stay together.

2. **`ConversationItem` is RESTRUCTURED, breaking.**
   - It becomes feed-level packaging — uuid, ts, request_id, source,
     `token_utilization`, `turn_accounting` — around a payload.
   - Agent output is now one arm, `AgentEmission agent = 5`.
   - Non-agent items keep their own arms: user prompt, permission, system
     failure, context cleared/compacted, session command, compaction summary,
     and the new async anchor.
   - Retired arms 10/12/13/14/15/34 and field 40 are `reserved`.
   - **Wire compatibility is NOT claimed for round 2.** This round iterates
     shape ahead of contract freeze; the reservations exist so that if the
     reshape is accepted, no retired number is ever reused for something with
     different semantics.

3. **`async-bubble.proto` is rewritten** (1 declaration → 21 in round 2, → 24
   in round 3).
   - Round 1's `AsyncNoticeItem` (a resolved sentence about detached work) is
     **gone**, replaced by `AsyncBubble`: `{ id, origin_tool_use_id,
     parent_bubble_id, label, started_at_ms, liveness, oneof kind }`.
   - **No `skill` arm**: skill invocation is a synchronous card, never
     detached work. Noted as reserved-for-future in a comment rather than
     pre-declared.
   - `AsyncAgentUpdate` carries `repeated AgentEmission` — the *same* message
     the top-level feed carries. That identity is the point of the round.
   - Journal rows are typed `{ label, detail, oneof status }`; byte spools
     append verbatim text with an explicit `from_offset` so a gap is
     detectable.
   - `AsyncLiveness` = `oneof { live, settled }`, and `AsyncSettled` carries
     `oneof outcome { done, error, killed }`, so "settled without an outcome"
     is unrepresentable.
   - `AsyncFold { dropped_before, tail_cap }` makes the existing 200-item tail
     cap a resolved daemon fact instead of per-frontend policy.

4. **`response-bubble.proto` and `tool-call.proto` stop being thin.**
   - `response-bubble.proto` gains `AgentResponse` (assistant message +
     `ResponseUsageStamp`) and `AgentThinking`.
   - `tool-call.proto` gains `AgentToolCall`, `AgentToolResult`,
     `AgentToolOutcome` — and with them `spawned_bubble_id`, the daemon's
     published classification verdict that retires the client-side identity
     ladder.

5. **`frame.proto` gains two things**: `FrontendFrame.async_bubble_delta = 20`
   and `StateSnapshot.async_bubbles = 11`.

6. **One round-1 open question is closed by construction.** The README used to
   record `ResponseUsageStamp` riding the `ConversationItem` envelope because
   the assistant arm was a `data.v1` message this package must not amend.
   `AgentResponse` wraps that message, so the stamp now sits on the thing it
   stamps and field 40 is retired.

### One deliberate carve-out, flagged for the reviewer

`AgentResponse.body` carries `ApiAssistantMessage` **with its thinking blocks
stripped**; thinking is emitted as its own `AgentThinking` emission. Both
halves are needed — without the strip, a frontend rendering both arms draws
the reasoning twice; without the arm, a frontend reaches into a durable
message's content array to find the grey block, which is precisely what both
frontends do today. It is the one place in this round where a durable
`data.v1` message is carried non-verbatim. If that is the wrong trade, the
alternative is to drop `AgentThinking` and let every renderer keep splitting
content blocks itself.

## Round 3 changes (async kind arms)

Round 3 touches `async-bubble.proto` only, and only its kind vocabulary. The
round-2 `spool` arm was the generic case — "bytes from something" — and it
absorbed both the known background-shell case and every spawn the classifier
did not recognize, discarding the tool's name on the way in. Round 3 splits
it, so that every arm names a producer the daemon can point at.

1. **The kind oneof becomes four arms.**
   - `AsyncAgentBubble agent = 10` — unchanged.
   - `AsyncWorkflowJournal journal = 11` — renamed from `AsyncJournalBubble`
     so the message says what it is: the Workflow tool's run journal, one row
     per `journal.jsonl` step, with no other producer. Its rows and step
     states rename to match (`AsyncWorkflowJournalRow`,
     `AsyncWorkflowStepRunning` / `Done` / `Failed`).
   - `AsyncShellBubble shell = 12` — a backgrounded shell command, carrying
     `string command` so the bubble header shows the command line rather than
     a task id.
   - `AsyncUnclassifiedBubble unclassified = 13` — a spawn whose tool the
     daemon does not recognize, carrying `string tool_name`. Its comment
     states that it is a contract arm a renderer must draw, not a fallback it
     may ignore, and that an unrecognized tool is never guessed into another
     kind.

2. **A shell's settle path is grounded in its exit status.** `AsyncSettled`
   gains `AsyncShellExit shell_exit`, a **sibling** of the `oneof outcome`
   rather than a member of it.
   - Present for work that is a process, absent for work with no exit status.
   - The daemon still resolves `done` / `error` from the code, so no client
     maps exit statuses; the code rides along so a card can show "exited 137"
     instead of an unexplained red dot.
   - Sibling rather than inside `done`/`error` because a reader would
     otherwise check two arms for one field, and because making the exit
     status *be* the outcome would leave exit-less work unable to say how it
     ended. `settled`-without-an-outcome remains unrepresentable.

3. **Byte-append messages are SHARED, not duplicated.** `shell` and
   `unclassified` are distinct arms of `AsyncBubbleUpdate` carrying the same
   `AsyncOutputAppend`; both bubble bodies carry the same `AsyncOutputSpool`
   (text + delivery cursor). The two kinds differ in what they ARE, not in how
   their output arrives, so there is no axis along which duplicated append
   messages could evolve apart — only a gap-detection rule that would then
   have to be kept identical in two places.

4. `AsyncBubbleUpdate`'s oneof becomes `agent = 10`, `journal = 11`,
   `shell = 12`, `unclassified = 13`, `liveness = 14`.

5. **Open decision 12 (the thinking arm) is untouched** and remains open.

Round 3 also carries two changes outside the async family:

6. **`QueueEntry`'s three hold fields fold into a `oneof hold`.**
   - `shutdown_hold` / `keep_alive_hold` / `revival_hold` become
     `shutdown = 7` / `keep_alive = 8` / `revival = 9` inside the oneof.
   - They are mutually exclusive session conditions, and the plain-field shape
     let all three be set at once — an entry with two different sets of exits
     and two different bubbles to render. The oneof makes that unrepresentable
     rather than merely unlikely.
   - **Field numbers are unchanged**, so nothing needs reserving; only the
     field names shorten (the `_hold` suffix is redundant inside `hold`).
   - Each arm keeps the substance of its old per-field comment: which
     affordances exist, and what releases the hold.
   - No arm set continues to mean the ordinary case — a turn is running, and
     the classifier decides delivery.

7. **`composer.proto` is renamed `prompt-queue.proto`.**
   - The primary deployment passes `?composer=0` — Emacs owns the input box —
     so the file's rendered component is not a composer at all; it is the
     queued-prompt strip.
   - What the file actually covers is the prompt-intake lifecycle: submit →
     classify → hold → deliver / force / cancel, plus interrupt. The new name
     says that.
   - Every cross-file import is updated (`frame.proto` was the only importer).
     Declaration contents are unchanged apart from item 6.

## Round 4 changes (de-sessioning, durable off the wire, the failure vocabulary)

Round 4 is three cuts to the webapp-facing surface, each done the same way:
inventory the real use sites first, then cut with a remediation note. The
complete field-by-field remediation table is **Part 4 of
`../SKETCH-figma-idl.md`** and is the deliverable for this round; what follows
is the shape summary.

### 4a. Session identity leaves the rendering surface

1. **A frontend's vocabulary loses "session".** Every per-workspace push now
   carries `string fence` — an opaque staleness token compared **byte-wise**
   and never parsed. Current-vs-stale is the only concept a renderer needs.

2. **`WorkspaceState.fence = 19` is the authority.** WorkspaceState remains the
   sole identity authority; the fence is its projection, re-minted whenever the
   owning session or controller generation rotates.

3. **Nine pushes fenced**: `ConversationDelta`, `TypingDelta`,
   `SessionInitView`, `QueueView`, `TokenBreakdownView`, `TaskCatalog`,
   `AsyncBubbleDelta`, `HeartbeatView`, `ProgressView`. Each reserves its old
   `session_id = 2`.

4. **`ResyncCmd` echoes one token**: `session_id` + `controller_generation_id`
   (two identities a client had to hold in agreement) become `fence = 4`.

5. **Three host/webapp splits forced** — flagged because each is a message that
   served both frontends:
   - `SessionView` → stays **host surface**, session-bearing, for Emacs, which
     genuinely manages sessions. Its webapp-facing jobs decompose into fenced
     component views: `WorkspaceGateView` (new, gate-revival.proto) for the
     hibernation gate, `TopbarView` for the identity line,
     `TokenBreakdownView` for economics, `FailureCardView` for a terminal
     session's account.
   - `SessionHealthView` / `SessionHealthCmd` → **host surface**, keep
     `session_id`; the topbar's rendered connectivity is `TopbarConnectivity`,
     already resolved and naming nothing.
   - `lifecycle.proto` → whole file marked **host surface**.

6. **`QueueEntryRevivalHold.session_id` removed outright** — it reached the DOM
   only as a debug attribute.

### 4b. The durable evidence layer leaves the push surface

7. **Three fields removed, nothing renamed**: `ConversationItem`'s
   `token_utilization = 36` and `turn_accounting = 37`, and
   `SessionView.token_utilization = 22`. All reserved.

8. **Resolved replacements**: `FooterAccountingCell` (new, footer.proto) with
   `oneof verdict {complete, incomplete, invalid}` and problems as
   display-ready phrases; `TopbarView.accounting_line` (a resolved string);
   `TokenBreakdownView` for session and per-model economics;
   `ResponseUsageStamp` for a response's corner.

9. **`durable.proto` is untouched structurally** — every message, name, package
   and field number is as it was, so persisted rows decode byte-identically.
   Its header now documents it as persistence-only.
   - The finding that made a rename unnecessary to even consider: no
     `frontend.v1` message is ever `Any`-wrapped and persistence is binary
     protobuf into BLOB columns with **no type-name discriminator**, so the
     constraint is field-number stability, not name stability.

10. **Achieved invariant**: nothing imports `durable.proto`, and no message
    outside it names a type declared in it. Verified mechanically.

### 4c. The failure vocabulary

11. **`ErrorClass` is deleted.** A failure's class is a condition, and the class
    was also the only thing any renderer branched on. It is now carried by the
    arm: each `FailureKind` arm belongs to exactly one side and says so.

12. **`SystemFailureItem` is replaced by `FailureKind`** — one `oneof kind` of
    58 dedicated messages, each with the typed fields its call sites actually
    carry. 50 arms come from the daemon's `errclass` registry; 8 are the
    client-local kinds.

13. **`errors.proto` becomes a VOCABULARY file**, like `tokens.proto`. Surface
    props live with their surfaces:
    - `FailureCardView` + `FailureCardRef` → new **failure-card.proto**
    - `FooterFailureRow` → **footer.proto**
    - `RosterNotice` → **sidebar.proto**
    - `CommandAck.failure` (kind) + `failure_card` (ref) → **frame.proto**
    - `AsyncOutcomeError` → **async-bubble.proto** (already its own)

14. **`resolved_at_ms`'s magic zero becomes
    `oneof lifecycle {open, resolved, terminal}`** — and `terminal` is a state
    the old field could not express.

15. **Client-local failures are arms of the same oneof**, numbered from 100 up,
    with the producer split stated on the message and enforced by the number
    range. One vocabulary, one renderer.


## Round 5 changes (pre-freeze orchestrator audit)

A final audit of the whole suite against the three landed principles (no
session awareness, oneof pattern, figma→idl), performed by the orchestrator
before freeze. Eight findings, all remediated in place:

1. **The three de-sessioning replacement views had no transport.** `TopbarView`,
   `TokenBreakdownView` and `WorkspaceGateView` were created in round 4 as the
   webapp's substitutes for host-surface `SessionView`, but no `FrontendFrame`
   arm and no `StateSnapshot` field carried them — the webapp literally could
   not receive them. Added `FrontendFrame.topbar = 21` /
   `token_breakdown = 22` / `workspace_gate = 23` and
   `StateSnapshot.topbars = 12` / `token_breakdowns = 13` /
   `workspace_gates = 14`, plus the `tokens-menu.proto` import.

2. **`TopbarView` gains `fence = 8`** — it is a per-workspace push and carries
   the staleness fence exactly as the round-4 nine do.

3. **`QueueClassification` (enum) → `oneof classification`** on `QueueEntry`,
   arms `pending = 10` / `interject = 11` / `hold_for_turn_end = 12` /
   `error = 13`. The verdict was a state carried as an enum beside two plain
   fields (`rationale`, `accepted`) that were meaningless in most states;
   each arm now carries its own facts (`rationale` on interject/hold,
   `accepted` on hold, `detail` on error). `QueueEntry` reserves 4/5/6.

4. **`TaskEntry.kind` and `.status` (free strings) → oneofs.** Kind arms
   mirror `AsyncBubble`'s vocabulary (agent/workflow/shell/unclassified, the
   last carrying `tool_name`); status arms are the six lifecycle endings as
   empty messages. `TaskEntry` reserves 2/4.

5. **`WorkspaceState.controller_generation_id` annotated HOST SURFACE** — a
   controller-generation identity on a shared message, un-annotated; the fence
   is its renderer-facing projection.

6. **The `SessionView` strip rule stated on the transport.**
   `StateSnapshot.sessions` and the `FrontendFrame.session_view` arm now say
   the daemon strips them from every GUI client, matching the precedent the
   host-only fields already document.

7. **`lifecycle.proto`'s host-surface header gains the `ShutdownScheduleView`
   carve-out** (the webapp renders drain progress from it), and
   `ShutdownHold.session_id` is annotated HOST SURFACE.

8. **`gate-revival.proto`'s stale `SessionView.hibernation` reference** (the
   webapp gate source round 4 replaced) now points at `WorkspaceGateView`;
   `footer.proto`'s detached `FooterAccountingCell` doc comment was re-attached
   to its message.

**Deliberate residue, examined and left:** `RenderState`,
`SessionConnectivity`, `SessionStatus` and `BackfillState` remain enums — they
are legacy live-wire vocabulary coupled to the SSM's append-only state log and
the host surface, and every webapp rendering of them is already replaced by
resolved views (`FooterPhase`, `TopbarConnectivity`); converting them is
daemon/host churn with no webapp-surface payoff and is deferred to a later
approval-gated pass. `ConversationSource` (provenance), `SessionCommand`
(command identity), `ResumeMode` (caller intent), `CompactionScope` (a chosen
scope), and `ClientLogLevel` (severity) are non-state closed sets and stay
enums per the doctrine's carve-out. `TaskCatalog`'s continued existence beside
the async-bubble family is flagged as an implementation-phase question (the
two describe the same dispatched work; the catalog may retire once
`spawned_bubble_id` routing lands), surfaced to the orchestrator's proto-change
gate rather than decided here.

## Completeness accounting

**181 declarations relocated across 23 files, 127 new (as of round 5).**

Round 5 retires `QueueClassification` (relocated 182 → 181) and adds 14 new
messages (4 queue-classification arms, 4 task kinds, 6 task statuses), so the
round-4 table below is superseded by: `prompt-queue.proto` 14 declarations,
`tool-call.proto` 15, total 308 = 181 relocated + 127 new.

Round-4 accounting, kept for the audit trail:

**182 declarations relocated across 23 files, 113 new, 5 additive fields.**

Round 4 is the first round to RETIRE a relocated declaration rather than move
it: `ErrorClass` and `SystemFailureItem` are deleted outright, so the relocated
count drops from 184 to 182. Every other top-level declaration in
`frontend.proto` still lands in exactly one file.

Every top-level `message` and `enum` in `frontend.proto` lands in exactly one
file. Nothing is dropped, nothing is duplicated.

| File | Declarations | New | Δ vs round 3 |
|---|---:|---:|---:|
| `sidebar.proto` | 33 | 1 | +1 |
| `agent-emission.proto` | 1 | 1 |  |
| `response-bubble.proto` | 3 | 3 |  |
| `user-bubble.proto` | 0 | 0 |  |
| `async-bubble.proto` | 24 | 24 |  |
| `tool-call.proto` | 5 | 3 |  |
| `permission-card.proto` | 1 | 0 |  |
| `topbar.proto` | 8 | 2 |  |
| `errors.proto` | 67 | 60 | +58 |
| `failure-card.proto` | 5 | 5 | +5 |
| `footer.proto` | 12 | 7 | +5 |
| `tokens-menu.proto` | 3 | 3 |  |
| `prompt-queue.proto` | 11 | 0 |  |
| `gate-revival.proto` | 11 | 3 | +3 |
| `slash-menu.proto` | 3 | 0 |  |
| `feed.proto` | 6 | 1 |  |
| `merge.proto` | 9 | 0 |  |
| `state.proto` | 9 | 0 |  |
| `frame.proto` | 6 | 0 |  |
| `host.proto` | 15 | 0 |  |
| `lifecycle.proto` | 16 | 0 |  |
| `durable.proto` | 44 | 0 |  |
| `tokens.proto` | 3 | 0 |  |
| **total** | **295** | **113** | **+72** |

295 − 113 new = **182 relocated** = the 184 top-level declarations in
`frontend.proto` minus the two round 4 retires (`ErrorClass`,
`SystemFailureItem`).

> **Correction to the round-1 accounting.** The round-1 summary line read
> "196 declarations, 12 new". Both figures were wrong: its own per-file table
> summed to 194 and 10, and 194 − 10 = 184 was correct. The table was right
> and the summary was not. Round 2's figures are counted mechanically off the
> files.

### Chosen field numbers for the additive changes

Real next-free numbers, read off the existing messages:

- `ProgressView.phase = 23`, `ProgressView.merge_chip = 24`
  (fields run 1–22, with 13 and 17 reserved).
- `ConversationItem.async_bubble = 38`, `ConversationItem.compaction_summary = 39`
  (fields ran 1–37, with 16, 17 and 18 reserved).
- `ConversationItem.agent = 5` (round 2) takes the lowest free packaging-side
  number rather than a high one, because it is now the message's principal
  arm and no longer one of many.
- `FrontendFrame.async_bubble_delta = 20` (arms run 1–19, with 8 reserved);
  `StateSnapshot.async_bubbles = 11` (fields run 1–10).

### Manifest gaps resolved

The sketch's manifest is explicit for most declarations and wins wherever it
speaks. These were not named and were placed by fit:

- ~~**`ResponseUsageStamp` rides `ConversationItem`, not the assistant
  arm.**~~ **CLOSED in round 2.** The round-1 reasoning was sound — the
  assistant arm was `agentshim.data.v1.ApiAssistantMessage`, which this
  package must not amend — but round 2 removes the constraint rather than
  working around it: `AgentResponse` (in `response-bubble.proto`) wraps the
  durable message and carries `usage_stamp` beside it, so the stamp sits on
  the thing it stamps and `ConversationItem` field 40 is retired.
- **`user-bubble.proto` is declaration-free.** The user-prompt arm is
  `agentshim.data.v1.ApiUserMessage`; this package owns no user-bubble message
  today. The file is kept so the component has a named home the moment it
  acquires resolved props. Round 2 does not change this: the user prompt is
  not agent output, so it stays a bare `ConversationItem` arm.
- ~~**`tool-call.proto` gets no view arms.**~~ **CLOSED in round 2.** The card
  now owns `AgentToolCall` / `AgentToolResult` / `AgentToolOutcome`, which
  wrap the `data.v1` blocks rather than replacing them and add the resolved
  `spawned_bubble_id` the card needs in order to draw its detached work.
- `SessionConnectivity`, `SessionStatus` → `state.proto` (the manifest's
  "connectivity/status enums").
- `ResumeMode` → `lifecycle.proto` (`CreateSessionCmd`'s own enum).
- `CompactionScope` → `gate-revival.proto` (`ReviveCompactFirst`'s own enum;
  it landed on master in `c209a6b7` while this artifact was being written).
- `TokenCacheCreation`, `TokenServerToolUse`, `TokenOutputDetails`,
  `TokenResponseTiming`, `TokenTimingTotals`, `SessionTokenUtilization`,
  `AgentTokenUtilization`, `ModelTokenUtilization` → `durable.proto`. Each is
  a sub-shape of `VendorTokenUsage` or `TokenUsageTotals` and is persisted
  with them, so it belongs to the frozen evidence layer rather than to the
  canonical economics vocabulary in `tokens.proto`.
- `DaemonHealthCmd`, `SessionHealthCmd` → `topbar.proto`, beside the
  `DaemonHealthView` / `SessionHealthView` the manifest already files there
  ("health views + cmds").
- `HostActionCompletedCmd`, `HostWorkspaceCreateFailed` → `host.proto` (the
  "HostAction family").
- `ShutdownScheduleView` and its `Idle`/`Draining`/`ShutdownHold`/
  `ShutdownHoldTurn`/`ShutdownHoldTasks` messages → `lifecycle.proto` (the
  manifest's "schedule/hold messages").

## Syntax check

The suite compiles as a set. Staged at its intended production path alongside
unmodified copies of `agentshim/core` and `agentshim/data`:

```
protoc --proto_path=<staging> \
       --descriptor_set_out=/tmp/draft.desc --include_imports \
       agentshim/frontend/v1/*.proto
```

Result: **clean**, no errors and no unused-import warnings. No bindings were
generated into this repository.
