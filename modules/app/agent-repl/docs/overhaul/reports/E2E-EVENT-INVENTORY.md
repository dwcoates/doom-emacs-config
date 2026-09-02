# E2E event-fabrication inventory (Step 0 of E2E-SIDECAR-PLAN.md)

Read-only inventory of every `daemon/e2e` call site that fabricates state
instead of driving the real fake SDK: `sidecar*Event(...)` helper calls,
`ingestTranscriptAsSidecar(...)` calls, and direct `protocolv1.Event{...}`
(or `ShimHello`/`ShimReady`/`Ack`) construction written to the store or the
daemon's shim socket. Produced by reading source only — `daemon/e2e` does not
compile right now (the five-way merge has not landed), and no test was run.

Method: `grep -rn` for `sidecar[A-Za-z]*Event(`, `ingestTranscriptAsSidecar`,
`protocolv1\.Event{`, and `\.write(` across the whole `daemon/e2e` package,
followed by full reads of every offending file and every helper definition.
One false positive was excluded: `mergedispatch_e2e_test.go`'s `d.write(...)`
writes a merge-dispatch-queue fixture file to disk, unrelated to the store.

**Totals: 26 files, ~90 fabricating call sites** (not counting the helper
*definitions* themselves, which live in 10 of those 26 files). Breakdown by
helper kind:

- `sidecar*Event(...)` (incl. `sidecarUserLineEvent`, `sidecarAssistantLineEvent`,
  `sidecarClearEvent`, `sidecarCompactEvent`, `sidecarLineEvent`,
  `sidecarAssistantUsageEvent`): ~55 call sites across 19 files.
- `vendorLineEvent(...)` wrapping `asyncToolCallLine` / `asyncToolResultLine` /
  `sidechainResponseLine` (detached-work/subagent family, defined in
  `detachedspecharness_test.go`): 12 call sites across 4 files.
- `ingestTranscriptAsSidecar(...)`: 3 call sites (2 callers + the rewind path
  inside `rewindByKeepAlivePing`) across 3 files.
- File-local one-off `protocolv1.Event{...}` constructors (`slashShapeAEvent`/
  `slashShapeBEvent`, `vendorStatusEvent`, `storeTaskStartedEvent`,
  `storedAssistantEvent`, `degradedStateEvent`): ~15 call sites across 6 files.
- Raw wire-protocol fabrication below the store (a hand-rolled shim speaking
  `ShimHello`/`DaemonHello`/`Event`/`ShimReady`/`Ack` directly on the daemon's
  shim socket): 1 call site in `promptreceiptreplay_e2e_test.go`.

---

## Per-file inventory

### clearcompact_e2e_test.go

Defines `storeProducer`/`storeSocket`/`dialStoreProducer` (the store-dialing
plumbing every other file in this list reuses), plus `sidecarClearEvent` and
`sidecarCompactEvent`.

- `sidecarClearEvent(vendorSessionID, lineUUID)`: `protocolv1.Event{Plane:FILE,
  Class:PERSISTENT, DedupKey:"clear:"+lineUUID, Payload:ContextCleared{}}` (an
  empty message — mirrors `handler.clearedEvent`).
- `sidecarCompactEvent(vendorSessionID, boundaryUUID, summary)`:
  `protocolv1.Event{Plane:FILE, Class:PERSISTENT, DedupKey:"compact:"+boundaryUUID,
  Payload:ContextCompacted{Trigger:MANUAL, PreTokens:120_000, PostTokens:18_000,
  DurationMs:4_200, Summary:summary}}` (mirrors `handler.compactedEvent`).
- `TestE2EInjectedClearReachesFrontendAsArm32` (line 383): one `sidecarClearEvent`.
- `TestE2EInjectedCompactCarriesSummaryAsArm33` (line 403): one `sidecarCompactEvent`
  with a distinctive summary string, asserted verbatim on the frontend arm.
- `TestE2EReplayFloorsAtTheClear` (line 435): one `sidecarClearEvent`, used as the
  replay floor.
- `TestE2ELiveAndReplayAgreeFromTheFloor` (line 479): one `sidecarClearEvent`.
- `TestE2EDuplicateClearYieldsOneFrame` (lines 521-523): the SAME
  `sidecarClearEvent(vendorID, lineUUID)` written TWICE (identical dedup key,
  deliberately), then a `sidecarCompactEvent` sentinel. Tests store-level
  dedup/idempotency, not a vendor behavior — see "needs a ruling" below.

### detachedspecharness_test.go

Shared harness for the detached-work / subagent-routing family — no tests of
its own, but defines the fabrication primitives four other files call.

- `vendorLineEvent(t, vendorSessionID, line)` (line 44): same envelope as
  `sidecarLineEvent`/`sidecarUserLineEvent` — FILE plane, PERSISTENT,
  `Event_Vendor{anypb.New(line)}`, no dedup key.
- `asyncToolCallLine(uuid, toolUseID, toolName)` (line 60): an assistant
  `TranscriptLine` with one `ContentBlock_ToolUse{Id, Name:toolName}` and no
  `Input` (arguments never modeled). Used with `toolName="Bash"` and `"Task"`.
- `asyncToolResultLine(uuid, toolUseID, resultText, outcome)` (line 72): a user
  `TranscriptLine` with one `ContentBlock_ToolResult{ToolUseId, ContentString}`
  plus `UserLine.ToolUseResult = outcome` (the typed side-channel, not an API
  content block).
- `agentAsyncLaunchOutcome(agentID, description)` (line 91): a
  `ToolUseResult_AgentAsyncLaunch{IsAsync:true, Status:ASYNC_LAUNCHED, AgentId,
  Description}` — the outcome of a backgrounded `Task` (`Agent`) launch.
- `bashBackgroundOutcome(taskID)` (line 105): `ToolUseResult_Bash{Bash:{BackgroundTaskId:taskID}}`
  — the outcome of a backgrounded `Bash` call.
- `bashTaskOutcome(taskID, command, output, status, exitCode, exitSet)` (line 113):
  `ToolUseResult_TaskOutput{TaskOutput:{RetrievalStatus:SUCCESS,
  Task:LocalBashTask{TaskId, TaskType:"local_bash", Status, Description:command,
  Output, ExitCode, ExitCodeSet}}}` — the outcome of an explicit poll/retrieval
  against a backgrounded shell task.
- `sidechainResponseLine(uuid, parentUUID, sourceToolUseID, agentID, text)`
  (line 134): an assistant `TranscriptLine` with `LineEnvelope{Uuid, ParentUuid,
  IsSidechain:true, AgentId, SourceToolUseId}` and one text block — one
  utterance from a detached/dispatched subagent.
- `degradedStateEvent(vendorSessionID, component, reason, droppedCount,
  recovered)` (line 153): **not vendor-derived at all** — STREAM plane,
  PERSISTENT, `Event_DegradedState{DegradedState{Component, Reason,
  DroppedCount, Recovered}}`. This is what the **shim itself** would write to
  report its own connectivity health to the store — see "needs a ruling".

### detachedspooloffset_e2e_test.go

- `TestE2EAShellDetachedWorkAppendsAreContiguousThroughTheSnapshotCursor`
  (lines 47-64): five writes — `vendorLineEvent(asyncToolCallLine(..., "Bash"))`,
  `vendorLineEvent(asyncToolResultLine(..., bashBackgroundOutcome(taskID)))`,
  THREE poll retrievals in a loop (`bashTaskOutcome(taskID, "tail -f build.log",
  <growing output>, RUNNING, 0, false)` with outputs `"compiling\n"` →
  `"compiling\nlinking the whole thing\n"` → `"...\ndone\n"`, status RUNNING
  throughout, no exit code), then `sidecarUserLineEvent(...barrierPrompt)`.

### detachedworkanchor_e2e_test.go

- `TestE2EALaunchAnchorsItsDetachedWorkInTheFeedWithLiveLiveness` (lines
  31-35): `vendorLineEvent(asyncToolCallLine(..., "Task"))`, then
  `vendorLineEvent(asyncToolResultLine(..., agentAsyncLaunchOutcome(agentID,
  "anchor the work")))`, then `sidecarUserLineEvent(...barrierPrompt)`.

### detachedworksettle_e2e_test.go

- `TestE2EASettledShellDetachedWorkCarriesItsOutcomeAndExitStatus` (lines
  42-49): `vendorLineEvent(asyncToolCallLine(..., "Bash"))`,
  `vendorLineEvent(asyncToolResultLine(..., bashBackgroundOutcome(taskID)))`,
  `vendorLineEvent(asyncToolResultLine(..., bashTaskOutcome(taskID,
  "sleep 1 && echo done", "done\n", FAILED, exitCode:3, exitSet:true)))`
  (a poll reporting the backgrounded task finished with a deliberately
  nonzero exit and FAILED status), then `sidecarUserLineEvent(...barrierPrompt)`.

### durableresync_e2e_test.go

Defines `storedAssistantEvent`.

- `storedAssistantEvent(t, vendorSessionID, uuid, text)` (line 258): STREAM
  plane, PERSISTENT, `DedupKey:"assistant:"+uuid`,
  `Event_Vendor{ClaudeStreamMessage_Assistant{Uuid, Message:{Content:[text]}}}`
  — "what the stream plane persisted for one assistant reply," in the store's
  own envelope. This is an ordinary shape any turn produces; the fabrication
  is not about the shape, it is about seeding a store with history for a
  session that has **no live shim in this test at all** (see "needs a
  ruling").
- `TestAFrontendConnectingAfterADaemonBounceReceivesThePriorConversation`
  (lines 335-336): two `storedAssistantEvent` calls via `producer.write`.
- `TestServingADurableResyncNeverSpawnsAShim` (line 354): one.
- `TestADurableResyncRespectsTheClientsFromSeq` (lines 372-373): two.
- (Not fully re-read past line ~380, but the same `producer.write(storedAssistantEvent(...))`
  pattern repeats for the remaining resync-scope tests in this file per the
  earlier full-directory grep.)

### failurecardshape_e2e_test.go

- `TestE2EAWindowShapedFailureResolvesUnderItsOwnUUID` (line 42):
  `degradedStateEvent(vendorID, component:"shim-store-client",
  reason:"e2e_store_link_lost", droppedCount:7, recovered:false)` — opens a
  degradation window.
- same test (line 67): `degradedStateEvent(..., recovered:true)` — closes the
  same window under the same component/reason.

### hibernationharness_test.go

Shared hibernation/keep-alive harness. Defines `ingestTranscriptAsSidecar`,
`sidecarAssistantLineEvent`, `writeFixtureTranscript`, `readFixtureTranscript`.

- `ingestTranscriptAsSidecar(t, s, vendorSessionID)` (line 682): reads the
  truncated transcript copy the **daemon itself** wrote to disk after a
  rewind, and replays each line into the store as `sidecarUserLineEvent` /
  `sidecarAssistantLineEvent` — "the backfill the production shim-sidecar
  performs by tailing the new file, which the harness (running no sidecar)
  must perform itself" (the file's own doc comment). Calls
  `store.write(sidecarUserLineEvent(...))` (line 712) and
  `store.write(sidecarAssistantLineEvent(...))` (line 714) once per fixture
  line.
- `sidecarAssistantLineEvent(t, vendorSessionID, lineUUID, text)` (line 741):
  identical envelope to `sidecarUserLineEvent` (machinery_e2e_test.go) but for
  an assistant text line.

### keepaliveresidue_e2e_test.go

- `pingCompletedWithNothingWaiting` / `TestE2EAPromptAfterAnUnattendedPingStillRunsOnTheRewoundConversation`
  (line 105): `ingestTranscriptAsSidecar(t, s, next)` — same backfill-after-rewind
  pattern as `keepaliverewind_e2e_test.go`, for the "nobody was waiting behind
  the ping" rewind path.

### keepaliverewind_e2e_test.go

- `rewindByKeepAlivePing` (line 61): `ingestTranscriptAsSidecar(t, s, next)`
  — the file's own header states this explicitly: "The production sidecar
  tails the truncated copy and backfills its records into the NEW seq space;
  the harness runs no sidecar, so the backfill is replayed here."
- `TestE2EARewindFlipsTheConversationIdentityAndItsSeqMarksTogether` (line
  120): `store.write(sidecarClearEvent(s.vendorID, "e2e-rewind-floor"))` —
  an ordinary clear, used to give the pre-rewind row non-zero seq marks.
- Also note (out of the three named patterns, but same defect family):
  `writeDefaultRewindFixture` (line 86-97) hand-writes a vendor transcript
  fixture straight to disk via `writeFixtureTranscript` and the local
  `userLine`/`assistantLine` helpers — this is the OTHER prohibited pattern
  the plan document itself names ("must not write vendor JSONL either"). See
  "needs a ruling".

### machinery_e2e_test.go

Defines `sidecarUserLineEvent` (reused by ~14 other files) and the
`machineryContent` constant.

- `sidecarUserLineEvent(t, vendorSessionID, lineUUID, content)` (line 40):
  FILE plane, PERSISTENT, `Event_Vendor{TranscriptLine_User{Envelope:{Uuid},
  Message:{ContentString:content}}}`, no dedup key (store derives its own
  `uuid:` key).
- `TestE2EMachineryUserLineArrivesAsAnInterceptedCommand` (lines 98-99):
  `sidecarUserLineEvent(vendorID, "e2e-machinery-line", machineryContent)`
  where `machineryContent = "<command-message>compact</command-message>\n<command-name>/compact</command-name>\n<command-args></command-args>"`
  — the CLI's own slash-command bookkeeping, as a raw **user**-typed record —
  then `sidecarUserLineEvent(vendorID, "e2e-real-line", realPrompt)`, an
  ordinary prompt line.
- `TestE2EMachineryThatNamesNoCommandIsStillWithheld` (lines 143-144): the
  same pair, but with `unnamed = "<local-command-stdout>total 4\ndrwxr-xr-x</local-command-stdout>"`
  (no `<command-name>` at all — the WITHHELD-UNNAMED shape).
- `TestE2EMachineryIsClassifiedOnAReplayToo` (lines 166-167): the same pair
  again, feeding a replay assertion.

### mergewindow_e2e_test.go

Defines `mergeSkillCallLine` and `assistantProseLine` (calls `sidecarLineEvent`
from `skillbody_e2e_test.go` and `sidecarUserLineEvent` from
`machinery_e2e_test.go`).

- `mergeSkillCallLine(t, uuid, toolUseID, skill, args)` (line 28): an assistant
  `TranscriptLine` with one `ContentBlock_ToolUse{Id, Name:"Skill",
  Input:structpb{"skill":skill, "args":args}}` — a `Skill` tool call naming an
  arbitrary skill and argument string (used with `skill="create-or-update-workspace"`,
  `args` one of `"merge"` / `"create feat/thing"`).
- `TestE2EAMergeInvocationOpensAWindowThatFoldsAndSettles` (lines 117-120):
  `sidecarLineEvent(mergeSkillCallLine(..., "create-or-update-workspace",
  "merge"))`, `sidecarLineEvent(assistantProseLine(...inside prose...))`,
  `sidecarUserLineEvent(...takeBack...)`, `sidecarUserLineEvent(...barrierPrompt)`.
- `TestE2EAMergeWindowReturnsTheFeedToTheUserAfterItSettles` (lines 177-180):
  same four-call pattern with the take-back BEFORE the assistant reply.
- `TestE2EANonMergeVerbOfTheSameSkillOpensASkillDetachedWorkNotAMergeOne`
  (lines 243-245): the same `mergeSkillCallLine` call but with args
  `"create feat/thing"` — a non-merge verb of the same skill, to prove it
  opens a plain SKILL work rather than a MERGE work.

### phaseword_e2e_test.go

Defines `vendorStatusEvent`; reuses `sidecarCompactEvent` / `sidecarClearEvent`.

- `vendorStatusEvent(t, vendorSessionID, dedupKey, status)` (line 53): STREAM
  plane, PERSISTENT, `Event_Vendor{ClaudeStreamMessage_Status{StatusMessage{Status:status}}}`.
  The file's own header says this is injected because "the fake engine has no
  compaction and so emits no such status (fake-query.ts has no compaction path
  at all)" — **this comment is now stale**: `session.ts`'s `COMPACT` /
  `COMPACT_AUTO` scenarios already call `ctx.systemMessage("status",
  {status:"compacting"})` before the boundary. See the mapping below.
- `TestE2ECompactingPhaseWordOpensOnTheVendorStatusAndClosesOnTheEvent` (lines
  102, 108): `vendorStatusEvent(vendorID, "e2e-status-compacting", "compacting")`
  then `sidecarCompactEvent(vendorID, "e2e-compact-phaseword", "what the
  discarded history said")`.
- `TestE2EClearingPhaseWordOpensOnDispatchAndClosesOnTheEvent` (line 130):
  the OPENING edge is driven for real (`/clear` over the frontend command
  surface); only the CLOSING edge is fabricated: `sidecarClearEvent(vendorID,
  "e2e-clear-phaseword")`.

### promptreceiptreplay_e2e_test.go

Defines `storedAssistantEvent`-adjacent usage (reuses `durableresync_e2e_test.go`'s
`storedAssistantEvent`) AND, separately, a fully hand-rolled fake shim
(`acceptOnceShim`).

- `acceptOnceShim.run` (lines 187-241): dials the daemon's **real shim
  socket** directly and speaks the wire protocol itself — `wire.WriteAny(conn,
  &protocolv1.ShimHello{...})`, then on receiving `DaemonHello` writes an
  ephemeral `protocolv1.Event{Plane:STREAM, Class:EPHEMERAL,
  Payload:Event_SessionStarted{...}}` (line 213) and a `protocolv1.ShimReady{...}`
  (line 225), then on `SubmitPrompt` writes exactly one `protocolv1.Ack{...}`
  (line 236) and disconnects — producing **zero durable events**. This
  fabricates below the layer the fake SDK (a TypeScript process inside a real
  shim) can reach at all: it never runs a shim process, real or fake. See
  "needs a ruling".
- `producer.write(storedAssistantEvent(...))` (line 593): same shape and same
  "no live shim" harness pattern as `durableresync_e2e_test.go` /
  `conversationpage_e2e_test.go`.

### revivalhold_e2e_test.go

- `TestE2EALandedCompactionReleasesTheRevivalHold` (line 173):
  `store.write(sidecarCompactEvent(revivedVendorID(t, s), "e2e-revival-hold-compact-1",
  "the conversation so far"))`.

### revive_e2e_test.go

Defines `revivedVendorID`.

- `TestE2EACompletedCompactionReleasesTheGatedPrompt` (line 213):
  `sidecarCompactEvent(revivedVendorID(t, s), "e2e-revival-compact-1", "the
  conversation so far")`.
- `TestE2EACompactionThatNeverLandsKeepsTheGateShut` (line 250):
  `sidecarClearEvent(revivedVendorID(t, s), "e2e-revival-gate-sentinel")` —
  used purely as an ordering sentinel unrelated to the test's real subject
  (compaction never landing).

### rotation_e2e_test.go

- `TestE2EClearUnderTheRotatedIdentityReachesTheFrontend` (line 272):
  `sidecarClearEvent(rot.next, "e2e-rotated-clear-1")` — an ordinary clear
  (same helper as clearcompact_e2e_test.go) but written under the ROTATED
  vendor session id (`rot.next`), to exercise the post-rotation seq space.
  The `!rotate` turn itself IS already driven for real through the fake SDK;
  only the post-rotation clear injection is fabrication.

### rotationrebase_e2e_test.go

- `TestE2ERebasedResyncAfterARotationRendersTheClear` (line 65):
  `sidecarClearEvent(rot.next, "e2e-rebased-clear-1")`.
- `TestE2ERebasedResyncPushesNoFailureCard` (line 133):
  `sidecarClearEvent(rot.next, "e2e-rebased-clear-2")`.

### rotationresync_e2e_test.go

- `TestE2EResyncFromARetiredSpaceMarkIsREFUSED` (line 156):
  `sidecarClearEvent(rot.next, "e2e-retired-mark-clear-1")`.
- `TestE2ETheReAnchorAfterARefusedMarkIsABoundedTailPage` (line 191):
  `sidecarClearEvent(rot.next, "e2e-retired-mark-clear-2")`.

### shutdownschedule_e2e_test.go

Defines `storeTaskStartedEvent`.

- `storeTaskStartedEvent(vendorSessionID, taskID)` (line 92): STREAM plane,
  PERSISTENT, `DedupKey:"task-started:"+taskID`,
  `Event_TaskStarted{TaskId, Kind:AGENT, Description:"e2e drain-lease
  background task"}` — "what the shim's converter forwards when the vendor
  starts a subagent," used purely to inflate `live_task_count` so the
  scheduled-shutdown drain lease has something in its hold list. The file's
  own comment says it is injected because "the `--fake` engine has no
  subagents: its only detached-work path emits a task-notification... a
  different arm entirely" — **also stale**: `subagents.ts` now has a full
  `SUBAGENT_DETACHED` / `SUBAGENT_DETACHED_LIVE` family that calls
  `ctx.startTask(...)` for real.
- `TestE2E...` (line 252): `store.write(storeTaskStartedEvent(vendorID, "e2e-drain-task-1"))`.
- (line 292): `store.write(storeTaskStartedEvent(vendorID, "e2e-drain-task-both"))`.

### skillbody_e2e_test.go

Defines `sidecarLineEvent` (reused by mergewindow_e2e_test.go),
`skillCallLine`, `skillResultLine`, `metaUserLine`.

- `sidecarLineEvent(t, vendorSessionID, line)` (line 33): same envelope as
  `sidecarUserLineEvent`, generic over any `*datav1.TranscriptLine`.
- `skillCallLine(uuid, toolUseID)` (line 49): assistant `TranscriptLine`,
  `ToolUseBlock{Id:toolUseID, Name:"Skill"}` (no input).
- `skillResultLine(uuid, toolUseID)` (line 59): user `TranscriptLine`,
  `ToolResultBlock{ToolUseId, ContentString:"Launching skill: demo"}`.
- `metaUserLine(uuid, parentUUID, text)` (line 74): user `TranscriptLine`,
  `LineEnvelope{Uuid, ParentUuid, IsMeta:true}`, one text block — the skill's
  document, joined back by `ParentUuid`/the caller's ordering (real production
  joins by `sourceToolUseID`, per the SKILL scenario's own comment; this
  harness's `metaUserLine` param is named `parentUUID` but is used with the
  RESULT line's uuid as the join key, not a `sourceToolUseID` field. Worth
  the rewrite author double-checking which field the real converter reads.)
- `TestE2ESkillBodyReachesTheFrontendAsItsOwnArm` (lines 97-99): `skillCallLine`
  + `skillResultLine` + `metaUserLine`, body text = `e2eSkillBody` constant
  (`"Base directory for this skill: /Users/x/.claude/skills/demo\n\n# Demo
  skill\n\nDo the thing."`).
- `TestE2ESkillBodyNeverReachesTheFrontendAsAUserTurn` (lines 128-131): same
  triple plus a trailing `sidecarUserLineEvent` real prompt.
- `TestE2EANonSkillMetaRecordIsWithheldEntirely` (lines 155-158): same triple
  but the meta line's text is an ordinary continuation nudge, not a skill
  body, plus a trailing real prompt.

### slashdurability_e2e_test.go

Defines `slashShapeAEvent`, `slashShapeBEvent`, `slashVendorEvent`,
`slashShapeAContent`; reuses `sidecarCompactEvent`.

- `slashShapeAEvent(t, vendorSessionID, lineUUID, promptID, content)` (line
  64): FILE plane, PERSISTENT, `TranscriptLine_User{Envelope:{Uuid, PromptId},
  Message:{ContentString:content}}` — content built by `slashShapeAContent`
  as `<command-message>{name}</command-message>\n<command-name>{literal}</command-name>\n<command-args></command-args>`
  (the SAME "Shape A" as `machinery_e2e_test.go`'s `machineryContent`, but
  parameterized and carrying a `PromptId`).
- `slashShapeBEvent(t, vendorSessionID, lineUUID, content)` (line 84): FILE
  plane, PERSISTENT, `TranscriptLine_System{Envelope:{Uuid, IsMeta:true},
  Subtype:LocalCommandLine{Content:content}}` — "Shape B": a `system`/
  `local_command` record. This is byte-for-byte what `SLASH_LOCAL`'s
  `ctx.files.transcript.append({type:"system", subtype:"local_command", ...})`
  already writes.
- Nine call sites in total (from the full-directory grep): lines 215, 281,
  325, 358, 398, 407, 416, 450, 451, 482, 536, 538 mix `slashShapeAEvent`,
  `slashShapeBEvent`, and `sidecarCompactEvent` sentinels across tests
  covering compaction-vs-slash-record ordering, ephemeral vs. durable
  classification, round-trips, and quoted/paired shapes (Part 5, tests 1-4
  and 7-10 of `FROZEN-slash-command-durability.md`).

### stalefencerefusal_e2e_test.go

- `TestE2EAResyncCarryingAStaleFenceIsRefusedWithoutReplay` (line 33):
  `sidecarUserLineEvent(vendorID, "e2e-stale-fence-history", history)` — a
  single ordinary user line, used only to give the workspace SOME history a
  wrongly-served replay could carry. No special shape; a real prompt line
  would do exactly as well.

### subagentrouting_e2e_test.go

- `TestE2EASubagentResponseIsRoutedToItsDetachedWorkAndNeverToTheFeed` (lines
  50-56): `vendorLineEvent(asyncToolCallLine(..., "Task"))`,
  `vendorLineEvent(asyncToolResultLine(..., agentAsyncLaunchOutcome(agentID,
  "investigate the routing defect")))`, `vendorLineEvent(sidechainResponseLine(...,
  parentUUID:"e2e-subagent-launch", sourceToolUseID:toolUseID, agentID,
  text:"e2e-subagent-utterance: this belongs inside the work"))` — the
  detached subagent's own utterance — then `sidecarUserLineEvent(...barrierPrompt)`.

### tokenutilization_e2e_test.go

Defines `sidecarAssistantUsageEvent`.

- `sidecarAssistantUsageEvent(t, vendorSessionID, lineUUID, messageID, agentID)`
  (line 220): FILE plane, PERSISTENT, `Event_Vendor{TranscriptLine_Assistant{
  Envelope:{Uuid, SessionId, AgentId:agentID}, Message:{Id:messageID,
  Model:"claude-opus-test", Content:[text "historical response"],
  Usage:{InputTokens:100, OutputTokens:200, CacheReadInputTokens:800,
  CacheCreationInputTokens:75, CacheCreation:{ephemeral_5m:25, ephemeral_1h:50},
  ServerToolUse:{web_search:2, web_fetch:3}, ServiceTier:"priority",
  Speed:"fast", InferenceGeo:"us-east-1"}}}}`. Deliberately a FILE-plane-ONLY
  record with no paired STREAM-plane `message_start` — "the historical case
  that must retain usage without inventing a generation duration."
- `TestE2EHistoricalUsageIsExplicitlyUntimedAndDeduplicated` (lines 169-170):
  the same event object written TWICE (dedup test).
- `TestE2ESessionViewAggregatesTimedAndUntimedActors` (line 271): one call,
  with `agentID="agent-child"` — a nested-subagent attribution.

### conversationpage_e2e_test.go

Reuses `durableresync_e2e_test.go`'s `storedAssistantEvent` and
`newBouncedHarness`/`dialFrontend`.

- `TestAColdOpenReceivesOnlyTheConversationsTail` (lines 100): six
  `producer.write(storedAssistantEvent(...))` calls in a loop, seeding six
  replies before paging for the tail.
- Six further tests (lines 121, 139, 158, 188-189, 209, 230) repeat the same
  `producer.write(storedAssistantEvent(...))` pattern to seed store history
  for cold-open / load-more / early-ack paging assertions, all against a
  harness with **no live shim**.

---

## Consolidated PROPOSED fake-SDK additions

1. **`slash-shape-a`** (new scenario, or a new option on `SLASH_LOCAL` in
   `session.ts`). Writes a "user"-typed `TranscriptLine` whose content is
   `<command-message>{name}</command-message>\n<command-name>{literal}</command-name>\n<command-args></command-args>`,
   parameterized by command name (e.g. `compact`), plus an "unnamed" variant
   that writes only `<local-command-stdout>...</local-command-stdout>` with no
   `<command-name>` at all (the WITHHELD-UNNAMED shape). Retires:
   `machinery_e2e_test.go`'s `machineryContent`/`unnamed` call sites,
   `slashdurability_e2e_test.go`'s `slashShapeAEvent` call sites (also needs a
   `PromptId` knob).

2. **Rewire to existing `SLASH_LOCAL` (`!slash`).** No new scenario code
   needed — it already writes the exact "Shape B" `system`/`local_command`
   isMeta record `slashShapeBEvent` fabricates. Retires:
   `slashdurability_e2e_test.go`'s `slashShapeBEvent` call sites.

3. **Rewire to existing `ROTATE` (`!rotate`).** Already produces
   `SessionIdentityRotated` + `AgentUpdate.context_cut(ContextCleared)` end to
   end. Retires: every `sidecarClearEvent` call site (clearcompact,
   keepaliverewind, rotation_e2e, rotationrebase_e2e, rotationresync_e2e,
   phaseword's closing edge, revive, revivalhold) — except
   `TestE2EDuplicateClearYieldsOneFrame`, see "needs a ruling". For the
   rotation-family tests that need a clear "under the rotated identity," drive
   a SECOND `!rotate` on the already-rotated session instead of fabricating.

4. **Rewire to existing `COMPACT` / `COMPACT_AUTO` / `COMPACT_FAILED`
   (`!compact` / `!compact-auto` / `!compact-failed`).** Already produce
   `status{compacting}` → `compact_boundary` (with trigger/tokens/duration) →
   `status{compact_result}`. Retires every `sidecarCompactEvent` call site
   AND `phaseword_e2e_test.go`'s `vendorStatusEvent` call (the test's own
   "the fake has no compaction" comment is stale). Needed addition: an
   optional `summary` override parameter on `COMPACT`, since several tests
   assert a distinctive per-test summary string rather than the scenario's
   fixed `"Conversation compacted"` metadata (`ContextCompacted.Summary` is
   currently derived from the record following the boundary, not exposed as a
   parameter).

5. **Rewire to existing `SKILL` (`!skill`).** Already emits the exact
   tool_use → ack → isMeta-document triple `skillbody_e2e_test.go` fabricates
   by hand. Needed addition: parameterize the skill name/args and the
   document body text (currently fixed to `"fake-skill"` /
   `"Base directory for this skill: /w/s/.claude/skills/fake-skill..."`), so a
   test can assert its own distinctive body text and, for
   `mergewindow_e2e_test.go`, so a test can name `skill="create-or-update-workspace"`
   with `args="merge"` / `args="create feat/thing"` instead of `"fake-skill"`.
   This is also the mechanism `mergewindow_e2e_test.go`'s `mergeSkillCallLine`
   needs — there is currently NO scenario that names an arbitrary skill.

6. **New option on `BASH_DETACH`, or a new `bash-detach-poll` scenario**
   (`shell.ts`). `BASH_DETACH` already covers backgrounding + incremental
   spool + completion notification, but none of `detachedworksettle_e2e_test.go`
   / `detachedspooloffset_e2e_test.go`'s explicit **poll/retrieval** tool call
   (a second tool_use that reads back `TaskOutput`/`LocalBashTask` with a
   status/exit code) exists in any scenario. Needs: after backgrounding, emit
   one or more explicit poll tool_use/tool_result pairs reporting
   RUNNING-with-growing-output or a terminal exit code/status, matching
   `bashTaskOutcome`'s shape.

7. **Rewire to existing `SUBAGENT_DETACHED` (`!subagent-detached`).** Already
   covers the `Task`/`Agent` tool_use → async-launch ack → eventual completion
   shape. Retires `detachedworkanchor_e2e_test.go`'s call sites directly.

8. **New option on `SUBAGENT_DETACHED` / `SUBAGENT_DETACHED_LIVE`**
   (`subagents.ts`): after backgrounding, emit ONE ordinary sidechain
   assistant text line (`IsSidechain`/`AgentId`/`SourceToolUseId` set) with no
   completion — modeling a detached subagent's mid-flight utterance arriving
   on the main stream. Needed for `subagentrouting_e2e_test.go`'s
   `sidechainResponseLine` call, which proves the router keeps a live
   subagent's prose out of the top-level feed.

9. **New scenario `usage-historical`** (or an option on `SUBAGENT_SYNC`, in
   `subagents.ts` or `session.ts`): write ONLY the FILE-plane/transcript
   assistant record (no paired STREAM-plane `message_start`), attributed to a
   nested subagent id, carrying the specific usage sub-fields
   `sidecarAssistantUsageEvent` sets (`cache_creation` ephemeral 5m/1h split,
   `server_tool_use` web_search/web_fetch counts, `service_tier`, `speed`,
   `inference_geo`). No capture in the manifest exercises a file-plane-only
   historical usage record with nested-subagent attribution, so per the
   plan's step 2b this needs an explicit "ungrounded" mark in the shim's
   manifest rather than a golden. Retires
   `tokenutilization_e2e_test.go`'s `sidecarAssistantUsageEvent` call sites.

10. **Rewire to existing `SUBAGENT_DETACHED` / `SUBAGENT_DETACHED_LIVE`.**
    `storeTaskStartedEvent` exists only to inflate `live_task_count`; both
    scenarios already call `ctx.startTask(...)` for real, which the shim's
    converter should already turn into the same `TaskStarted` this event
    fabricates. Retires `shutdownschedule_e2e_test.go`'s
    `storeTaskStartedEvent` call sites (the file's own "the fake engine has no
    subagents" comment is stale) — use `SUBAGENT_DETACHED_LIVE` specifically
    where the test needs the task to stay live through the whole drain-lease
    window.

No new scenario is needed for `stalefencerefusal_e2e_test.go`'s
`sidecarUserLineEvent` call (an ordinary prompt line — retire by driving a
real `submitPrompt`), nor for the `storedAssistantEvent` call sites in
`durableresync_e2e_test.go` / `conversationpage_e2e_test.go` /
`promptreceiptreplay_e2e_test.go` (the shape is ordinary; what's missing is
harness support, see below).

---

## Needs a ruling

1. **`promptreceiptreplay_e2e_test.go`'s `acceptOnceShim`** (lines 162-246).
   A hand-rolled Go implementation of the daemon-facing shim wire protocol
   (`ShimHello` → `DaemonHello` → ephemeral `SessionStarted` Event → `ShimReady`
   → `SubmitPrompt` → `Ack`, then disconnect with **zero durable events**).
   This never runs a shim process, real or fake — it dials the daemon's shim
   socket directly and speaks the protocol by hand. A fake-SDK scenario
   addition cannot help here: the fake SDK runs *inside* a real shim process,
   one layer above where this test operates. Reason for the ruling: is a
   hand-rolled minimal shim wire-protocol double an accepted exception to "no
   hand-fabricated protocol traffic" (since it isn't a *store* injection and
   isn't a *vendor* fabrication either — it's a third, narrower category), or
   should the real shim/sidecar instead gain a crash-injection primitive
   ("accept exactly one prompt, then die before persisting anything") that a
   real process can be driven into?

2. **`degradedStateEvent`** (`detachedspecharness_test.go` line 153, used by
   `failurecardshape_e2e_test.go`). A STREAM-plane `Event_DegradedState` that
   the **shim itself** would write to report its own store-link health — not
   a vendor/transcript artifact at all. The fake SDK operates entirely at the
   vendor-transcript layer and has no notion of the shim's connection health
   to the store, so no scenario extension can produce this. Reason for the
   ruling: should the harness instead provoke a REAL degraded-state window
   (e.g. by actually interrupting the shim↔store connection under test), or
   does direct injection of shim-authored (non-vendor) telemetry stay a
   permitted exception, distinct in kind from the sidecar-derived vendor
   events this plan retires?

3. **`storedAssistantEvent`-seeded "bounced"/"cold" tests**
   (`durableresync_e2e_test.go`, `conversationpage_e2e_test.go`,
   `promptreceiptreplay_e2e_test.go`'s non-`acceptOnceShim` tests). Not
   blocked on a fake-SDK gap — the fabricated shape is an ordinary assistant
   reply any scenario already produces. What's missing is that
   `newBouncedHarness` has no mechanism to run a real session + a real
   sidecar to completion (populating the store for real) before simulating
   the "daemon bounce" and reconnecting; it currently seeds the store
   directly instead. Reason for the ruling: should this remediation extend
   `newBouncedHarness` to run a real turn first and then bounce (a
   harness-architecture change, not a scenario addition), or is seeding
   pre-restart durable state via direct store write an accepted, narrower
   exception distinct from injecting sidecar-derived events mid-session?

4. **`TestE2EDuplicateClearYieldsOneFrame`** (`clearcompact_e2e_test.go` lines
   521-523). Writes the identical `sidecarClearEvent` (same dedup key) TWICE
   on purpose, to prove store-level dedup/idempotency. No real end-to-end
   flow can produce a genuine duplicate under normal operation — this is
   exactly why the dedup guarantee matters (crash-and-retry / at-least-once
   redelivery), and it cannot be provoked by driving the fake SDK. Reason for
   the ruling: is this acceptable to keep as a direct, narrowly-scoped store
   write (it is testing the store's own redelivery contract, not the
   sidecar or vendor), or should idempotency coverage move entirely into the
   store's own unit/integration suite and be dropped from `daemon/e2e`?

5. **Hand-written vendor JSONL fixtures** (`hibernationharness_test.go` /
   `keepaliverewind_e2e_test.go`'s `writeDefaultRewindFixture`, `userLine`,
   `assistantLine`, `writeFixtureTranscript`). Outside the three named
   patterns this inventory targeted, but the same defect family: these write
   a fabricated vendor transcript file straight to disk, bypassing the fake
   SDK entirely, which the plan document's own header separately prohibits
   ("must not write vendor JSONL either"). It also blocks cleanly retiring
   `ingestTranscriptAsSidecar` — even once a real sidecar is spawned per the
   plan, it still needs SOME real transcript file on disk to tail, and today
   that file is hand-authored rather than fake-SDK-authored. Reason for the
   ruling: should the rewind fixture instead be produced by running a real
   `!bash`-or-similar turn through the fake SDK and then truncating the
   REAL resulting transcript file at a real turn boundary (modeling exactly
   what the daemon's own rewind does), or is directly writing a truncated
   transcript copy an accepted exception since it is modeling the DAEMON's
   own output rather than a vendor behavior?

---

## PROJECT-LEAD RULINGS (2026-09-02)

The "needs a ruling" items above are SETTLED as follows. These rulings are
binding on step 6; no agent re-litigates them.

### 0. Precondition: the fake SDK arrives with the five-way merge

The five-way merge carries the WHOLE shim branch into `overhaul/integration`,
including `src/fake` and the `overhaul/shim-fakesdk` scenario additions (which
the project lead merges into `overhaul/shim` first). Step 6 therefore needs NO
separate landing step for the fake SDK. This resolves the blocker raised by the
step-3 audit (`E2E-SCENARIO-COVERAGE.md`), which found 0/69 named scenarios
reachable from `daemon/e2e` because the scenario registry was absent on that
branch.

### 1. `acceptOnceShim` — RULED: daemon/e2e never fakes one of our subsystems

`daemon/e2e` is the CROSS-SYSTEM suite: every one of our subsystems in it is
REAL. A hand-rolled Go double of the shim wire protocol has no place there. A
test that needs a SCRIPTED shim belongs in `daemon/integration`, which fakes the
shim by design and will be on `overhaul/integration` after the merge.

In step 6, for each such test choose exactly one:
- rewrite it against the real shim + fake SDK, if the behavior is reachable
  that way;
- move it to `daemon/integration` and express it with that suite's `fakeshim`;
- delete it, IF `daemon/integration` already pins the same behavior — and cite
  the specific test that does.

Compile gate only, as always; the rewriting agent never runs the suite.

### 2. `degradedStateEvent` — RULED: provoke a REAL store outage

The degraded-state telemetry is produced by the REAL shim when the REAL store
goes away, so the test drives that condition rather than fabricating its
report. The step-5 harness gains a control to STOP and RESTART the test's store
process; tests use it to open and close a genuine outage window. No fabricated
telemetry survives.

### 3. `storedAssistantEvent`-seeded bounce/cold tests — RULED: step 5 scope

Not a fake-SDK gap. The step-5 harness gains a helper that drives a fake-SDK
scenario through a REAL session to completion BEFORE the simulated daemon
bounce, so the store holds real rows when the test reconnects. Seeding the
store directly is retired.

### 4. Duplicate-clear / store idempotency

Covered by the general rule in ruling 5 and by ruling 1's disposition menu: a
behavior that no real end-to-end flow can provoke is not a `daemon/e2e`
behavior. Step 6 either drives it for real, moves it to the suite that owns the
contract, or deletes it citing the test that already pins it.

### 5. Hand-written vendor JSONL fixtures — RULED: same rule as everything else

No exception for fixtures that model the daemon's own output. Each hand-written
vendor transcript becomes a NAMED fake-SDK scenario, grounded by a golden or
marked "ungrounded, invented" in the shim manifest with its reason. If a
fixture's shape is one the vendor NEVER emits, DELETE the test and record that
deletion, with the reason, in this inventory.

### 6. `context_budget_warning` — RULED: the orchestrator's ruling stands

Provisionally settled as implemented on `overhaul/shim-fakesdk`: a NEW,
separately-named `!context-budget-warning` producer; the landing-5 pinned tests
(`test/fake/scenarios/session.test.ts:166` and `:212`) untouched; the manifest
entry marked ungrounded pending a grounding capture. LANDING 5 IS NOT
OVERTURNED.

### 7. Step-6 bookkeeping obligation

The step-6 rewrites will be the FIRST `daemon/e2e` tests to name fake-SDK
scenarios. Keep the per-scenario coverage table in
`E2E-SCENARIO-COVERAGE.md` updated as each rewrite lands, so the 0/69 baseline
moves with the work instead of being re-audited from scratch at the end.
