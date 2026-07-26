# Design: agent-shim architecture — single source of truth over a vendor-agnostic protobuf protocol

Status: **APPROVED DESIGN, IMPLEMENTATION PENDING.** This document supersedes
(and replaces wholesale) the deleted `design-status-single-source-of-truth.md`.
It is the single design doc for the system; there is no other.

Scope: `modules/app/agent-repl/` — the daemon (`daemon/`), the shim ecosystem
(`agent-shim/`: `claude/shim/`, `claude/shim-sidecar/`, `shim-store/`), the
protobuf protocol (`proto/`), the Emacs elisp, and the webapp (`webapp/`).

Grounding: all Claude JSON shapes referenced in §5 were enumerated in a
clean-room investigation of `@anthropic-ai/claude-agent-sdk@0.1.77` typings,
~1,540 on-disk transcripts (CLI 2.1.168–2.1.215), 20 workflow journals, the
`/tmp/claude-<uid>` spools, and live haiku probes (`/tmp/sdk-probe/`). The
schema must carry TOTAL FIDELITY to that enumeration — no shape folding, no
generic passthrough substitutes for shapes we know.

---

## 1. Goals and non-goals

Goals:

1. **One source of truth for all agent/session/task state.** Every consumer
   (Emacs tab-bar, webapp, future frontends) renders state the daemon resolves;
   no consumer derives state independently.
2. **A vendor-agnostic protocol.** The daemon speaks one protobuf contract to
   per-vendor shims (`claude-shim` today, `codex-shim` someday). The daemon
   never touches an agent SDK or a vendor file format.
3. **Durable, replayable event history.** Everything the daemon renders as
   final has already been persisted and seq-stamped by the `shim-store`;
   reconnects replay exactly.
4. **Loud validation everywhere.** Converters hard-error on missing expected
   fields and log-and-capture unexpected new fields. No silent drops, no
   silent fallbacks (see AGENTS.md and `metaprompt.md` no-fallbacks rules).
5. **One-change cutover.** The old mechanisms (Emacs sentinel hooks, the
   `:pending-subagents` counter, the daemon tailer, managed settings hooks)
   are deleted in the same change that lands the new system. No phases, no
   backward compatibility, no shadow modes.

Non-goals:

- MCP tool-ification of workspace commands (parked indefinitely).
- Multi-machine or remote operation; everything is one user on one host.
- Backward compatibility of any wire format (breaking changes are free but
  gated on explicit user approval per AGENTS.md).

## 2. Components

| Component | Process model | Language | Responsibility (exclusive) |
|---|---|---|---|
| `claude-shim` | one per session, spawned by daemon | TypeScript (Node) | Drive the Claude Agent SDK; convert SDK stream ↔ protobuf; forward file-plane events; delta bypass |
| `shim-claude-sidecar` | singleton, launchd-managed | Go | Read/parse harness JSONL + spool files into protobuf events; write them to the `shim-store` |
| `shim-store` | singleton, launchd-managed | Go | Sole owner of the event DB (SQLite); assign per-session seq; merge+dedup producers; serve subscriptions |
| daemon (`claude-repld`) | singleton (existing) | Go | Fleet supervision; consume shim streams; session-state manager; merge execution; frontend serving |
| Emacs elisp | in-editor (existing) | elisp | Dumb frontend: render daemon-pushed state; workspace UX |
| webapp | per-workspace webview (existing) | TypeScript | Dumb frontend: render daemon-pushed conversation + state |

**The shim ecosystem (`claude-shim`, `shim-claude-sidecar`, `shim-store`) exists
exclusively to facilitate agent-backend interaction** (AGENTS.md rule). It
never serves frontends, never holds merge/workspace state, never derives
render-state.

The **session-state manager (SSM)** is an in-daemon Go module (not a separate
process) with its own SQLite DB. It replaces the Emacs-persisted workspace
state (`:done`, `:thinking`, `:merged`, …). See §9.2.

## 3. Topology and transports

```
                    ┌───────────────────────────── frontends ─────────────────────────────┐
                    │                                                                      │
   Emacs ◄──UDS (protojson frames)──► daemon ◄──WS/TCP (protojson frames)──► webapp       │
                    │                    ▲                                                 │
                    └────────────────────┼─────────────────────────────────────────────────┘
                                         │ UDS: agent-shim protobuf protocol (control + event stream)
                                         ▼
                                   claude-shim (per session)
                                    ▲          │   ▲
                          SDK stream│          │   │ store subscription (session-filtered, from seq)
                                    │          ▼   │
                              Claude SDK    shim-store ◄──UDS (protobuf events + cursor batches)── shim-claude-sidecar
                                                 │                                                    ▲
                                                 ▼                                                    │ reads
                                            SQLite (events)                harness files: transcripts, agent spools,
                                                                           shell spools, workflow journals
```

- **Every internal hop is UDS carrying protobuf** (binary). There is exactly
  one protobuf schema set (§5); the same messages written to the store are the
  ones flowing on every socket.
- **Frontend hops carry the same proto-defined messages serialized with the
  canonical proto3 JSON mapping (protojson)**: UDS to Emacs, WS/TCP to the
  webapp (webviews cannot open UDS). Both frontends therefore consume
  literally identical shapes and can never diverge.
- Webapp ↔ Emacs never communicate directly; anything crossing goes through
  the daemon.
- Socket paths live under `~/.cache/agent-repl/sock/`:
  `store.sock` (shim-store server), `daemon-frontend.sock` (Emacs), and one
  `session-<id>.sock` per claude-shim (shim listens; daemon connects, which is
  what makes daemon-restart REATTACH possible — see §4.4).

## 4. Data flows

### 4.1 Prompt path (daemon → agent)

1. Frontend sends a prompt command to the daemon (frontend surface, §5.4).
2. Daemon sends `SubmitPrompt` (core control, §5.2) with a `request_id` over
   the session's shim UDS.
3. claude-shim pushes it into the SDK `query()` input iterable and replies
   with a synchronous `Ack{request_id}` on the same connection.
4. All resulting activity returns asynchronously via the event path (§4.2),
   carrying the originating `request_id` where attributable.

### 4.2 Event path (agent/files → daemon), the store round-trip

1. claude-shim converts each SDK stream message to its `data.v1` proto
   (§5.3) wrapped in a core `Event` envelope, and writes it to the shim-store.
2. shim-claude-sidecar independently converts file-plane records (transcript lines,
   agent-spool lines, journal records, shell-spool bytes) into the same
   `Event` envelope shapes and writes them to the shim-store with cursor
   batches (§7.3).
3. The shim-store ingests from both producers, dedups (§6.4), assigns the
   per-session `seq`, persists, and fans out to subscribers.
4. claude-shim's store-subscription loop receives the merged, seq-stamped,
   deduped session stream and forwards it verbatim to the daemon over the
   shim↔daemon UDS.
5. The daemon consumes exactly one totally-ordered stream per session.
   Everything it sees is already durable. It never dedups.

### 4.3 Delta bypass (the ONE store exemption)

`stream_event` partials (live typing) and `tool_progress` elapsed heartbeats
are EPHEMERAL class (§5.2.3): claude-shim forwards them shim→daemon directly,
never writing them to the store. When the final complete message arrives it
takes the normal §4.2 round-trip; consumers reconcile by `uuid`, REPLACING the
streamed preview. Loss of cross-path ordering is irrelevant under per-`uuid`
reconciliation. This is a designed event classification, not a fallback.

### 4.4 Replay and reattach

- The daemon tracks `last_seen_seq` per session. On (re)connect to a shim it
  sends `Subscribe{session_id, from_seq}`; the shim re-serves from its store
  subscription (the store replays from any seq). Only `from_seq` is relayed:
  the shim substitutes the VENDOR session id (Claude's uuid) as the store's
  subscription key, because that — not the daemon's `s_…` id — is what the
  store files events under, and what the sidecar's file plane keys the same
  conversation by. On `--resume` the uuid is known at spawn; a fresh session
  adopts it from the first converted event and reopens the subscription at the
  same `from_seq` (the store replays `seq > from_seq` from disk, so nothing is
  lost by learning it late).
- claude-shim listens on its own `session-<id>.sock` and OUTLIVES a dead
  daemon: a UDS disconnect does not end the SDK turn. The restarted daemon
  reconnects to the SAME live shim — no `--resume` respawn when the shim
  survives. (Respawn with `--resume` remains the path when the shim itself is
  gone.)
- Store downtime is honest downtime (metaprompt no-fallbacks rule): the shim
  hard-fails event writes, loud-logs every dropped event, and reports a
  degraded state on the daemon connection; the display goes stale until the
  store returns. NO spill buffers, NO shadow paths.
- The shim REDIALS the store's producer connection once per outage, on the
  next write that finds it down. The store is launchd-managed and restarts
  under live shims, killing a connection that is otherwise never rebuilt —
  so without this "until the store returns" never arrives and every later
  write drops against a corpse. This is connection lifecycle, the same shape
  as daemon->shim reattach above, not a fallback: one attempt, no timer, no
  background loop; a failed redial drops the batch loudly, a rejected batch
  is still never retried, and a deliberate close() never redials. The
  SUBSCRIPTION connection is not redialed — its `from_seq` belongs to the
  daemon, which re-subscribes on reattach.

### 4.5 Turn/task lifecycle (the stream-only family)

`task_started` / `task_updated` / `task_notification` /
`background_tasks_changed` and turn boundaries exist ONLY in the SDK stream
(never persisted to transcripts by the harness). claude-shim converts and
writes them to the store like any other event (§4.2), which is what makes the
store the complete durable record. The file plane supplies the durable echo
for turn ends (`stop_hook_summary` lines) and everything sidechain/journal/
shell — the store's dedup (§6.4) reconciles the overlap.

### 4.6 Merge flow

The daemon ports Emacs's workspace-merge sentinel handling (`merge-handlers.el`
producers) and runs merges itself (`git -C <dir>` via its existing exec
plumbing). Merge state transitions are recorded ONLY in the daemon's
session-state manager DB (§9.2) — never in the shim-store (which is
agent-interaction-only). Emacs keeps the conflict-resolution UX, reacting to
the daemon-published `merge-conflict` state by opening magit; state ownership
stays with the daemon.

### 4.7 Session-state flow

The daemon forwards lifecycle-relevant events (turn start/stop, session
start/end, task counts) from the shim stream into the SSM, which persists
resolved per-workspace state (`thinking`, `idle`, `idle-async`, `done`,
`merged`, `merge-conflict`, …) in its own SQLite DB. Frontends receive state
changes pushed over the frontend surface. Precedence is a SQL query away —
there is no hardcoded elisp precedence ladder anymore.

---

## 5. The agent-shim protobuf protocol

Canonical schema lives in `proto/agentshim/`; the `.proto` files ARE the
contract, including behavioral semantics as comments (delta reconciliation,
replay handshake, who initiates). Three packages:

- `agentshim.core.v1` — vendor-neutral envelope, control plane, lifecycle.
- `agentshim.data.v1` — the full-fidelity data vocabulary: typed models of
  every shape found in the investigation. Derived from the Claude harness but
  TREATED as vendor-agnostic (doctrine in `proto/AGENTS.md`): no consumer
  special-cases a vendor, and a new vendor's incongruities are resolved by
  revising the API (breaking changes allowed, gated on user approval).
- `agentshim.frontend.v1` — the daemon→frontend resolved surface (protojson
  on the wire).

Evolution rules: proto3; field numbers never reused; backward-compatible
evolution free; breaking changes require explicit user approval (AGENTS.md).

### 5.1 Conversion & validation contract (applies to every converter)

- Missing EXPECTED field → hard error: the record is persisted as an
  `UnparsedEvent` carrying raw bytes + error, and the failure is loud-logged.
  It never silently becomes a zero value.
- Unexpected NEW field → the value is captured into the message's
  `extras` (`google.protobuf.Struct`) field and loud-logged once per distinct
  field name. Nothing is dropped.
- Every claude message carries `raw_compat` semantics: string-or-object
  unions (e.g. `toolUseResult`) are modeled as explicit oneofs with a
  `raw_string` arm; `content` string-vs-blocks unions likewise.
- Casing: stream (`tool_use_result`, `session_id`, `pre_tokens`) and disk
  (`toolUseResult`, `sessionId`, `preTokens`) differ; converters normalize
  into ONE proto shape and record the plane in `Event.plane`.

### 5.2 `agentshim.core.v1` inventory (~24 messages/enums)

Envelope & stream:

| Message | Purpose / key fields |
|---|---|
| `Event` | THE envelope: `session_id`, `seq` (store-assigned; 0 pre-ingest), `plane` (STREAM/FILE/SYNTHETIC), `class` (PERSISTENT/EPHEMERAL), `request_id?`, `produced_at`, `payload` (oneof: core lifecycle below, `claude.v1` extension via `google.protobuf.Any`), `extras` |
| `EventBatch` | ordered `Event` list + producer cursor info (sidecar batches) |
| `UnparsedEvent` | `source_path`, `byte_offset`, `raw` (bytes, bounded), `error`, `producer` |
| `SessionStarted` / `SessionEnded` | vendor-neutral session lifecycle; `source` (fresh/resume/compact-continue) |
| `TurnStarted` / `TurnEnded` | turn boundaries; `stop_reason`, `duration_ms` |
| `TaskStarted` / `TaskProgress` / `TaskEnded` | detached-work lifecycle; `task_id`, `kind` (AGENT/SHELL/WORKFLOW), `terminal_status` (DONE/ERROR/KILLED/STOPPED/LOST), `output_path` |
| `ContentDelta` | EPHEMERAL live-typing delta: `uuid`, `block_index`, `text`/`thinking`/`input_json` oneof |
| `HeartbeatProgress` | EPHEMERAL `tool_progress` relay: `tool_use_id`, `elapsed_seconds` |
| `MessageLatency` | EPHEMERAL first-token-latency relay: `uuid`, `ttft_ms` (off the `message_start` stamp) |
| `DegradedState` | shim→daemon sad-path report: `component`, `reason`, `dropped_count` |

Control plane (daemon↔shim, both directions):

| Message | Purpose |
|---|---|
| `SubmitPrompt` | `request_id`, `text`, `origin`, `permission_mode?` |
| `Interrupt` | `request_id`, hard/soft |
| `Ack` / `Nack` | synchronous receipt for any `request_id` (`Nack.reason`) |
| `Subscribe` | `session_id`, `from_seq` (daemon→shim on connect; shim→store identically) |
| `PermissionRequest` / `PermissionResponse` | `canUseTool` round-trip: `request_id`, `tool_name`, `input` (Struct), decision, updated inputs |
| `ShimHello` / `DaemonHello` | connection handshake: versions, session identity, capabilities |
| `Heartbeat` | liveness on every long-lived connection |
| `StoreWrite` / `StoreWriteAck` | producer→store append (events + optional cursor advance), idempotency key |
| `CursorState` | sidecar cursor recovery: `file_id (dev,inode)`, `path`, `offset`, `carry` |

Enums: `Plane`, `EventClass`, `TaskKind`, `TerminalStatus`, `SessionSource`,
`PermissionDecision`.

### 5.3 `agentshim.data.v1` inventory (~95 messages/enums, total fidelity)

`stream.proto` — SDK stream messages (typed union members + observed-only):

| Message | Source |
|---|---|
| `ClaudeStreamMessage` | oneof wrapper over everything below |
| `UserMessage` | SDKUserMessage (+ `is_replay` fold of SDKUserMessageReplay) |
| `AssistantMessage`, `AssistantMessageError` (enum) | SDKAssistantMessage |
| `ResultMessage`, `ResultSubtype` (enum), `Usage`, `ModelUsage`, `PermissionDenial` | SDKResultMessage success+error variants, incl. empirical `api_error_status`, `ttft_ms`, `ttft_stream_ms`, `time_to_request_ms`, `stop_reason`, `terminal_reason`, `fast_mode_state`, `ModelUsage.max_output_tokens` |
| `SystemInit`, `McpServerStatus`, `PluginRef` | SDKSystemMessage subtype init (+ empirical `fast_mode_state`) |
| `StreamEvent`, `RawMessageStreamEvent` + `MessageStart`/`ContentBlockStart`/`ContentBlockDelta` (+`TextDelta`/`ThinkingDelta`(+`estimated_tokens`)/`InputJsonDelta`/`SignatureDelta`)/`ContentBlockStop`/`MessageDelta`/`MessageStop` | SDKPartialAssistantMessage |
| `CompactBoundary`, `CompactMetadata` | SDKCompactBoundaryMessage |
| `StatusMessage` | SDKStatusMessage (`compacting`/none) |
| `HookResponse` | SDKHookResponseMessage + empirical `hook_id`, `output`, `outcome` |
| `ToolProgress` | SDKToolProgressMessage |
| `AuthStatus` | SDKAuthStatusMessage |
| `RateLimitEvent`, `RateLimitInfo` | observed-only |
| `HookStarted` | observed-only system subtype |
| `ThinkingTokens` | observed-only system subtype |
| `SystemNotification` | observed-only system subtype (`key`, `text`, `priority`) |
| `TaskStartedMsg` | observed-only (`task_id`, `tool_use_id`, `description`, `task_type`, `subagent_type?`, `prompt?`) |
| `TaskUpdatedMsg`, `TaskPatch` | observed-only (`patch.status`, `patch.end_time`) |
| `TaskNotificationMsg`, `TaskUsage` | observed-only (`status`, `output_file`, `summary`, `usage?`) |
| `BackgroundTasksChanged`, `BackgroundTaskRef` | observed-only |
| `ControlRequest`/`ControlResponse`/`ControlCancelRequest`/`KeepAlive` | control-channel wire types |

`transcript.proto` — on-disk JSONL lines:

| Message | Source |
|---|---|
| `TranscriptLine` | oneof over all 12 disk line types |
| `LineEnvelope` | the ~10 always-present + ~25 optional envelope fields (B2 census: `parent_uuid`, `is_sidechain`, `uuid`, `timestamp`, `user_type`, `cwd`, `session_id`, `version`, `git_branch`, `entrypoint?`, `request_id?`, `prompt_id?`, `agent_id?`, `source_tool_assistant_uuid?`, `effort?`, `attribution_agent/skill/plugin?`, `slug?`, `prompt_source?`, `permission_mode?`, `origin?`, `is_meta?`, `source_tool_use_id?`, `tool_ends_turn?`, `is_api_error_message?`+`error`+`api_error_status`+`error_details`, `tool_denial_kind?`, `classifier_meta_lines?`, `interrupted_message_id?`, `queue_priority?`, `is_visible_in_transcript_only?`, `is_compact_summary?`, `logical_parent_uuid?`) |
| `UserLine` | envelope + `ApiUserMessage` + `tool_use_result` oneof |
| `AssistantLine` | envelope + `ApiAssistantMessage` (`id`, `model`, `content`, `stop_reason`, `stop_sequence`, `stop_details`, `usage`, `diagnostics`, `context_management`, `container`) |
| `ApiUserMessage` / `ApiAssistantMessage` | content string-or-blocks oneof |
| system subtypes (9): `StopHookSummaryLine` (+`HookInfo`), `TurnDurationLine`, `LocalCommandLine`, `AgentsKilledLine`, `CompactBoundaryLine` (+`PreservedSegment`, `PreservedMessages` — camelCase disk variant, richer than SDK), `InformationalLine`, `ScheduledTaskFireLine`, `ModelRefusalNoFallbackLine`, `ApiErrorLine` (+`ApiErrorDetail`) | B3 |
| metadata lines (8): `ModeLine`, `PermissionModeLine`, `QueueOperationLine`, `LastPromptLine`, `AiTitleLine`, `PrLinkLine`, `FileHistorySnapshotLine`, `FileHistoryDeltaLine` (+`FileBackup`) | B4 |
| `AttachmentLine` + 23 attachment payloads: `HookSuccessAttachment`, `HookNonBlockingErrorAttachment`, `HookBlockingErrorAttachment`, `DeferredToolsDeltaAttachment`, `SkillListingAttachment`, `AgentListingDeltaAttachment`, `TaskReminderAttachment`, `AutoModeAttachment`, `EditedTextFileAttachment`, `DiagnosticsAttachment`, `CommandPermissionsAttachment`, `QueuedCommandAttachment`, `ReadTruncationNoticeAttachment`, `StructuredOutputAttachment`, `CompactFileReferenceAttachment`, `ContextTipAttachment`, `DateChangeAttachment`, `NestedMemoryAttachment`, `FileAttachment`, `UltrathinkEffortAttachment`, `DynamicSkillAttachment`, `UltraEffortEnterAttachment`/`UltraEffortExitAttachment`, `PlanModeExitAttachment` | B5 |
| `AgentMetaJson` | the sidechain companion `agent-<id>.meta.json` (`agent_type`, `description`, `tool_use_id`, `spawn_depth`, `model`) |

`tools.proto` — content blocks, tool inputs, tool results:

| Message | Source |
|---|---|
| `ContentBlock` oneof: `TextBlock`, `ThinkingBlock`, `ToolUseBlock` (+`Caller`), `ToolResultBlock` (content string-or-blocks; `is_error`), `ImageBlock` (+`ImageSource`), `ToolReferenceBlock` | C2 |
| `ToolUseResult` | top-level oneof: `raw_string` (error/rejection strings) \| every typed result below; plus `extras` |
| `BashResult` | sync + background-launch fold (`background_task_id?`, `background_cwd_hint?`, `return_code_interpretation?`, `git_operation?`, `persisted_output_path/size?`, `timed_out_after_ms?`, `stale_read_file_state_hint?`) |
| `AgentResult`, `AgentToolStats` | sync completed |
| `AgentAsyncLaunch` | background launch (`is_async`, `output_file`, `can_read_output_file`) |
| `TaskOutputResult`, `LocalBashTask`, `LocalAgentTask` | `retrieval_status` + task variants |
| `TaskStopResult` | |
| `WorkflowLaunchResult` | |
| `MonitorResult` | |
| `ReadResult` (+`ReadFile`), `EditResult`, `WriteResult`, `SkillResult`, `ToolSearchResult`, `TaskCreateResult`, `TaskUpdateResult`, `TaskListResult`, `SendMessageResult`, `WebFetchResult`, `WebSearchResult`, `AskUserQuestionResult`, `ScheduleWakeupResult` | C3 others |
| tool inputs (16, from sdk-tools.d.ts): `AgentInput`, `BashInput`, `TaskOutputInput`, `KillShellInput`, `FileEditInput`, `FileReadInput`, `FileWriteInput`, `GlobInput`, `GrepInput`, `NotebookEditInput`, `TodoWriteInput` (+`TodoItem`), `WebFetchInput`, `WebSearchInput`, `AskUserQuestionInput` (+`Question`+`QuestionOption`), `ConfigInput`, `ExitPlanModeInput`, `McpInput`, `ListMcpResourcesInput`, `ReadMcpResourceInput` | C1 |
| enums: `RawTaskStatus` (`async_launched`, `running`, `completed`, `failed`, `killed`, `stopped`), `RetrievalStatus`, `PermissionModeEnum`, `ApiKeySource`, `McpServerState`, `Entrypoint`, `PromptSource`, `ToolDenialKind`, `QueueOp`, `CompactTrigger` | C4 |

`journal.proto` — workflow journals:

| Message | Source |
|---|---|
| `JournalRecord` | oneof `started` \| `result` |
| `JournalStarted` | `key`, `agent_id` |
| `JournalResult` | `key`, `agent_id`, `result` (Struct-or-string oneof; schema is workflow-defined) |

### 5.4 `agentshim.frontend.v1` inventory (~10 messages)

| Message | Purpose |
|---|---|
| `FrontendFrame` | oneof wrapper for the push channel |
| `WorkspaceState` | THE resolved render-state: `workspace`, `state` (closed enum: `init`, `idle`, `idle_async`, `thinking`, `permission`, `done`, `stop_failed`, `merging`, `merge_queued`, `merge_conflict`, `merge_failed`, `merged`, `dead`, `degraded`), inputs snapshot (turn-active, live-task count, merge phase) for debuggability |
| `SessionView` | per-session metadata (model, tokens, cost, title, slug) |
| `ConversationDelta` | rendered conversation additions (complete messages, tool cards) |
| `TypingDelta` | ephemeral relay of `ContentDelta` for live typing |
| `TaskCatalog` | live detached-task list per session |
| `FrontendCommand` | frontend→daemon: submit prompt, interrupt, permission answer, merge/close/open workspace |
| `CommandAck` | `request_id` receipt |
| `DegradedNotice` | store/sidecar/shim outage surfaced honestly |
| `StateSnapshot` | full resync on frontend (re)connect |

#### 5.4.1 Resync and the bounded below-floor history re-pull

`StateSnapshot` carries NO conversation, by design. Conversation reaches a
frontend only as pushed `ConversationDelta`s, which are fire-and-forget: a
freshly-mounted GUI never sees what was pushed before it existed. So a
(re)connecting frontend sends `ResyncCmd{from_seq}` and the daemon answers in
two layers:

1. **The retained ring** (`sessiondrv`, 4,096 events). Replayed from `from_seq`
   as ordinary `ConversationDelta`s. Idempotent: frontends reconcile by uuid, so
   a re-push REPLACES.
2. **The below-floor re-pull** (`daemon/internal/storesub`), when `from_seq`
   falls below the ring's oldest retained seq — which after a daemon restart is
   EVERY request, since the ring is empty then. The floor comes from the ring
   when it holds anything, and otherwise from the durable `last_seen_seq` plus
   one.

The re-pull deliberately reopens a tradeoff this design closed elsewhere: the
daemon subscribes each shim from its HIGH-WATER mark precisely to avoid replay
storms (a resumed conversation that re-subscribed from 0 once queued the first
prompt behind ~6,000 replayed frames and blew its ack timeout). Four standing
constraints are what keep the re-pull from becoming that:

- **Frontend-initiated.** Nothing in the daemon ever starts one; it exists only
  as the tail of a `ResyncCmd`.
- **Bounded.** It stops at the ring floor, a hard event cap, an idle window, and
  a deadline. A bound that trips is reported, never absorbed.
- **A side channel.** It opens its OWN throwaway subscriber connection to the
  store. It must NOT be done by sending the shim a low `Subscribe{from_seq}`:
  the shim's `onSubscribe` reopens its one standing store subscription, which
  would move the daemon's own position backwards AND replay everything down the
  connection the SSM, task catalog, and progress resolver feed from.
- **Conversation only.** Re-pulled events reach conversation translation and
  nothing else — not the SSM, not `BuildTaskCatalog`, not the progress resolver,
  not the retained ring. Those planes consumed these events once already, and
  double-applying them is exactly what makes historical tasks masquerade as live
  activity (see §5.4.2).

At most one re-pull runs per workspace. A concurrent request whose range the
in-flight pull already covers is coalesced onto it (the pull's output is
broadcast to every subscriber of the workspace, so the second caller really is
served); one reaching further back is refused loudly rather than silently
under-served.

#### 5.4.2 `BackgroundTasksChanged` is the authoritative live-task set

`data.BackgroundTasksChanged` is the only event carrying the FULL current live
task set, and the daemon used to drop it. It is now folded into both task
planes as authoritative reconciliation:

- `BuildTaskCatalog` sweeps any running entry absent from the list (status
  `lost`, ended at the snapshot's timestamp) and opens an entry for any id in
  the list it had never seen.
- The SSM appends reconciliation rows so `live_task_count` equals the list's
  size exactly, in BOTH failure directions: ghost starts with no end, and — the
  shape a cursor-recovery failure produces — mass `task_ended` with no logged
  `task_started`, which is what drove the observed `IMPOSSIBLE
  live_task_count=-114`.

The negative-count clamp in `ssm/resolve.go` stays: it is the loud report of an
impossible state, and reconciliation removes its cause rather than its voice.

---

## 6. shim-store specification

### 6.1 Process & storage

Go daemon, launchd-managed (§13), UDS server at
`~/.cache/agent-repl/sock/store.sock`. SQLite (WAL) at
`~/.cache/agent-repl/store/events.db`. The store is deliberately TINY and
frozen: schema, seq assignment, dedup, fan-out — no interpretation, no
parsing, no vendor knowledge (payloads are opaque `Any` bytes to it).

### 6.2 Schema

```sql
CREATE TABLE event (
  session_id TEXT NOT NULL,
  seq        INTEGER NOT NULL,          -- per-session, store-assigned
  plane      INTEGER NOT NULL,
  class      INTEGER NOT NULL,          -- PERSISTENT only; EPHEMERAL never lands
  kind       TEXT NOT NULL,             -- extracted payload type URL suffix
  task_id    TEXT,                      -- extracted when task-scoped
  uuid       TEXT,                      -- extracted claude uuid when present
  dedup_key  TEXT,                      -- see 6.4
  produced_at INTEGER NOT NULL,
  payload    BLOB NOT NULL,             -- serialized core.Event
  PRIMARY KEY (session_id, seq)
);
CREATE UNIQUE INDEX event_dedup ON event(session_id, dedup_key) WHERE dedup_key IS NOT NULL;
CREATE INDEX event_task ON event(session_id, task_id) WHERE task_id IS NOT NULL;
CREATE TABLE cursor (                    -- sidecar cursor recovery (§7.3)
  file_id  TEXT PRIMARY KEY,             -- "dev:inode"
  path     TEXT NOT NULL,
  offset   INTEGER NOT NULL,
  carry    BLOB,
  updated_at INTEGER NOT NULL
);
CREATE TABLE schema_meta (version INTEGER NOT NULL);
```

Indexed extracted columns (`kind`, `task_id`, `uuid`) keep SQL queries
possible over opaque proto blobs.

### 6.3 Ingest & seq

- `StoreWrite` carries events (+ optional cursor advance for sidecar batches);
  the store assigns `seq` in arrival order per session, commits events +
  cursor in ONE transaction, replies `StoreWriteAck`.
- Idempotency: a batch's `dedup_key`s make replays no-ops
  (`INSERT OR IGNORE` semantics against `event_dedup`); the ack reports
  accepted vs deduped counts.

### 6.4 Dedup policy (stream plane vs file plane)

The same fact can arrive twice (shim stream + transcript line). Dedup keys:

- claude messages with a `uuid` → `uuid:<uuid>`.
- tool results → `tur:<tool_use_id>`.
- journal records → `wf:<run_id>:<key>:<type>`.
- turn ends → `turn:<session>:<turn-uuid>` (stream `TurnEnded` vs disk
  `stop_hook_summary` reconcile here).
- First writer wins; the loser is counted in the ack and loud-logged at
  debug level only (expected overlap, not an anomaly).

### 6.5 Subscriptions

`Subscribe{session_id, from_seq}` → the store streams persisted events from
that seq, then live-tails. Slow consumers get bounded server-side buffering
then a hard disconnect with a loud log (reconnect replays — no data loss by
construction). EPHEMERAL events are fanned out to live subscribers in
arrival position but never persisted and never replayed.

---

## 7. shim-claude-sidecar specification

### 7.1 Process & discovery

Go daemon, launchd-managed, sole file-plane observer. Discovery unions:

- Config roots (`~/.claude`, `~/.claude-chesscom`, … — configured list):
  `projects/*/<session>.jsonl` (session transcripts),
  `projects/*/<session>/subagents/agent-*.jsonl` (+ `.meta.json`),
  `projects/*/<session>/subagents/workflows/wf_*/journal.jsonl`.
- Spools: `/tmp/claude-<uid>/<slug>/<session>/tasks/<taskid>.output`
  (`a*`=agent JSONL transcript, `b*`=shell raw text, `w*`=workflow).

Session attribution falls out of the PATH (the `<session>` segment) — no
hints needed. `fsnotify` for latency, periodic full rescan as the
completeness backstop (watch events drop; files appear while down).

### 7.2 Layered read architecture (the agreed two-layer split)

- **Layer 1 (generic streaming core, no schema knowledge):** per-file cursor
  `(dev, inode, offset, carry)`; on wake: stat, detect truncation/rotation
  (inode change or `size < offset` → loud log + reset), read appended bytes,
  frame via the file-type codec, decode records, hand to the handler, batch
  the resulting events + cursor advance into ONE `StoreWrite`.
- **Codecs:** `JSONLCodec` (consume through last `\n`, bounded partial-tail
  carry, per-line decode; on parse failure emit `UnparsedEvent` and resync at
  next newline) and `RawTextCodec` (shell spools: byte chunks, no parse).
- **Layer 2 (handlers, pure record→events, zero IO):**
  - `SessionTranscriptHandler` — envelope+line parse (transcript.proto);
    emits the file-plane twins of messages, `TaskStarted` (from launch
    results incl. path construction for shell launches), `TurnEnded` (from
    `stop_hook_summary`), kill signals (from `TaskStop` results).
  - `AgentTranscriptHandler` — REUSES the transcript line parser (sidechain
    lines are the same shape); emits task progress + recursive grandchild
    launches.
  - `WorkflowJournalHandler` — `started`/`result` records.
  - `ShellOutputHandler` — byte-count progress only.

### 7.3 Cursor recovery & exactly-once

Cursors live in the STORE's `cursor` table (§6.2): the sidecar is nearly
stateless, recovering cursors via a store query at startup; events + cursor
advance commit atomically in the store, so a crash at any point replays
cleanly and dedup keys absorb overlap.

### 7.4 Completion inference & staleness policy

No terminal markers exist on disk (verified): shell spools just end, journals
have no terminal record. Completion truth therefore comes from stream-plane
lifecycle events (`task_notification` et al.) when the shim is alive, plus
these file-plane inferences, each an explicit, loud-logged transition to
terminal status `LOST` (never `DONE`):

1. **Vanished file:** watched task file disappears while its task is open →
   grace period (default 30s) → `LOST` (a vanish means "stopped", not
   "succeeded").
2. **Silence timeout:** no new bytes for a per-kind window (shell 30m, agent
   60m, workflow 60m — tunable) AND no stream-plane liveness → `LOST`.
3. **Boot sweep:** at startup, any open task whose `started_at` predates the
   current boot time → `LOST` (nothing survives a reboot).

### 7.5 What the sidecar does NOT do

No DB ownership (store's), no interpretation of resolved state (daemon's),
no SDK/stream knowledge (shim's), no frontend anything.

---

## 8. claude-shim specification

The existing shim at `agent-shim/claude/shim/` evolves in place (same
spawn/`--resume` model) with:

1. **UDS server** replacing stdio: listens on `session-<id>.sock`; daemon
   connects; disconnect does NOT end the turn (reattach, §4.4).
2. **SDK↔proto converter** (`stream.proto` shapes) with the §5.1 validation
   contract; every SDK message becomes a core `Event` wrapping the claude
   payload.
3. **Store client:** writes PERSISTENT events via `StoreWrite`; subscribes to
   the merged session stream and forwards it to the daemon verbatim.
4. **Delta bypass:** `stream_event`/`tool_progress` → `ContentDelta`/
   `MessageLatency`/`HeartbeatProgress` (EPHEMERAL) direct to daemon.
5. **Control handling:** `SubmitPrompt`→ SDK input; `Interrupt`→
   `query.interrupt()`; `canUseTool`→ `PermissionRequest` round-trip to the
   daemon (blocking, correlation by `request_id`).
6. **Sad path:** store write failure → loud log per dropped event +
   `DegradedState` to daemon. No spill, no retry-forever, no fallback.

---

## 9. Daemon specification (changes to `daemon/`)

### 9.1 Consumption side

- DELETE: `tailer.go` (all of it), `asyncsource.go` classification-from-prose,
  the Layer-1 NDJSON stdio plumbing, the `async_live` integer on
  `GET /sessions`, the HTTP frontend surface for Emacs.
- ADD: shim UDS client (control + event stream, `last_seen_seq` tracking),
  protojson frontend server (UDS listener for Emacs, WS for webapp), both
  serving `frontend.v1` frames.

### 9.2 Session-state manager (SSM)

In-daemon Go module (`daemon/internal/ssm/`), own SQLite DB
(`~/.cache/agent-repl/ssm/state.db`):

```sql
CREATE TABLE workspace_state (
  workspace   TEXT NOT NULL,
  session_id  TEXT,
  state       TEXT NOT NULL,          -- thinking|idle|idle_async|done|merged|…
  cause_kind  TEXT NOT NULL,          -- the event kind that caused it
  cause_seq   INTEGER,                -- store seq when event-caused
  at          INTEGER NOT NULL,
  PRIMARY KEY (workspace, at)
);                                     -- append-only state log; current = MAX(at)
```

- Inputs: lifecycle events forwarded from shim streams (turn/task/session)
  AND daemon-local merge transitions (§4.6). Merge state lives ONLY here.
- Resolved state = SQL over the log + live task counts; changing precedence
  is changing a query, not elisp.
- Every transition is loud-logged with its inputs (the instrumentation
  contract, §12) and pushed to frontends as `WorkspaceState`.
- Replaces the Emacs `:agent-state` persistence entirely.

### 9.3 Merge port

Port `merge-handlers.el` producer logic (cherry-pick driver, finalize,
conflict detection) to `daemon/internal/workspace/merge/`. Emacs keeps only the
reactive conflict UX. The resolve-and-continue handoff: Emacs signals
"conflict resolved, continue" via a `FrontendCommand`; the daemon resumes the
cherry-pick and writes the resulting transition into the SSM.

---

## 10. Emacs changes (dumb renderer)

- DELETE: sentinel dispatch for `stop_`/`stop_failure_`/`subagent_start_`/
  `subagent_stop_`/`prompt_submit_`/`session_start_` (`sentinel.el`), the
  `:pending-subagents` machinery + `agent-repl--fully-stopped-p`
  (`status.el`), managed settings hooks (`install.el`), the `GET /sessions`
  poller + `async_live` handling (`frontend-client.el`), the render-status
  precedence `cond` (`workspace.el` — replaced by daemon-pushed state),
  merge-state producers (`merge-handlers.el` producers).
- ADD: a UDS client (`make-network-process :family 'local`) speaking
  protojson `frontend.v1` frames; a thin JSON→plist mapping layer; render =
  lookup of the pushed `WorkspaceState.state` keyword in
  `agent-repl-ws-state-icons`. The metaprompt read-directive re-fire moves to
  the daemon (it owns prompt submission context now).

## 11. Webapp changes

- Replace the bespoke daemon-frame vocabulary with `frontend.v1` protojson
  frames (same shapes as Emacs).
- Status/tail rows read `WorkspaceState`/`TaskCatalog` instead of deriving
  from raw frames; typing keeps flowing via `TypingDelta` into the existing
  `smooth.ts` reveal.
- **Scope: protobuf adoption for the ALREADY-supported shapes only.** No new
  visual support is added for shapes the webapp does not currently render;
  unsupported shapes are ignored explicitly (typed, logged at debug, never
  crashed on).
- **Coverage-enumeration deliverable:** the cutover's final report MUST
  enumerate every protobuf shape the web frontend does not support visually
  (even where visual support would make little sense), so unsupported
  coverage is a known, listed quantity rather than an unknown.

## 12. Logging & instrumentation contract

- Converters and the sidecar: loud on every anomaly (parse failure, missing
  expected field, unknown new field, truncation/rotation, staleness
  transition) — each with file/offset/session identifiers. `UnparsedEvent`s
  persist the evidence.
- SSM: every state transition logged with old→new + causing event kind + seq.
- Store: ingest anomalies (dedup storms, slow-consumer disconnects) logged.
- NO steady-state poll logging; log deltas and transitions only. Coalesce
  repeated identical errors (log once per distinct error, log recovery).
- Each service logs to `~/.cache/agent-repl/log/<service>.log` with a shared
  prefix format (`HH:MM:SS.mmm [component] {session=… ws=…}`).

## 13. Lifecycle & supervision

- `shim-store` and `shim-claude-sidecar`: launchd user agents (`KeepAlive`,
  `RunAtLoad`), installed by `.claude/install.sh`; plists under
  `modules/app/agent-repl/launchd/`.
- claude-shims: spawned/supervised by the daemon (existing model), except
  they now survive daemon death until their session ends.
- Heartbeats on every long-lived UDS connection; a missed-heartbeat window
  surfaces `DegradedNotice` to frontends (reporting, not fallback).
- Boot sweep (§7.4.3) runs in the sidecar at startup.

---

## 14. File manifest

Grouping below is the subagent farm-out plan: one agent per GROUP (closely
related files), not per file. Every new-code group adds unit tests that pass
before the agent resolves. Test commands: Go groups run `go test ./...`
within their module; webapp runs `npm test` + `npm run typecheck` from
`webapp/`; elisp runs the ert suite per repo CLAUDE.md.

### 14.0 The directory tree (MIRROR RULE)

**Mirror rule: every subsequent change to the planned filesystem layout MUST
be reflected in this tree in the same change.** Legend: A = addition,
M = modification, D = deletion; ✓ = already landed on `master`.

```
~/.config/doom/
├── AGENTS.md                                                M ✓ (protocol gate, doctrine, shim scope)
├── .claude/
│   └── install.sh                                           M ✓ (build + launchctl bootstrap both services)
└── modules/app/agent-repl/
    ├── design-agent-shim-architecture.md                    A ✓
    ├── metaprompt.md                                        M ✓ (no-fallbacks rule)
    ├── bin/build-frontend.sh                                M ✓ (SHIM_DIR → agent-shim/claude/shim; S5: SHIM_ARTIFACT is the esbuild bundle at dist/main.js)
    ├── proto/                                                 ✓ (G1)
    │   ├── AGENTS.md                                        A ✓
    │   ├── Makefile                                         A ✓
    │   └── agentshim/
    │       ├── core/v1/core.proto                           A ✓
    │       ├── data/v1/stream.proto                         A ✓
    │       ├── data/v1/transcript.proto                     A ✓
    │       ├── data/v1/tools.proto                          A ✓
    │       ├── data/v1/journal.proto                        A ✓
    │       └── frontend/v1/frontend.proto                   A ✓
    ├── agent-shim/
    │   ├── AGENTS.md                                        A ✓
    │   ├── wire/                                              (shared Go framing, pre-built)
    │   │   ├── AGENTS.md                                    A ✓
    │   │   ├── go.mod                                       A ✓
    │   │   ├── wire.go                                      A ✓
    │   │   └── wire_test.go                                 A ✓
    │   ├── claude/                                            (vendor group: the shim + its file-plane sidecar)
    │   │   ├── shim/                                           (relocated under claude/ ✓; G4+G5)
    │   │   │   ├── AGENTS.md                                    A ✓
    │   │   │   ├── package.json                                 M ✓ (G5: @bufbuild/protobuf dep; S5: esbuild devDep + `build`→build.mjs, main=dist/main.js bundle)
    │   │   │   ├── package-lock.json                            M ✓ (G5: @bufbuild/protobuf lock; S5: esbuild direct devDep)
    │   │   │   ├── tsconfig.json                                M ✓ (G5: rootDir=../.., @bufbuild paths, gen/ts include)
    │   │   │   ├── vitest.config.ts                             A ✓ (G5: server.fs.allow the proto/gen subtree)
    │   │   │   ├── build.mjs                                    A ✓ (S5: esbuild single-file bundle → dist/main.js; inlines @bufbuild, SDK external)
    │   │   │   ├── src/main.ts                                  M ✓ (S5: --uds-socket/--store-socket/--version flags; UDS-mode branch; stdio RETAINED behind default, marked SUPERSEDED)
    │   │   │   ├── src/session.ts                                 (S5: UNCHANGED — reattach lives in uds-session.ts; stdio ShimSession kept intact until the daemon cutover)
    │   │   │   ├── src/uds/uds-session.ts                       A ✓ (S5: UDS-mode session engine — SDK drive + convert/delta/store/control wiring; reattach lifetime)
    │   │   │   ├── src/proto/convert.ts                         A ✓ (S5: + sessionSource seam for SessionStarted.source RESUME/FRESH)
    │   │   │   ├── src/proto/extras.ts                          A ✓
    │   │   │   ├── src/proto/delta.ts                           A ✓
    │   │   │   ├── src/uds/proto.ts                             A ✓ (G5: single import site for gen/ts core stubs)
    │   │   │   ├── src/uds/log.ts                               A ✓ (G5: §12 loud structured logging)
    │   │   │   ├── src/uds/framing.ts                           A ✓ (G5: wire.go twin + Any-envelope + MessageConn)
    │   │   │   ├── src/uds/server.ts                            A ✓ (G5)
    │   │   │   ├── src/uds/store-client.ts                      A ✓ (G5)
    │   │   │   ├── src/uds/control.ts                           A ✓ (G5)
    │   │   │   ├── test/uds-harness.ts                          A ✓ (G5: framed-peer test helper, not a suite)
    │   │   │   ├── test/uds-session.test.ts                     A ✓ (S5: UDS entry-wiring integration — routing, round-trip, reattach, permission)
    │   │   │   └── test/{framing,server,store-client,control,reattach}.test.ts  A ✓ (G5)
    │   │   └── shim-sidecar/                                    (G3)
    │   │       ├── AGENTS.md                                    A ✓
    │   │       ├── go.mod                                       A ✓
    │   │       ├── go.sum                                       A ✓
    │   │       ├── main.go (+_test)                             A ✓
    │   │       └── internal/
    │   │           ├── discover/discover.go (+_test)            A ✓
    │   │           ├── discover/watcher.go                      A ✓ (fsnotify wrapper)
    │   │           ├── tail/tailer.go (+_test)                  A ✓
    │   │           ├── tail/codec.go (+_test)                   A ✓
    │   │           ├── tail/context.go                          A ✓ (Context+Kind+Handler; breaks handler↔tail cycle)
    │   │           ├── handler/handler.go                       A ✓ (shared Context alias + event builders)
    │   │           ├── handler/transcript.go (+_test)           A ✓
    │   │           ├── handler/agent.go                         A ✓
    │   │           ├── handler/journal.go                       A ✓
    │   │           ├── handler/shell.go                         A ✓
    │   │           ├── convert/convert.go (+_test)              A ✓
    │   │           ├── convert/registry.go                      A ✓ (concrete-message factory)
    │   │           ├── stale/stale.go (+_test)                  A ✓
    │   │           └── storeclient/client.go (+_test)           A ✓
    │   └── shim-store/                                        (G2)
    │       ├── AGENTS.md                                    A ✓
    │       ├── go.mod                                       A ✓
    │       ├── main.go                                      A ✓
    │       └── internal/
    │           ├── db/db.go                                 A ✓
    │           ├── db/ingest.go (+_test)                    A ✓
    │           ├── db/query.go (+_test)                     A ✓
    │           ├── server/server.go                         A ✓
    │           ├── server/fanout.go (+_test)                A ✓
    │           └── dedup/dedup.go (+_test)                  A ✓
    ├── daemon/
    │   ├── go.mod                                           M ✓ (agentrepl/proto, agentrepl/wire, protobuf, sqlite)
    │   └── internal/
    │       ├── protodeps/protodeps.go                       D ✓ (temporary dep pin; deleted at stitch as planned)
    │       ├── ssm/                                           (G6)
    │       │   ├── AGENTS.md                                A ✓
    │       │   ├── ssm.go (+_test)                          A ✓
    │       │   ├── db.go                                    A ✓
    │       │   └── resolve.go (+_test)                      A ✓
    │       ├── shimclient/                                    (G7)
    │       │   ├── AGENTS.md                                A ✓
    │       │   ├── client.go (+_test)                       A ✓
    │       │   ├── control.go (+_test)                      A ✓
    │       │   └── events.go (+_test)                       A ✓
    │       ├── workspace/
    │       │   ├── AGENTS.md                                A ✓
    │       │   └── merge/                                     (G8)
    │       │       ├── AGENTS.md                            A ✓
    │       │       ├── merge.go (+_test)                    A ✓
    │       │       └── state.go (+_test)                    A ✓
    │       ├── frontend/                                      (G9)
    │       │   ├── AGENTS.md                                A ✓
    │       │   ├── server.go (+_test)                       A ✓
    │       │   ├── translate.go (+_test)                    A ✓
    │       │   └── commands.go (+_test)                     A ✓
    │       ├── server/server.go                             M ✓ (HTTP frontend + async_live dropped; D-phase: POST /sessions, /commands{,/refresh}, /status{,/refresh}, POST /shutdown and the sentinel poke deleted; routes() table + APIPrefixes)
    │       ├── session/tailer.go                            D ✓
    │       ├── sentinel/                                     D ✓ (D-phase: the last poke went; Emacs listens to none)
    │       ├── protocol/layer2.go                            M ✓ (D-phase: Envelope/L2Frame/~45 *Frame types deleted; only Layer2Version survives)
    │       ├── protocol/layer1.go                            M ✓ (D-phase: the legacy inbound client-command union deleted; shim-stdio direction kept)
    │       ├── session/asyncsource.go                       D ✓ (deleted entirely with the async plane, not just its prose parsing)
    │       ├── registry/registry.go                         M ✓ (shim-socket reattach fields; moved out of session/)
    │       ├── remediation/remediation.go                   M ✓ (prose path)
    │       └── workspacecmd/workspacecmd.go                 M ✓ (route via SSM + frontend push)
    ├── codex.el                                             M ✓ (D-phase: managed hooks + installer + hooks.json doctor deleted)
    ├── hooks/*-notify.sh                                    D ✓ (D-phase: six orphaned sentinel scripts; prepare-commit-msg-emoji.sh kept)
    ├── bin/smoke-real-sdk.mjs                               D ✓ (D-phase: unreferenced, and dead against every current protocol)
    ├── frontend-uds.el                                      A ✓ (G10)
    ├── frontend-state.el                                    A ✓
    ├── test-frontend-uds.el                                 A ✓
    ├── test-frontend-state.el                               A ✓
    ├── sentinel.el                                          M ✓ (D-phase: dispatch alist + every sentinel READER deleted; the drain rig and the ws-for-dir/load-barrier halves stay)
    ├── test-sentinel.el                                     M ✓
    ├── status.el                                            M ✓ (counter + fully-stopped-p deleted)
    ├── test-status.el                                       M ✓
    ├── workspace.el                                         M ✓ (render-status becomes pushed-state lookup)
    ├── test-workspace.el                                    M ✓
    ├── frontend-client.el                                   M ✓ (HTTP poller deleted; delegates to frontend-uds.el)
    ├── test-frontend-client.el                              M ✓
    ├── merge-handlers.el                                    M ✓ (producers deleted; conflict UX kept)
    ├── test-merge-handlers.el                               M ✓
    ├── install.el                                           M ✓ (managed settings hooks deleted; D-phase: the whole settings.json hook-array writer went with the codex hook plane)
    ├── test-install.el                                      M ✓
    ├── webapp/                                                (G11 + stitch S3)
    │   ├── index.html                                       M ✓ (degraded-state banner element)
    │   ├── src/frontend-proto.ts                            A ✓ (hand-typed protojson; no new dep)
    │   ├── src/state-adapter.ts                             A ✓
    │   ├── src/protocol.ts                                  M ✓ (daemon→webapp frame vocab DELETED; data types + commands kept)
    │   ├── src/store.ts                                     M ✓ (applyRaw/parseFrame path DELETED; typed ingest(effects))
    │   ├── src/main.ts                                      M ✓ (onMessage: decode→adapt→ingest; degraded banner)
    │   ├── src/tasks.ts                                     M ✓ (sessionTasks DELETED; roster now from TaskCatalog)
    │   ├── src/topbar.ts                                    M ✓ (session roster passed in, not item-derived)
    │   ├── src/styles.css                                   M ✓ (degraded-banner style)
    │   ├── test/frontend-proto.test.ts                      A ✓ (LOCKED; unchanged at stitch)
    │   ├── test/state-adapter.test.ts                       A ✓ (LOCKED; unchanged at stitch)
    │   ├── test/store.test.ts                               M ✓ (rewritten to the ingest path)
    │   ├── test/protocol.test.ts                            D ✓ (parseFrame gone)
    │   ├── test/{tasks,topbar,queue,ws}.test.ts             M ✓ (old-vocab assertions rewritten/removed)
    │   └── (src/render.ts, src/wslog.ts, package.json)          (unchanged: render consumes the store model as before; wslog keeps only ClientLogCmd; frontend-proto is hand-typed so no protojson dep)
    ├── launchd/                                               (G12)
    │   ├── AGENTS.md                                        A ✓
    │   ├── com.agentrepl.shim-store.plist                   A ✓
    │   └── com.agentrepl.shim-claude-sidecar.plist          A ✓
    ├── scripts/
    │   ├── AGENTS.md                                        A ✓
    │   └── agent-shim-doctor.sh                             A ✓
    └── testdata/corpus/                                       (G13)
        ├── AGENTS.md                                        A ✓
        ├── MANIFEST.md                                      A ✓
        └── <fixtures per the corpus contract>               A ✓
```

Notes: `proto/gen/` (generated Go/TS stubs via `proto/Makefile`, plus
`gen/go/go.mod` as module `agentrepl/proto`) IS COMMITTED (A ✓) so every
group compiles against identical stubs; regenerate with `make` after any
`.proto` change. Consumers import Go stubs via a
`replace agentrepl/proto => <relative>/proto/gen/go` directive and TS stubs
via relative imports plus the `@bufbuild/protobuf` runtime. Exact fixture
filenames under `testdata/corpus/` are chosen by the G13 agent from the real
corpus.

### 14.1 NEW files (by farm-out group)

**G1 `proto/` — schema (authored with this doc, not farmed):**
- `proto/agentshim/core/v1/core.proto` — §5.2 inventory.
- `proto/agentshim/data/v1/stream.proto` — §5.3 stream table.
- `proto/agentshim/data/v1/transcript.proto` — §5.3 transcript table.
- `proto/agentshim/data/v1/tools.proto` — §5.3 tools table.
- `proto/agentshim/data/v1/journal.proto` — §5.3 journal table.
- `proto/agentshim/frontend/v1/frontend.proto` — §5.4 inventory.
- `proto/Makefile` — `protoc` codegen: Go (`daemon/`, `agent-shim/shim-store/`,
  `agent-shim/claude/shim-sidecar/`), TS (`agent-shim/claude/shim/`, `webapp/`).

**G2 `agent-shim/shim-store/` — the store service (Go module):**
- `agent-shim/shim-store/main.go` — flags (socket path, db path), launchd entry,
  signal handling, logging setup.
- `agent-shim/shim-store/internal/db/db.go` — §6.2 schema, migrations via
  `schema_meta`, open/close, WAL config.
- `agent-shim/shim-store/internal/db/ingest.go` — §6.3 transactional
  events+cursor append, seq assignment, dedup (`INSERT OR IGNORE`), ack
  accounting.
- `agent-shim/shim-store/internal/db/query.go` — replay-from-seq reads, cursor
  recovery reads, extracted-column queries.
- `agent-shim/shim-store/internal/server/server.go` — UDS listener, connection
  lifecycle, `StoreWrite`/`Subscribe`/`CursorState`/`Heartbeat` dispatch.
- `agent-shim/shim-store/internal/server/fanout.go` — §6.5 subscriber registry,
  live-tail broadcast, EPHEMERAL pass-through, slow-consumer disconnect.
- `agent-shim/shim-store/internal/dedup/dedup.go` — §6.4 dedup-key derivation from
  `Event` payloads (type-URL switch; uuid/tool_use_id/journal/turn keys).
- Tests: `db/ingest_test.go`, `db/query_test.go`, `server/fanout_test.go`,
  `dedup/dedup_test.go` (table-driven, AAA; in-memory SQLite; golden
  events from G13 fixtures).

**G3 `agent-shim/claude/shim-sidecar/` — the file-plane reader (Go module):**
- `agent-shim/claude/shim-sidecar/main.go` — flags (config roots, spool root, store socket),
  launchd entry, boot sweep trigger.
- `agent-shim/claude/shim-sidecar/internal/discover/discover.go` — §7.1 globs + fsnotify +
  rescan; path→(session, file-type) classification (incl. `a*`/`b*`/`w*`
  task-id prefixes and the `<session>` path segment).
- `agent-shim/claude/shim-sidecar/internal/tail/tailer.go` — §7.2 Layer-1 core: cursor loop,
  truncation/rotation detection, bounded reads, batch assembly.
- `agent-shim/claude/shim-sidecar/internal/tail/codec.go` — `JSONLCodec` + `RawTextCodec`.
- `agent-shim/claude/shim-sidecar/internal/handler/transcript.go` — `SessionTranscriptHandler`
  (+ the shared line/envelope parser used by agent.go).
- `agent-shim/claude/shim-sidecar/internal/handler/agent.go` — `AgentTranscriptHandler`.
- `agent-shim/claude/shim-sidecar/internal/handler/journal.go` — `WorkflowJournalHandler`.
- `agent-shim/claude/shim-sidecar/internal/handler/shell.go` — `ShellOutputHandler`.
- `agent-shim/claude/shim-sidecar/internal/convert/convert.go` — JSON→proto mapping with the
  §5.1 validation contract (missing-expected hard error → `UnparsedEvent`;
  unknown-field capture into `extras` + once-per-name loud log).
- `agent-shim/claude/shim-sidecar/internal/stale/stale.go` — §7.4 vanish grace, per-kind
  silence timeouts, boot sweep, `LOST` transitions.
- `agent-shim/claude/shim-sidecar/internal/storeclient/client.go` — UDS client:
  `StoreWrite` batches, cursor recovery, heartbeats.
- Tests: one `_test.go` per file above; conversion tests are golden-file
  driven from G13.

**G4 `agent-shim/claude/shim/` converter layer (TS, inside existing shim):**
- `agent-shim/claude/shim/src/proto/convert.ts` — SDK message→`claude.v1` proto with §5.1
  validation; the complete `SDKMessage` union switch incl. observed-only
  families.
- `agent-shim/claude/shim/src/proto/extras.ts` — unknown-field capture + once-per-name
  loud-log helper.
- `agent-shim/claude/shim/src/proto/delta.ts` — `stream_event`/`tool_progress` →
  `ContentDelta`/`MessageLatency`/`HeartbeatProgress` (EPHEMERAL) mapping.
- Tests: `webapp`-style vitest in `agent-shim/claude/shim/test/` over G13 stream fixtures
  (every message type at least once; every union arm).

**G5 `agent-shim/claude/shim/` transport layer (TS):**
- `agent-shim/claude/shim/src/uds/framing.ts` — the `agentrepl/wire` twin
  (4-byte BE length prefix, 32MiB `MAX_FRAME`, clean-EOF only at a boundary,
  loud errors + decoder-poison on truncation/oversize, no resync) PLUS the
  message-multiplexing envelope: each frame payload is a serialized
  `google.protobuf.Any` (typeUrl `type.googleapis.com/agentshim.core.v1.<Msg>`)
  so several message types share one connection. This is the SAME convention
  the daemon-side shimclient (G7) uses; `MessageConn` binds a socket to it.
- `agent-shim/claude/shim/src/uds/server.ts` — `session-<id>.sock` listener; daemon
  connection lifecycle; disconnect-tolerant turn survival + reattach (a new
  connection re-handshakes; unsent events are dropped, not spilled — the store
  replays them on the next Subscribe); handshake (`ShimHello` first, then
  `DaemonHello`); heartbeats; Ack/Nack receipts.
- `agent-shim/claude/shim/src/uds/store-client.ts` — store connection: `StoreWrite`
  with `StoreWriteAck` accounting, `Subscribe{from_seq}` + continuous forward
  loop to an injected sink, honest sad path (drop + loud-log every event +
  `DegradedState` to an injected reporter; NO spill, NO retry of a rejected
  batch); producer-connection redial, once per outage, driven by the next
  write (§4.4).
- `agent-shim/claude/shim/src/uds/control.ts` — `SubmitPrompt`/`Interrupt` dispatch onto an
  injected `SdkControlTarget` (does NOT import src/session.ts); `canUseTool`→
  `PermissionRequest` round-trip blocking on a pending-request map keyed by
  `request_id`.
- Support: `src/uds/proto.ts` (single import site for the gen/ts core stubs),
  `src/uds/log.ts` (§12 loud structured logging), `vitest.config.ts`
  (`server.fs.allow` the proto subtree), `tsconfig.json` (rootDir widened to
  `../..`, `@bufbuild` path mappings, gen/ts added to `include`).
- Tests: `test/{framing,server,store-client,control,reattach}.test.ts` +
  `test/uds-harness.ts`; framing round-trip + typeUrl assertions, control
  dispatch with a mocked SDK session, reattach (drop daemon conn mid-stream,
  reconnect, `Subscribe{from_seq}`, verify continuation w/o loss or dup).

**G6 `daemon/internal/ssm/` — session-state manager:**
- `ssm/ssm.go` — module API (Go-channel fed): `Apply(event)`,
  `ApplyMergeTransition(...)`, `Current(workspace)`, `Snapshot()`.
- `ssm/db.go` — §9.2 schema + append/query.
- `ssm/resolve.go` — the state-resolution SQL/logic (precedence lives here,
  as queries), transition loud-logging with cause kind + seq.
- Tests: `ssm_test.go`, `resolve_test.go` — every state transition edge,
  precedence table-driven.

**G7 `daemon/internal/shimclient/` — shim protocol client:**
- `shimclient/client.go` — per-session UDS connection, handshake,
  `Subscribe{from_seq}`, `last_seen_seq` tracking, reconnect-to-live-shim.
- `shimclient/control.go` — `SubmitPrompt`/`Interrupt`/`PermissionResponse`
  senders with `request_id` correlation.
- `shimclient/events.go` — event demux: lifecycle→SSM, conversation→
  frontend translation, `DegradedState`→`DegradedNotice`.
- Tests: mocked UDS peer, replay/reattach, correlation.

**G8 `daemon/internal/workspace/merge/` — merge port:**
- `merge/merge.go` — cherry-pick driver ported from
  `merge-handlers.el:227-236` (`git -C` via existing exec wrappers),
  conflict detection, finalize (port of `:349-372`), resume-after-resolve.
- `merge/state.go` — SSM transition emission (`merging`, `merge_queued`,
  `merge_conflict`, `merge_failed`, `merged`).
- Tests: table-driven over scripted git fixtures (temp repos), conflict +
  resume paths.

**G9 `daemon/internal/frontend/` — frontend serving:**
- `frontend/server.go` — UDS listener (Emacs) + WS upgrade (webapp), both
  emitting protojson `frontend.v1` frames; `StateSnapshot` on connect.
- `frontend/translate.go` — internal events/SSM state → `WorkspaceState`,
  `ConversationDelta`, `TypingDelta`, `TaskCatalog`, `SessionView`.
- `frontend/commands.go` — `FrontendCommand` dispatch (prompt, interrupt,
  permission answer, merge/close/open) with `CommandAck`.
- Tests: translation golden tests; command dispatch with mocked internals.

**G10 Emacs frontend (elisp):**
- `frontend-uds.el` — UDS client (`make-network-process :family 'local`),
  protojson frame decode to plists, reconnect + `StateSnapshot` handling.
- `frontend-state.el` — `WorkspaceState`→render-state application (replaces
  the local derivation), `DegradedNotice` surfacing.
- Tests: `test-frontend-uds.el`, `test-frontend-state.el` (pure elisp,
  fixture JSON strings, no processes per AGENTS.md test rules).

**G11 webapp frontend adoption (TS):**
- `webapp/src/frontend-proto.ts` — `frontend.v1` protojson frame types +
  decode/validation.
- `webapp/src/state-adapter.ts` — `WorkspaceState`/`TaskCatalog`/
  `TypingDelta` → existing store/render inputs (feeding `smooth.ts`
  unchanged).
- Tests: vitest over frame fixtures; adapter parity with catalogue
  scenarios.
- Deliverable beyond code: the complete enumeration of protobuf shapes the
  web frontend does NOT support visually (§11), handed to the stitch phase
  for the final report.

**G12 supervision + install:**
- `launchd/com.agentrepl.shim-store.plist`,
  `launchd/com.agentrepl.shim-claude-sidecar.plist` — KeepAlive user agents.
- `.claude/install.sh` additions — build + `launchctl bootstrap` both
  services; idempotent re-install.
- `scripts/agent-shim-doctor.sh` — connectivity/liveness check across all
  sockets (diagnostics, §12).

**G13 golden corpus fixtures:**
- `testdata/corpus/` — anonymized real samples: one of EVERY transcript
  line type/subtype/attachment, every toolUseResult shape, agent sidechain
  lines, journal pairs, shell spool with/without clean end, stream probe
  captures (`/tmp/sdk-probe/`). Plus `testdata/corpus/MANIFEST.md`
  documenting provenance. Shared by G2/G3/G4 tests.

### 14.2 CHANGED files (stitched by the main agent, not farmed)

| File | Change |
|---|---|
| `agent-shim/claude/shim/src/main.ts`, `agent-shim/claude/shim/src/session.ts` | stdio→UDS rewiring; converter/transport integration; stdin-EOF no longer ends input (reattach) |
| `daemon/internal/server/server.go` | delete HTTP frontend surface + `async_live`; wire shimclient/frontend/ssm/merge modules |
| `daemon/internal/session/*` | delete `tailer.go`; delete prose-regex spawn parsing from `asyncsource.go` (typed events replace it); session registry gains shim-socket reattach fields |
| `daemon/internal/workspacecmd/*` | route results through SSM + frontend push instead of Emacs sentinels |
| `sentinel.el` | delete status-hook dispatch (keep only non-status sentinels if any remain; expected: file shrinks drastically or dies) |
| `status.el` | delete counter machinery + `agent-repl--fully-stopped-p` |
| `workspace.el` | `agent-repl--ws-render-status` becomes a lookup of daemon-pushed state |
| `frontend-client.el` | delete HTTP poller; delegate to `frontend-uds.el` |
| `merge-handlers.el` | delete producers; keep conflict-UX consumer reacting to pushed state |
| `install.el` | delete managed settings hooks |
| `webapp/src/{main,store,protocol,tasks,topbar}.ts` + `index.html` + `styles.css` | one-change cutover: `main.ts` onMessage decodes `frontend.v1` (`frontend-proto.ts`) → `state-adapter.ts` effects → `store.ingest`; `protocol.ts` drops the daemon→webapp frame vocab (keeps data types + commands); `store.ts` replaces `applyRaw`/reducers with `ingest(effects)`; session task roster comes from `TaskCatalog`; degraded banner added. `wslog.ts` unchanged (keeps only `ClientLogCmd`); `render.ts` unchanged (still consumes the store item/state model) |
| `.claude/install.sh` | G12 additions |
| `AGENTS.md` | post-implementation: component docs, debugging pointers for the new logs |

## 15. Testing strategy

- **Golden corpus (G13) is the backbone:** every converter (G3 `convert.go`,
  G4 `convert.ts`) must decode every corpus fixture with zero `UnparsedEvent`s
  and zero unknown-field logs; corpus gaps found later become fixtures first,
  fixes second.
- Table-driven AAA Go tests throughout; in-memory SQLite for store/SSM tests.
- Elisp tests stay pure (no processes, mock the wrappers) per AGENTS.md.
- Webapp: `npm test` + `npm run typecheck`.
- End-to-end (manual, not in ert/CI): scripted haiku probe session through
  the full stack, verified via `agent-shim-doctor.sh` + store SQL spot
  checks.

## 16. Cutover

One change, no phases, no backward compatibility, no shadow modes: the
branch lands the new system AND deletes every superseded mechanism (§9.1,
§10 deletions) together. If something breaks after the cutover, we fix
forward. The `metaprompt.md` no-fallbacks rule and the AGENTS.md
no-redundant-mechanisms rule are the governing policies.
