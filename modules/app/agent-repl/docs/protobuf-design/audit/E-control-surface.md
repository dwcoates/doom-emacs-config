# Partition E — SDK control surface and session-level types

Scope: `@anthropic-ai/claude-agent-sdk` 0.3.220 (`sdk.d.ts`; `agentSdkTypes.d.ts` is a 1-line stub, nothing declared) against `conversation/v1/{session,permission,api,turn,history,agent}.proto` and `shim/v1/service.proto` + every `endpoint_*.proto`. Record = `docs/protobuf-design/figma-to-idl-redesign.md` (line refs `rec:N`). Shim usage facts from `agent-shim/claude/shim/src` grep: the shim today calls `interrupt, setModel, setPermissionMode, stopTask, supportedCommands, supportedModels, usage_EXPERIMENTAL_*, close` and passes options `model, cwd, canUseTool, permissionMode, env, resume, thinking, effort, systemPrompt, settingSources, pathToClaudeCodeExecutable, mcpServers, includePartialMessages, abortController`.

Status legend: REP = represented; DROP(reason) = dropped with a cited reason; UNSUP = no home, no reason; NS = non-static resolution; NOROUTE = proto field with no SDK source.

---

## 1. FORWARD — proto fields → SDK source

### 1.1 `SessionStarted` (session.proto) / `StartSessionSuccess`

| proto field | status | SDK source / reason |
|---|---|---|
| `vendor_session_id` | REP | `SDKSystemMessage.session_id` (sdk.d.ts:4455) — but `init` "arrives only with the first turn" (endpoint_start_session.proto header). Fresh: shim can mint via `Options.sessionId` (sdk.d.ts:1809) and report it statically. Resume: echo of `Options.resume` (1803). Static. |
| `runtime.shim_build_sha` | NOROUTE (by design) | shim build constant. |
| `runtime.sdk_version` | NOROUTE (by design) | `node_modules/.../package.json` version, not an SDK API. Note: `init.claude_code_version` (4420) — the CLI version, which versions independently (rec:2868-2873) — has NO proto home. |
| `effective_model` (fresh) | REP | `Options.model` (1713) echoed. |
| `effective_model` (resume) | NS | rec:32-34 / proto comment: "recovered from the transcript". No SDK call returns the resumed session's model before the first turn (`init.model` 4426 arrives with the first turn). Requires a backward scan of the JSONL for the last `assistant.message.model` — a variable-length read, not a keyed lookup. `getSessionInfo()` (729) returns `SDKSessionInfo` (4327) — check whether it carries a model; if not, this stays NS. |
| `permission_mode` (resume) | NS | same: transcript scan for last prompt's mode; no SDK getter. |
| `main_agent_id` | NOROUTE (by design) | shim-minted, persisted (proto comment). |
| `model_catalog[]` → `ModelOption` | REP | `supportedModels()` (2407) → `ModelInfo` (1224): `value`→`model.name`, `displayName`→`display_name`, `description`→`description`. |
| `turn_in_flight` | NOROUTE | shim bookkeeping. SDK `session_state_changed.state='running'` (4376) says *a* turn is running but carries no id; a resumed CLI emits nothing at startup. Since a daemon restart does not restart the shim, the shim's own open-turn register is the source — static. |
| `live_work[]` | NOROUTE on resume | Only source is `SDKBackgroundTasksChangedMessage` (2915), a level signal that is "per-process: nothing is emitted at startup, so consumers must reset to the empty set whenever the session's CLI process (re)starts" (doc at 3061). After a shim restart + `resume` the set is unobtainable. rec:249-250 claims "`KillSession`'s every live task is the vendor's own `backgroundTasks()` answer" — WRONG at the type surface: `backgroundTasks(toolUseId?)` (2563) returns `Promise<boolean>` (Ctrl+B), not a list. |

### 1.2 `SessionCold` / `SessionColdRemediation` / `SessionColdCompact`

| proto field | status | SDK source / reason |
|---|---|---|
| `context_tokens` | NS / partial | `getContextUsage().totalTokens` (2423, 3063-3070) needs a running query — on a not-yet-started resume the only source is the transcript's last `usage` (scan). |
| `last_request_at_ms` | NS | transcript-only (`SDKUserMessage.timestamp` 4601 on stored entries); no SDK getter. |
| `requested_model` | REP | `Options.model` / transcript last model. |
| `lapsed.cache_ttl_ms` | NOROUTE | SDK exposes nothing about prompt-cache TTL tier; the shim must assume the vendor's 5m/1h. |
| `model_switch` | REP (derived) | requested ≠ transcript last (rec:423). |
| `remediation.pay` | REP | plain `Options.resume`. |
| `remediation.clear` | NOROUTE | "Discard the context; keep the session identity" — no SDK primitive keeps the id with an empty context: `forkSession` (1595) mints a NEW id; `sessionId` (1809) "cannot be used with resume unless forkSession"; `deleteSession` (530) destroys. Only `resumeSessionAt` (1815) pointed at the first message approximates it, and that is a context rewrite via a fresh query, not a clear. Flag. |
| `remediation.compact.*` | DROP (rec:590-596) | shim-implemented throwaway session; owed vetting: `sessionStore` is `@alpha` (1787), `compact_boundary` survival. |

### 1.3 `SessionUpdate` arms (WatchSession)

| proto arm / field | status | SDK source / reason |
|---|---|---|
| `identity_rotated.previous/vendor_session_id` | NS-ish | No SDK message announces rotation; detectable only by comparing `session_id` across consecutive `SDKMessage`s (every message carries it). Constant per message — static, but inferred. |
| `identity_rotated.reason` | NOROUTE | no vendor reason exists on any message. |
| `query_died.unexpected_eof` | REP | async iterator returns without `result`. |
| `query_died.iterator_failure.cause` | REP | iterator throws (`AbortError` 17 or Error). |
| `query_died` ← `SDKWorkerShuttingDownMessage.reason` (4672) | UNSUP | CLI-stated shutdown reason (`host_exit`, …) has no arm. |
| `model_changed.effective_model` | REP | `setModel()` resolves void (2327); the effective model is read from the next `SDKAssistantMessage.message.model`. No session-level "model changed" message exists. |
| `fast_mode.on` / `off.reason` | PARTIAL | `fast_mode_state` (4445 init, 4283/4317 result) is `'off'|'cooldown'|'on'` (640) — **`cooldown` has no arm**. `off.reason` ← `fast_mode_disabled_reason` (635, 10 literals) verbatim ✓. No standalone fast-mode push message; it rides init/result. |
| `mcp_server.name/connected/failed.error` | PARTIAL + NS | `mcpServerStatus()` (2417) → `McpServerStatus` (1075): `name`, `status`, `error` ✓. **`needs-auth`, `pending`, `disabled` have no arm.** No SDKMessage announces an MCP health change (only `init.mcp_servers` 4427 once), so "health changed" requires polling — NS at the event level. `serverInfo`, `config`, `scope`, `tools[]` UNSUP. |
| `account_usage.observed_at_ms` | REP | shim clock. |
| `account_usage.subscription_type` | REP | `usage_EXPERIMENTAL…().subscription_type` (3199) or `accountInfo().subscriptionType` (26). |
| `available.five_hour.utilization_percent/resets_at_ms` | REP | `rate_limits.five_hour.utilization/resets_at` (3210-3219; ISO string → ms). |
| `unavailable.service_unavailable` | REP | `rate_limits_available=false` / `rate_limits=null` (3203-3209). |
| `unavailable.window_unavailable` | REP | `five_hour` absent/null. |
| `unavailable.utilization_unavailable` | REP | `utilization: null`. |
| `unavailable.sampling_failure.cause` | REP | call threw. |
| (push route) `SDKRateLimitEvent` (4237) | UNSUP | `status`, `rateLimitType` (6 windows), `utilization`, `resetsAt`, overage fields, `errorCode` — a live push the proto's "LIVE, whenever the shim observes it" could use; only `five_hour` utilization could be folded in, the rest has no home. |
| `permission_mode_changed.permission_mode` | REP | `setPermissionMode()` void (2300); authoritative value from `SDKStatusMessage.permissionMode` (4405) or `PermissionUpdate{type:'setMode'}` applied. |

### 1.4 `SessionDiagnostics` / `SessionKilled` / `SessionLive`

| proto field | status | SDK source |
|---|---|---|
| `SessionDiagnostics.*`, `SessionFault`, `SessionDegradedWindow` | NOROUTE (by design) | shim self-report (proto header). |
| `SessionKilled.idle` | REP | `close()` (2581) with nothing open. |
| `SessionKilledForced.interrupted_turn` | REP | `interrupt()` (2292) + shim's open-turn id. |
| `SessionKilledForced.stopped_work[]` | REP | `stopTask(taskId)` (2550) per id from the held `background_tasks_changed` set (see 1.1 live_work caveat). |
| `SessionLive.*` | REP | shim registers. |

### 1.5 `permission.proto` ← `CanUseTool` (206) / `PermissionResult` (2114) / `SDKPermissionDeniedMessage` (4168)

| proto field | status | SDK source |
|---|---|---|
| `AgentPermission.id.value` | REP | shim-minted; natural key `options.requestId` (248) or `toolUseID` (236). |
| `gated_call` | REP | `options.toolUseID` (236). |
| `start.prompt.title` | REP but optional-mismatch | `options.title?` (217) — SDK optional, proto required string. |
| `start.prompt.display_name` | REP but optional-mismatch | `options.displayName?` (222). |
| `start.prompt.description` | REP | `options.description?` (227) → optional ✓. |
| `start.trigger.blocked_path.path` | REP | `options.blockedPath?` (213). |
| `start.trigger.ask_rule.{source,tool_name,rule_content}` | REP | `options.matchedAskRule` (257-261) — exact. |
| `start.trigger.note.text` | REP | `options.decisionReason?` (215). NOTE: SDK can carry `blockedPath` AND `decisionReason` AND `matchedAskRule` together (doc 250-256 says the ask "carries the tool's own decisionReason" alongside the rule); proto `trigger` is a oneof — **one of three is lost** when two coincide. |
| `start.offered_standing.changes[]` | REP | `options.suggestions?: PermissionUpdate[]` (210). |
| `start.started_at` | REP | shim clock. |
| `AgentPermissionChange.destination` | REP | `PermissionUpdateDestination` (2162): all 5 literals typed. |
| `change.add_rules/replace_rules/remove_rules/set_mode/add_directories/remove_directories` | REP | `PermissionUpdate` (2133-2160): all 6 arms typed, field-for-field (`rules`,`behavior`,`mode`,`directories`). |
| `AgentPermissionRule.{tool_name,rule_content}` | REP | `PermissionRuleValue` (2128). |
| `AgentPermissionBehavior` | REP | `PermissionBehavior` (2069) 3/3. |
| `AgentPermissionMode` 6 arms | REP | `PermissionMode` (2092) 6/6. |
| `success.allowed.once` | REP | return `{behavior:'allow'}`. |
| `success.allowed.standing.standing` | REP | `updatedPermissions` = echoed `suggestions`. |
| `success.denied.user.message` | REP | `{behavior:'deny', message}`. |
| `success.denied.policy.{decider,reason,message}` | REP | `SDKPermissionDeniedMessage.decision_reason_type/decision_reason/message` (4179-4189). `agent_id` → frame `agent_id` ✓. |
| `success.abandoned` | REP | `options.signal` (208) aborted before a decision; callback returns `null` (266). |
| `AgentPermissionFailure` | empty | derived at wave. |
| `AgentPermissionDecision.ask` | REP | keyed to pending `requestId`. Static. |

### 1.6 Endpoint requests (shim.v1)

| rpc / field | status | SDK route |
|---|---|---|
| `StartSessionFresh.model/permission_mode` | REP | `Options.model`, `Options.permissionMode`. |
| `StartSessionResume.vendor_session_id` | REP | `Options.resume`. |
| `StartSessionResume.cold_remediation` | see 1.2 | `clear` NOROUTE. |
| `StartSessionFailure` further arms | derived | binary fails to start → iterator throws before init. |
| `SetSessionModelRequest.model` | REP | `setModel(name)` (2327). `setModel(undefined)` = "use the default" is NOT representable (`AgentModel.name` required) — DROP, fine. |
| `cold_threshold_tokens` / `cold_remediation` | shim policy | see 1.2 (`getContextUsage().totalTokens` is the live source for the threshold test — static). |
| "resolves after the current turn ends" | NS | rec:490-492: departs from the SDK's mid-turn `setModel` on purpose; shim waits on the turn's `result` — a wait on an event, not a lookup. Acceptable, noted. |
| `SetSessionPermissionModeRequest.permission_mode` | REP | `setPermissionMode(mode)` (2300). "A gate already open keeps the mode it opened under" — shim must not re-evaluate pending asks; SDK semantics match (mode applies to next check). |
| `GetSessionDiagnostics` | NOROUTE (by design) | — |
| `KillSessionRequest.force` | REP | `interrupt()` + `stopTask()`* + `close()`. |
| `StartTurnRequest.{turn,said,origin}` | REP | `streamInput()` (2545) / `SDKUserMessage` (4583): `uuid?` (4619) can carry the daemon `TurnId` so `result.user_message_uuid` (4301) and `still_queued` uuids key back statically. `origin` is shim record only (SDK `origin` 4593 is a different vocabulary: human/channel/peer). |
| `StartTurn` "refuse if a turn is open" | REP | shim register; rec:2769. |
| `WatchTurn` | REP | per-message `session_id`/`parent_tool_use_id` routing (other partition). |
| `UpdateTurn.input.stop` | REP | `interrupt()`. Receipt `still_queued` (3489) / `cancelled` (3493) → rec:2769 "defensive evidence … a fault to surface" — **no proto home exists to surface that fault** (no `UpdateTurnFailure` arm, no `SessionFault` kind yet). |
| `UpdateTurn.input.answer.permission_decision` | REP | resolve the pending `canUseTool` promise keyed by `requestId`. Static. |
| `UpdateTurn.input.prompt` | REP | `streamInput()` with `priority`/`shouldQuery` (4592/4597) — delivery mechanism the proto leaves to the producer. |
| `KillTurnRequest.force` | REP | `interrupt()` + `stopTask()` over the turn's descendants (root-stamp, rec:232-238). |
| `WatchSubagent/UpdateSubagent/WatchBash/StopBash/WatchWorkflow/UpdateWorkflow` | REP (ids) | `stopTask(taskId)` (2550) keyed by `DetachedWorkId`; subagent prompt via `SDKUserMessage.parent_tool_use_id` (4586). Other partition for bodies. |
| `DetachForegroundRequest.unit` | REP | `backgroundTasks(toolUseId)` (2563); `false` → failure "matched no foreground task" (derived arm). |
| `ReadHistoryRequest.{agent,first,next}` | REP | `getSessionMessages` (759) / `getSubagentMessages` (796) with options (764/801) — check for cursor/limit support in those option types; continuation is shim-minted. Out of this partition's depth. |

---

## 2. REVERSE — SDK → proto

### 2.1 `Query` methods (sdk.d.ts:2279-2582)

| method (line) | status | home / reason |
|---|---|---|
| `interrupt()` (2292) | REP | `AgentInput.stop`, `KillTurn`, `KillSession`. |
| `interrupt` → `still_queued` (3489) | DROP(rec:2769) | "defensive evidence, a fault to surface" — but NO surfacing arm exists (see 1.6). |
| `interrupt` → `cancelled` (3493) / request `cancel_queued` (3479) | UNREACHABLE | `Query.interrupt()` takes no args at 0.3.220; `cancel_queued` exists only on the internal `SDKControlInterruptRequest`. rec:2858 cites it as usable — it is not from the public `Query`. |
| `cancel_async_message` (3001) | UNREACHABLE | internal control request only; no `Query` method. rec:2859 cites it. Moot given one-submitter (rec:2769). |
| `setPermissionMode(mode)` (2300) | REP | `SetSessionPermissionMode`. |
| `setMcpPermissionModeOverride(server, mode)` (2318) | UNSUP | per-server tighten-only override; no home. |
| `setModel(model?)` (2327) | REP | `SetSessionModel` (undefined arm dropped, see 1.6). |
| `setMaxThinkingTokens()` (2346) | DROP | `@deprecated` (2337). |
| `applyFlagSettings()` (2370) | UNSUP | mid-session settings merge (incl. `effortLevel`); the shim passes `effort`/`thinking` at start with no proto home either. |
| `initializationResult()` (2379) → `SDKControlInitializeResponse` (3448) | PARTIAL | `models` ✓ catalog, `account.subscriptionType` ✓, `fast_mode_state/_disabled_reason` ✓(minus cooldown). `commands` → slash_command.proto (other partition). `agents`, `output_style`, `available_output_styles` UNSUP. |
| `reinitialize()` (2395) | UNSUP / NOTE | redelivers blocked `can_use_tool` requests after a transport gap — the shim-side analogue of WatchTurn replay for OPEN asks; rec does not mention it. Relevant to "callbacks should be idempotent per request_id" → `AgentPermissionId` keyed on `requestId` satisfies it. |
| `supportedCommands()` (2401) | REP elsewhere | slash_command.proto (partition not E). |
| `supportedModels()` (2407) | REP | `model_catalog`. |
| `supportedAgents()` (2413) → `AgentInfo` (105) | UNSUP | name/description/model — no home. |
| `mcpServerStatus()` (2417) | PARTIAL | see 1.3. |
| `getContextUsage()` (2423) | PARTIAL | only `totalTokens` usable for `SessionCold.context_tokens`; `categories[]`, `maxTokens`, `percentage`, `gridRows` UNSUP. |
| `usage_EXPERIMENTAL…()` (2437) | PARTIAL | `subscription_type`, `rate_limits.five_hour` ✓; `session.{total_cost_usd,durations,lines,model_usage}`, `seven_day*`, `model_scoped[]`, `extra_usage`, `behaviors` UNSUP. Proto chose five-hour only — no recorded reason (DROP without citation). Method is marked EXPERIMENTAL; the shim already calls it. |
| `readFile()` (2448) | UNSUP | sidebar viewer; out of scope by nature. |
| `reloadPlugins()` / `reloadSkills()` (2458/2464) | UNSUP | |
| `accountInfo()` (2470) → `AccountInfo` (23) | PARTIAL | `subscriptionType` ✓; `email`, `organization`, `tokenSource`, `apiKeySource`, `apiProvider` UNSUP. |
| `rewindFiles(userMessageId,{dryRun})` (2486) → `RewindFilesResult` (2693) | UNSUP + REQUIREMENT GAP | Not on the wire anywhere. It rewinds **tracked files**, not context (requires `enableFileCheckpointing` 1484). The keep-alive rollback requirement (rec:823-827 Owed G: "roll back context to just after the last real prompt") has NO in-query SDK primitive: the only context rollback is `Options.resumeSessionAt` (1815), which means closing and re-spawning the query — a per-prompt process restart. `SessionRewound`/`KeepAliveDiscard` are shim-internal (rec:310). This is the largest NOROUTE in the partition. |
| `seedReadState()` (2497) | UNSUP | |
| `reconnectMcpServer()` / `toggleMcpServer()` (2511/2519) | UNSUP | |
| `setMcpServers()` (2542) → `McpSetServersResult` (1135) | UNSUP | |
| `streamInput()` (2545) | REP | StartTurn / UpdateTurn.prompt / UpdateSubagent.prompt. |
| `stopTask(taskId)` (2550) | REP | UpdateSubagent.stop / StopBash / UpdateWorkflow.stop. |
| `backgroundTasks(toolUseId?)` (2563) | REP | DetachForeground (targeted). The no-arg "background ALL" form has no proto request — DROP (daemon iterates). |
| `close()` (2581) | REP | KillSession. |
| `WarmQuery` (7118), `startup()` (6757) | UNSUP | pre-warmed process pool; no home. |

### 2.2 `Options` (sdk.d.ts:1322-2063) — every key

| option (rel. line in Options) | status | home / reason |
|---|---|---|
| `abortController` | REP (internal) | KillSession. |
| `additionalDirectories` | UNSUP | (`addDirectories` PermissionUpdate covers the runtime form). |
| `agent`, `agents` (AgentDefinition 38) | UNSUP | |
| `allowedTools`, `disallowedTools`, `toolAliases`, `tools`, `toolConfig` | UNSUP | |
| `canUseTool` | REP | permission.proto. |
| `continue` | DROP(rec:586-588) | daemon never needs most-recent-in-cwd. |
| `cwd` | NOROUTE-reverse | shim passes it (workspace); no conversation.v1 home; `init.cwd` (4424) unrepresented. |
| `env`, `executable`, `executableArgs`, `extraArgs`, `pathToClaudeCodeExecutable`, `spawnClaudeCodeProcess`, `debug`, `debugFile`, `stderr` | DROP | process plumbing, shim-internal. |
| `fallbackModel` | UNSUP | interacts with `effective_model` (a fallback silently changes it; `SDKModelRefusalFallbackMessage` 4090 is the signal — no arm). |
| `enableFileCheckpointing` | UNSUP | see rewindFiles. |
| `forkSession` | DROP(rec:594-595 candidate for compact) | observable only as identity rotation. |
| `betas` (`SdkBeta` 2930) | UNSUP | |
| `hooks` (HookEvent 835, 31 events) | UNSUP | no proto surface; `PermissionRequest` hook (2094) is a second permission path the shim must NOT wire (would bypass `canUseTool`). |
| `onElicitation`, `onUserDialog`, `supportedDialogKinds` | UNSUP | `UserDialogRequest` (7051) is a third blocking-input kind beside question/permission — no `AgentUpdate` arm. |
| `persistSession` | UNSUP | must stay true for transcript-based recovery. |
| `sessionStore`, `sessionStoreFlush`, `loadTimeoutMs` | DROP(rec:594 `@alpha`) | |
| `includeHookEvents` | UNSUP | |
| `includePartialMessages`, `forwardSubagentText`, `agentProgressSummaries` | shim config | affect other partitions' streams; no session-level home, none needed. |
| `thinking`, `effort`, `maxThinkingTokens` | UNSUP | shim passes `thinking`/`effort` with no proto field; `ModelInfo.supportsEffort/supportedEffortLevels/supportsAdaptiveThinking` likewise dropped. |
| `maxTurns`, `maxBudgetUsd`, `taskBudget` | UNSUP | `SDKResultError.subtype error_max_turns/error_max_budget_usd` (4271) then has no failure arm either. |
| `mcpServers`, `strictMcpConfig`, `plugins` | UNSUP | |
| `model` | REP | |
| `outputFormat` | UNSUP | |
| `permissionMode` | REP | 6/6. |
| `planModeInstructions`, `allowDangerouslySkipPermissions`, `permissionPromptToolName` | UNSUP | `bypass` mode requires `allowDangerouslySkipPermissions` (2092 doc) — a precondition the proto does not state. |
| `promptSuggestions` | UNSUP | |
| `resume` | REP | |
| `sessionId` | REP-candidate | fresh-start id minting (1.1). |
| `resumeSessionAt` | UNSUP / keep-alive candidate | see rewindFiles. |
| `sandbox`, `settings`, `managedSettings`, `settingSources`, `skills` | UNSUP | |
| `systemPrompt`, `title` | UNSUP | |

### 2.3 `system/init` — `SDKSystemMessage` (4412-4456)

| field | status | home |
|---|---|---|
| `session_id` | REP | `vendor_session_id`. |
| `model` | REP | `effective_model`. |
| `permissionMode` | REP | |
| `fast_mode_state`, `fast_mode_disabled_reason` | PARTIAL | cooldown missing. |
| `capabilities?` (4450) | UNSUP | open set incl. `interrupt_receipt_v1`, `interrupt_cancel_queued_v1`. rec:2860/2872 relies on them; no proto field carries what the CLI can do (e.g. `SessionRuntime`). |
| `mcp_servers[]` | PARTIAL | initial health → `SessionMcpServer` frames. |
| `agents`, `apiKeySource` (124), `betas`, `claude_code_version`, `cwd`, `tools`, `slash_commands`, `output_style`, `skills`, `plugins[]` | UNSUP | (`slash_commands` is the other partition's.) |

### 2.4 Other session-level messages

| type (line) | status | home / reason |
|---|---|---|
| `SDKSessionStateChangedMessage` (4373) idle/running/requires_action | DROP(rec:2820ish "liveness of a work item → the stream being open") | `requires_action` is implied by an open `AgentPermission`/`AgentQuestion`. |
| `SDKCompactBoundaryMessage` (2943) `trigger`, `pre_tokens`, `post_tokens`, `duration_ms`, `preserved_segment`, `preserved_messages` | UNSUP | Compaction is shim-owned (rec:590) but the vendor's AUTO compaction still fires and is unannounced on the session stream. rec:596 owes "whether our bookkeeping survives the vendor's compact_boundary format" — the metadata needed to do that (`preserved_messages.uuids`) has no home. Other partitions may carry it as a record entry. |
| `SDKStatusMessage` (4401) `status: compacting|requesting`, `compact_result`, `compact_error`, `permissionMode` | PARTIAL | `permissionMode` → `permission_mode_changed`; compaction status/failure UNSUP. |
| `SDKRateLimitEvent` / `SDKRateLimitInfo` (4237-4267) | UNSUP (except five_hour fold) | 13 fields; overage/credits vocabulary entirely absent. |
| `SDKAuthStatusMessage` (2903) | UNSUP | `isAuthenticating`, `output[]`, `error`. rec:4272 lists "authenticating" as an activity in an older design; no session arm now. |
| `SDKWorkerShuttingDownMessage` (4672) | UNSUP | see 1.3. |
| `SDKModelRefusalFallbackMessage` (4090) / `NoFallback` (4122) | UNSUP | silently changes effective model. |
| `SDKBackgroundTasksChangedMessage` (2915) / `BackgroundTaskSummary` (131) | DROP(rec:240-248 relay the level) | the proto in this partition has no "level" frame; `SessionStarted.live_work` is the only set. `BackgroundTaskSummary.{type,status,description,command,agent_type,server,tool,name}` — detached-work partition. |
| `SDKPermissionDeniedMessage` (4168) | REP | `DeniedByPolicy`; `tool_name`, `tool_use_id` carried by `gated_call`. |
| `AccountInfo.apiProvider` (33) | UNSUP | distinguishes firstParty vs bedrock/vertex/gateway — determines whether `account_usage` can ever be available; would explain `service_unavailable` structurally. |
| `ModelInfo` (1224) `resolvedModel`, `supportsEffort`, `supportedEffortLevels`, `supportsAdaptiveThinking`, `supportsFastMode`, `supportsAutoMode` | UNSUP | 6 of 9 fields dropped; `supportsAutoMode`/`supportsFastMode` gate `AgentPermissionModeAuto` and `SessionFastMode` per model. |
| `PermissionResult.allow.updatedInput` (2116) | UNSUP | the user cannot amend the tool input on allow. |
| `PermissionResult.allow/deny.decisionClassification` (2118/2123) | DROP (derivable) | once→`user_temporary`, standing→`user_permanent`, deny→`user_reject` (doc 2072). |
| `PermissionResult.deny.interrupt` (2122) | UNSUP | "deny and stop the turn"; composable from deny + `AgentInput.stop` but not atomic. |
| `PermissionResult.toolUseID` (2117) | DROP | implicit in the keyed answer. |
| `CanUseTool.options.requestId` (248) | DROP | shim key only. |
| `CanUseTool.options.agentID` (238) | REP | `AgentFrame.agent_id`. |
| `PermissionDeniedHookInput` / `PermissionRequestHook*` (2076-2112) | UNSUP | hook path; deliberately not wired. |
| `ExitReason` (630) incl. `bypass_permissions_disabled` | UNSUP | no query-end reason arm. |
| `TerminalReason` (6909) | other partition | |

---

## 3. Consolidated UNSUPPORTED (no home, no recorded reason)

Query methods (13): `setMcpPermissionModeOverride`, `applyFlagSettings`, `reinitialize`, `supportedAgents`, `readFile`, `reloadPlugins`, `reloadSkills`, `rewindFiles`, `seedReadState`, `reconnectMcpServer`, `toggleMcpServer`, `setMcpServers`, `WarmQuery/startup`.
Query partials (5): `initializationResult` (agents/output_style), `mcpServerStatus` (3 statuses + 4 fields), `getContextUsage` (all but total), `usage_EXPERIMENTAL` (session cost, 6 windows, extra_usage, behaviors), `accountInfo` (5 of 6 fields).
Options (30): additionalDirectories, agent, agents, allowedTools, disallowedTools, toolAliases, tools, toolConfig, cwd(no proto home), fallbackModel, enableFileCheckpointing, betas, hooks, onElicitation, onUserDialog, supportedDialogKinds, persistSession, includeHookEvents, thinking, effort, maxThinkingTokens, maxTurns, maxBudgetUsd, taskBudget, mcpServers, strictMcpConfig, plugins, outputFormat, planModeInstructions, allowDangerouslySkipPermissions, permissionPromptToolName, promptSuggestions, resumeSessionAt, sandbox, settings, managedSettings, settingSources, skills, systemPrompt, title.
init fields (10): capabilities, agents, apiKeySource, betas, claude_code_version, cwd, tools, output_style, skills, plugins.
Messages (7): SDKCompactBoundaryMessage metadata, SDKStatusMessage compaction fields, SDKRateLimitEvent, SDKAuthStatusMessage, SDKWorkerShuttingDownMessage, SDKModelRefusalFallback/NoFallback, UserDialogRequest (third blocking kind).
Enum literals (4): `FastModeState.cooldown`; `McpServerStatus.status` `needs-auth|pending|disabled`.
Type fields (9): ModelInfo ×6, PermissionResult `updatedInput`, `deny.interrupt`, AccountInfo `apiProvider`.

## 4. Proto fields with NO SDK route

| field | class |
|---|---|
| `SessionRuntime.shim_build_sha`, `.sdk_version` | by design (shim/package) |
| `SessionStarted.main_agent_id` | by design (rec) |
| `SessionStarted.turn_in_flight` | shim register (no SDK turn id) |
| `SessionStarted.live_work` on resume | **real gap** — level signal is per-process, empty after restart; rec:250's `backgroundTasks()` claim is wrong |
| `SessionColdLapsed.cache_ttl_ms` | **real gap** — SDK never states the TTL tier |
| `SessionColdRemediation.clear` | **real gap** — no "empty context, same id" primitive |
| `SessionColdCompact.*` | by design (rec:590) |
| `SessionIdentityRotated.reason` | **real gap** — no vendor reason exists |
| `SessionDiagnostics.*` | by design |
| `AgentPermissionPrompt.title/display_name` required | SDK optional (217/222) — producer must synthesize when absent |
| keep-alive rollback (`SessionRewound`/`KeepAliveDiscard`, rec Owed G) | **real gap** — no in-query context rewind; `resumeSessionAt` only via re-spawn; `rewindFiles` is files-only |
| `still_queued` fault surfacing (rec:2769) | **real gap** — no failure arm / SessionFault kind to carry it |

## 5. NON-STATIC resolutions

1. `SessionStarted.effective_model` on resume — backward transcript scan for last assistant model (no SDK getter before first turn).
2. `SessionStarted.permission_mode` on resume — same scan for last prompt's mode.
3. `SessionCold.context_tokens` / `last_request_at_ms` before the query exists — transcript tail scan; `getContextUsage()` is only available on a live query.
4. `SessionUpdate.mcp_server` "health changed" — no push message; requires polling `mcpServerStatus()` and diffing (a retained set + diff, which rec:240-244 rejects for tasks).
5. `SessionIdentityRotated` — inferred by diffing `session_id` across consecutive messages (constant per message; borderline static).
6. `SetSessionModel` "resolves after the current turn ends" — an awaited event, by design (rec:490).
7. `ReadHistory` — `getSessionMessages()` returns the whole array (759); paging a page requires either its option type to support a cursor or the shim's own store; verify in the history partition.

## 6. Record errors found

- rec:249-250: "`KillSession`'s every live task is the vendor's own `backgroundTasks()` answer" — `backgroundTasks()` returns `boolean` (2563); the set only ever arrives via `background_tasks_changed` and is lost across a CLI restart.
- rec:2858-2859: `cancel_queued` and `cancel_async_message` are cited as available; neither is reachable from `Query` at 0.3.220 (`interrupt()` takes no args; `cancel_async_message` is an internal control type at 3001).
- rec:2993: cites `setModel` at sdk.d.ts:2327 ✓ and `setPermissionMode` at 2300 ✓ — line refs still correct.
