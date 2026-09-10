# Partition A — SDK stream message surface vs conversation.v1

Source of truth: `agent-shim/claude/shim/node_modules/@anthropic-ai/claude-agent-sdk/sdk.d.ts` (`SDKMessage` union at L4019, 39 members). Embedded vendor types from `node_modules/@anthropic-ai/sdk/resources/beta/messages/messages.d.ts` (cited as `beta:L`) and `resources/messages/messages.d.ts` (cited as `msg:L`). Design record: `docs/protobuf-design/figma-to-idl-redesign.md` (cited as `rec:L`).

Status key: **REP** = represented (home named) · **DROP** = dropped with a recorded reason · **UNS** = unsupported (no home, no recorded reason) · **OTHER** = producer/consumer belongs to another partition (tool input/output types, Query control methods, daemon). "static" annotations follow the rule at rec:197-235: bounded number of keyed single lookups.

Common envelope fields (`uuid`, `session_id`) appear on every subtype; they are tabulated once in §0 and omitted from later tables.

---

## 0. Envelope fields common to every subtype

| field | status | home / reason |
|---|---|---|
| `uuid: UUID` | UNS | No conversation.v1 identity is the SDK uuid; `AgentActivityId` is the `tool_use` id or shim-minted for text/thinking. The uuid is the key the vendor's own retraction signals (`supersedes`, `retracted_message_uuids`) name, so dropping it also makes those unconsumable. No recorded reason. |
| `session_id: string` | REP (static) | `SessionStarted.vendor_session_id` (from init); a later message whose `session_id` differs from the one held → `SessionIdentityRotated{previous, new}` (one lookup of the current id = static). `SessionIdentityRotated.reason` has NO producer (no SDK message states why). |

---

## 1. Per-subtype tables

### 1.1 `SDKAssistantMessage` (L2854-2899)

| field | status | home / reason |
|---|---|---|
| `type:'assistant'` | REP | arm discriminator only |
| `message: BetaMessage` | see §1.1a | |
| `parent_tool_use_id` | REP (static) | `AgentFrame.agent_id`: null → `SessionStarted.main_agent_id`; non-null → one lookup spawn-call→`AgentSubagentStart.created_agent_id`. Depth>1 works the same (the spawning call lives in the subagent's own stream). |
| `error?: SDKAssistantMessageError` (L2901) | REP partial (static) | `AgentFailure.api_request_failed.kind`: `authentication_failed`→`authentication_failed`; `rate_limit`→`rate_limited` (`retry_after_ms` has NO producer here); `overloaded`→`overloaded` (same); `invalid_request`→`invalid_request`; `model_not_found`→`not_found`; `server_error`→`internal`; `billing_error`, `oauth_org_not_allowed`, `unknown` → `unmodeled{type}`. `max_output_tokens` is NOT an API failure (it is a stop) and has no arm: `AgentResponseFailureReason` is empty ("arms DERIVED at the wave"). `ApiRequestFailed.message` must be taken from the message's text block. `ApiPermissionDenied`(403) and `ApiRequestTooLarge`(413) have NO producer on this surface (the SDK enum has no such values). |
| `request_id?` | UNS | no home |
| `resumed_from_incomplete_thinking?: true` | UNS | "This turn continued the preceding truncated assistant turn inside its trailing signed thinking block … a history replayed through the bridge must carry this flag back." No home; loses replay fidelity. |
| `supersedes?: UUID[]` | UNS | "Wire uuids of previously-delivered messages that this message replaces (refusal-fallback supersede) … Evict the named messages on arrival." conversation.v1 has no retraction/eviction frame; units are upsert-only. Resolution per uuid is a single lookup (bounded by list length), but the uuid is not stored (§0). |
| `aborted?: true` | REP partial | Evidence for `AgentResponseFailure` / `AgentThinkingFailure` (prose-so-far carried) and for `AgentSuccess.interrupted`; but `AgentResponseFailureReason` and `AgentThinkingFailure` are empty, so the *reason* (abort) is not stated. |
| `subagent_type?`, `task_description?` | DROP | Redundant with `AgentSubagentPrompt.subagent_type/description` already on the spawn unit (rec:1234 flat attribution by agent_id). |
| `timestamp?` | UNS | No unit carries a settle instant; `AgentActivityStartedAt` is issue time only. (Note the SDK says one API turn may yield several assistant messages sharing `message.id`, "each with its own timestamp".) |

#### 1.1a `BetaMessage` (beta:1487-1612) and `BetaUsage` (beta:2851-2913)

| field | status | home / reason |
|---|---|---|
| `id` | REP (static) | Minting key for text/thinking `AgentActivityId` (`message.id`+index). Not itself on the wire. |
| `container` | DROP | rec:981-984 (api.proto "fine"); also content_blocks header: server-side execution kinds collapse. |
| `content: BetaContentBlock[]` | see §1.1b | |
| `context_management` | UNS | no home |
| `diagnostics` | UNS | no home |
| `model` | REP (static) | `SessionUpdate.model_changed.effective_model` when it differs from the held effective model (one lookup); also the source `SessionStarted.effective_model` is recovered from on resume (rec:407-417). `ModelMarker.SYNTHETIC` recognized before commit. |
| `role` | DROP | always `assistant` |
| `stop_details: BetaRefusalStopDetails` (beta:1849) | UNS | refusal category/explanation — no arm (`AgentResponseFailureReason` empty). |
| `stop_reason: BetaStopReason` (beta:2094) | UNS | `end_turn`/`tool_use`/`max_tokens`/`refusal`/`pause_turn`/`compaction`/`model_context_window_exceeded`/`stop_sequence` — no arm. rec:2364-2370 flags it "unsettled" (fixture reports end_turn everywhere). `max_tokens`/`refusal` are the natural `AgentResponseFailureReason` arms; empty today. |
| `stop_sequence` | DROP | rec:5043 "deliberately remain" unmodelled |
| `usage.input_tokens` | REP | `TokenUsage.input_misses.unwritten` |
| `usage.cache_creation_input_tokens` | REP | `TokenUsage.input_misses.written` |
| `usage.cache_read_input_tokens` | REP | `TokenUsage.input_hits.read` |
| `usage.output_tokens` | REP | `TokenUsage.output_tokens` |
| `usage.output_tokens_details.thinking_tokens` (beta:1753) | REP | `TokenUsage.output_thinking_tokens` (rec:976-980) |
| `usage.cache_creation{ephemeral_5m,ephemeral_1h}` (beta:304) | DROP | rec:981-984 |
| `usage.fallback_credit` (beta:1115) | UNS | post-dates the rec:981 survey; no home |
| `usage.inference_geo` | DROP | rec:981-984 |
| `usage.iterations` | DROP | rec:981-984 |
| `usage.server_tool_use{web_fetch_requests,web_search_requests}` (beta:2020) | DROP | rec:981-984 |
| `usage.service_tier` | DROP | rec:981-984 |
| `usage.speed` | DROP | rec:981-984 (fast mode itself is `SessionFastMode`, produced from `fast_mode_state` elsewhere) |
| `usage` placement rule | REP (static, caveat) | `AgentActivity.usage` on the unit of the response's FIRST block only (rec:1370-1380). With per-block assistant messages sharing `message.id`, "first" = one lookup "have I stamped this message.id" — static, but see NON-STATIC §3 item 1. |

#### 1.1b `BetaContentBlock` members (beta:902) as they appear in `message.content` and in `content_block_start.content_block`

| block / field | status | home / reason |
|---|---|---|
| `BetaTextBlock.text` (beta:2095) | REP | `AgentResponse.success.prose.markdown` (whole text) |
| `BetaTextBlock.citations` | UNS | no home |
| `BetaThinkingBlock.thinking` (beta:2186) | REP | `AgentThinkingSuccess.text`; empty thinking with signature → `withheld` |
| `BetaThinkingBlock.signature` | DROP | rec:2374-2378 (withheld = block+signature, nothing to draw) |
| `BetaRedactedThinkingBlock.data` (beta:1838) | REP (data dropped) | `AgentThinkingWithheld`; the opaque `data` has no home (reasonable: "draws nothing"). |
| `BetaToolUseBlock.id` (beta:2811) | REP | `AgentActivityId.value` for the tool unit; `AgentPermission.gated_call` |
| `BetaToolUseBlock.name` | REP | selects the `AgentActivity.item` arm; unknown → `AgentUnmodeledStart.tool_name` |
| `BetaToolUseBlock.input` | OTHER | per-tool input partition (`AgentReadStart.path`, etc.; `AgentUnmodeledStart.arguments`) |
| `BetaToolUseBlock.caller` | UNS | direct vs server-tool caller — no home |
| `BetaServerToolUseBlock{id,input,name,caller}` (beta:2030) | REP via unmodeled | content_blocks header rules all 17 vendor kinds are "called/returned"; lands as `AgentUnmodeled.start` (name is the closed server set). |
| `BetaWebSearchToolResultBlock`, `BetaWebFetchToolResultBlock`, `BetaAdvisorToolResultBlock`, `BetaCodeExecutionToolResultBlock`, `BetaBashCodeExecutionToolResultBlock`, `BetaTextEditorCodeExecutionToolResultBlock`, `BetaToolSearchToolResultBlock`, `BetaMCPToolResultBlock` | REP via unmodeled (static: `tool_use_id` → call) | `AgentUnmodeledSuccess/Failure.content` — but their structured bodies are NOT text/image; they must be squashed into `ToolResultContentBlock.unsupported{kind, raw}`. Acceptable per `UnsupportedBlock` rule ("kind the producer has no schema for"). |
| `BetaMCPToolUseBlock` | REP via unmodeled | `AgentUnmodeledStart` |
| `BetaContainerUploadBlock` (beta:883) | UNS | assistant-side block with NO home: `AgentActivity.item` has no `unsupported` arm (only `UserContentBlock` and `ToolResultContentBlock` do). |
| `BetaCompactionBlock` (beta:777) | UNS | same gap as above; server-side compaction block in assistant content is unrepresentable. |
| `BetaFallbackBlock{from,to,trigger}` (beta:1008) | UNS | same gap; the model-switch fact it carries could feed `SessionModelChanged` (static) but the block itself has no home. |

### 1.2 `SDKUserMessage` (L4583-4627) and `SDKUserMessageReplay` (L4629-4650)

| field | status | home / reason |
|---|---|---|
| `message: MessageParam` (msg:808) | see §1.2a | |
| `parent_tool_use_id` | REP (static) | as §1.1 |
| `isSynthetic?` | UNS | Synthetic user records (interrupt notices, "[Request interrupted by user]", auto-continuations) — no home and nothing distinguishes them from a person's prompt in `UserSaid`. |
| `tool_use_result?: unknown` | OTHER | per-tool output partition (the structured report: `AgentSubagentSuccess.totals/report/models_used/worktree`, `AgentRead*`, `AgentWrite*.patch`, …) |
| `priority?: 'now'|'next'|'later'` | OTHER/DROP | input-side; the DAEMON is the only queue (rec:2745) — the daemon decides WHEN; not a record fact |
| `origin?: SDKMessageOrigin` (L4024-4072) | UNS | all variants and fields: `human`, `channel.server`, `peer.{from,name,fromSession,senderTaskId,body,verifiedPeerPid}`, `task-notification.subkind`, `coordinator`, `observer.{from,senderTaskId}`, `auto-continuation`, `observer-activity`. `UserSaid` says "which of the two [person vs spawning agent] is read from the parent chain, never from a field"; peer/channel/coordinator/scheduled origins have no reading at all. rec:381 flags only the daemon's `PromptOrigin`, not this. |
| `shouldQuery?` | UNS | "When false, the message is appended without triggering an assistant turn … merged into the next user message that does query." A prompt that opens no turn is unrepresentable (`HistoryPrompt` is keyed by `TurnId`). |
| `timestamp?` | UNS | no instant on `UserSaid`/`HistoryPrompt` |
| `uuid?`, `session_id?` | §0 | |
| `subagent_type?`, `task_description?` | DROP | as §1.1 |
| Replay-only `isReplay: true` | DROP | ack of the daemon's own submission; `HistoryPrompt.id` is the daemon-minted `TurnId` (rec:792-800) |
| Replay-only `file_attachments?: unknown[]` | UNS | untyped; no home |
| NOT DECLARED: `isMeta`, `sourceToolUseID`, `isCompactSummary`, `permissionMode`, `parent_agent_id` | finding | The skill document (rec:1534: `isMeta:true` + `sourceToolUseID:T` → `AgentSkillUseSuccess.document`) and the compaction summary (`ContextCompacted.summary`) are resolved from fields that do NOT exist on `SDKUserMessage` in sdk.d.ts (grep: zero hits for `isMeta`/`sourceToolUseID`; `parent_agent_id` only on `SessionMessage` L4739). They are JSONL transcript fields; on the live stream surface those producers are UNDECLARED. `AgentSkillAllowedTools` comes from an `attachment` record (rec:1537) that is not an `SDKMessage` at all. |

#### 1.2a `MessageParam.content` blocks (msg:555)

| block / field | status | home / reason |
|---|---|---|
| `content: string` | REP | `UserContentBlock.text` (prompt echo) or a tool_result string |
| `TextBlockParam.text` (msg:1059) | REP | `UserContentBlock.text` / `ToolResultContentBlock.text` |
| `TextBlockParam.citations`, `cache_control` | DROP | request plumbing |
| `ImageBlockParam.source` Base64 (msg:95) | REP (spill) | `ImageBlock.path` ("a producer that spilled vendor-inlined bytes to disk uses this") + `media_type` |
| `ImageBlockParam.source` URL (msg:1560) | REP | `ImageBlock.url` + `media_type` |
| `ToolResultBlockParam.tool_use_id` (msg:1359) | REP (static) | join key → the tool unit (one lookup) |
| `ToolResultBlockParam.is_error` | REP | selects `*Failure` vs `*Success` arm |
| `ToolResultBlockParam.content` | REP | `ToolResultContent.blocks` for unmodeled; typed tools take payload from `tool_use_result` (OTHER). `SearchResultBlockParam`/`DocumentBlockParam`/`ToolReferenceBlockParam` inside a result → `unsupported{kind,raw}`. |
| `DocumentBlockParam`, `SearchResultBlockParam`, `MidConversationSystemBlockParam` (msg:836) as top-level user blocks | REP | `UserContentBlock.unsupported{kind,raw}` |
| `ThinkingBlockParam`, `RedactedThinkingBlockParam`, `ToolUseBlockParam`, `ServerToolUseBlockParam`, `*ToolResultBlockParam`, `ContainerUploadBlockParam` in a USER message | UNS | `UserContent` states "no reasoning and no tool calls, because a person produces neither" — but the vendor type admits them on `role:'user'` (and `role:'assistant'|'system'` on `MessageParam.role`). A replayed `assistant`-role `MessageParam` (user-side prefill/continuation) has no home. |

### 1.3 `SDKResultSuccess` (L4292-4325) / `SDKResultError` (L4269-4288)

| field | status | home / reason |
|---|---|---|
| `subtype` | REP partial | `success` → `AgentSuccess.completed`; `error_during_execution`/`error_max_turns`/`error_max_budget_usd`/`error_max_structured_output_retries` → NO `AgentFailure` arm (only `api_request_failed`; "further arms DERIVED at the wave"). UNS for the four error subtypes. |
| `duration_ms`, `duration_api_ms` | DROP | rec:541 `response_timing → none (derived from frame instants)` |
| `ttft_ms?`, `ttft_stream_ms?`, `time_to_request_ms?`, `request_sent_wall_ms?`, `time_to_request_from_spawn_ms?`, `warm_spare_claimed?`, `time_origin_ms?` | UNS | latency telemetry; no home. (rec:4419 once named `optional ttft_ms` on a frontend tokens element — that surface no longer carries it from conversation.v1.) |
| `user_message_uuid?` | UNS | link to the prompt; `TurnId` is daemon-minted so the link is already known — harmless, no reason recorded |
| `is_error`, `api_error_status?` | REP partial | selects success/failure; status code has no field (`ApiRequestFailed` kinds are arms, not codes) |
| `num_turns` | UNS | no home |
| `result: string` | DROP | `AgentCompleted.answer` NAMES the last response unit instead (rec:963-966); static ("last response id" one lookup). |
| `stop_reason` | UNS | as §1.1a |
| `total_cost_usd` | UNS | rec:3530 lists `total_cost_usd` as "drawn home" on a frontend surface, but nothing in conversation.v1 carries it and `TokenUsage` says rates are not stored. |
| `usage: NonNullableUsage` (turn aggregate) | DROP | `AgentActivity.usage` per response; "a consumer SUMS this across units" |
| `modelUsage: Record<string,ModelUsage>` (L1265) — `inputTokens, outputTokens, cacheReadInputTokens, cacheCreationInputTokens, webSearchRequests, costUSD, contextWindow, maxOutputTokens, canonicalModel, provider` | UNS | per-model breakdown; `contextWindow` in particular is what `SessionCold.context_tokens` wants and has no live producer here. |
| `permission_denials: SDKPermissionDenial[]` (L4159 `{tool_name, tool_use_id, tool_input}`) | DROP | the live `permission_denied` system message (§1.26) already produced `AgentPermissionDenied.policy` per call |
| `errors: string[]` | REP partial | `ApiRequestFailed.message` when the failure is an API one; otherwise UNS |
| `structured_output?` | UNS | no home |
| `deferred_tool_use?: SDKDeferredToolUse{id,name,input}` (L3848) | UNS | tool deferred to the host (`terminal_reason 'tool_deferred'`) — no arm |
| `terminal_reason?: TerminalReason` (L6909) | REP partial | `aborted_streaming`/`aborted_tools` = evidence for `AgentSuccess.interrupted` (the "acknowledged stop"); `completed` → completed; `api_error`/`model_error` → `api_request_failed`; `blocking_limit`, `rapid_refill_breaker`, `prompt_too_long`, `image_error`, `malformed_tool_use_exhausted`, `stop_hook_prevented`, `hook_stopped`, `tool_deferred`, `max_turns`, `background_requested`, `budget_exhausted`, `structured_output_retry_exhausted`, `tool_deferred_unavailable`, `turn_setup_failed` → UNS (no arm). |
| `fast_mode_state?` (L640), `fast_mode_disabled_reason?` (L635) | REP (static) | `SessionFastMode.on` / `.off.reason` when it differs from held (one lookup) |
| `origin?` | UNS | as §1.2 |

### 1.4 `SDKPartialAssistantMessage` (L4150-4157) — `stream_event`

| field | status | home / reason |
|---|---|---|
| `parent_tool_use_id` | REP (static) | as §1.1 |
| `ttft_ms?` | UNS | no home |
| `event: message_start.message` (beta:1830) | REP (static) | `message.id` + `usage` (initial input counts) — mints the block identities; usage is finalized from the following `assistant` message |
| `event: message_delta.delta.{stop_reason, stop_sequence, stop_details, container}` (beta:1789-1829) | UNS/DROP | as §1.1a |
| `event: message_delta.usage: BetaMessageDeltaUsage` (beta:1613) — `cache_creation_input_tokens, cache_read_input_tokens, fallback_credit, input_tokens, iterations, output_tokens, server_tool_use` | DROP | cumulative; superseded by the assistant message's `usage` (same `TokenUsage` mapping) |
| `event: message_delta.context_management` | UNS | as §1.1a |
| `event: message_stop` | DROP | nothing to draw; settle comes from the assistant message |
| `event: content_block_start.index` | REP (static, caveat) | keyed lookup `(agent, index)` → open unit; see §3 item 1 |
| `event: content_block_start.content_block` text/thinking | REP | `AgentResponse.start` / `AgentThinking.start`; `redacted_thinking` → `AgentThinking.update.withheld` |
| `event: content_block_start.content_block` tool_use/server_tool_use | REP partial | only `id`+`name` are present (input empty); the tool `start` arm needs the input (path, command) which arrives only as `input_json_delta` fragments → the start frame is emitted from the later `assistant` message, not here. Tool units therefore do NOT exist during streaming of their arguments (DROP per `AgentUnmodeled` "no update arm"; for typed tools no reason recorded). |
| `event: content_block_delta.delta: text_delta.text` (beta:2118) | REP | `AgentResponseUpdate.new_markdown` (Ruling 3, rec:255-278) |
| `event: content_block_delta.delta: thinking_delta.thinking` (beta:2243) | REP | `AgentThinkingTextDelta.new_text` |
| `event: content_block_delta.delta: thinking_delta.estimated_tokens` | UNS | rec:1365/1384 "owed item D" — live thinking ESTIMATE has no type; recorded as OWED, not as a decision to drop. Counted UNS. |
| `event: content_block_delta.delta: signature_delta.signature` (beta:2056) | DROP | as signature |
| `event: content_block_delta.delta: input_json_delta.partial_json` (beta:1266) | DROP (typed tools: no reason) | no tool has a pre-call update arm; accumulating it would be a variable-size buffer (forbidden). |
| `event: content_block_delta.delta: citations_delta.citation` (beta:522) | UNS | as citations |
| `event: content_block_delta.delta: compaction_delta.{content,encrypted_content}` (beta:812) | UNS | as compaction block |
| `event: content_block_stop.index` | REP (static) | closes the `(agent,index)` slot; the settled `success` frame comes from the assistant message |

### 1.5 `SDKSystemMessage` — `init` (L4412-4456)

| field | status | home / reason |
|---|---|---|
| `agents?: string[]` | UNS | available subagent types; no home |
| `apiKeySource: ApiKeySource` (L124) | UNS | no home |
| `betas?: string[]` | UNS | no home |
| `claude_code_version` | UNS | `SessionRuntime` carries `shim_build_sha` and `sdk_version` (package version) but NOT the CLI binary version the SDK reports. |
| `cwd` | UNS | no home (workspace.v1 holds the workspace path on the daemon side) |
| `tools: string[]` | UNS | no home |
| `mcp_servers[].name` | REP | `SessionMcpServer.name` |
| `mcp_servers[].status: string` | REP partial | `SessionMcpServer.health` connected/failed — the vendor status is a free string (values not in evidence: "connected", "failed", "pending", "needs-auth"?); `SessionMcpServerFailed.error` has NO producer here (init carries no error text). |
| `model` | REP | `SessionStarted.effective_model` |
| `permissionMode: PermissionMode` (L2092) | REP | `SessionStarted.permission_mode` (six arms match 1:1) |
| `slash_commands: string[]` | DROP | `SessionCommand` is a CLOSED enum by design (slash_command.proto header); custom commands expand into prompts |
| `output_style` | UNS | no home |
| `skills: string[]` | UNS | no home |
| `plugins[].{name,path,version}` | UNS | no home |
| `fast_mode_state?`, `fast_mode_disabled_reason?` | REP | `SessionUpdate.fast_mode` (no start-time field on `SessionStarted`; would have to be emitted as the first update) |
| `capabilities?: string[]` | UNS | feature-detect list; shim-internal at best, no recorded reason |

### 1.6 `SDKStatusMessage` (L4401-4410)

| field | status | home / reason |
|---|---|---|
| `status: 'compacting'|'requesting'|null` (L4399) | UNS | frontend.v1 footer `StatusActivity` has arms for these activities (rec:4268) but conversation.v1 carries nothing to drive them; session.proto says "nothing about any turn … rides here". |
| `permissionMode?` | REP (static) | `SessionPermissionModeChanged` |
| `compact_result?: 'success'|'failed'` | UNS | a failed compaction has no home (`ContextCut` only states a cut that happened) |
| `compact_error?` | UNS | same |

### 1.7 `SDKCompactBoundaryMessage` (L2943-2975)

| field | status | home / reason |
|---|---|---|
| `compact_metadata.trigger: 'manual'|'auto'` | DROP | rec:851-853: a cut is the family's outcome "by ANY trigger" |
| `compact_metadata.pre_tokens` | REP | `ContextCompacted.tokens.tokens_before` |
| `compact_metadata.post_tokens?` | REP partial | `tokens_after` — optional upstream but required (non-optional int64) downstream; absence would be written as 0 (the rec:855 "can `tokens_after` be filled honestly" question stands). |
| `compact_metadata.duration_ms?` | UNS | no home |
| `compact_metadata.preserved_segment{head_uuid,anchor_uuid,tail_uuid}` | UNS | relink info for partial compaction; no home |
| `compact_metadata.preserved_messages{anchor_uuid,uuids[]}` | UNS | same |
| (summary text) | no producer here | `ContextCompacted.summary` needs the compact-summary user message (JSONL `isCompactSummary`, undeclared on the SDK surface) |

### 1.8 `SDKAPIRetryMessage` (L2842-2852)

| field | status | home / reason |
|---|---|---|
| `attempt`, `max_retries`, `retry_delay_ms`, `error_status`, `error: SDKAssistantMessageError` | UNS | no home; a retry is neither a failure nor an activity in conversation.v1. (`retry_delay_ms` is the only vendor retry hint in evidence, yet `ApiRateLimited.retry_after_ms` is attached to the FAILURE, which the SDK never pairs with a delay.) |

### 1.9 `SDKControlRequestProgressMessage` (L3734-3748)

| field | status | home / reason |
|---|---|---|
| `request_id`, `status: 'started'|'api_retry'`, `attempt?`, `max_retries?`, `retry_delay_ms?`, `error_status?` | UNS | progress of a client-originated control request (side_question); no home |

### 1.10 `SDKModelRefusalFallbackMessage` (L4090-4120)

| field | status | home / reason |
|---|---|---|
| `trigger:'refusal'`, `direction` | UNS | |
| `original_model`, `fallback_model` | REP partial (static) | `SessionModelChanged.effective_model = fallback_model` (the swap is "persistent for the session"); `original_model` dropped, no reason |
| `request_id` | UNS | |
| `api_refusal_category?`, `api_refusal_explanation?` | UNS | no refusal arm (`AgentResponseFailureReason` empty; rec:5016 `StopRefusal` belonged to the DELETED record model) |
| `retracted_message_uuids?` | UNS | eviction signal; no retraction frame exists (see `supersedes`) |
| `refused_user_message_uuid?` | UNS | rewind target; no home |
| `content: string` | UNS | banner text; no home |

### 1.11 `SDKModelRefusalNoFallbackMessage` (L4122-4136)

| field | status | home / reason |
|---|---|---|
| `original_model`, `request_id`, `api_refusal_category?`, `api_refusal_explanation?`, `refused_user_message_uuid?`, `content` | UNS | "The structured counterpart to detecting stop_reason 'refusal'" — no arm |

### 1.12 `SDKLocalCommandOutputMessage` (L3972-3981)

| field | status | home / reason |
|---|---|---|
| `content: string` | UNS | "Output from a local slash command (e.g. /voice, /usage). Displayed as assistant-style text." The slash_command family suppresses the user message for a recognized command and carries only `ContextCut` outcomes; `/cost`, `/usage`, `/status`, … output has no home. |

### 1.13 `SDKHookStartedMessage` (L3929), `SDKHookProgressMessage` (L3901), `SDKHookResponseMessage` (L3914)

| field | status | home / reason |
|---|---|---|
| `hook_id`, `hook_name`, `hook_event` | UNS | no hook unit exists |
| progress `stdout`, `stderr`, `output` | UNS | |
| response `output`, `stdout`, `stderr`, `exit_code?`, `outcome: 'success'|'error'|'cancelled'` | UNS | (footer `StatusActivity.hook` exists on frontend.v1 with no conversation.v1 feeder) |

### 1.14 `SDKPluginInstallMessage` (L4214-4225)

| field | status | home / reason |
|---|---|---|
| `status`, `name?`, `error?` | UNS | headless plugin install progress; no home |

### 1.15 `SDKToolProgressMessage` (L4553-4572)

| field | status | home / reason |
|---|---|---|
| `tool_use_id`, `tool_name`, `parent_tool_use_id` | REP (static) | join to the unit |
| `elapsed_time_seconds` | DROP | `AgentActivityStartedAt` comment: "the producer never restates an elapsed figure … it would arrive at the producer's heartbeat cadence"; `AgentBash` header: "carries elapsed time and a liveness flag and no bytes". (rec:2799 earlier said "an UPDATE FRAME"; the landed comment supersedes it.) |
| `task_id?` | REP (static) | join to `DetachedWorkId` |
| `heartbeat?` | DROP | liveness = open stream (rec:2796) |
| `subagent_type?` | DROP | on the spawn unit |
| `subagent_retry?{agent_id, attempt, max_retries, retry_delay_ms, error_status, error_category}` | UNS | a subagent's API retry; no home (same gap as §1.8) |

### 1.16 `SDKAuthStatusMessage` (L2903-2910)

| field | status | home / reason |
|---|---|---|
| `isAuthenticating`, `output: string[]`, `error?` | UNS | no session arm (footer `authenticating{line}` on frontend.v1 has no feeder) |

### 1.17 `SDKTaskStartedMessage` (L4498-4520)

| field | status | home / reason |
|---|---|---|
| `task_id` | REP | `AgentDetachedWork.work.value`; ASSUMED also to equal the subagent's `agentId` for `AgentSubagentStart.created_agent_id` — not stated by the type; the agent id is otherwise only in the tool_result trailer (OTHER). |
| `tool_use_id?` | REP (static) | present → `origin.detached.detached_from_id`; absent → `origin.created` (rec:2455-2480) |
| `description` | REP | `AgentSubagentPrompt.description` (optional, unset for workflow agents rec:1029) |
| `subagent_type?` | REP | `AgentSubagentPrompt.subagent_type` |
| `task_type?` | REP (static) | selects `DetachableWork` arm (`subagent` / `bash` / `'local_workflow'`→`workflow`); vocabulary not in evidence beyond `local_workflow` |
| `workflow_name?` | REP | `AgentWorkflowStart.name` |
| `prompt?` | REP | `AgentSubagentPrompt.text` |
| `skip_transcript?` | UNS | rec:2931 rules it "rides the stream as a RENDERING property" — but NO conversation.v1 field carries it. Recorded intent with no field. |
| (cause) | no producer here | `DetachedWorkDetached.cause{requested|by_user|timed_out}`: `requested` needs the spawn input `run_in_background` (OTHER, one lookup); `by_user` possibly from `task_updated.patch.is_backgrounded`; `timed_out` and `DetachedCauseTimedOut.timeout_ms` have NO producer on this surface. |

### 1.18 `SDKTaskProgressMessage` (L4476-4496)

| field | status | home / reason |
|---|---|---|
| `task_id`, `tool_use_id?` | REP (static) | join |
| `description`, `subagent_type?` | REP | `AgentSubagentUpdate.prompt` (`.text` needs one lookup of the start — static) |
| `usage.{total_tokens, tool_uses, duration_ms}` | REP | `AgentSubagentProgress.{total_tokens, tool_use_count, duration_ms}` (rec:2060 notes a SHELL task also carries these; `AgentBash` has no progress arm → for bash tasks UNS) |
| `last_tool_name?` | REP | `AgentSubagentActivityLabel.tool_name` |
| `summary?` | REP | `AgentSubagentNote.text` |

### 1.19 `SDKTaskNotificationMessage` (L4458-4474)

| field | status | home / reason |
|---|---|---|
| `task_id`, `tool_use_id?` | REP (static) | join |
| `status: 'completed'|'failed'|'stopped'` | REP partial | subagent: completed → `AgentSubagentSuccess` (body from OTHER), stopped → the stream's `AgentSuccess.interrupted`, failed → `AgentSubagentFailure` (empty arms). workflow: completed → `AgentWorkflowCompleted`, stopped → `AgentWorkflowInterrupted`, failed → `AgentWorkflowRunEnded` (empty). bash: completed → `AgentBashSuccess` (output from the file: OTHER). |
| `output_file` | UNS | path of the task's spool/report; no home (the detached bash `AgentBashUpdate` stream would be READ from it, but the path itself is never carried) |
| `summary` | REP partial | `AgentWorkflowSummary.text` for a workflow; for a subagent the report comes from `tool_use_result` (OTHER) — summary itself has no subagent home beyond `AgentSubagentNote` |
| `usage?.{total_tokens, tool_uses, duration_ms}` | REP partial | `AgentSubagentTotals.{tool_use_count, duration_ms}`; `TokenUsage usage` (a breakdown) CANNOT be filled from `total_tokens` — needs OTHER (`tool_use_result`). |
| `skip_transcript?` | UNS | as §1.17 |

### 1.20 `SDKTaskUpdatedMessage` (L4522-4542)

| field | status | home / reason |
|---|---|---|
| `task_id` | REP (static) | join |
| `patch.status?: pending|running|completed|failed|killed|paused` | UNS | This is the BACKGROUND-TASK state (subagent/bash), not the tracker; detached work has no status-update arm (only terminal arms). Note the vocabulary coincides exactly with `AgentTaskState.status`, whose producer is the TaskCreate/TaskUpdate TOOL (OTHER) — a reader could confuse the two. |
| `patch.description?` | UNS | |
| `patch.end_time?` | UNS | no settle instant anywhere |
| `patch.total_paused_ms?` | UNS | |
| `patch.error?` | UNS | would be the natural `AgentSubagentFailure`/`AgentWorkflowRunEnded` arm payload; those are empty |
| `patch.is_backgrounded?` | REP partial (static) | candidate producer for `DetachedCauseByUser` (Ctrl+B) — one lookup by task_id; not recorded as such. A patch is a delta but each field is self-contained, so no diff is needed. |

### 1.21 `SDKBackgroundTasksChangedMessage` (L2915-2933)

| field | status | home / reason |
|---|---|---|
| `tasks[].{task_id, task_type, description}` | UNS (contradicts record) | rec:243-249 Ruling 2: "relays the level verbatim as a session fact for the daemon". `SessionUpdate` has NO such arm; `SessionStarted.live_work` exists only at start and needs full `AgentDetachedWork` announcements (each one lookup by task_id — static). The recorded intent has no field. |

### 1.22 `SDKThinkingTokensMessage` (L4544-4551)

| field | status | home / reason |
|---|---|---|
| `estimated_tokens`, `estimated_tokens_delta` | UNS (recorded as OWED) | rec:1365, 1384-1392: "needs its own type … OWED". Zero observed occurrences. |

### 1.23 `SDKSessionStateChangedMessage` (L4373-4379)

| field | status | home / reason |
|---|---|---|
| `state: 'idle'|'running'|'requires_action'` | DROP | liveness is structural: turn stream open = running; a blocking `AgentUpdate.question/permission` = requires_action (rec:2796, 2877-2881) |

### 1.24 `SDKWorkerShuttingDownMessage` (L4672-4680)

| field | status | home / reason |
|---|---|---|
| `reason: string` | UNS | `SessionQueryDied` has `unexpected_eof` / `iterator_failure{cause}` only; an announced graceful teardown with a reason has no arm |

### 1.25 `SDKCommandsChangedMessage` (L2935-2941) and `SlashCommand` (L6641)

| field | status | home / reason |
|---|---|---|
| `commands[].{name, description, argumentHint, aliases?}` | DROP | closed `SessionCommand` enum with `SessionCommandSpec` options; custom commands are prompts (slash_command.proto header). The vendor's `aliases` ("/cost and /stats both resolve to /usage") contradict the enum's one-literal-per-command — flagged, not a field. |

### 1.26 `SDKPermissionDeniedMessage` (L4168-4190)

| field | status | home / reason |
|---|---|---|
| `tool_name` | DROP | the gated unit's arm already names it |
| `tool_use_id` | REP | `AgentPermission.gated_call` |
| `agent_id?` | REP (static) | `AgentFrame.agent_id` (the only SDK message that names the agent directly) |
| `decision_reason_type?` | REP (mismatch) | `AgentPermissionDeniedByPolicy.decider` is a REQUIRED string; upstream is optional → empty-string sentinel when absent |
| `decision_reason?` | REP | `AgentPermissionDeniedByPolicy.reason` (optional, matches) |
| `message` | REP | `AgentPermissionDeniedByPolicy.message` |
| (id) | shim-minted | `AgentPermission.id`; `started_at` = receive time; `start` frame: "a consumer sees `start` and this in one frame, or this alone" |

### 1.27 `SDKNotificationMessage` (L4138-4148)

| field | status | home / reason |
|---|---|---|
| `key`, `text`, `priority`, `color?`, `timeout_ms?` | UNS | loop-side notification; no home |

### 1.28 `SDKFilesPersistedEvent` (L3866-3878)

| field | status | home / reason |
|---|---|---|
| `files[].{filename,file_id}`, `failed[].{filename,error}`, `processed_at` | UNS | no home |

### 1.29 `SDKToolUseSummaryMessage` (L4574-4581)

| field | status | home / reason |
|---|---|---|
| `summary`, `preceding_tool_use_ids[]` | UNS | model-written summary of a run of tool calls; no home (each id is a single lookup, bounded by list length) |

### 1.30 `SDKMemoryRecallMessage` (L3997-4017)

| field | status | home / reason |
|---|---|---|
| `mode`, `memories[].{path, scope, content?}` | UNS | no home |

### 1.31 `SDKRateLimitEvent` (L4237-4248) / `SDKRateLimitInfo` (L4250-4267)

| field | status | home / reason |
|---|---|---|
| `rate_limit_info.status: allowed|allowed_warning|rejected` | UNS | no arm |
| `resetsAt?` | REP partial | `SessionUsageWindow.resets_at_ms` (when `rateLimitType==='five_hour'`) |
| `rateLimitType?` | REP partial | only `five_hour` has a home; `seven_day*`, `overage` → UNS (rec:548 flags the weekly allowance as having no producer) |
| `utilization?` | REP partial | `SessionUsageWindow.utilization_percent` (scale not stated by the type) |
| `overageStatus?`, `overageResetsAt?`, `overageDisabledReason?`, `isUsingOverage?`, `overageInUse?`, `surpassedThreshold?`, `errorCode?`, `canUserPurchaseCredits?`, `hasChargeableSavedPaymentMethod?` | UNS | no home |
| (subscription_type) | no producer here | `SessionAccountUsage.subscription_type` comes from the shim's own usage sampler (`subscription-usage.ts`), not this message |

### 1.32 `SDKElicitationCompleteMessage` (L3857-3864)

| field | status | home / reason |
|---|---|---|
| `mcp_server_name`, `elicitation_id` | UNS | no home |

### 1.33 `SDKPromptSuggestionMessage` (L4227-4235)

| field | status | home / reason |
|---|---|---|
| `suggestion` | UNS | no home |

### 1.34 `SDKMirrorErrorMessage` (L4074-4088)

| field | status | home / reason |
|---|---|---|
| `error`, `key.{projectKey, sessionId, subpath?}` | UNS | vendor transcript-mirror data loss; `SessionDiagnostics.SessionFault` is the SHIM's own health (pulled), not the vendor's |

### 1.35 `SDKInformationalMessage` (L3942-3964)

| field | status | home / reason |
|---|---|---|
| `content`, `level: info|notice|suggestion|warning`, `tool_use_id?`, `prevent_continuation?` | UNS | "hook feedback (e.g. a UserPromptSubmit hook's block reason), slash-command output"; `prevent_continuation` ("execution stops after this message") is a turn-ending fact with no `AgentFailure` arm |

### 1.36 `SDKConversationResetMessage` (L3841-3846)

| field | status | home / reason |
|---|---|---|
| `new_conversation_id: UUID` | UNS | a reset (likely `/clear`) — `ContextCleared.tokens` needs before/after counts this message does not carry; the new id has no home |

### 1.37 `SDKActiveGoalMessage` (L2826-2838) — declared but NOT in the `SDKMessage` union

| field | status | home / reason |
|---|---|---|
| `value{condition, iterations, set_at, tokens_at_start, last_reason?} | null` | UNS | out of union; listed for completeness |

---

## 2. Consolidated UNSUPPORTED list (no home, no recorded reason)

Grouped; each line one field (or one tightly bound group on one type).

**Identity / retraction**
1. every subtype `.uuid` — the key all vendor retraction signals name
2. `SDKAssistantMessage.supersedes` — "Evict the named messages on arrival"
3. `SDKModelRefusalFallbackMessage.retracted_message_uuids` — "resolution-time eviction signal"
4. `SDKModelRefusalFallbackMessage.refused_user_message_uuid`, `SDKModelRefusalNoFallbackMessage.refused_user_message_uuid` — "the rewind target"
5. `SDKAssistantMessage.resumed_from_incomplete_thinking` — replay fidelity flag the bridge "must carry back"
6. `SDKAssistantMessage.request_id`, `SDKModelRefusal*.request_id`, `SDKControlRequestProgressMessage.request_id`

**Stop / refusal / error taxonomy**
7. `BetaMessage.stop_reason` (all 8 values) and `message_delta.delta.stop_reason`
8. `BetaMessage.stop_details` / `BetaRefusalStopDetails` (category, explanation)
9. `SDKModelRefusalFallbackMessage.{trigger, direction, original_model, api_refusal_category, api_refusal_explanation, content}`
10. `SDKModelRefusalNoFallbackMessage.{original_model, api_refusal_category, api_refusal_explanation, content}`
11. `SDKAssistantMessageError` values `max_output_tokens` (a stop, not an API error) — no reason arm
12. `SDKResultError.subtype` four error kinds; `SDKResultSuccess/Error.terminal_reason` 14 of 19 values (listed §1.3)
13. `SDKResultSuccess/Error.{num_turns, is_error+api_error_status (code), structured_output, deferred_tool_use, errors[] when not API, total_cost_usd, modelUsage.*}`
14. `SDKAPIRetryMessage.*` (attempt, max_retries, retry_delay_ms, error_status, error)
15. `SDKToolProgressMessage.subagent_retry.*`
16. `SDKControlRequestProgressMessage.*`
17. `SDKInformationalMessage.{content, level, tool_use_id, prevent_continuation}`

**Assistant content**
18. `BetaTextBlock.citations`, `citations_delta.citation`
19. `BetaToolUseBlock.caller`, `BetaServerToolUseBlock.caller`
20. `BetaContainerUploadBlock`, `BetaCompactionBlock`, `BetaFallbackBlock`, `compaction_delta` — assistant-side blocks with no `unsupported` arm on `AgentActivity.item`
21. `BetaMessage.{context_management, diagnostics}`, `message_delta.context_management`
22. `BetaUsage.fallback_credit`
23. `thinking_delta.estimated_tokens`, `SDKThinkingTokensMessage.{estimated_tokens, estimated_tokens_delta}` (recorded as OWED, never typed)
24. `SDKAssistantMessage.timestamp`, `SDKUserMessage.timestamp`, `SDKPartialAssistantMessage.ttft_ms`, `SDKResultSuccess.{ttft_ms, ttft_stream_ms, time_to_request_ms, request_sent_wall_ms, time_to_request_from_spawn_ms, warm_spare_claimed, time_origin_ms, user_message_uuid}`

**User side**
25. `SDKUserMessage.isSynthetic`
26. `SDKUserMessage.origin` / `SDKMessageOrigin` — all 9 variants and every field
27. `SDKUserMessage.shouldQuery`
28. `SDKUserMessageReplay.file_attachments`
29. `MessageParam.role:'assistant'|'system'` and thinking/tool_use/server-tool/container blocks inside a user-role message
30. (finding, not a field) `isMeta`, `sourceToolUseID`, `isCompactSummary`, `parent_agent_id` are UNDECLARED on `SDKUserMessage` yet are the recorded producers of `AgentSkillUseSuccess.document` and `ContextCompacted.summary`; `AgentSkillAllowedTools` comes from an `attachment` record outside the union

**Session / init**
31. `SDKSystemMessage(init).{agents, apiKeySource, betas, claude_code_version, cwd, tools, output_style, skills, plugins[], capabilities}`
32. `SDKSystemMessage(init).mcp_servers[].status` beyond connected/failed; `SessionMcpServerFailed.error` has no producer
33. `SDKStatusMessage.{status, compact_result, compact_error}`
34. `SDKCompactBoundaryMessage.compact_metadata.{duration_ms, preserved_segment, preserved_messages}`; `post_tokens` optional→required mismatch
35. `SDKConversationResetMessage.new_conversation_id`
36. `SDKAuthStatusMessage.{isAuthenticating, output, error}`
37. `SDKRateLimitInfo.{status, rateLimitType≠five_hour, overageStatus, overageResetsAt, overageDisabledReason, isUsingOverage, overageInUse, surpassedThreshold, errorCode, canUserPurchaseCredits, hasChargeableSavedPaymentMethod}`
38. `SDKWorkerShuttingDownMessage.reason`
39. `SDKMirrorErrorMessage.{error, key.*}`
40. `SDKPluginInstallMessage.{status, name, error}`
41. `SDKNotificationMessage.{key, text, priority, color, timeout_ms}`
42. `SDKFilesPersistedEvent.{files[], failed[], processed_at}`
43. `SDKElicitationCompleteMessage.{mcp_server_name, elicitation_id}`
44. `SDKPromptSuggestionMessage.suggestion`
45. `SDKMemoryRecallMessage.{mode, memories[]}`
46. `SDKToolUseSummaryMessage.{summary, preceding_tool_use_ids}`
47. `SDKLocalCommandOutputMessage.content`
48. `SDKHookStartedMessage.*`, `SDKHookProgressMessage.*`, `SDKHookResponseMessage.*`

**Detached work / tasks**
49. `SDKBackgroundTasksChangedMessage.tasks[]` — rec Ruling 2 says "relay the level"; no `SessionUpdate` arm exists
50. `SDKTaskStartedMessage.skip_transcript`, `SDKTaskNotificationMessage.skip_transcript` — rec:2931 "rides the stream as a RENDERING property"; no field
51. `SDKTaskNotificationMessage.output_file`
52. `SDKTaskNotificationMessage.usage` for a BASH task (no `AgentBash` progress/totals arm); `SDKTaskProgressMessage.usage` for a bash task likewise
53. `SDKTaskUpdatedMessage.patch.{status, description, end_time, total_paused_ms, error}` (`is_backgrounded` is a plausible but unrecorded `DetachedCauseByUser` producer)
54. `SDKTaskNotificationMessage.status:'failed'` detail — lands on empty `AgentSubagentFailure` / `AgentWorkflowRunEnded`

**Out of union**
55. `SDKActiveGoalMessage.*`

Count: 55 line items; by individual field roughly 190.

---

## 3. NON-STATIC resolutions (would need retained variable state, positional inference, or a diff)

1. **Stream `index` → settled block identity.** `content_block_*` events carry only `index`; the settled `SDKAssistantMessage` is "one per content block sharing `message.id`" (L2889) and its `message.content` is then a one-element array whose original index is NOT carried. Joining the settled block to the unit minted at `content_block_start(index)` requires counting how many assistant messages with that `message.id` have already arrived (POSITIONAL). The SDK's "per-block derived uuids" (L4108) might be deterministic but their derivation is undocumented. Mitigation: mint the unit id from `(message.id, index)` at stream time and match the settled block by `(message.id, block type, ordinal)` — still ordinal. Flag.
2. **First-block usage stamping.** "EXACTLY ONE UNIT PER API RESPONSE carries `usage`: the unit for the response's FIRST content block." With per-block assistant messages, knowing "first" needs "has `message.id` been stamped" — a single keyed lookup (static) only if the shim keeps the last stamped `message.id`; keyed by message.id across a session it is a growing set unless bounded to the current message.
3. **`input_json_delta` accumulation** — concatenating `partial_json` to obtain a tool's input before the assistant message lands is a variable-size buffer. Static alternative (used here): emit tool `start` from the assistant message only; cost is that tool units do not exist while their arguments stream.
4. **Whole text on `AgentResponseSuccess`/`AgentThinkingSuccess`.** Ruling 3 (rec:255) forbids the shim accumulating deltas; the whole text must come from the settled assistant message — fine, but ONLY because of (1) the per-block message. If the vendor reverted to one multi-block message, still static (index present).
5. **`AgentCompleted.answer`** = last top-level response id: one retained key per agent (static per rec:230).
6. **`SessionIdentityRotated`, `SessionModelChanged`, `SessionFastMode`, `SessionPermissionModeChanged`** are DIFFS against a held previous value — one keyed lookup each (static), but they are diffs: the SDK never says "changed", only states the current value on init/status/result/assistant messages.
7. **`supersedes` / `retracted_message_uuids` / `preceding_tool_use_ids`** — bounded-length lists of single lookups (static per list), but each needs the SDK uuid→unit index, which is not kept (§0).
8. **`DetachedWorkDetached.cause`** — `requested` needs the spawning call's input (`run_in_background`), one lookup by `tool_use_id` (static); `timed_out` would need the Bash tool_result text (OTHER) — no static source on this surface.
9. **`SDKBackgroundTasksChangedMessage`** — the record explicitly forbids diffing it (Ruling 2); relaying it verbatim has no arm, so today it is neither diffed nor relayed.
10. **Skill document linking** — static by `sourceToolUseID` (rec:1540) but that field is undeclared on the SDK surface; if it is absent at runtime the only fallback is positional ("next isMeta user message after the Skill call"), which is what rec:1539 calls "positional and fragile".
11. **`AgentSubagentStart.created_agent_id`** — if `task_id ≠ agentId`, the agent id is available only in the Agent tool_result trailer (OTHER), which arrives AFTER subagent frames (carrying `parent_tool_use_id`) may already have been emitted for a detached agent: attribution of those early frames then needs a deferred/queued resolution. Flag pending verification that `task_id == agentId`.

---

## 4. conversation.v1 fields with NO producer on this surface

(Producer is elsewhere where noted: **tool** = per-tool input/output partition; **ctl** = Query control methods / canUseTool; **shim** = shim-minted; **daemon**; **none** = no producer known anywhere.)

- `AgentId`/`SessionStarted.main_agent_id` — shim. `AgentSubagentStart.created_agent_id` — tool (tool_result trailer) unless `task_id` is the agent id.
- `AgentQuestion.*` (all: `id`, `start.batch`, `success.answered/unanswered`, `failure`) — ctl (canUseTool AskUserQuestion) + shim resolve.
- `AgentPermission.start.{prompt, trigger, offered_standing, started_at}`, `success.allowed.{once,standing}`, `success.denied.user`, `success.abandoned`, `AgentPermissionDecision` — ctl (canUseTool) / daemon; only `denied.policy` is produced here.
- All tool `start` payloads (`ReadPath`, `AgentBashCommand`, `AgentGrepQuery`, `AgentGlobQuery`, `AgentSkillName/args`, `AgentSendMessageStart.{addressed_to,summary,body}`, `AgentUnmodeledStart.arguments`, `AgentSubagentPrompt.{text (here only via task_started.prompt), requested_name, requested_model, isolation}`) — tool.
- All tool `success` payloads (`AgentReadSuccess.extent`, `AgentWriteSuccess.{outcome,patch,user_modified}`, `AgentEditSuccess`, `AgentGrep*`, `AgentGlob*`, `AgentBashSuccess.{outcome, output, extent, image}`, `AgentSkillUseSuccess.{document, allowed_tools}`, `AgentSendMessageSuccess.{recipient_agent_id, delivery}`, `AgentSubagentSuccess.{report, totals.usage, totals.tool_stats, models_used, worktree}`) — tool / undeclared JSONL fields.
- `AgentTaskAct.*` (tracker) — tool (TaskCreate/TaskUpdate).
- `AgentBashUpdate.{new_output, from_offset}` — read from the spool file (tool/shim); nothing on this surface streams shell output.
- `DetachedWorkDetached.cause.{requested, by_user, timed_out}`, `DetachedCauseTimedOut.timeout_ms` — tool / none.
- `AgentWorkflowStart.{script.path, resumed_from, placement.local.run_id, placement.remote.session_url, notice, started_at (receive time only)}`, `AgentWorkflowScriptRejected.error` — tool.
- `AgentFailure` arms beyond `api_request_failed` — none (empty by design); `ApiRateLimited.retry_after_ms`, `ApiOverloaded.retry_after_ms`, `ApiPermissionDenied`, `ApiRequestTooLarge` — none on this surface.
- `AgentResponseFailureReason`, `AgentThinkingFailure`, `Agent*Failure` arm bodies — none (empty by design).
- `SessionRuntime.{shim_build_sha, sdk_version}` — shim. `SessionStarted.model_catalog` — ctl (`supportedModels()`). `SessionStarted.{turn_in_flight, live_work}` — shim/daemon.
- `SessionCold.*`, `SessionColdRemediation.*` — shim (transcript read). `SessionIdentityRotated.reason` — none.
- `SessionQueryDied.{unexpected_eof, iterator_failure}` — shim (iterator, not a message). `SessionMcpServerFailed.error` — none here.
- `SessionAccountUsage.{observed_at_ms, subscription_type}`, `SessionAccountUsageUnavailable.*` — shim sampler.
- `SessionDiagnostics.*`, `SessionKilled.*`, `SessionLive.*`, `TurnKilled.*`, `TurnLive.*`, `TurnId`, `HistoryPage.*`, `HistoryContinuation` — shim/daemon.
- `UserSaid`/`UserContent` — daemon (SubmitPrompt); this surface only echoes it. `ImageBlockPath.path` — shim spill.
- `ContextCleared.tokens.{before, after}`, `ContextCompacted.summary` — none here / undeclared JSONL field.
- `AgentModel` catalog `ModelOption.{display_name, description}` — ctl.
- `AgentPermissionStanding` / `AgentPermissionChange` / `AgentPermissionMode` as an echoed standing — ctl (`PermissionUpdate` L2133, which does map 1:1 onto the six change arms and five destinations).
