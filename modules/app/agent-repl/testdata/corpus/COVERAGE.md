# Corpus COVERAGE report

Checklist of the full `agentshim.data.v1` shape inventory (design doc §5.3 + the four `data/v1/*.proto` files). Every message/enum is marked FOUND (with fixture path), PARTIAL, or NOT-FOUND-ON-MACHINE. NOT-FOUND shapes were deliberately NOT fabricated (corpus contract).

## stream.proto

| Shape | Status | Fixture / reason |
|---|---|---|
| `ClaudeStreamMessage` | FOUND | stream/ (oneof wrapper; every stream/*.jsonl) |
| `UserMessage` | FOUND | stream/user.jsonl |
| `AssistantMessageError` | NOT-FOUND-ON-MACHINE | no error-variant assistant message in the probes |
| `AssistantMessage` | FOUND | stream/assistant.jsonl |
| `ResultSubtype` | PARTIAL | SUCCESS in stream/result_success.jsonl; error subtypes not in probes |
| `ResultMessage` | PARTIAL | stream/result_success.jsonl (success variant only; error variant not in probes) |
| `Usage` | FOUND | stream/result_success.jsonl |
| `ModelUsage` | FOUND | stream/result_success.jsonl (model_usage map) |
| `PermissionDenial` | FOUND | stream/result_success.jsonl (permission_denials) |
| `McpServerState` | NOT-FOUND-ON-MACHINE | init.mcp_servers empty in probes (this session had no MCP servers) |
| `McpServerStatus` | NOT-FOUND-ON-MACHINE | init.mcp_servers empty in probes |
| `PluginRef` | FOUND | stream/system_init.jsonl (plugins) |
| `ApiKeySource` | FOUND | stream/system_init.jsonl |
| `SystemInit` | FOUND | stream/system_init.jsonl |
| `StreamEvent` | FOUND | stream/stream_event-*.jsonl |
| `RawMessageStreamEvent` | FOUND | stream/stream_event-*.jsonl |
| `MessageStartEvent` | FOUND | stream/stream_event-message_start.jsonl |
| `ContentBlockStartEvent` | FOUND | stream/stream_event-content_block_start.jsonl |
| `ContentBlockDeltaEvent` | FOUND | stream/stream_event-content_block_delta-*.jsonl |
| `TextDelta` | FOUND | stream/stream_event-content_block_delta-text.jsonl |
| `ThinkingDelta` | FOUND | stream/stream_event-content_block_delta-thinking.jsonl (estimated_tokens present) |
| `InputJsonDelta` | NOT-FOUND-ON-MACHINE | no input_json_delta among the probes’ content_block_delta events |
| `SignatureDelta` | FOUND | stream/stream_event-content_block_delta-signature.jsonl |
| `ContentBlockStopEvent` | FOUND | stream/stream_event-content_block_stop.jsonl |
| `MessageDeltaEvent` | FOUND | stream/stream_event-message_delta.jsonl |
| `MessageStopEvent` | FOUND | stream/stream_event-message_stop.jsonl |
| `CompactTrigger` | FOUND | disk twin: transcript-lines/system-compact_boundary.jsonl (DiskCompactMetadata.trigger) |
| `CompactBoundary` | NOT-FOUND-ON-MACHINE | stream compact_boundary not in probes; disk twin CompactBoundaryLine is covered |
| `StatusMessage` | FOUND | stream/status.jsonl |
| `HookResponse` | FOUND | stream/hook_response.jsonl |
| `ToolProgress` | NOT-FOUND-ON-MACHINE | no tool_progress message in the probes |
| `AuthStatus` | NOT-FOUND-ON-MACHINE | no auth_status message in the probes |
| `RateLimitInfo` | FOUND | stream/rate_limit_event.jsonl |
| `RateLimitEvent` | FOUND | stream/rate_limit_event.jsonl |
| `HookStarted` | FOUND | stream/hook_started.jsonl |
| `ThinkingTokens` | FOUND | stream/thinking_tokens.jsonl |
| `SystemNotification` | FOUND | stream/notification.jsonl |
| `TaskStartedMsg` | FOUND | stream/task_started.jsonl |
| `TaskPatch` | FOUND | stream/task_updated.jsonl |
| `TaskUpdatedMsg` | FOUND | stream/task_updated.jsonl |
| `TaskUsage` | FOUND | stream/task_notification.jsonl (agent-task variant with usage) |
| `TaskNotificationMsg` | FOUND | stream/task_notification.jsonl |
| `BackgroundTaskRef` | FOUND | stream/background_tasks_changed.jsonl (2 tasks) |
| `BackgroundTasksChanged` | FOUND | stream/background_tasks_changed.jsonl |
| `ControlRequest` | NOT-FOUND-ON-MACHINE | control channel is shim-internal; not present in probes |
| `ControlResponse` | NOT-FOUND-ON-MACHINE | control channel not in probes |
| `ControlCancelRequest` | NOT-FOUND-ON-MACHINE | control channel not in probes |
| `KeepAlive` | NOT-FOUND-ON-MACHINE | control channel not in probes |

## transcript.proto

| Shape | Status | Fixture / reason |
|---|---|---|
| `Entrypoint` | FOUND | envelope on transcript-lines/*.jsonl |
| `PromptSource` | FOUND | transcript-lines/user.jsonl (promptSource) |
| `ToolDenialKind` | FOUND | tool-results/raw_string.jsonl (toolDenialKind) |
| `QueueOp` | FOUND | transcript-lines/queue-operation.jsonl |
| `OriginKind` | FOUND | attachments/queued_command.jsonl (origin.kind="human") |
| `TranscriptLine` | FOUND | oneof wrapper; every transcript-lines/*.jsonl |
| `Origin` | FOUND | attachments/queued_command.jsonl |
| `LineEnvelope` | FOUND | every user/assistant/system/attachment fixture |
| `UserLine` | FOUND | transcript-lines/user.jsonl |
| `AssistantLine` | FOUND | transcript-lines/assistant.jsonl |
| `SystemLine` | FOUND | transcript-lines/system-*.jsonl |
| `HookInfo` | FOUND | transcript-lines/system-stop_hook_summary.jsonl (4 hookInfos) |
| `StopHookSummaryLine` | FOUND | transcript-lines/system-stop_hook_summary.jsonl |
| `TurnDurationLine` | FOUND | transcript-lines/system-turn_duration.jsonl |
| `LocalCommandLine` | FOUND | transcript-lines/system-local_command.jsonl |
| `AgentsKilledLine` | FOUND | transcript-lines/system-agents_killed.jsonl |
| `PreservedSegment` | FOUND | transcript-lines/system-compact_boundary.jsonl |
| `PreservedMessages` | FOUND | transcript-lines/system-compact_boundary.jsonl |
| `DiskCompactMetadata` | FOUND | transcript-lines/system-compact_boundary.jsonl |
| `CompactBoundaryLine` | FOUND | transcript-lines/system-compact_boundary.jsonl |
| `InformationalLine` | FOUND | transcript-lines/system-informational.jsonl |
| `ScheduledTaskFireLine` | FOUND | transcript-lines/system-scheduled_task_fire.jsonl |
| `ModelRefusalNoFallbackLine` | FOUND | transcript-lines/system-model_refusal_no_fallback.jsonl (rare: 1 on machine) |
| `ApiErrorDetail` | FOUND | transcript-lines/system-api_error.jsonl |
| `ApiErrorLine` | FOUND | transcript-lines/system-api_error.jsonl |
| `ModeLine` | FOUND | transcript-lines/mode.jsonl |
| `PermissionModeLine` | FOUND | transcript-lines/permission-mode.jsonl |
| `QueueOperationLine` | FOUND | transcript-lines/queue-operation.jsonl |
| `LastPromptLine` | FOUND | transcript-lines/last-prompt.jsonl |
| `AiTitleLine` | FOUND | transcript-lines/ai-title.jsonl |
| `PrLinkLine` | FOUND | transcript-lines/pr-link.jsonl |
| `FileHistorySnapshotLine` | FOUND | transcript-lines/file-history-snapshot.jsonl |
| `FileBackup` | FOUND | transcript-lines/file-history-delta.jsonl |
| `FileHistoryDeltaLine` | FOUND | transcript-lines/file-history-delta.jsonl |
| `AttachmentLine` | FOUND | attachments/*.jsonl |
| `HookSuccessAttachment` | FOUND | attachments/hook_success.jsonl |
| `HookNonBlockingErrorAttachment` | FOUND | attachments/hook_non_blocking_error.jsonl |
| `HookBlockingErrorAttachment` | FOUND | attachments/hook_blocking_error.jsonl |
| `DeferredToolsDeltaAttachment` | FOUND | attachments/deferred_tools_delta.jsonl |
| `SkillListingAttachment` | FOUND | attachments/skill_listing.jsonl |
| `AgentListingDeltaAttachment` | FOUND | attachments/agent_listing_delta.jsonl |
| `TaskReminderAttachment` | FOUND | attachments/task_reminder.jsonl |
| `AutoModeAttachment` | FOUND | attachments/auto_mode.jsonl |
| `EditedTextFileAttachment` | FOUND | attachments/edited_text_file.jsonl |
| `DiagnosticsAttachment` | FOUND | attachments/diagnostics.jsonl |
| `CommandPermissionsAttachment` | FOUND | attachments/command_permissions.jsonl |
| `QueuedCommandAttachment` | FOUND | attachments/queued_command.jsonl |
| `ReadTruncationNoticeAttachment` | FOUND | attachments/read_truncation_notice.jsonl |
| `StructuredOutputAttachment` | FOUND | attachments/structured_output.jsonl |
| `CompactFileReferenceAttachment` | FOUND | attachments/compact_file_reference.jsonl |
| `ContextTipAttachment` | FOUND | attachments/context_tip.jsonl |
| `DateChangeAttachment` | FOUND | attachments/date_change.jsonl |
| `NestedMemoryAttachment` | FOUND | attachments/nested_memory.jsonl |
| `FileAttachment` | FOUND | attachments/file.jsonl |
| `UltrathinkEffortAttachment` | FOUND | attachments/ultrathink_effort.jsonl |
| `DynamicSkillAttachment` | FOUND | attachments/dynamic_skill.jsonl |
| `UltraEffortEnterAttachment` | FOUND | attachments/ultra_effort_enter.jsonl |
| `UltraEffortExitAttachment` | FOUND | attachments/ultra_effort_exit.jsonl |
| `PlanModeExitAttachment` | FOUND | attachments/plan_mode_exit.jsonl (rare: 1 on machine) |
| `AgentMetaJson` | PARTIAL | sidechain/agent-*.meta.json (model field absent in sample; other fields present) |

## tools.proto

| Shape | Status | Fixture / reason |
|---|---|---|
| `RawTaskStatus` | FOUND | tool-results/agent.jsonl, workflow_launch.jsonl (status) |
| `RetrievalStatus` | NOT-FOUND-ON-MACHINE | no TaskOutput toolUseResult on the machine (retrievalStatus never observed) |
| `ContentBlock` | FOUND | content-blocks/*.jsonl |
| `TextBlock` | FOUND | content-blocks/text.jsonl |
| `ThinkingBlock` | FOUND | content-blocks/thinking.jsonl |
| `Caller` | FOUND | content-blocks/tool_use.jsonl (caller={type:direct}) |
| `ToolUseBlock` | FOUND | content-blocks/tool_use.jsonl, tool-inputs/*.jsonl |
| `ToolResultBlock` | FOUND | content-blocks/tool_result.jsonl |
| `ToolResultBlockList` | FOUND | tool-results/read-image.jsonl (tool_result.content is a block list) |
| `ImageSource` | FOUND | content-blocks/image.jsonl |
| `ImageBlock` | FOUND | content-blocks/image.jsonl |
| `ToolReferenceBlock` | NOT-FOUND-ON-MACHINE | tool_reference content block not observed on the machine |
| `ApiUserMessage` | FOUND | transcript-lines/user.jsonl, tool-results/*.jsonl |
| `ApiContentBlocks` | FOUND | transcript-lines/user.jsonl (content is block array) |
| `ApiAssistantMessage` | FOUND | transcript-lines/assistant.jsonl |
| `ApiUsage` | FOUND | transcript-lines/assistant.jsonl (usage) |
| `ToolUseResult` | FOUND | tool-results/*.jsonl |
| `BashResult` | FOUND | tool-results/bash.jsonl, bash-background.jsonl |
| `AgentToolStats` | FOUND | tool-results/agent.jsonl (toolStats) |
| `AgentResult` | FOUND | tool-results/agent.jsonl |
| `AgentAsyncLaunch` | FOUND | tool-results/agent_async_launch.jsonl |
| `LocalBashTask` | NOT-FOUND-ON-MACHINE | TaskOutput result not on the machine |
| `LocalAgentTask` | NOT-FOUND-ON-MACHINE | TaskOutput result not on the machine |
| `TaskOutputResult` | NOT-FOUND-ON-MACHINE | TaskOutput result not on the machine |
| `TaskStopResult` | FOUND | tool-results/task_stop.jsonl |
| `WorkflowLaunchResult` | FOUND | tool-results/workflow_launch.jsonl |
| `MonitorResult` | FOUND | tool-results/monitor.jsonl |
| `ReadResult` | FOUND | tool-results/read.jsonl (text), read-image.jsonl (image) |
| `EditResult` | FOUND | tool-results/edit.jsonl |
| `WriteResult` | FOUND | tool-results/write.jsonl |
| `SkillResult` | FOUND | tool-results/skill.jsonl |
| `ToolSearchResult` | FOUND | tool-results/tool_search.jsonl |
| `TaskCreateResult` | FOUND | tool-results/task_create.jsonl |
| `TaskUpdateResult` | FOUND | tool-results/task_update.jsonl |
| `TaskListResult` | FOUND | tool-results/task_list.jsonl |
| `SendMessageResult` | FOUND | tool-results/send_message.jsonl |
| `WebFetchResult` | FOUND | tool-results/web_fetch.jsonl |
| `WebSearchResult` | FOUND | tool-results/web_search.jsonl |
| `AskUserQuestionResult` | FOUND | tool-results/ask_user_question.jsonl |
| `ScheduleWakeupResult` | FOUND | tool-results/schedule_wakeup.jsonl |
| `ToolInput` | FOUND | tool-inputs/*.jsonl (oneof wrapper) |
| `AgentInput` | FOUND | tool-inputs/agent.jsonl (tool name "Agent") |
| `BashInput` | FOUND | tool-inputs/bash.jsonl |
| `TaskOutputInput` | FOUND | tool-inputs/task_output.jsonl |
| `KillShellInput` | NOT-FOUND-ON-MACHINE | no KillShell tool use on the machine (proto already notes zero disk occurrences) |
| `FileEditInput` | FOUND | tool-inputs/file_edit.jsonl |
| `FileReadInput` | FOUND | tool-inputs/file_read.jsonl |
| `FileWriteInput` | FOUND | tool-inputs/file_write.jsonl |
| `GlobInput` | FOUND | tool-inputs/glob.jsonl |
| `GrepInput` | NOT-FOUND-ON-MACHINE | no Grep tool on the machine (grep is run via Bash) |
| `NotebookEditInput` | NOT-FOUND-ON-MACHINE | no NotebookEdit tool use on the machine |
| `TodoItem` | NOT-FOUND-ON-MACHINE | no TodoWrite tool (this harness uses TaskCreate/TaskUpdate/TaskList instead) |
| `TodoWriteInput` | NOT-FOUND-ON-MACHINE | no TodoWrite tool on the machine |
| `WebFetchInput` | FOUND | tool-inputs/web_fetch.jsonl |
| `WebSearchInput` | FOUND | tool-inputs/web_search.jsonl |
| `QuestionOption` | FOUND | tool-inputs/ask_user_question.jsonl (questions[].options) |
| `Question` | FOUND | tool-inputs/ask_user_question.jsonl |
| `AskUserQuestionInput` | FOUND | tool-inputs/ask_user_question.jsonl |
| `ConfigInput` | NOT-FOUND-ON-MACHINE | no Config tool use on the machine |
| `ExitPlanModeInput` | NOT-FOUND-ON-MACHINE | no ExitPlanMode tool use on the machine |
| `McpInput` | NOT-FOUND-ON-MACHINE | no mcp__* tool use on the machine (no MCP servers configured this window) |
| `ListMcpResourcesInput` | NOT-FOUND-ON-MACHINE | no ListMcpResources tool use on the machine |
| `ReadMcpResourceInput` | NOT-FOUND-ON-MACHINE | no ReadMcpResource tool use on the machine |

## journal.proto

| Shape | Status | Fixture / reason |
|---|---|---|
| `JournalRecord` | FOUND | journals/*.jsonl (oneof wrapper) |
| `JournalStarted` | FOUND | journals/started.jsonl, complete-journal.jsonl |
| `JournalResult` | FOUND | journals/result-object.jsonl (Struct), result-string.jsonl (string) |

## Summary

- FOUND: 145
- PARTIAL: 3
- NOT-FOUND-ON-MACHINE: 26
- Total shapes: 174

### NOT-FOUND / PARTIAL detail (nothing fabricated)

- `stream:AssistantMessageError` (NOT-FOUND-ON-MACHINE) — no error-variant assistant message in the probes
- `stream:ResultSubtype` (PARTIAL) — SUCCESS in stream/result_success.jsonl; error subtypes not in probes
- `stream:ResultMessage` (PARTIAL) — stream/result_success.jsonl (success variant only; error variant not in probes)
- `stream:McpServerState` (NOT-FOUND-ON-MACHINE) — init.mcp_servers empty in probes (this session had no MCP servers)
- `stream:McpServerStatus` (NOT-FOUND-ON-MACHINE) — init.mcp_servers empty in probes
- `stream:InputJsonDelta` (NOT-FOUND-ON-MACHINE) — no input_json_delta among the probes’ content_block_delta events
- `stream:CompactBoundary` (NOT-FOUND-ON-MACHINE) — stream compact_boundary not in probes; disk twin CompactBoundaryLine is covered
- `stream:ToolProgress` (NOT-FOUND-ON-MACHINE) — no tool_progress message in the probes
- `stream:AuthStatus` (NOT-FOUND-ON-MACHINE) — no auth_status message in the probes
- `stream:ControlRequest` (NOT-FOUND-ON-MACHINE) — control channel is shim-internal; not present in probes
- `stream:ControlResponse` (NOT-FOUND-ON-MACHINE) — control channel not in probes
- `stream:ControlCancelRequest` (NOT-FOUND-ON-MACHINE) — control channel not in probes
- `stream:KeepAlive` (NOT-FOUND-ON-MACHINE) — control channel not in probes
- `transcript:AgentMetaJson` (PARTIAL) — sidechain/agent-*.meta.json (model field absent in sample; other fields present)
- `tools:RetrievalStatus` (NOT-FOUND-ON-MACHINE) — no TaskOutput toolUseResult on the machine (retrievalStatus never observed)
- `tools:ToolReferenceBlock` (NOT-FOUND-ON-MACHINE) — tool_reference content block not observed on the machine
- `tools:LocalBashTask` (NOT-FOUND-ON-MACHINE) — TaskOutput result not on the machine
- `tools:LocalAgentTask` (NOT-FOUND-ON-MACHINE) — TaskOutput result not on the machine
- `tools:TaskOutputResult` (NOT-FOUND-ON-MACHINE) — TaskOutput result not on the machine
- `tools:KillShellInput` (NOT-FOUND-ON-MACHINE) — no KillShell tool use on the machine (proto already notes zero disk occurrences)
- `tools:GrepInput` (NOT-FOUND-ON-MACHINE) — no Grep tool on the machine (grep is run via Bash)
- `tools:NotebookEditInput` (NOT-FOUND-ON-MACHINE) — no NotebookEdit tool use on the machine
- `tools:TodoItem` (NOT-FOUND-ON-MACHINE) — no TodoWrite tool (this harness uses TaskCreate/TaskUpdate/TaskList instead)
- `tools:TodoWriteInput` (NOT-FOUND-ON-MACHINE) — no TodoWrite tool on the machine
- `tools:ConfigInput` (NOT-FOUND-ON-MACHINE) — no Config tool use on the machine
- `tools:ExitPlanModeInput` (NOT-FOUND-ON-MACHINE) — no ExitPlanMode tool use on the machine
- `tools:McpInput` (NOT-FOUND-ON-MACHINE) — no mcp__* tool use on the machine (no MCP servers configured this window)
- `tools:ListMcpResourcesInput` (NOT-FOUND-ON-MACHINE) — no ListMcpResources tool use on the machine
- `tools:ReadMcpResourceInput` (NOT-FOUND-ON-MACHINE) — no ReadMcpResource tool use on the machine

### Extra REAL shapes captured that are NOT in the proto inventory
(converters will route these to `Event.extras`; fixtures exist so that behavior is tested)

- system subtype `model_refusal_fallback` — transcript-lines/system-model_refusal_fallback.jsonl (proto models the `no_fallback` twin only)
- system subtype `away_summary` — transcript-lines/system-away_summary.jsonl
- attachment `hook_cancelled` — attachments/hook_cancelled.jsonl
- attachment `invoked_skills` — attachments/invoked_skills.jsonl
- content block `fallback` — content-blocks/fallback.jsonl
- top-level line type `frame-link` — transcript-lines/frame-link.jsonl
- toolUseResult objects with no typed twin — tool-results/unclassified-*.jsonl (exercise ToolUseResult.unclassified)

