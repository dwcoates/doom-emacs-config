# Corpus MANIFEST

The golden corpus (design doc §14.1 G13, §5.3 shape inventory). Every fixture is a REAL, anonymized harness artifact. Converters (G3 `convert.go`, G4 `convert.ts`) must decode every fixture with zero `UnparsedEvent`s and zero unknown-field logs (§15).

- Harvest date: 2026-07-23
- Source machine: developer workstation (`~/.claude`, `~/.claude-chesscom` session stores; `/tmp/sdk-probe` SDK stream probes; `/tmp/claude-501` background-task spools).
- Anonymization (applied by a shared walker, see notes column):
  - Secret-looking tokens (Anthropic/GitHub/AWS/Slack keys, Bearer/JWT, `authorization`/`api-key`/`password`/`secret` values) -> `REDACTED`. No selected sample actually contained a secret, so no `REDACTED` markers appear.
  - Strings > 900 chars truncated to first 400 + `…[TRUNCATED N chars for corpus]`.
  - Opaque blobs (`signature`, base64 image `data`) truncated to a short prefix + `…[TRUNCATED N chars]`.
  - ALL structural fields preserved verbatim: uuids, session ids, paths, timestamps, tool-use ids, keys.
- Source paths are shown as generalized patterns (project/session segments elided); exact private session ids are not reproduced here.

| Fixture | Source pattern | Shape(s) covered | Anonymization / notes |
|---|---|---|---|
| `attachments/agent_listing_delta.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.AgentListingDeltaAttachment | shortest real sample; structural fields intact |
| `attachments/auto_mode.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.AutoModeAttachment | shortest real sample; structural fields intact |
| `attachments/command_permissions.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.CommandPermissionsAttachment | shortest real sample; structural fields intact |
| `attachments/compact_file_reference.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.CompactFileReferenceAttachment | shortest real sample; structural fields intact |
| `attachments/context_tip.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.ContextTipAttachment | shortest real sample; structural fields intact |
| `attachments/date_change.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.DateChangeAttachment | shortest real sample; structural fields intact |
| `attachments/deferred_tools_delta.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.DeferredToolsDeltaAttachment | shortest real sample; structural fields intact |
| `attachments/diagnostics.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.DiagnosticsAttachment | shortest real sample; structural fields intact |
| `attachments/dynamic_skill.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | AttachmentLine.DynamicSkillAttachment | shortest real sample; structural fields intact |
| `attachments/edited_text_file.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.EditedTextFileAttachment | shortest real sample; structural fields intact |
| `attachments/file.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.FileAttachment | shortest real sample; structural fields intact |
| `attachments/hook_blocking_error.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.HookBlockingErrorAttachment | shortest real sample; structural fields intact |
| `attachments/hook_cancelled.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | EXTRA observed attachment type "hook_cancelled" (NOT in proto; converter -> extras) | shortest real sample; structural fields intact |
| `attachments/hook_non_blocking_error.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.HookNonBlockingErrorAttachment | shortest real sample; structural fields intact |
| `attachments/hook_success.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.HookSuccessAttachment | shortest real sample; structural fields intact |
| `attachments/invoked_skills.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | EXTRA observed attachment type "invoked_skills" (NOT in proto; converter -> extras) | shortest real sample; structural fields intact |
| `attachments/nested_memory.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.NestedMemoryAttachment | shortest real sample; structural fields intact |
| `attachments/plan_mode_exit.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.PlanModeExitAttachment | shortest real sample; structural fields intact |
| `attachments/queued_command.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` (line 1), `~/.claude/projects/<project>/<session>.jsonl` (line 2) | AttachmentLine.QueuedCommandAttachment | TWO lines, one per `prompt_value` arm: line 1 the string form (census 702/731), line 2 the blocks form (census 29/731); shortest real sample of each, structural fields intact |
| `attachments/read_truncation_notice.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.ReadTruncationNoticeAttachment | shortest real sample; structural fields intact |
| `attachments/skill_listing.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.SkillListingAttachment | shortest real sample; structural fields intact |
| `attachments/structured_output.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.StructuredOutputAttachment | shortest real sample; structural fields intact |
| `attachments/task_reminder.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | AttachmentLine.TaskReminderAttachment | shortest real sample; structural fields intact |
| `attachments/ultra_effort_enter.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | AttachmentLine.UltraEffortEnterAttachment | shortest real sample; structural fields intact |
| `attachments/ultra_effort_exit.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | AttachmentLine.UltraEffortExitAttachment | shortest real sample; structural fields intact |
| `attachments/ultrathink_effort.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | AttachmentLine.UltrathinkEffortAttachment | shortest real sample; structural fields intact |
| `content-blocks/fallback.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | EXTRA observed content block type "fallback" (NOT in proto; converter -> extras) | shortest real sample; structural fields intact |
| `content-blocks/image.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ContentBlock.image / ImageBlock / ImageSource (base64 data truncated) | image base64 data truncated by anonymizer |
| `content-blocks/text.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ContentBlock.text / TextBlock (full assistant/user line containing a text block) | shortest real sample; structural fields intact |
| `content-blocks/thinking.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ContentBlock.thinking / ThinkingBlock (signature truncated) | thinking.signature truncated by anonymizer |
| `content-blocks/tool_result.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ContentBlock.tool_result / ToolResultBlock | shortest real sample; structural fields intact |
| `content-blocks/tool_use.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ContentBlock.tool_use / ToolUseBlock / Caller | shortest real sample; structural fields intact |
| `journals/complete-journal.jsonl` | `~/.claude/projects/<project>/<session>/subagents/workflows/wf_<id>/journal.jsonl` | JournalRecord started+result pair, complete small journal in file order | shortest real sample; structural fields intact |
| `journals/result-object.jsonl` | `~/.claude/projects/<project>/<session>/subagents/workflows/wf_<id>/journal.jsonl` | JournalRecord.result / JournalResult.result_object (Struct) | shortest real sample; structural fields intact |
| `journals/result-string.jsonl` | `~/.claude/projects/<project>/<session>/subagents/workflows/wf_<id>/journal.jsonl` | JournalRecord.result / JournalResult.result_string | shortest real sample; structural fields intact |
| `journals/started.jsonl` | `~/.claude/projects/<project>/<session>/subagents/workflows/wf_<id>/journal.jsonl` | JournalRecord.started / JournalStarted | shortest real sample; structural fields intact |
| `sidechain/agent-aef975b7bc3422d4b.jsonl` | `~/.claude/projects/<project>/<session>/subagents/agent-<id>.jsonl` | Agent sidechain transcript (is_sidechain=true lines); 6 of 21 lines | shortest real sample; structural fields intact |
| `sidechain/agent-aef975b7bc3422d4b.meta.json` | `~/.claude/projects/<project>/<session>/subagents/agent-<id>.meta.json` | AgentMetaJson (agent_type/description/tool_use_id/spawn_depth/model) | shortest real sample; structural fields intact |
| `spools/agent.output` | `/tmp/claude-501/<project>/<session>/tasks/a*.output` | Background AGENT task spool (a*.output). Truncated to 4000 chars. | shortest real sample; structural fields intact |
| `spools/bash-clean.output` | `/tmp/claude-501/<project>/<session>/tasks/b*.output` | Background shell spool, CLEAN ending (terminal EXIT=<code> marker) | shortest real sample; structural fields intact |
| `spools/bash-midoutput.output` | `/tmp/claude-501/<project>/<session>/tasks/b*.output` | Background shell spool, MID-OUTPUT: no terminal EXIT marker, truncated to 2550 chars ending mid-line (simulates a reader catching a still-writing spool). | shortest real sample; structural fields intact |
| `stream/assistant.jsonl` | `/tmp/sdk-probe/stream4.jsonl` | ClaudeStreamMessage.assistant / AssistantMessage / ApiAssistantMessage | shortest real sample; structural fields intact |
| `stream/background_tasks_changed.jsonl` | `/tmp/sdk-probe/stream2.jsonl` | ClaudeStreamMessage.background_tasks_changed / BackgroundTasksChanged / BackgroundTaskRef (observed-only) | shortest real sample; structural fields intact |
| `stream/hook_response.jsonl` | `/tmp/sdk-probe/stream1.jsonl` | ClaudeStreamMessage.hook_response / HookResponse | shortest real sample; structural fields intact |
| `stream/hook_started.jsonl` | `/tmp/sdk-probe/stream1.jsonl` | ClaudeStreamMessage.hook_started / HookStarted (observed-only) | shortest real sample; structural fields intact |
| `stream/notification.jsonl` | `/tmp/sdk-probe/stream1.jsonl` | ClaudeStreamMessage.notification / SystemNotification (observed-only) | shortest real sample; structural fields intact |
| `stream/rate_limit_event.jsonl` | `/tmp/sdk-probe/stream1.jsonl` | ClaudeStreamMessage.rate_limit_event / RateLimitEvent / RateLimitInfo (observed-only) | shortest real sample; structural fields intact |
| `stream/result_success.jsonl` | `/tmp/sdk-probe/stream4.jsonl` | ClaudeStreamMessage.result / ResultMessage (success) / ResultSubtype / Usage / ModelUsage (+observed ttft/stop_reason/terminal_reason/fast_mode_state) | shortest real sample; structural fields intact |
| `stream/status.jsonl` | `/tmp/sdk-probe/stream4.jsonl` | ClaudeStreamMessage.status / StatusMessage | shortest real sample; structural fields intact |
| `stream/stream_event-content_block_delta-signature.jsonl` | `/tmp/sdk-probe/stream4.jsonl` | ContentBlockDeltaEvent.signature_delta / SignatureDelta | shortest real sample; structural fields intact |
| `stream/stream_event-content_block_delta-text.jsonl` | `/tmp/sdk-probe/stream4.jsonl` | ContentBlockDeltaEvent.text_delta / TextDelta | shortest real sample; structural fields intact |
| `stream/stream_event-content_block_delta-thinking.jsonl` | `/tmp/sdk-probe/stream4.jsonl` | ContentBlockDeltaEvent.thinking_delta / ThinkingDelta | shortest real sample; structural fields intact |
| `stream/stream_event-content_block_start.jsonl` | `/tmp/sdk-probe/stream4.jsonl` | RawMessageStreamEvent.content_block_start / ContentBlockStartEvent | shortest real sample; structural fields intact |
| `stream/stream_event-content_block_stop.jsonl` | `/tmp/sdk-probe/stream4.jsonl` | RawMessageStreamEvent.content_block_stop / ContentBlockStopEvent | shortest real sample; structural fields intact |
| `stream/stream_event-message_delta.jsonl` | `/tmp/sdk-probe/stream4.jsonl` | RawMessageStreamEvent.message_delta / MessageDeltaEvent | shortest real sample; structural fields intact |
| `stream/stream_event-message_start.jsonl` | `/tmp/sdk-probe/stream4.jsonl` | StreamEvent / RawMessageStreamEvent.message_start / MessageStartEvent | shortest real sample; structural fields intact |
| `stream/stream_event-message_stop.jsonl` | `/tmp/sdk-probe/stream4.jsonl` | RawMessageStreamEvent.message_stop / MessageStopEvent | shortest real sample; structural fields intact |
| `stream/system_init.jsonl` | `/tmp/sdk-probe/stream1.jsonl` | ClaudeStreamMessage.system_init / SystemInit / McpServerStatus / ApiKeySource | shortest real sample; structural fields intact |
| `stream/task_notification.jsonl` | `/tmp/sdk-probe/stream2.jsonl` | ClaudeStreamMessage.task_notification / TaskNotificationMsg / TaskUsage (observed-only) | shortest real sample; structural fields intact |
| `stream/task_started.jsonl` | `/tmp/sdk-probe/stream2.jsonl` | ClaudeStreamMessage.task_started / TaskStartedMsg (observed-only) | shortest real sample; structural fields intact |
| `stream/task_updated.jsonl` | `/tmp/sdk-probe/stream2.jsonl` | ClaudeStreamMessage.task_updated / TaskUpdatedMsg / TaskPatch (observed-only) | shortest real sample; structural fields intact |
| `stream/thinking_tokens.jsonl` | `/tmp/sdk-probe/stream1.jsonl` | ClaudeStreamMessage.thinking_tokens / ThinkingTokens (observed-only) | shortest real sample; structural fields intact |
| `stream/user.jsonl` | `/tmp/sdk-probe/stream3.jsonl` | ClaudeStreamMessage.user / UserMessage | shortest real sample; structural fields intact |
| `tool-inputs/agent.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseBlock + ToolInput.agent / AgentInput (tool name "Agent") | tool_use block from an assistant line |
| `tool-inputs/ask_user_question.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | ToolUseBlock + ToolInput.AskUserQuestionInput / Question / QuestionOption | tool_use block from an assistant line; input intact |
| `tool-inputs/bash.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | ToolUseBlock + ToolInput.BashInput | tool_use block from an assistant line; input intact |
| `tool-inputs/file_edit.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | ToolUseBlock + ToolInput.FileEditInput | tool_use block from an assistant line; input intact |
| `tool-inputs/file_read.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | ToolUseBlock + ToolInput.FileReadInput | tool_use block from an assistant line; input intact |
| `tool-inputs/file_write.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | ToolUseBlock + ToolInput.FileWriteInput | tool_use block from an assistant line; input intact |
| `tool-inputs/glob.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | ToolUseBlock + ToolInput.GlobInput | tool_use block from an assistant line; input intact |
| `tool-inputs/task_output.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseBlock + ToolInput.TaskOutputInput | tool_use block from an assistant line; input intact |
| `tool-inputs/web_fetch.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseBlock + ToolInput.WebFetchInput | tool_use block from an assistant line; input intact |
| `tool-inputs/web_search.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseBlock + ToolInput.WebSearchInput | tool_use block from an assistant line; input intact |
| `tool-results/agent.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.agent / AgentResult / AgentToolStats | shortest real sample; structural fields intact |
| `tool-results/agent_async_launch.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.agent_async_launch / AgentAsyncLaunch | shortest real sample; structural fields intact |
| `tool-results/ask_user_question.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.ask_user_question / AskUserQuestionResult | shortest real sample; structural fields intact |
| `tool-results/bash-background.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.bash / BashResult (background-launch fold: backgroundTaskId set, empty stdout/stderr) | Selected specifically for backgroundTaskId presence. |
| `tool-results/bash.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.bash / BashResult | shortest real sample; structural fields intact |
| `tool-results/edit.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.edit / EditResult | shortest real sample; structural fields intact |
| `tool-results/monitor.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.monitor / MonitorResult | shortest real sample; structural fields intact |
| `tool-results/raw_string.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.raw_string | shortest real sample; structural fields intact |
| `tool-results/read-image.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | ToolUseResult.read / ReadResult (type=="image" variant) + ContentBlock.image | image base64 truncated |
| `tool-results/read.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.read / ReadResult | shortest real sample; structural fields intact |
| `tool-results/schedule_wakeup.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.schedule_wakeup / ScheduleWakeupResult | shortest real sample; structural fields intact |
| `tool-results/send_message.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.send_message / SendMessageResult | shortest real sample; structural fields intact |
| `tool-results/skill.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.skill / SkillResult | shortest real sample; structural fields intact |
| `tool-results/task_create.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.task_create / TaskCreateResult | shortest real sample; structural fields intact |
| `tool-results/task_list.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.task_list / TaskListResult | shortest real sample; structural fields intact |
| `tool-results/task_output-local_agent.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.task_output / TaskOutputResult / LocalAgentTask (retrieval_status=success) | shortest real sample; structural fields intact |
| `tool-results/task_output.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.task_output / TaskOutputResult / LocalBashTask (retrieval_status=not_ready, exitCode null) | shortest real sample; structural fields intact |
| `tool-results/task_stop.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.task_stop / TaskStopResult | shortest real sample; structural fields intact |
| `tool-results/task_update.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.task_update / TaskUpdateResult | shortest real sample; structural fields intact |
| `tool-results/tool_search.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.tool_search / ToolSearchResult | shortest real sample; structural fields intact |
| `tool-results/unclassified-message_success.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | ToolUseResult.unclassified (real object with no typed twin) | shortest real sample; structural fields intact |
| `tool-results/unclassified-path_title_url.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | ToolUseResult.unclassified (real object with no typed twin) | shortest real sample; structural fields intact |
| `tool-results/web_fetch.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.web_fetch / WebFetchResult | shortest real sample; structural fields intact |
| `tool-results/web_search.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.web_search / WebSearchResult | shortest real sample; structural fields intact |
| `tool-results/workflow_launch.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.workflow_launch / WorkflowLaunchResult | shortest real sample; structural fields intact |
| `tool-results/write.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | ToolUseResult.write / WriteResult | shortest real sample; structural fields intact |
| `transcript-lines/ai-title.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | TranscriptLine.ai_title / AiTitleLine | shortest real sample; structural fields intact |
| `transcript-lines/assistant.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | TranscriptLine.assistant / AssistantLine / ApiAssistantMessage | shortest real sample; structural fields intact |
| `transcript-lines/file-history-delta.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | TranscriptLine.file_history_delta / FileHistoryDeltaLine / FileBackup | shortest real sample; structural fields intact |
| `transcript-lines/file-history-snapshot.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | TranscriptLine.file_history_snapshot / FileHistorySnapshotLine | shortest real sample; structural fields intact |
| `transcript-lines/frame-link.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | EXTRA observed line type "frame-link" (NOT in proto; converter -> Event.extras) | shortest real sample; structural fields intact |
| `transcript-lines/last-prompt.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | TranscriptLine.last_prompt / LastPromptLine | shortest real sample; structural fields intact |
| `transcript-lines/mode.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | TranscriptLine.mode / ModeLine | shortest real sample; structural fields intact |
| `transcript-lines/permission-mode.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | TranscriptLine.permission_mode / PermissionModeLine | shortest real sample; structural fields intact |
| `transcript-lines/pr-link.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | TranscriptLine.pr_link / PrLinkLine | shortest real sample; structural fields intact |
| `transcript-lines/queue-operation.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | TranscriptLine.queue_operation / QueueOperationLine / QueueOp | shortest real sample; structural fields intact |
| `transcript-lines/system-agents_killed.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | SystemLine.AgentsKilledLine | shortest real sample; structural fields intact |
| `transcript-lines/system-api_error.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | SystemLine.ApiErrorLine / ApiErrorDetail | shortest real sample; structural fields intact |
| `transcript-lines/system-away_summary.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | EXTRA observed system subtype "away_summary" (NOT in proto; converter -> extras) | shortest real sample; structural fields intact |
| `transcript-lines/system-compact_boundary.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | SystemLine.CompactBoundaryLine / DiskCompactMetadata / PreservedSegment / PreservedMessages | shortest real sample; structural fields intact |
| `transcript-lines/system-informational.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | SystemLine.InformationalLine | shortest real sample; structural fields intact |
| `transcript-lines/system-local_command.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | SystemLine.LocalCommandLine | shortest real sample; structural fields intact |
| `transcript-lines/system-model_refusal_fallback.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | EXTRA observed system subtype "model_refusal_fallback" (proto models the *no_fallback* twin; converter -> extras) | shortest real sample; structural fields intact |
| `transcript-lines/system-model_refusal_no_fallback.jsonl` | `~/.claude/projects/<project>/<session>.jsonl` | SystemLine.ModelRefusalNoFallbackLine | shortest real sample; structural fields intact |
| `transcript-lines/system-scheduled_task_fire.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | SystemLine.ScheduledTaskFireLine | shortest real sample; structural fields intact |
| `transcript-lines/system-stop_hook_summary.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | SystemLine.StopHookSummaryLine / HookInfo | shortest real sample; structural fields intact |
| `transcript-lines/system-turn_duration.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | SystemLine.TurnDurationLine | shortest real sample; structural fields intact |
| `transcript-lines/user.jsonl` | `~/.claude-chesscom/projects/<project>/<session>.jsonl` | TranscriptLine.user / UserLine / LineEnvelope / ApiUserMessage | shortest real sample; structural fields intact |
