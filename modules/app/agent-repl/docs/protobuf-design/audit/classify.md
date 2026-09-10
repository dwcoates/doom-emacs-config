
## 2. Classification

Legend: **REP** = REPRESENTED (names the conversation.v1 field it fills; "join" = a key used only to resolve another record, never carried); **DROP** = DROPPED-WITH-REASON (proto comment or design-record sentence cited); **UNS** = UNSUPPORTED (no home, no recorded reason). Design record = `docs/protobuf-design/figma-to-idl-redesign.md` (cited as `DR:<line>`).

### 2.1 Envelope keys shared by `user` / `assistant` / `system` / `attachment`

| path | class | target / reason |
|---|---|---|
| `type` | REP | record-kind discriminator (routes to user/agent/system/attachment handling) |
| `uuid` | REP | `AgentActivityId.value` for thinking/text blocks ("for the agent's reasoning or prose it identifies the block"); join key for `parentUuid` |
| `parentUuid` | DROP | `AgentFrame.agent_id` comment: "nothing on the wire carries ancestry". Kept only as the static join `isCompactSummary` → `compact_boundary` (209/209 summaries have `parentUuid` = boundary uuid) |
| `sessionId` | REP | `SessionStarted.vendor_session_id` |
| `session_id` (snake-case twin, 85,615 attachment / 22,018 user / 45,769 assistant / 4,344 system) | UNS | redundant duplicate of `sessionId`; no ruling |
| `agentId` | REP | `AgentFrame.agent_id` (`AgentId.value`); absent ⇒ main agent (`SessionStarted.main_agent_id`) |
| `timestamp` (assistant record carrying a `tool_use`) | REP | `AgentActivityStartedAt.at_ms` on every `*Start` arm |
| `timestamp` (user prompt / text & thinking blocks / system / attachment) | DROP | `turn.proto`: "the daemon stamps the feed rows a turn produced"; `AgentThinkingStart`: "No start instant rides here: nothing draws a clock against reasoning" |
| `cwd` | UNS | no home (workspace.v1 holds the daemon's cwd, not the record's) |
| `gitBranch` | UNS | no home |
| `version` (binary version, e.g. 2.1.220) | UNS | `SessionRuntime.sdk_version` is the SDK's, not the binary's; no ruling |
| `userType` (`external`) | UNS | no home |
| `entrypoint` (`sdk-cli` / `cli`) | UNS | no home |
| `isSidechain` | REP | redundant discriminator for subagent transcripts (true in every `subagents/agent-*.jsonl` record); `agentId` already carries it |
| `slug` | UNS | no home (DR:3077 `HostWorkspaceNaming.slug` is the daemon's naming, not the vendor's) |
| `sessionKind` (`bg`) | UNS | no home |
| `isMeta` (with `sourceToolUseID`) | REP | discriminator: the record is an `AgentSkillDocument`, not a `UserSaid` (DR:1534) |
| `isMeta` (without `sourceToolUseID`, 276 records: "Continue from where you left off", resume envelopes) | UNS | no home; must not become `UserSaid` (not typed by a person) and has no arm |
| `sourceToolUseID` | REP (join) | links the skill document to `AgentSkillUse` (`AgentActivityId`); DR:1538 "links the document DIRECTLY to the invoking call" |
| `sourceToolAssistantUUID` (131,298) | REP (join, unused) | alternative static key to the calling assistant record; `tool_use_id` suffices |

### 2.2 `user` records

| path | class | target / reason |
|---|---|---|
| `message.role` | REP | discriminator |
| `message.content` (string, typed prompt) | REP | `UserSaid.content.blocks[].text` (`TextBlock.text`) |
| `message.content` (string starting `<command-name>`, 480) | DROP | `user.proto`: "A SESSION COMMAND IS NOT ONE OF THESE … a recognized command earns no user message" (`/clear` 75, `/compact` 294, `/model` 67, `/effort` 25, `/login` 8, `/exit` 7, `/agents` 2, …) |
| `message.content` (string `<local-command-stdout>`, 271) / `<local-command-caveat>` (481) | UNS | the command's OUTPUT has no home (only `ContextCleared`/`ContextCompacted` outcomes exist) |
| `message.content` (string `<task-notification>…`, 1,805) | REP (non-static, see §4) | terminal frames for detached work: `AgentSubagentSuccess.report` (`<result>`), `AgentSubagentTotals.duration_ms`/`tool_use_count` (`<usage>`), `AgentSubagentWorktree` (`<worktree>`), `AgentBashSuccess` (`<event>` for shells) — all parsed out of XML-in-prose |
| `message.content` (string `Background agent … was stopped by the user`, `queuePriority`) | UNS | a detached stop acknowledgement delivered as prose; `AgentInterrupted` carries nothing and the agent id is only in the prose |
| `message.content[].type=text` (`user/text`, 3,449) | REP | `UserContentBlock.text`; when `isMeta`+`sourceToolUseID` → `AgentSkillDocument.markdown` |
| `message.content[].type=tool_result` → `tool_use_id` | REP (join) | `AgentActivityId` of the call (static, by id) |
| `message.content[].type=tool_result` → `content` (string) | REP | per tool: `AgentBashOutputText.stdout` (Bash, only when `toolUseResult` is a string), `ToolResultContent.blocks[].text` (unmodeled), failure text for every `*Failure` arm (all empty — "Arms are DERIVED at the implementation wave") |
| `message.content[].content[].type=text` | REP | `ToolResultContentBlock.text` / `AgentSubagentReport.prose` (Agent) |
| `message.content[].content[].type=image` + `source.{type=base64,media_type,data}` (Read, 45) | REP | `ImageBlock.path` after the producer spills bytes ("a producer that spilled vendor-inlined bytes to disk uses this"), `media_type` |
| `message.content[].content[].type=tool_reference` + `tool_name` (ToolSearch, 601) | UNS | ToolSearch has no arm (see §2.6) |
| `message.content[].is_error` | REP | selects the `failure` arm vs `success` (Bash: "A NONZERO EXIT IS STILL [success]", so Bash `is_error` only selects failure when `toolUseResult` is a bare string) |
| `isCompactSummary`, `isVisibleInTranscriptOnly` | REP | `ContextCompacted.summary` (`AgentResponseProse`) |
| `interruptedByShutdown` (150), `interruptedMessageId` (149) | REP | `AgentSuccess.interrupted` evidence ("only an acknowledged stop sets it"); `interruptedMessageId` = join to the aborted assistant `message.id` |
| `permissionMode` (`auto` 6,724 / `default` 1,584 / `bypassPermissions` 117 / `plan` 2 / `dontAsk` 1) | REP | `AgentPermissionMode` arms (`SessionStarted.permission_mode`, `SessionPermissionModeChanged`) — DR:414 |
| `promptId` | UNS | vendor prompt identity; `TurnId` is daemon-minted and never the vendor's |
| `promptSource` (`sdk` 3,685 / `typed` 3,011 / `system` 1,602 / `queued` 130) | UNS | no home; `system` (1,602) would be the only structural way to tell an injected prompt from a typed one |
| `queuePriority` (`later`) | UNS | no home |
| `toolDenialKind` = `user-rejected` (195) | REP | `AgentPermissionDenied.user` (`AgentPermissionDeniedByUser.message` = result text) |
| `toolDenialKind` = `automode-blocked` (178) / `permission-rule` (9) | REP | `AgentPermissionDenied.policy` (`decider`, `message`) |
| `toolDenialKind` = `automode-unavailable` (45: "claude-sonnet-5[1m] is temporarily unavailable") | UNS | neither user nor policy; no arm |
| `toolDenialKind` = `interrupted` (1) | REP | `AgentPermissionAbandoned` |
| `toolEndsTurn` (StructuredOutput) | UNS | no home |
| `turnCompanion` (39; skill document for a user-typed `/skill`, no `sourceToolUseID`) | UNS | no `AgentSkillUse` exists (no tool call); `user.proto` says the expansion "really is the turn's opening", but the DOCUMENT then has no home |
| `userFeedback` (10; AskUserQuestion rejected with "clarify") | UNS | `AgentQuestionUnanswered` carries nothing; the feedback prose has no field |
| `classifierMetaLines` (738; `{"meta":{"gitStatus":…}}`) | UNS | no home |
| `origin.kind` (`human`, 5,368) | UNS | no home |
| `origin.from/name/senderTaskId/body` (36; a SendMessage delivered INTO this agent) | UNS | `UserSaid` has no sender field ("Which of the two is read from the parent chain, never from a field"); the recipient-side evidence of `AgentSendMessage` has no home |

### 2.3 `assistant` records

| path | class | target / reason |
|---|---|---|
| `message.id` | REP (join) | groups the one-block-per-record split (see §4.1) so `AgentActivity.usage` lands on exactly one unit |
| `message.model` | REP | `AgentModel.name` (`SessionModelChanged.effective_model`; `AgentSubagentSuccess.models_used` from the subagent file) |
| `message.role`, `message.type` | REP / UNS | role = discriminator; `type`=`message` has no home |
| `message.stop_reason` (`tool_use` 146,567 / `end_turn` 14,171 / `stop_sequence` 446 / null 69,949) | DROP | DR:2364 "the turn's own conclusion names the answering response rather than a finality field"; observed data settles the open question there (real transcripts DO report `tool_use`, the fixture was unfaithful) |
| `message.stop_sequence`, `message.stop_details`, `message.container` | UNS | no home (all null in corpus) |
| `message.context_management.applied_edits[]` (31 non-empty) | UNS | server-side context editing; `ContextCut` has no arm for it |
| `message.diagnostics.cache_miss_reason.{type,cache_missed_input_tokens}` | DROP | DR:983 "`cache_missed_input_tokens` + `cache_miss_reason` … observed and NOT modelled, by the user's ruling that `api.proto` is fine" |
| `message.usage.input_tokens` | REP | `TokenCacheMisses.unwritten` |
| `message.usage.cache_creation_input_tokens` | REP | `TokenCacheMisses.written` |
| `message.usage.cache_read_input_tokens` | REP | `TokenCacheHits.read` |
| `message.usage.output_tokens` | REP | `TokenUsage.output_tokens` |
| `message.usage.output_tokens_details.thinking_tokens` | REP | `TokenUsage.output_thinking_tokens` (DR:978) |
| `message.usage.cache_creation.ephemeral_5m_input_tokens` / `ephemeral_1h_input_tokens` | DROP | DR:982 "`cache_creation`'s 5m/1h split" |
| `message.usage.iterations[].*` | DROP | DR:984 |
| `message.usage.server_tool_use.{web_fetch_requests,web_search_requests}` | DROP | DR:984 |
| `message.usage.service_tier`, `speed`, `inference_geo` | DROP | DR:984 |
| `requestId` | UNS | no home |
| `effort` (`xhigh`, 221,509 records) | UNS | no field anywhere in conversation.v1 (not on `AgentModel`, `SessionStarted`, or `AgentSubagentPrompt`) |
| `attributionSkill` (20,802) | UNS | no home. NOTE: `AgentSkillUse` comment says "NOTHING DELIMITS A SKILL'S SCOPE … no producer statement about where a skill's influence stops"; this key IS that statement, observed on 9% of assistant records |
| `attributionAgent` (139,323), `attributionPlugin` (856) | UNS | no home |
| `isApiErrorMessage` + `error` + `apiErrorStatus` | REP | `AgentFailure.api_request_failed`: `rate_limit`/429 → `ApiRateLimited`; `server_error`/529 → `ApiOverloaded`; `server_error`/500 → `ApiInternal`; `authentication_failed` → `ApiAuthenticationFailed`; `model_not_found`/404 → `ApiNotFound`; `server_error`/null (30) and null/null (1) → `ApiUnmodeledError.type`; `content[].text` → `ApiRequestFailed.message` |
| `isAbortedMidStream` (43) | REP | `AgentResponseFailure.prose` (+ empty `reason`) and `AgentInterrupted` |
| `message.content[].type=thinking` → `thinking` = `""` (60,135 of 60,181) | REP | `AgentThinkingWithheld` |
| `message.content[].type=thinking` → `thinking` (text, 46) | REP | `AgentThinkingText.text` |
| `message.content[].signature` | DROP | `AgentThinkingWithheld` comment: "it emits the block and a signature but no text at all. There is nothing to draw" |
| `message.content[].type=text` → `text` | REP | `AgentResponseProse.markdown` |
| `message.content[].type=tool_use` → `id` | REP | `AgentActivityId.value` ("the call's own identity as the producer reported it") |
| `message.content[].type=tool_use` → `name` | REP | arm selection (`read`/`write`/`edit`/`bash`/`subagent`/`skill_use`/`send_message`/`task_act`/`unmodeled`) |
| `message.content[].caller.type` (`direct`; null ×162) | UNS | no home |
| `message.content[].input.*` | see §2.5 per tool | |

### 2.4 `system` records

| subtype / path | class | target / reason |
|---|---|---|
| `compact_boundary` → `compactMetadata.preTokens`, `postTokens` | REP | `ContextCompacted.tokens.tokens_before/after` |
| `compact_boundary` → `compactMetadata.trigger` (`manual`/`auto`) | UNS | `ContextCut` has no trigger arm (DR:849 notes the binary "also compacts AUTOMATICALLY" but records no field) |
| `compact_boundary` → `compactMetadata.durationMs`, `cumulativeDroppedTokens`, `preCompactDiscoveredTools[]`, `preservedMessages.{uuids[],allUuids[],anchorUuid}`, `preservedSegment.{anchorUuid,headUuid,tailUuid}`, `logicalParentUuid` | UNS | no home |
| `api_error` → `error.{message,formatted,connection.{code,message,isSSLError},status,rateLimits,isNetworkDown}`, `retryAttempt`, `retryInMs`, `maxRetries`, `source=request_retry` | UNS | a TRANSIENT retry; `ApiRequestFailed` is terminal ("the agent could not continue") and `SessionUpdate` has no retrying arm (`ApiRateLimited.retry_after_ms` could take `retryInMs` only if the retry were modelled) |
| `stop_hook_summary` → `hookCount`, `hookInfos[].{command,durationMs}`, `hookErrors[]`, `hasOutput`, `preventedContinuation`, `stopReason`, `hookAdditionalContext[]`, `toolUseID`, `level` | UNS | hooks have no home in conversation.v1 |
| `turn_duration` → `durationMs`, `messageCount` | UNS | `AgentCompleted` carries only `answer`; no settled turn duration field |
| `away_summary` → `content` | UNS | no home |
| `local_command` → `content` (`<local-command-stdout>…`), `level` | UNS | output of a session command; see §2.2 |
| `scheduled_task_fire` → `content`, `cronKind` | UNS | no home |
| `agents_killed` | UNS | carries no ids, so it cannot produce `TurnKilledForced.stopped_work` |
| `informational` → `content`, `level` (`notice`/`warning`) | UNS | no home |
| `pendingBackgroundAgentCount` (2,007), `pendingWorkflowCount` (200) | UNS | `TurnLive.live_work` wants ids, a count cannot produce it |
| `isMeta` (false), `subtype`, `level` | REP | discriminators |

### 2.5 Tool calls with an arm — `tool_use.input.*` and `toolUseResult.*`

**Read → `AgentRead`**

| path | class | target / reason |
|---|---|---|
| `input.file_path`, `toolUseResult.file.filePath` | REP | `ReadPath.path` |
| `input.offset` (5,940), `toolUseResult.file.startLine` | UNS | `AgentReadHead` is "A HEAD rather than a slice from the middle, deliberately" — 5,940 observed reads ARE slices; the contents of an offset read cannot be stated honestly as a head |
| `input.limit` (6,178) | REP (derived) | selects `head` vs `whole` together with `numLines`/`totalLines` |
| `input.file_offset`, `input.parameter` (1 each, malformed), `input.__unparsedToolInput.{raw,len}` (18) | UNS | malformed call; `AgentReadFailure` has no arms |
| `toolUseResult.file.content` | REP | `AgentReadWhole.contents` / `AgentReadHead.contents` |
| `toolUseResult.file.numLines`, `totalLines` | REP | `AgentReadHead.total_lines`; `numLines < totalLines` selects `head` |
| `toolUseResult.file.truncatedByTokenCap` (51) | REP | selects `head` |
| `toolUseResult.file.type` (`image/jpeg`), `base64` | REP | `ImageBlock` via spill (§2.2); `media_type` |
| `toolUseResult.file.dimensions.{displayWidth,displayHeight,originalWidth,originalHeight}`, `originalSize` | UNS | no home on `ImageBlock` |
| `toolUseResult.type` (`text`) | REP | discriminator |
| `toolUseResult` (string, 90: "Error: File does not exist…") | REP | `AgentReadFailure` (empty) |

**Bash → `AgentBash` / `AgentDetachedWork`**

| path | class | target / reason |
|---|---|---|
| `input.command` | REP | `AgentBashCommand.line` |
| `input.description` (68,746) | UNS | NO FIELD on `AgentBashStart`/`AgentBashCommand`; the headline is the raw command line |
| `input.run_in_background` | REP | `DetachedCauseRequested` |
| `input.timeout` (9,504) | REP (partial) | only reaches the wire as `DetachedCauseTimedOut.timeout_ms` when the command auto-backgrounds; otherwise no home |
| `input.dangerouslyDisableSandbox` (253), `toolUseResult.dangerouslyDisableSandbox` (216) | UNS | no home |
| `input.cmd2`, `command2`, `command_type`, `query2`, `timeout_ms` (1–3 each, malformed) | UNS | |
| `toolUseResult.stdout`, `stderr` | REP | `AgentBashOutputText.stdout/stderr` |
| `toolUseResult.interrupted` | REP | `AgentBashSuccess.outcome` (`completed`/`interrupted`) |
| `toolUseResult.isImage` | REP | `AgentBashOutput.form` discriminator (image bytes would ride `stdout`) |
| `toolUseResult.persistedOutputPath`, `persistedOutputSize` (408) | REP (derived) | `AgentBashOutputPartial.bytes_omitted` = `persistedOutputSize − len(stdout)` (DR:1948) |
| `toolUseResult.backgroundTaskId` (2,810) | REP | `DetachedWorkId.value`; `detached_from_id` = the call id |
| `toolUseResult.backgroundedByUser` (76) | REP | `DetachedCauseByUser` |
| `toolUseResult.timedOutAfterMs` (134) | REP | `DetachedCauseTimedOut.timeout_ms` |
| `toolUseResult.backgroundCwdHint`, `returnCodeInterpretation`, `noOutputExpected`, `staleReadFileStateHint` | DROP | DR:2073 "all documented as MODEL-FACING notes — text written for the agent to read, which nothing draws" |
| `toolUseResult.gitOperation.{commit.{sha,kind,branch},branch.{action,ref},push.branch,pr.{number,action,url}}` | DROP | DR:2079 "FLAGGED, not modelled … Nothing drawn consumes it" |
| `toolUseResult.backgroundEndsWithFinalResponse` (9) | UNS | no home |
| `toolUseResult` (string, ~3,600: "Error: Exit code 1…" or denial text) | REP | `AgentBashSuccess` with the text as stdout when the command ran; `AgentBashFailure` (empty) when it did not |

**Edit → `AgentEdit`**

| path | class | target / reason |
|---|---|---|
| `input.file_path`, `toolUseResult.filePath` | REP | `ReadPath.path` |
| `input.old_string`, `new_string`, `toolUseResult.oldString`, `newString` | DROP | `AgentEditStart`: "The matched and replacement text are deliberately absent: a consumer draws the CHANGE from the success arm's hunks" |
| `input.replace_all`, `toolUseResult.replaceAll` | DROP | `AgentEditSuccess.patch`: "the occurrence count is an observable rather than a flag" |
| `toolUseResult.structuredPatch[].{oldStart,oldLines,newStart,newLines,lines[]}` | REP | `FilePatchHunk.old_range/new_range/lines` |
| `toolUseResult.userModified` | REP | `AgentEditSuccess.user_modified` |
| `toolUseResult.originalFile` | DROP | DR:2254 "the entire pre-change file, which the hunks already summarise" |
| `toolUseResult.memdirStamped` (190), `staleRecovered` (167) | UNS | no home |
| `toolUseResult` (string, 306) | REP | `AgentEditFailure` (empty) |

**Write → `AgentWrite`**

| path | class | target / reason |
|---|---|---|
| `input.file_path`, `toolUseResult.filePath` | REP | `ReadPath.path` |
| `input.content`, `toolUseResult.content` | DROP | `AgentWriteSuccess.patch`: "a card can show the CHANGE rather than the whole file it was handed" |
| `toolUseResult.type` (`create`/`update`) | REP | `AgentWriteCreated` / `AgentWriteUpdated` |
| `toolUseResult.structuredPatch[]…`, `userModified` | REP | as Edit |
| `toolUseResult.originalFile` | DROP | DR:2254 |
| `toolUseResult.memdirStamped` (43) | UNS | |
| `toolUseResult` (string, 64) | REP | `AgentWriteFailure` (empty) |

**Skill → `AgentSkillUse`**

| path | class | target / reason |
|---|---|---|
| `input.skill`, `toolUseResult.commandName` | REP | `AgentSkillName.name` |
| `input.args` | REP | `AgentSkillUseStart.args` |
| `toolUseResult.success` | REP | arm discriminator |
| `toolUseResult.allowedTools[]` (498) | REP | `AgentSkillAllowedTools.tool_names` (static; preferred over the `command_permissions` attachment, which has no link — §4) |
| `tool_result.content` ("Launching skill: …") | DROP | `AgentSkillUse`: "THE INVOCATION'S OWN RETURN IS WORTHLESS TO DRAW" |
| `toolUseResult` (string, 32: "Error: Unknown skill…") | REP | `AgentSkillUseFailure` (empty) |

**Agent → `AgentSubagent`**

| path | class | target / reason |
|---|---|---|
| `input.description`, `toolUseResult.description` | REP | `AgentSubagentPrompt.description` |
| `input.prompt`, `toolUseResult.prompt` | REP | `AgentSubagentPrompt.text` |
| `input.subagent_type`, `toolUseResult.agentType` | REP | `AgentSubagentPrompt.subagent_type` |
| `input.model` | REP | `AgentSubagentPrompt.requested_model` |
| `input.isolation` (`worktree`/`remote`) | REP | `AgentSubagentPrompt.isolation` |
| `input.run_in_background` | REP | `DetachedCauseRequested` |
| `toolUseResult.agentId` | REP | `AgentSubagentStart.created_agent_id`; join to `subagents/agent-<id>.jsonl` |
| `toolUseResult.isAsync`, `outputFile` | REP (join) | detached; `outputFile` is where the report text lands |
| `toolUseResult.canReadOutputFile` | UNS | no home |
| `toolUseResult.status` (`completed`) | REP | `success` arm |
| `toolUseResult.content[].text` (sync only, 153) | REP | `AgentSubagentReport.prose` |
| `toolUseResult.totalDurationMs` | REP | `AgentSubagentTotals.duration_ms` |
| `toolUseResult.totalToolUseCount` | REP | `AgentSubagentTotals.tool_use_count` |
| `toolUseResult.totalTokens` | REP | `AgentSubagentProgress.total_tokens` (coarse) |
| `toolUseResult.usage.{input_tokens,cache_creation_input_tokens,cache_read_input_tokens,output_tokens,output_tokens_details.thinking_tokens}` (sync only, 153) | REP | `AgentSubagentTotals.usage` |
| `toolUseResult.usage.{cache_creation.*,iterations[],server_tool_use.*,service_tier,speed,inference_geo}` | DROP | DR:984 |
| `toolUseResult.toolStats.{readCount,searchCount,bashCount,editFileCount,linesAdded,linesRemoved,otherToolCount}` | REP | `AgentSubagentToolStats.*` |
| `toolUseResult.resolvedModel` | REP | `AgentSubagentSuccess.models_used[0]` (only ever ONE observed; "more than one entry" has no producer) |
| `toolUseResult.worktreePath`, `worktreeBranch` (6) | REP | `AgentSubagentWorktree.path/branch` |
| `toolUseResult` (string, 58: "Agent type 'x' not found…") | REP | `AgentSubagentFailure` (empty) |

**SendMessage → `AgentSendMessage`**

| path | class | target / reason |
|---|---|---|
| `input.to` / `input.recipient` (duplicates) | REP | `AgentSendMessageStart.addressed_to` |
| `input.content` / `input.message` (duplicates) | REP | `AgentSendMessageBody.text` |
| `input.summary` | REP | `AgentSendMessageSummary.text` |
| `input.type` (`message`) | UNS | no home |
| `toolUseResult.success` | REP | arm |
| `toolUseResult.pin.id` | REP | `AgentSendMessageSuccess.recipient_agent_id` |
| `toolUseResult.pin.name`, `pin.ref` | UNS | no home |
| `toolUseResult.resumedAgentId` (154) | REP | presence ⇒ `resumed_recipient`, absence ⇒ `queued_to_live` |
| `toolUseResult.message` | DROP | `AgentSendMessageResumedRecipient`: "that distinction exists ONLY inside a prose sentence … this arm states the fact that is structurally available and no more" |
| `toolUseResult.display` (3, "Not sent — …") | REP | `AgentSendMessageFailure` (empty; text dropped) |

**TaskCreate / TaskUpdate → `AgentTaskAct`**

| path | class | target / reason |
|---|---|---|
| TaskCreate `input.subject`, `description`, `activeForm`; `toolUseResult.task.{id,subject}` | REP | `AgentTaskCreated`, `AgentTaskId.value`, `AgentTaskState.subject/description`, `AgentTaskRunning.active_form` |
| TaskCreate `input.prompt`, `input.tasks` (1 each, misuse) | UNS | |
| TaskUpdate `input.taskId`, `toolUseResult.taskId` | REP | `AgentTaskId` |
| TaskUpdate `input.status`, `toolUseResult.statusChange.to` | REP | `AgentTaskState.status` arm |
| TaskUpdate `input.subject`, `description`, `activeForm` (1–4 each) | REP | state fields — but see §4 (state must be resolved from earlier acts) |
| TaskUpdate `toolUseResult.statusChange.from` | DROP | `AgentTaskAct.state`: "resolved by the producer, so no consumer replays a sequence of acts" |
| TaskUpdate `input.addBlockedBy[]`, `input.metadata.*`, `toolUseResult.updatedFields[]`, `toolUseResult.success` | UNS | no dependency/metadata fields on `AgentTaskState` |

**AskUserQuestion → `AgentQuestion`**

| path | class | target / reason |
|---|---|---|
| `input.questions[].{question,header,multiSelect}` | REP | `AgentQuestionAsked.question/header/choices` |
| `input.questions[].options[].{label,description,preview}` | REP | `AgentQuestionOption.*` |
| `toolUseResult.questions[]…` | REP | `AgentQuestionSuccess.batch` (restated) |
| `toolUseResult.answers.{question}` (string) | REP (lossy) | `AgentQuestionSelection.chosen[]` when it equals an option label (the vendor appends " (Recommended)"), else `free_text`; a multi-select is a comma-joined string, ambiguous when a label contains a comma |
| `toolUseResult.annotations.{question}.preview` (12) | UNS | no home (not `AgentQuestionNote`, which is per-selection prose) |
| `toolUseResult` (string "User rejected tool use", 51) + `userFeedback` | REP / UNS | `AgentQuestionUnanswered`; feedback prose has no field |
| `AgentQuestionSelection.note` | — | NO PRODUCER observed |

**Workflow → `AgentWorkflowStart`**

| path | class | target / reason |
|---|---|---|
| `input.script` (inline source) | DROP | `AgentWorkflowScript.path` carries the path; `toolUseResult.scriptPath` supplies it |
| `toolUseResult.workflowName` | REP | `AgentWorkflowStart.name` |
| `toolUseResult.scriptPath` | REP | `AgentWorkflowScript.path` |
| `toolUseResult.runId` | REP | `AgentWorkflowPlacementLocal.run_id` |
| `toolUseResult.taskId` | REP | `DetachedWorkId` |
| `toolUseResult.transcriptDir` | REP (join) | where `subagents/workflows/<run>/journal.jsonl` and its agents' files live |
| `toolUseResult.summary` | REP | `AgentWorkflowNotice.text` (closest; it is launch-time prose) |
| `toolUseResult.status` (`async_launched`), `taskType` (`local_workflow`) | REP | discriminators |
| `AgentWorkflowStart.resumed_from`, `AgentWorkflowPlacementRemote`, `AgentWorkflowCompleted.summary`, `AgentWorkflowScriptRejected` | — | NO PRODUCER observed (no remote run, no rejection, no resume in corpus) |

### 2.6 Tool calls with NO arm (every `input.*` and `toolUseResult.*` key is UNS)

The proto forbids routing these through `AgentUnmodeled` ("A recognizable built-in arriving here is a PRODUCER DEFECT"), and DR:2499 records only that a ruling is OWED for SendMessage/Task*. Each is therefore UNSUPPORTED wholesale:

| tool | calls | keys (all UNS) |
|---|---|---|
| ToolSearch | 508 | `input.query`, `max_results`; `toolUseResult.matches[]`, `query`, `total_deferred_tools`; result `content[].tool_reference.tool_name` |
| WebFetch | 98 | `input.url`, `prompt`; `toolUseResult.{bytes,code,codeText,durationMs,result,url}` |
| WebSearch | 56 | `input.query`, `allowed_domains[]`; `toolUseResult.{durationSeconds,query,searchCount,results[].{content[].{title,url},tool_use_id}}` |
| Monitor | 192 | `input.{command,description,persistent,timeout_ms,timeout,target,until,bash_id,timeout_seconds,wait_for_completion}`; `toolUseResult.{taskId,persistent,timeoutMs}` — a Monitor is a DETACHED task (`taskId`) with no `DetachableWork` arm (the oneof has subagent/bash/workflow only) |
| TaskStop | 137 | `input.task_id`; `toolUseResult.{task_id,task_type,command,message}` |
| TaskOutput | 7 | `input.{task_id,block,timeout}`; `toolUseResult.{retrieval_status,task.{task_id,task_type,description,status,output,exitCode,prompt,result,isRawTranscript}}` |
| TaskList | 4 | `toolUseResult.tasks[].{id,subject,status,blockedBy[]}` |
| ScheduleWakeup | 103 | `input.{delaySeconds,prompt,reason,stop,noop}`; `toolUseResult.{scheduledFor,clampedDelaySeconds,wasClamped,stopped,cancelledWakeups}` |
| EnterWorktree | 21 | `input.{name,path}`; `toolUseResult.{message,worktreePath,worktreeBranch}` |
| ExitWorktree | 2 | `input.{action,discard_changes}`; `toolUseResult.{action,discardedCommits,discardedFiles,message,originalCwd,worktreeBranch,worktreePath}` |
| ListAgents | 23 | `toolUseResult.listing` |
| StructuredOutput | 9 | `input.{verdict,reason}`; `toolUseResult` (string) |
| BashOutput | 5 | `input.bash_id`; `toolUseResult.{command,exitCode,shellId,status,stderr,stderrLines,stdout,stdoutLines,timestamp}` — note `exitCode` exists HERE, contradicting DR:2070 "No exit code exists on `BashOutput` at all" (that sentence was about the Bash tool's result type; the BashOutput TOOL's result does carry one) |

### 2.7 `attachment` records (by `attachment.type`)

| attachment.type (n) | paths | class | target / reason |
|---|---|---|---|
| `hook_success` (146,972), `hook_non_blocking_error` (1,137), `hook_blocking_error` (366), `hook_cancelled` (11), `hook_system_message` (6) | `command`, `hookName`, `hookEvent`, `toolUseID`, `durationMs`, `exitCode`, `stdout`, `stderr`, `timedOut`, `timeoutMs`, `blockingError.{blockingError,command}`, `content` | UNS | hooks have no home; 148k records, 83% of all attachments |
| `command_permissions` (502) | `allowedTools[]` | REP (non-static) | `AgentSkillAllowedTools.tool_names` (DR:1536) — carries NO `toolUseID`/`sourceToolUseID`; link is positional (§4) |
| `total_tokens_reminder` (5,942) | `text` | UNS | no home (`SessionCold.context_tokens` is a different fact) |
| `task_reminder` (4,916) | `content[].{id,subject,description,status,activeForm,blockedBy[],blocks[],metadata.*}`, `itemCount` | UNS as a record; fields overlap `AgentTaskState` (see §4 — it is the only restatement of `description`) |
| `deferred_tools_delta` (4,367) | `addedNames[]`, `removedNames[]`, `readdedNames[]`, `addedLines[]` | UNS | no home (`SessionMcpServer` is connection health only) |
| `skill_listing` (4,307) | `names[]`, `skillCount` | UNS | |
| `diagnostics` (2,426) | `files[].uri`, `files[].diagnostics[].{message,severity,source,code,range.{start,end}.{line,character}}`, `isNew` | UNS | LSP diagnostics; no home |
| `agent_listing_delta` (2,293) | `addedTypes[]`, `removedTypes[]`, `showConcurrencyNote` | UNS | |
| `queued_command` (1,063) | `prompt` (str or `[]{type,text}`), `commandMode`, `timestamp` | UNS | the binary's own queue; the daemon's hold is the model (`AgentInput.prompt` comment) but the vendor-side queue event has no home |
| `edited_text_file` (825) | `filename`, `snippet` | UNS | |
| `compact_file_reference` (389) | `filename`, `displayPath` | UNS | |
| `file` (343) | `filename`, `content.{type,file.{filePath,content,numLines,startLine,totalLines}}` | UNS | an `@file` the user attached; `UserContentBlock` has text/image only and the proto forbids `UnsupportedBlock` for a knowable kind |
| `nested_memory` (146) | `path`, `content.{path,type,content,contentDiffersFromDisk,parent}` | UNS | |
| `date_change` (140) | `newDate` | UNS | |
| `auto_mode` (122) / `auto_mode_exit` (1) | `autoModeConsentFlow`, `bashFirst`, `steerOnly`, `bypass` | UNS | `AgentPermissionModeAuto` is the mode; these flags have no fields |
| `bash_output_audience_note` (83) | `toolUseID` | DROP | DR:2073 (model-facing note) |
| `invoked_skills` (74) | `skills[].{name,path,content}` | UNS | re-injected skill bodies with no call link |
| `read_truncation_notice` (62) | `banner` | DROP | redundant: `toolUseResult.file.{numLines,totalLines}` already produces `AgentReadHead.total_lines` |
| `task_status` (48) | `taskId`, `taskType`, `description`, `status`, `deltaSummary`, `outputFilePath` | REP | `AgentSubagentUpdate.note.text` ← `deltaSummary`; join by `taskId` = `agentId` |
| `dynamic_skill` (12) | `skillDir`, `skillNames[]`, `displayPath` | UNS | |
| `structured_output` (9) | `data.{verdict,reason}`, `toolUseID` | UNS | |
| `silent_turn_reminder` (7) | `text` | UNS | |
| `plan_mode` (2) / `plan_mode_exit` (1) | `reminderType`, `isSubAgent`, `planFilePath`, `planExists` | UNS | `AgentPermissionModePlan` is the mode; plan file has no field |
| common: `isInitial` (6,600), `isMeta` (1), `needsAuthMcpServers[]` (2,274), `pendingMcpServers[]` (2,363), `source_uuid` (28), `origin.*` (358), `displayPath` (897) | — | UNS | `needsAuth`/`pending` are MCP states with no `SessionMcpServer` arm (only connected/failed) |

### 2.8 Other top-level record types

| type (n) | paths | class | target / reason |
|---|---|---|---|
| `queue-operation` (20,379) | `operation` (`enqueue`/`dequeue`), `content`, `sessionId`, `timestamp` | UNS | vendor prompt queue |
| `last-prompt` (18,887) | `lastPrompt`, `leafUuid` | UNS | |
| `mode` (12,490) | `mode` (`normal`) | UNS | |
| `permission-mode` (7,926) | `permissionMode` | REP | `SessionPermissionModeChanged.permission_mode` |
| `ai-title` (7,853) / `custom-title` (18) / `agent-name` (93) | `aiTitle` / `customTitle` / `agentName` | UNS | naming is the daemon's (DR:3077) |
| `file-history-snapshot` (3,339) / `file-history-delta` (611) | `messageId`, `isSnapshotUpdate`, `snapshot.{messageId,timestamp,trackedFileBackups.{file}.{backupFileName,backupTime,realParentDir,version}}`; `backup.*`, `trackingPath`, `snapshotMessageId` | UNS | rewind bookkeeping |
| `pr-link` (1,157) | `prNumber`, `prRepository`, `prUrl` | UNS | |
| `atis-latch` (642) | `atis` | UNS | |
| `relocated` (40) | `relocatedCwd` | UNS | |
| `worktree-state` (40) | `worktreeSession.{originalBranch,originalCwd,originalHeadCommit,preEnterOriginalCwd,sessionId,worktreeBranch,worktreeName,worktreePath}` | UNS | EnterWorktree has no arm |
| `started` / `result` (86 each; `subagents/workflows/<run>/journal.jsonl`) | `key`, `agentId`, `result` | REP / UNS | `result` → `AgentSubagentReport.prose` for a workflow agent (join by `agentId`); `key` has no home; `started` carries nothing `AgentSubagentStart` needs (no prompt — `AgentSubagentPrompt.description` "UNSET FOR AN AGENT A WORKFLOW SCRIPT SPAWNED" is the recorded reason) |

## 3. Consolidated UNSUPPORTED list (no home, no recorded reason)

Ordered by volume; example values abbreviated.

| # | key path(s) | n | example |
|---|---|---|---|
| 1 | `attachment.type=hook_*` (`command`, `hookName`, `hookEvent`, `stdout`, `stderr`, `exitCode`, `durationMs`, `toolUseID`, `blockingError.*`) | 148,492 | `hookName="SessionStart:startup"`, `stdout="IMPORTANT - DO NOT IGNORE — GNS Plugin A…"`, `blockingError="[…run-subproject-tests.sh]: daemon test suite failed"` |
| 2 | `assistant.effort` | 221,509 | `xhigh` |
| 3 | `assistant.attributionAgent` / `attributionSkill` / `attributionPlugin` | 139,323 / 20,802 / 856 | `statusline-setup` / `create-or-update-workspace` / `gns-cowork` |
| 4 | `*.session_id` (snake-case duplicate) | 157,746 | uuid |
| 5 | `*.cwd`, `gitBranch`, `version`, `userType`, `entrypoint`, `slug`, `sessionKind` | each ≈ all records | `/`, `HEAD`, `2.1.220`, `external`, `sdk-cli`, `jazzy-puzzling-hare`, `bg` |
| 6 | `assistant.requestId` | 226,262 | `req_011CdV3RAbeLdrLddF6wZpC1` |
| 7 | `user.promptId` | 144,751 | uuid |
| 8 | `assistant.message.content[].caller.type` | 131,351 | `direct` (null ×162) |
| 9 | `Bash input.description` | 68,746 | `Resolve source workspace name, path, and…` |
| 10 | `attachment.type=total_tokens_reminder.text` | 5,942 | `<total_tokens>14962655 tokens left</total_tokens>` |
| 11 | `system/stop_hook_summary.*` (`hookCount`, `hookInfos[]`, `hookErrors[]`, `preventedContinuation`, `stopReason`, …) | 5,259 | `hookErrors[0]="Failed to run: Hook \"powershell…"` |
| 12 | `attachment.type=task_reminder.content[]` | 4,916 | `{id:"1", subject:"Verify daemon stand-down…", status:"in_progress"}` |
| 13 | `system/turn_duration.{durationMs,messageCount}` | 4,435 | `12626`, `25` |
| 14 | `attachment.type=deferred_tools_delta`, `skill_listing`, `agent_listing_delta`, `diagnostics` | 4,367 / 4,307 / 2,293 / 2,426 | `addedNames=["CronCreate",…]`; `diagnostics[].message="\"errors\" imported and not used"` |
| 15 | `user.origin.kind` (+ `from/name/senderTaskId/body`) | 5,368 (+36) | `human`; `from="opus-medium"`, `body="Commit df6e8eaf on DWC/…"` |
| 16 | `Read input.offset` / `toolUseResult.file.startLine` (mid-file slices the proto's HEAD-only `extent` cannot state) | 5,940 | `offset=270` |
| 17 | `user.promptSource` | 8,420 | `sdk` / `typed` / `system` / `queued` |
| 18 | `system.pendingBackgroundAgentCount` / `pendingWorkflowCount` | 2,007 / 200 | `2` / `1` |
| 19 | `attachment.type=queued_command.{prompt,commandMode,timestamp}` | 1,063 | `commandMode="prompt"` |
| 20 | `user.content` = `Background agent "…" was stopped by the user.` (`queuePriority`) | 82 | prose |
| 21 | `attachment.type=edited_text_file`, `compact_file_reference`, `file`, `nested_memory`, `invoked_skills`, `dynamic_skill`, `date_change`, `silent_turn_reminder`, `plan_mode*`, `auto_mode*`, `structured_output` | 825 / 389 / 343 / 146 / 74 / 12 / 140 / 7 / 3 / 123 / 9 | `file.content.file.filePath="/Users/…/test.ts"` |
| 22 | `attachment.needsAuthMcpServers[]`, `pendingMcpServers[]` | 2,274 / 2,363 | `claude.ai Google Calendar` |
| 23 | `user.classifierMetaLines` | 738 | `{"meta":{"gitStatus":{"staged":0,"modified":9,…}}}` |
| 24 | `system/compact_boundary.compactMetadata.{trigger,durationMs,cumulativeDroppedTokens,preCompactDiscoveredTools[],preservedMessages.*,preservedSegment.*}`, `logicalParentUuid` | 209 | `trigger="manual"`, `cumulativeDroppedTokens=370263` |
| 25 | `system/api_error.*` (transient retry) | 27 | `error.formatted="Unable to connect to API (ECONNRESET)"`, `retryInMs=547` |
| 26 | `system/away_summary`, `local_command` output, `scheduled_task_fire`, `informational`, `agents_killed` | 416 / 411 / 37 / 4 / 8 | `<local-command-stdout>Not enough messages to compact.` |
| 27 | `user.toolDenialKind=automode-unavailable` | 45 | `claude-sonnet-5[1m] is temporarily unavailable` |
| 28 | `user.userFeedback` (question clarification) | 10 | `The user wants to clarify these questions…` |
| 29 | `user.turnCompanion` skill document (typed `/skill`, no call to link) | 39 | `Base directory for this skill: …` |
| 30 | `user.isMeta` without `sourceToolUseID` | 276 | `Continue from where you left off.` |
| 31 | `assistant.message.context_management.applied_edits[]` | 31 | context edits |
| 32 | `assistant.message.{type,container,stop_details,stop_sequence}` | all | null |
| 33 | Tools with no arm (§2.6): ToolSearch 508, Monitor 192, TaskStop 137, ScheduleWakeup 103, WebFetch 98, WebSearch 56, ListAgents 23, EnterWorktree 21, StructuredOutput 9, TaskOutput 7, BashOutput 5, TaskList 4, ExitWorktree 2 | 1,165 calls | `Monitor input.command="until ! ps -p 90540…"`, `WebFetch toolUseResult.code=301` |
| 34 | Per-tool leftovers: Bash `dangerouslyDisableSandbox` (469), `backgroundEndsWithFinalResponse` (9); Edit `memdirStamped` (190), `staleRecovered` (167); Write `memdirStamped` (43); Agent `canReadOutputFile` (1,653); Read `dimensions.*`/`originalSize` (45), `__unparsedToolInput` (18); SendMessage `input.type` (337), `pin.name/ref` (279); TaskUpdate `addBlockedBy[]`/`metadata.*`/`updatedFields[]` (3/3/175); AskUserQuestion `annotations.*` (171) | | |
| 35 | Metadata record types: `queue-operation` 20,379, `last-prompt` 18,887, `mode` 12,490, `ai-title` 7,853, `file-history-snapshot` 3,339, `pr-link` 1,157, `atis-latch` 642, `file-history-delta` 611, `agent-name` 93, `relocated` 40, `worktree-state` 40, `custom-title` 18; workflow journal `key` (172) | 65,689 | |

## 4. NON-STATIC resolutions

| # | what | why it is not static | static alternative (if any) |
|---|---|---|---|
| 1 | `AgentActivity.usage` on "exactly one unit per API response" | An API response is written as ONE RECORD PER CONTENT BLOCK (231,143 of 231,166 assistant records carry one block; 99.9%). The blocks of one `message.id` arrive as 1–5+ records, EACH repeating `message.usage`, and in 38,109 cases the later record's usage DIFFERS from the first (the first often has `stop_reason=null`, i.e. the message was still streaming). The producer cannot know at the first record whether the usage it holds is final. | Keyed lookup `message.id → first block's AgentActivityId` and UPSERT that unit's `usage` on every later record of the same id (the protocol's upsert rule permits it). One keyed lookup; static. The current sidecar has no such map. |
| 2 | `<task-notification>` (1,805) — the terminal frame of every DETACHED subagent/shell/monitor/workflow | It is XML embedded in a user-prompt STRING, parsed as prose; `<tool-use-id>` is present in only 1,499/1,805; the other 306 carry `<task-id>` only, which resolves to a call only through the `backgroundTaskId`/`agentId` a PREVIOUS record's `toolUseResult` stated. `<usage>` gives a single `subagent_tokens` figure, so `AgentSubagentTotals.usage` (the `TokenUsage` breakdown) has NO static producer for async spawns — obtaining it means walking the subagent's own file and summing. | Keyed `task-id → call id` map (static). The `TokenUsage` breakdown stays non-static. |
| 3 | `command_permissions` attachment → `AgentSkillAllowedTools` (DR:1536) | The attachment carries no `toolUseID`/`sourceToolUseID`; its link to the skill call is positional (it follows the skill document record). | `toolUseResult.allowedTools[]` on the Skill result itself (498 of 502 cases) — keyed by `tool_use_id`, static. |
| 4 | `AgentTaskAct.state` ("resolved by the producer") | A `TaskUpdate` carries only the changed fields (`status` in 160/167; `description` in 4; `subject` in 1). `subject`/`description`/`active_form` for the resolved state live on the ORIGINAL `TaskCreate` or on a later `task_reminder` attachment — prior-record state. | Keyed `task id → last state` map (static per act, but the map is unbounded across a session; acceptable as "a constant number of keyed lookups"). |
| 5 | `ContextCompacted.summary` | The boundary (`system/compact_boundary`) is written BEFORE the summary (`user` with `isCompactSummary`); the current sidecar resolves it with the `next` line (positional, `convert.go:179`). | The summary's `parentUuid` == the boundary's `uuid` in 209/209 cases; emit the frame at the summary record with one keyed lookup. Static. |
| 6 | Skill document (`user` `isMeta`+`sourceToolUseID`) | The current sidecar keeps a skill-NAME map and matches "what arrives next" (`convert/detached.go:65`, noted at DR:1538). | `sourceToolUseID` — keyed, static (745/791 skill calls; the remaining 46 are failed invocations with no document). |
| 7 | `AgentSubagentSuccess.models_used` "in the order they were used" | Only `resolvedModel` (one value) is stated; multiple models would require walking the subagent file's `message.model` values. | None; one entry only. |
| 8 | `AgentCompleted.answer` ("the last top-level prose it produced") | Needs the LAST text block of the turn, knowable only when the turn ends (`stop_reason=end_turn` on the final record, or the next user prompt). `stop_reason` is dropped (DR:2364), so "last" is positional. | Keyed `agent → last text AgentActivityId` (one lookup at turn end); static if `stop_reason`/next-prompt marks the end. |
| 9 | Read image results (base64 in `tool_result.content[].source.data` / `toolUseResult.file.base64`) | `ImageBlock` is by reference; the producer must spill 45 payloads (up to 2.3 MB) to disk. Static, but a side effect per record. | — |
| 10 | AskUserQuestion `answers.{question}` → `chosen[]` vs `free_text` | Distinguishing a chosen label from free text means comparing the string against the option labels (with the vendor's " (Recommended)" suffix stripped) and splitting a multi-select on commas. Constant work, but lossy. | — |

## 5. conversation.v1 fields whose only producer would be the transcript, for which NO key path exists

| field | status in corpus |
|---|---|
| `AgentGrep` (every arm), `AgentGrepQuery`, `AgentGrepContent/Files/Count` | NO PRODUCER: zero `Grep` tool calls in 641,428 records (searches run through `Bash`) |
| `AgentGlob` (every arm) | NO PRODUCER: zero `Glob` calls |
| `AgentBashUpdate.{new_output,from_offset}` | no key path; a detached shell's growing spool is read from the `persistedOutputPath`/task output FILE, not from any record |
| `AgentBashOutputImage` | `toolUseResult.isImage` is `false` in all 74,331 Bash results |
| `AgentBashInterrupted` | `toolUseResult.interrupted` is present; no `true` example was sampled (count not broken out) |
| `AgentPermission.start` (`AgentPermissionPrompt`, `AgentPermissionTrigger`, `offered_standing`), `AgentPermissionAllowed.{once,standing}`, `AgentPermissionStanding`, all `AgentPermissionChange` arms | NO key path: the transcript records no permission PROMPT at all, only the denial outcome (`toolDenialKind`). Allowed calls are indistinguishable from calls that never asked |
| `AgentPermissionDeniedByPolicy.reason`, `.decider` | only `toolDenialKind` (the kind) and the result text; no separate decider/reason keys |
| `AgentQuestionSelection.note`, `AgentQuestionFreeText` (structurally) | no key path; `answers.{q}` is one string |
| `AgentReadFailure`, `AgentWriteFailure`, `AgentEditFailure`, `AgentBashFailure`, `AgentSubagentFailure`, `AgentSkillUseFailure`, `AgentSendMessageFailure`, `AgentUnmodeledFailure`, `AgentThinkingFailure`, `AgentResponseFailureReason`, `AgentWorkflowRunEnded` | arms are empty by design ("DERIVED at the implementation wave"); the transcript DOES carry the failure text (`toolUseResult` string, `tool_result.content` with `is_error`) that those arms would need |
| `AgentThinkingUpdate.text` / `AgentResponseUpdate.new_markdown` (deltas) | no key path: the transcript holds settled blocks only; deltas are a live-stream fact |
| `AgentSubagentProgress.{duration_ms,tool_use_count,total_tokens}` | only at conclusion (`task-notification <usage>` / sync `toolUseResult`); no mid-run key path (`task_status.deltaSummary` gives `note` only) |
| `AgentSubagentTotals.usage` for an ASYNC spawn | no key path (`<subagent_tokens>` is one figure) |
| `AgentSubagentPrompt.requested_name` | no `name` key observed on `Agent` input |
| `AgentSubagentIsolationRemote` | `input.isolation` observed as `worktree` only (475) |
| `AgentSendMessageSuccess.recipient_agent_id` when the recipient was named by NAME | `pin.id` is always the id; fine — but `AgentSendMessageFailure` text (`display`) has no field |
| `AgentTaskState.owner`, `AgentTaskFailed`, `AgentTaskKilled`, `AgentTaskPaused` | no `owner` key; observed statuses are `pending`/`in_progress`/`completed` only |
| `AgentWorkflowStart.resumed_from`, `AgentWorkflowPlacementRemote.session_url`, `AgentWorkflowCompleted.summary`, `AgentWorkflowScriptRejected.error`, `AgentWorkflowInterrupted` | no key path; the workflow journal has `started`/`result` per agent only, no run-level terminal record |
| `AgentWorkflowUpdate.agent_start` for a workflow agent | the journal's `started` record carries only `key`+`agentId` — no prompt text (`AgentSubagentPrompt.text` is required, not optional) |
| `AgentSkillUseSuccess.document` for a typed `/skill` | the document arrives (`turnCompanion`) but no `AgentSkillUse` unit exists to settle |
| `SessionStarted.model_catalog`, `SessionRuntime.*`, `SessionCold.*`, `SessionAccountUsage.*`, `SessionFastMode`, `SessionMcpServer.{connected,failed}`, `SessionIdentityRotated`, `SessionQueryDied` | no transcript key path (live-SDK facts; `needsAuthMcpServers`/`pendingMcpServers` are the only MCP traces and match neither arm) |
| `TurnKilledForced.stopped_work[]`, `TurnLive.live_work[]` | only counts (`pendingBackgroundAgentCount`) and an id-less `agents_killed` record |
| `ContextCleared.tokens` | `/clear` appears as a `<command-name>` user record with no token figures |
| `ApiRateLimited.retry_after_ms`, `ApiOverloaded.retry_after_ms` | terminal API errors carry no retry figure; `retryInMs` appears only on the transient `system/api_error` |
| `ImageBlockUrl` | no URL-form image observed (all base64) |
| `UserContentBlock.image` | NO user-pasted image observed in 149,402 user records (the only images are Read results) |
