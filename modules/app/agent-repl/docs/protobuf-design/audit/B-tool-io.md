# Partition B — per-tool INPUT / OUTPUT field audit

Sources (tier: declared types):
- `T` = `agent-shim/claude/shim/node_modules/@anthropic-ai/claude-agent-sdk/sdk-tools.d.ts` (3807 lines; line refs below are `T:NNN`)
- `S` = `.../claude-agent-sdk/sdk.d.ts` — structured output delivery is `SDKUserMessage.tool_use_result?: unknown` (S:4591) and `SDKUserMessageReplay.tool_use_result` (S:4637): "the tool's full Output object ... keyed by the matching tool_use block's name". There is NO per-tool typed `structured` field on `SDKAssistantMessage`; delivery is on the USER-role message carrying the `tool_result` block. Per-call progress is `SDKTaskProgressMessage` (S:4478: `task_id, tool_use_id?, description, subagent_type?, usage{total_tokens,tool_uses,duration_ms}, last_tool_name?, summary?`).
- `P` = `proto/src/conversation/v1/agent_activity.proto`, `question.proto`, `detached_work.proto`, `workflow.proto`
- `D` = `docs/protobuf-design/figma-to-idl-redesign.md` (line refs `D:NNNN`)

Current shim state (load-bearing for the "lands in AgentUnmodeled?" question): the shim does NOT yet produce `conversation.v1` activities. `protocol.ts:404-417` forwards `tool_use_result` verbatim as `ToolResultEvt.structured` ("Forwarded verbatim rather than projected"); `session.ts:673` attaches it; `uds/proto.ts:27-31` re-exports `content_pb/message_pb/payloads_pb/tokens_pb` which do not correspond to the landed `agent_activity.proto`. So TODAY nothing lands in `AgentUnmodeled`; every answer below about "lands in AgentUnmodeled" is about the mapping the shim MUST implement, classified per `AgentUnmodeled`'s own comment (P:1367-1381: "A recognizable built-in arriving here is a PRODUCER DEFECT").

Static-resolution rule applied: a field is STATIC when it is a constant number of keyed reads off `tool_use` input, `tool_use_result`, or the `task_progress` record with the matching `tool_use_id`. Scans, regexes over prose, or searches through a message list are NON-STATIC.

Legend: REP = represented; DROP = dropped with cited reason; UNS = unsupported (no home, no reason on record).

---

## 1. Bash — `BashInput` T:522-553, `BashOutput` T:2898-2997 → `AgentBash` P:794-940

FORWARD
| arm field | source | static? |
|---|---|---|
| `start.command.line` | `input.command` | yes |
| `start.started_at.at_ms` | assistant-message receive time / `timestamp` (S) — NOT from BashInput/Output; no per-call issue instant exists on the tool types | yes (envelope) |
| `update.new_output` / `from_offset` | NOT from any T type; sidecar spool tail (D:2058-2067). Detached-only (P:831-838) | n/a here |
| `success.command.line` | `input.command` (must be re-read from the tool_use block keyed by `tool_use_id`) | yes |
| `success.outcome.completed` vs `interrupted` | `output.interrupted` (T:2909) | yes |
| `*.output.form.text.stdout/stderr` | `output.stdout` / `output.stderr` (T:2902,2906) | yes |
| `*.output.form.text.extent.whole` vs `partial` | `output.persistedOutputPath` presence (T:2946) | yes |
| `*.output.form.text.extent.partial.bytes_omitted` | `output.persistedOutputSize - (stdout+stderr byte length)` (T:2950) — derived arithmetic; `persistedOutputSize` is "total size", inline portion length must be measured | yes but DERIVED (flag) |
| `*.output.form.image.data` / `media_type` | `output.isImage` (T:2912) is a BOOL; the bytes ride the `tool_result` content block image (base64 + media_type). NOT on BashOutput. Requires reading the tool_result content block, static (first image block) | yes, cross-record |
| `failure` (empty) | `tool_result.is_error` | yes |

REVERSE (Input)
| field | status | home / reason |
|---|---|---|
| `command` T:526 | REP | `AgentBashCommand.line` |
| `timeout` T:530 | DROP | D:2042-2047 — only surfaces as `DetachedCauseTimedOut.timeout_ms` via `output.timedOutAfterMs`; the requested value itself is not drawn (no explicit comment; classify as DROP-by-omission with the detach rationale) |
| `description` T:546 | UNS | no proto field; D does not mention Bash `description`. A user-facing one-liner the model writes for the reader — arguably drawable as headline |
| `run_in_background` T:550 | REP (indirect) | `DetachedCauseRequested` (detached_work.proto) per D:2048-2056 |
| `dangerouslyDisableSandbox` T:554 | UNS | no home; consent-relevant fact (same class as skill `allowed_tools` the record chose to keep) |

REVERSE (Output)
| field | status | home / reason |
|---|---|---|
| `stdout` T:2902 | REP | `AgentBashOutputText.stdout` |
| `stderr` T:2906 | REP | `AgentBashOutputText.stderr` |
| `rawOutputPath` T:2909 | DROP | D:2073 "an MCP concern" |
| `interrupted` T:2913 | REP | `outcome { completed | interrupted }` |
| `isImage` T:2917 | REP | `AgentBashOutput.form.image` |
| `backgroundTaskId` T:2921 | REP (indirect) | `DetachedWorkId.value` |
| `backgroundedByUser` T:2925 | REP | `DetachedCauseByUser` |
| `timedOutAfterMs` T:2929 | REP | `DetachedCauseTimedOut.timeout_ms` |
| `backgroundCwdHint` T:2933 | DROP | D:2073-2076 model-facing |
| `dangerouslyDisableSandbox` T:2937 | UNS | no home (same as input) |
| `returnCodeInterpretation` T:2941 | DROP | D:2074 model-facing |
| `noOutputExpected` T:2945 | DROP | D:2074 model-facing |
| `structuredContent` T:2949 | DROP | D:2076 untyped |
| `persistedOutputPath` T:2953 | REP (indirect) | selects `extent.partial`; path itself not carried — no stated reason for dropping the PATH (a reader could open it). Flag: partial-drop with no comment |
| `persistedOutputSize` T:2957 | REP (derived) | `AgentBashOutputPartial.bytes_omitted` |
| `staleReadFileStateHint` T:2961 | DROP | D:2074 model-facing |
| `ghRateLimitHint` T:2965 | DROP | D:2075 model-facing |
| `gitOperation.*` T:2969-2996 | DROP (flagged) | D:2079-2084 "FLAGGED, not modelled" |

No-producer proto fields: none on Bash. NON-STATIC: none, but `bytes_omitted` is arithmetic over measured byte lengths and `image.data` crosses to the tool_result block.

---

## 2. Read — `FileReadInput` T:602-619, `FileReadOutput` T:201-332 → `AgentRead` P:366-433

FORWARD
| arm field | source | static? |
|---|---|---|
| `start.path.path` | `input.file_path` | yes |
| `success.path.path` | `output.file.filePath` (text/notebook/pdf/parts/file_unchanged arms; ABSENT on the `image` arm T:236-268 — must fall back to input) | yes |
| `success.extent.whole.contents` | `output.file.content` when `numLines == totalLines` (T:214-226) | yes |
| `success.extent.head.contents` | `output.file.content` when `numLines < totalLines` OR `truncatedByTokenCap` | yes |
| `success.extent.head.total_lines` | `output.file.totalLines` (T:226) | yes |
| `failure` | `is_error` | yes |

PRODUCER GAP (HEAD semantics): P:416-420 states head is "cut on a line boundary ... reading from offset zero". But `FileReadInput.offset` (T:610) yields `output.file.startLine != 1` (T:222) — a MIDDLE SLICE. D:2266-2269: "A third `range` arm ... was offered and declined." So an offset read has NO honest arm: `whole` is false, `head` is a lie about where it starts. UNSUPPORTED-by-decision; flag as a contract hole, not a producer defect.

REVERSE (Input)
| field | status | reason |
|---|---|---|
| `file_path` T:606 | REP | `ReadPath.path` |
| `offset` T:610 | DROP (contested) | D:2268-2269 range arm declined — but see gap above |
| `limit` T:614 | DROP | same ruling; extent arm supersedes |
| `pages` T:618 | UNS | PDF page range; no arm for a PDF read at all |

REVERSE (Output) — union arms
| field | status | reason |
|---|---|---|
| `type:"text"` `.file.filePath/content/numLines/startLine/totalLines` T:203-226 | REP / `numLines` DROP (derivable) / `startLine` UNS (see gap) | |
| `.file.truncatedByTokenCap` T:230 | REP (indirect) | selects `head`; note this is a token-cap cut, which may NOT be on a line boundary as P:417 claims — verify at implementation |
| `type:"image"` `.file.base64/type/originalSize/dimensions.*` T:236-268 | UNS | no image-read arm; `AgentReadWhole.contents` is `string`. No D entry. An image Read arriving must either be stuffed as base64 text (dishonest) or go unmodeled (PRODUCER DEFECT per P:1372) |
| `type:"notebook"` `.file.filePath/cells` T:270-284 | UNS | `cells: unknown[]`; no home, no D entry |
| `type:"pdf"` `.file.filePath/base64/originalSize` T:286-300 | UNS | no home, no D entry |
| `type:"parts"` `.file.filePath/originalSize/count/outputDir` T:302-320 | UNS | no home |
| `type:"file_unchanged"` `.file.filePath`, `.source:"seeded"` T:322-332 | UNS | dedup'd re-read returns NO content; `whole.contents` would be empty and `head` has no total. No arm says "unchanged since last read". No D entry |

No-producer proto fields: none. NON-STATIC: none.

---

## 3. Write — `FileWriteInput` T:620-629, `FileWriteOutput` T:3073-3116 → `AgentWrite` P:471-523

FORWARD
| arm field | source | static? |
|---|---|---|
| `start.path.path` | `input.file_path` | yes |
| `success.path.path` | `output.filePath` T:3079 | yes |
| `success.outcome.created/updated` | `output.type` "create"/"update" T:3076 | yes |
| `success.patch[]` (`old_range.start/lines`, `new_range.start/lines`, `lines[]`) | `output.structuredPatch[].oldStart/oldLines/newStart/newLines/lines` T:3087-3095 | yes (array map, bounded by hunks) |
| `success.user_modified` | `output.userModified` T:3114 (optional → default false) | yes |
| `failure` | `is_error` | yes |

REVERSE (Input): `file_path` REP; `content` T:627 DROP — D:2239-2241 hunks are the presentation of the change (implicit; the start comment P:486-489 covers it).
REVERSE (Output): `type` REP; `filePath` REP; `content` T:3083 DROP (same); `structuredPatch` REP; `originalFile` T:3099 DROP D:2254; `gitDiff.*` T:3100-3111 DROP D:2255; `userModified` REP.

---

## 4. Edit — `FileEditInput` T:584-601, `FileEditOutput` T:3025-3072 → `AgentEdit` P:526-564

FORWARD: `start.path` ← `input.file_path`; `success.path` ← `output.filePath` T:3029; `success.patch[]` ← `output.structuredPatch[]` T:3045-3051; `success.user_modified` ← `output.userModified` T:3055; failure ← `is_error`. All static.

REVERSE (Input): `file_path` REP; `old_string`/`new_string` T:592,596 DROP — P:541-544 "deliberately absent ... second, unresolved description"; `replace_all` T:600 DROP — D:2243 "its effect is the number of hunks".
REVERSE (Output): `filePath` REP; `oldString`/`newString` T:3033,3037 DROP (same); `originalFile` T:3041 DROP D:2254; `structuredPatch` REP; `userModified` REP; `replaceAll` T:3059 DROP D:2243; `gitDiff.*` T:3060-3071 DROP D:2255.

---

## 5. Grep — `GrepInput` T:640-701, `GrepOutput` T:3143-3154 → `AgentGrep` P:571-696

FORWARD
| arm field | source | static? |
|---|---|---|
| `*.query.pattern` | `input.pattern` T:644 | yes |
| `*.query.path` | `input.path` T:648 (optional) | yes |
| `*.query.glob` | `input.glob` T:652 | yes |
| `success.matches` arm | `output.mode` T:3144 (optional!) — fallback to `input.output_mode`, default "files_with_matches" | yes, 2 lookups |
| `content.content` | `output.content` T:3147 | yes |
| `content.extent.all.lines_returned` | `output.numLines` T:3148 | yes |
| `content.extent.partial.lines_returned/lines_omitted` | `numLines`, `totalLines - numLines` T:3150 | derived |
| `files.paths[]` | `output.filenames` T:3146 | yes |
| `files.extent.all/partial.files_returned/omitted` | `numFiles`, `totalFiles - numFiles` T:3145,3149 | derived |
| `count.matches` | `output.numMatches` T:3151 | yes |
| `failure` | `is_error` | yes |

NO-PRODUCER / AMBIGUITY: `totalLines`/`totalFiles` are OPTIONAL (T:3149-3150). When absent the shim cannot choose between `all` and `partial` honestly. `appliedLimit` (T:3152) dropped per D:2181 but is the only other truncation signal; with `totalLines` absent and `appliedLimit` present the producer must guess. Flag: extent arm has an undetermined case.

REVERSE (Input)
| field | status | reason |
|---|---|---|
| `pattern` T:644 | REP | `AgentGrepQuery.pattern` |
| `path` T:648 | REP | `.path` |
| `glob` T:652 | REP | `.glob` |
| `output_mode` T:656 | REP | `matches` arm |
| `-B` `-A` `-C` `context` T:660-672 | DROP (implicit) | P:630-633 "including whatever ... context decoration the call requested" — rendered into `content` |
| `-n` T:676 | DROP (implicit) | same |
| `-i` T:680 | UNS | case-insensitivity is not visible in rendered output; no comment |
| `-o` T:684 | DROP (implicit) | rendered |
| `type` T:688 | UNS | a file-type filter semantically identical to `glob` yet not carried; no comment |
| `head_limit` T:692 | DROP | D:2181-2183 |
| `offset` T:696 | DROP | D:2181-2183 (`appliedOffset`) — but an offset>0 makes `partial.lines_omitted` = "left out" ambiguous (omitted before vs after) |
| `multiline` T:700 | UNS | no home, no comment |

REVERSE (Output): `mode` REP; `numFiles` REP; `filenames` REP; `content` REP; `numLines` REP; `numMatches` REP; `totalFiles`/`totalLines` REP (derived); `appliedLimit`/`appliedOffset` T:3152-3153 DROP D:2181.

---

## 6. Glob — `GlobInput` T:630-639, `GlobOutput` T:3117-3142 → `AgentGlob` P:698-776

FORWARD: `query.pattern` ← `input.pattern`; `query.path` ← `input.path`; `paths[]` ← `output.filenames` T:3129; `extent.all.files_returned` ← `numFiles` when `!truncated` T:3125,3133; `extent.partial.files_returned` ← `numFiles`; `partial.omitted.exact.files_omitted` ← `totalMatches - numFiles` when `countIsComplete` T:3137,3141; `at_least.files_omitted_at_least` ← same when `countIsComplete == false`. All static; omitted figure derived.

NO-PRODUCER: `totalMatches`/`countIsComplete` are OPTIONAL ("Absent on results persisted by CLI versions predating this field"). With `truncated == true` and no `totalMatches`, neither `exact` nor `at_least` can be filled; the partial arm's `omitted` oneof would be UNSET. Flag.

REVERSE: `pattern` REP; `path` REP; `durationMs` T:3121 DROP D:2183; `numFiles` REP; `filenames` REP; `truncated` REP; `totalMatches` REP; `countIsComplete` REP.

---

## 7. Agent (Task) — `AgentInput` T:484-521, `AgentOutput` T:99-199 + `task_progress` S:4478 → `AgentSubagent` P:962-1169

FORWARD
| arm field | source | static? |
|---|---|---|
| `start.created_agent_id.value` | `output.agentId` (completed T:101, async T:152) — NOT available at issue. At `start` time the only candidate is `task_progress.task_id` (S:4480) or `task_started.task_id`, matched by `tool_use_id`. P:982-990 requires it ON START. | NON-STATIC / no start-time producer — flag |
| `*.prompt.description` | `input.description` T:488 | yes |
| `*.prompt.text` | `input.prompt` T:492 | yes |
| `*.prompt.subagent_type` | `input.subagent_type` T:496 | yes |
| `*.prompt.requested_name` | `input.name` T:508 | yes |
| `*.prompt.requested_model` | `input.model` T:500 | yes |
| `*.prompt.isolation.none/worktree/remote` | `input.isolation` T:520 | yes |
| `update.progress.duration_ms/tool_use_count/total_tokens` | `task_progress.usage.duration_ms/tool_uses/total_tokens` S:4487-4491 | yes |
| `update.note.text` | `task_progress.summary` S:4494 | yes |
| `update.activity.tool_name` | `task_progress.last_tool_name` S:4493 | yes |
| `success.report.prose.markdown` | `output.content[].text` (T:103-107) — ARRAY of text blocks; join | yes (bounded) |
| `success.totals.duration_ms` | `output.totalDurationMs` T:112 | yes |
| `success.totals.usage` (TokenUsage) | `output.usage.*` T:114-130 | yes |
| `success.totals.tool_use_count` | `output.totalToolUseCount` T:111 | yes |
| `success.totals.tool_stats.*` | `output.toolStats.readCount/searchCount/bashCount/editFileCount/linesAdded/linesRemoved/otherToolCount` T:131-139 | yes |
| `success.models_used[]` | `output.modelsUsed` T:110, fallback `resolvedModel` T:109 | yes |
| `success.worktree.path/branch` | `output.worktreePath/worktreeBranch` T:142-143 | yes |
| `failure` | `is_error` | yes |

Async/remote branches: `status:"async_launched"` (T:146-172) carries `agentId, description, resolvedModel, modelsUsed, prompt, outputFile, canReadOutputFile`; `status:"remote_launched"` (T:174-198) carries `taskId, sessionUrl, description, prompt, outputFile`. The completed report for a detached spawn never arrives on `tool_use_result`; D:1748-1750 says the shim maps both onto one lifecycle — the DETACHED success arm's source is `task_updated`/`task_notification`, which carry NO report/totals (S:4520-4535: `status, description, end_time, total_paused_ms, error, is_backgrounded`). NO-PRODUCER: `AgentSubagentSuccess.report/totals/tool_stats/models_used` for a detached spawn.

REVERSE (Input): `description` REP; `prompt` REP; `subagent_type` REP; `model` REP; `run_in_background` T:504 REP (indirect, `DetachedCauseRequested`); `name` REP; `team_name` T:512 DROP — T says "Deprecated; ignored"; `mode` T:516 DROP — T "Deprecated; ignored"; `isolation` REP.

REVERSE (Output, completed): `agentId` REP; `agentType` T:102 UNS (actual type vs requested — no field; `prompt.subagent_type` is the REQUEST); `content[].text` REP; `content[].citations` T:106 UNS; `resolvedModel` REP (folded); `modelsUsed` REP; `totalToolUseCount` REP; `totalDurationMs` REP; `totalTokens` T:113 DROP (derivable from usage; implicit); `usage.*` REP (TokenUsage; `service_tier`, `inference_geo`, `speed`, `iterations`, `cache_creation.ephemeral_*` T:121-129 depend on TokenUsage's own fields — out of this partition); `toolStats.frameCount` T:138 UNS; `status` REP (arm); `prompt` REP; `worktreePath/Branch` REP.
REVERSE (Output, async): `isAsync` DROP (implicit); `outputFile` T:159 UNS — D:2058 names the sidecar spool; the proto carries no path; `canReadOutputFile` T:171 DROP (model-facing; implicit).
REVERSE (Output, remote): `taskId` REP (`DetachedWorkId`); `sessionUrl` T:183 UNS — `AgentSubagentIsolationRemote` is EMPTY (P:1047); the workflow's remote placement carries `session_url` but the subagent's does not. No D reason.

---

## 8. Skill — NO `SkillInput`/`SkillOutput` in T (grep confirms; D:1522-1525 records it) → `AgentSkillUse` P:1190-1255

Sources are transcript-observed (D:1527-1536): input `{skill, args}`; `toolUseResult {commandName, success}`; document on a later `user` record with `isMeta:true, sourceToolUseID`; allowances on an `attachment {type:"command_permissions", allowedTools}`.

FORWARD: `start.skill.name` ← `input.skill`; `start.args` ← `input.args`; `success.skill.name` ← `input.skill`; `success.document.markdown` ← the `sourceToolUseID`-keyed meta record's text block (static by id per D:2286-2290 — but that record is a SEPARATE message; the shim must hold the unit open and key by `sourceToolUseID`, which is a keyed lookup, static); `success.allowed_tools.tool_names[]` ← `attachment.allowedTools` — the attachment carries NO `sourceToolUseID` per D:1535; correlation is positional (next attachment after the meta record). NON-STATIC — flag. `failure` ← `toolUseResult.success == false` / `is_error`.

REVERSE: `skill` REP; `args` REP; `commandName` DROP (D:1543-1545 restates the name); `success` REP (arm). UNTYPED — every field here rests on observation, not a declared type; note as the one tool with no type-surface guarantee.

---

## 9. SendMessage — NO `SendMessageInput`/`Output` in T (grep confirms; D does not record this absence explicitly — note it) → `AgentSendMessage` P:1268-1343

Observed shape (D:1480-1488): input `{to, summary?, message}`; result prose `message` + `pin{id,name,ref}` + optional `resumedAgentId`.
FORWARD: `start.addressed_to` ← `input.to`; `start.summary.text` ← `input.summary`; `start.body.text` ← `input.message`; `success.recipient_agent_id.value` ← `pin.id`; `delivery.queued_to_live` vs `resumed_recipient` ← presence of `resumedAgentId`. All static IF the observed shape holds — no type guards it.
REVERSE: `message` (prose) DROP D:1497-1500; `pin.name` DROP D:1500-1501; `pin.ref` DROP D:1501-1502; `resumedAgentId` REP (arm selector; the id value itself is dropped without comment — if it differs from `pin.id` that is lost).

---

## 10. Task tracker — `TaskCreateInput` T:2483-2502 / `TaskCreateOutput` T:3602-3607, `TaskUpdateInput` T:2509-2548 / `TaskUpdateOutput` T:3618-3627 → `AgentTaskAct` P:132-216

FORWARD (create): `task.value` ← `output.task.id` T:3604; `act.created` ← tool name; `state.subject` ← `input.subject` T:2487; `state.description` ← `input.description` T:2491; `state.owner` UNSET (TaskCreateInput has no owner); `state.status.pending` ← constant (a created task is pending); `running.active_form` ← `input.activeForm` T:2495 (but status is pending at creation, so the field has NO slot at create time — it lands only on a later `changed` act whose status is running; the create-time `activeForm` is lost). All static.

FORWARD (update): `task.value` ← `input.taskId` T:2513; `act.changed`; `state.subject/description/owner` ← `input.subject/description/owner` T:2517,2521,2541 — ALL OPTIONAL on the input ("New subject"), and `TaskUpdateOutput` carries only `updatedFields: string[]` + `statusChange{from,to}` (T:3621-3626), NOT the resulting task. P:147-149 requires `state` "resolved by the producer" — the producer (tool types) does NOT resolve it. The shim must keep its own per-task map across turns and agents to fill `state` on a `changed` act. NON-STATIC (state accumulation across the whole session; a `changed` act for a task whose create was on a stream this shim did not see cannot be filled). FLAG — biggest finding in this partition.
`state.status` ← `input.status` / `output.statusChange.to` — values `pending|in_progress|completed|deleted` (T:2529).

STATUS ARM MISMATCH (finding): P:167-187 / D:2424-2428 took the six status arms from `SDKTaskUpdatedMessage.patch.status` (S:4530: `pending|running|completed|failed|killed|paused`). That type is the BACKGROUND-TASK state machine (shell/subagent/workflow tasks), NOT the task TRACKER's. The tracker's producer set (`TaskUpdateInput.status` T:2529, `TaskGetOutput.task.status` T:3613, `TaskListOutput.tasks[].status` T:3632) is `pending | in_progress | completed | deleted`. Consequences:
- `AgentTaskFailed`, `AgentTaskKilled`, `AgentTaskPaused` have NO tracker producer (no-producer proto fields).
- `status: "deleted"` (T:2529) has NO arm. A deleted task cannot be stated. UNSUPPORTED.
- `AgentTaskRunning` ← `in_progress` (name mismatch only).
- D:2429-2431 "stopping is a change whose resulting status is killed" — `TaskStop` (T:702) targets BACKGROUND tasks (`task_id` "background task ... agent ID"), not tracker tasks; `TaskStopOutput` T:3155-3172 has `task_type`. So TaskStop is not a tracker act at all (see §14).

REVERSE (TaskCreateInput): `subject` REP; `description` REP; `activeForm` REP (slot exists only on running); `metadata` T:2499 UNS (arbitrary; no comment).
REVERSE (TaskCreateOutput): `task.id` REP; `task.subject` REP.
REVERSE (TaskUpdateInput): `taskId` REP; `subject/description/activeForm/owner/status` REP (via accumulated state); `addBlocks`/`addBlockedBy` T:2533,2537 UNS — dependency edges have no home; `TaskGetOutput.blocks/blockedBy` and `TaskListOutput.blockedBy` (T:3614-3615,3634) confirm the tracker models them; `metadata` T:2545 UNS.
REVERSE (TaskUpdateOutput): `success` REP (arm vs failure); `taskId` REP; `updatedFields` DROP (implicit — `state` carries the result); `error` T:3622 UNS (failure arm is empty; the only error text is lost); `statusChange.from` DROP (implicit), `.to` REP.

---

## 11. AskUserQuestion — `AskUserQuestionInput` T:848-2424, `AskUserQuestionOutput` T:3396-3582 → `AgentQuestion` (question.proto)

FORWARD: `id.value` ← minted from `tool_use_id` (question.proto "Minted by the shim from the asking call"); `start.batch.questions[]` ← `input.questions[]` (1-4 tuple union, T:855): `.question.text` ← `question`, `.header` ← `header`, `choices.single_select|multi_select` ← `multiSelect` (T:3551 on output; present per question on input), `.options[].label.label/description/preview` ← `options[].label/description/preview` (T:877-889). `success.batch` ← `output.questions[]` (T:3400-3552, same shape). `success.outcome.answered.answers[]` ← `output.answers` map (T:3556) — keyed by question text: one lookup per question, STATIC; `chosen[]` ← split the comma-joined value on "," and match labels (D:1589-1594 "comma-joining is unrecoverable for any label containing a comma. The shim undoes both at the boundary") — SPLIT IS NON-STATIC AND LOSSY; flag. `free_text.text` ← `output.response` (T:3560) — but `response` is PER ASK not per question (one string), while `AgentQuestionSelection.free_text` is PER QUESTION: no producer decides which question it attaches to. NO-PRODUCER mapping for multi-question batches; flag. `note.text` ← `output.annotations[question].notes` (T:3566-3575), static. `unanswered` ← `output.afkTimeoutMs` presence (T:3580).

REVERSE (Input): `questions[].question/header/options[].label/description/preview/multiSelect` REP; `annotations` (T:2411-2420 on INPUT) UNS — an input-side annotations map exists (model-supplied?) with no home, no comment; `metadata.source` T:2421-2423 DROP (T: "Not displayed to user"; implicit).
REVERSE (Output): `questions[]` REP; `answers` REP; `response` REP (misattributed per above); `annotations[].preview` UNS (selected option's preview echo; no comment — `AgentQuestionChoice` carries only the label); `annotations[].notes` REP; `afkTimeoutMs` REP as arm, duration DROP D:1654-1656.

---

## 12. Workflow — `WorkflowInput` T:2564-2595, `WorkflowOutput` T:3735-3771 → `AgentWorkflowStart` (workflow.proto) via `DetachableWork.workflow`

FORWARD: `name` ← `output.workflowName` (T:3742, optional "absent only on transcripts written before this field existed") fallback `task_started.workflow_name` (S:4505); `script.path` ← `output.scriptPath` T:3755 (optional! P says "ALWAYS SET" — the type disagrees: optional, and `error` T:3769 "Set if syntax check failed" implies no persisted path on a failed parse). NO-PRODUCER in the syntax-error case; `resumed_from` ← `input.resumeFromRunId` T:2594; `placement.local.run_id` ← `output.runId` T:3747; `placement.remote.session_url` ← `output.sessionUrl` T:3763; placement arm ← `output.status` T:3736 / `taskType` T:3738; `notice.text` ← `output.warning` T:3767; `started_at` ← envelope. All static.

REVERSE (Input): `script` T:2568 DROP — workflow.proto "deliberately NOT carried on the wire"; `name` T:2572 REP (via output); `description`/`title` T:2576,2580 DROP — T "Ignored"; `args` T:2584 UNS — the run's parameters; no home, no comment; `scriptPath` T:2590 REP; `resumeFromRunId` REP.
REVERSE (Output): `status` REP; `taskId` REP (`DetachedWorkId`); `taskType` REP (arm); `workflowName` REP; `runId` REP; `summary` T:3751 — `AgentWorkflowSummary` exists in workflow.proto but is referenced from agent.proto's workflow frame (out of partition) — REP; `transcriptDir` T:3753 UNS; `scriptPath` REP; `sessionUrl` REP; `warning` REP; `error` T:3769 UNS — there is no workflow FAILURE arm in workflow.proto/detached_work.proto; a syntax-failed workflow has no honest shape (it never starts, so `AgentWorkflowStart` is wrong, and `tool_result.is_error` maps to nothing).

---

## 13. Thinking / Response / ToolResultContent
Not tool I/O; out of this partition's Input/Output types. `ToolResultContent` (P:1352-1366) is filled from the `tool_result` content block array (`text`, `image`, else `unsupported`) — static per block. Note the block union in the SDK also has `document` (PDF) blocks for REPL/Read — those land in `UnsupportedBlock`.

---

## 14. TOOLS WITH NO ARM — each lands (once the shim maps) in `AgentUnmodeled` unless given an arm; honesty judged against P:1367-1381 ("A recognizable built-in arriving here is a PRODUCER DEFECT")

All of these are declared built-ins in `ToolInputSchemas` (T:11-54). By the message's own comment, EVERY ONE is a producer defect if routed to `AgentUnmodeled`. The record acknowledges the owed ruling only for SendMessage/TaskCreate/TaskUpdate/TaskStop (D:2498-2499) and has since given arms to the first three. Nothing in D rules on the rest.

| tool | Input fields (T) | Output fields (T) | unmodeled honest? |
|---|---|---|---|
| TaskOutput | `task_id, block, timeout` T:554-567 | none typed | NO (built-in) |
| TaskStop | `task_id?, shell_id?` T:702-711 | `message, task_id, task_type, command?` T:3155-3172 | NO. Also note: this is the background-task stop, which D:2429 conflated with a tracker `killed` act |
| TaskGet | `taskId` T:2503 | `task{id,subject,description,status,blocks,blockedBy} \| null` T:3608-3617 | NO |
| TaskList | `{}` T:2549 | `tasks[]{id,subject,status,owner?,blockedBy}` T:3628-3636 | NO |
| TodoWrite | `todos[]{content,status,activeForm}` T:814-823 | `oldTodos[], newTodos[]` T:3309-3326 | NO — and it is the legacy task tracker; a Todo write is semantically N `AgentTaskAct`s with no ids |
| WebFetch | `url, prompt` T:824-833 | `bytes, code, codeText, result, durationMs, url, artifactRead{slug,ver}` T:3327-3356 | NO |
| WebSearch | `query, allowed_domains?, blocked_domains?` T:834-847 | `query, results[]({tool_use_id, content[]{title,url}} \| string), durationSeconds, searchCount?` T:3357-3395 | NO |
| NotebookEdit | `notebook_path, cell_id?, new_source, cell_type?, edit_mode?` T:727-748 | `new_source, old_source?, cell_id?, cell_type, language, edit_mode, error?, notebook_path, original_file, updated_file` T:3173-3214 | NO — it is an edit; `AgentEdit` could carry it only by fabricating hunks |
| Monitor | `description, timeout_ms, persistent, command?, ws{url,protocols}` T:2653-2677 | `taskId, timeoutMs, persistent?` T:3668-3681 | NO — AND it is detachable background work (`BackgroundTaskSummary.type` includes 'monitor', D:2044) yet `DetachableWork` (detached_work.proto) has no `monitor` arm: "a kind absent here cannot claim to be detached" — a Monitor can never be announced as detached work |
| ToolSearch | NOT DECLARED in T at all (grep: no match) | none | the one tool for which `AgentUnmodeled` is HONEST by the type surface — the producer holds no schema |
| EnterPlanMode | `{}` T:2482 | `message` T:3688 | NO |
| ExitPlanMode | `allowedPrompts?[]{tool,prompt}` (deprecated), `[k]: unknown` T:568-583 | `plan, isAgent, filePath?, hasTaskTool?, planWasEdited?, awaitingLeaderApproval?, requestId?` T:2998-3024 | NO — and `plan` is user-facing prose the permission gate draws |
| ListMcpResources | `server?` T:712 | `[]{uri,name,mimeType?,description?,server}` T:333-354 | NO (built-in, though MCP-adjacent) |
| RefreshMcpTools | `server?` T:718 | `[]{server,status,toolCount?,added?,removed?,error?}` T:355-383 | NO |
| ReadMcpResource | `server, uri` T:759 | `contents[]{uri,mimeType?,text?,blobSavedTo?}, error?` T:3238-3261 | NO |
| ReadMcpResourceDir | `server, uri` T:749 | `resources[]{uri,name,mimeType?}, error?` T:3215-3237 | NO |
| Mcp (generic) | `[k]: unknown` T:724 | `string \| {type,...}[] \| {...}` T:384-392 | YES — this is exactly the arm's stated purpose |
| ReportFindings | `level?, findings[]{file,line?,summary,short_summary?,failure_scenario,category?,verdict?,outcome?}` T:769-813 | `count, level?, findings[]` T:3262-3308 | NO |
| SendFeedback | `type, title, details, area?` T:2425-2442 | `success, message` T:3583-3586 | NO |
| ClaudeDesign | `operation, arguments{}` T:2443-2454 | `operation, content[]{}, isError?` T:3801-3807 | borderline — arguments are "server-validated" open shape; closest to the MCP case |
| Projects | `method, path?, content?, local_path?, present_to_user?, query?, n?` T:2455-2481 | 5-arm union T:421-483 | NO |
| REPL | `code, description?, timeout?` T:2550-2563 | `code, result{}, stdout, stderr, error?, registeredTools?, images[], documents[]` T:3694-3734 | NO — it is a Bash-shaped execution |
| CronCreate/Delete/List | T:2596-2620 | T:3772-3790 | NO |
| ScheduleWakeup | `delaySeconds?, reason?, prompt?, stop?` T:2621-2638 | `scheduledFor, clampedDelaySeconds, wasClamped, stopped?, cancelledWakeups?` T:3646-3667 | NO |
| RemoteTrigger | `action, trigger_id?, body?` T:2639-2651 | `status, json, summary?` T:3637-3641 | NO |
| ShowOnboardingRolePicker | `{}` T:2652 | `role?, dismissed?` T:3642-3645 | NO (though unlikely in this stack) |
| ProposeSkills | `proposals[1..3]{name,kind,target?,description,evidence?,skillMd}` T:2678-2828 | `proposalCount` T:3682-3687 | NO |
| Artifact | `action?, file_path?, favicon?, limit?, scope?, title?, description?, label?, url?, force?` T:2829-2870 | publish/list union T:393-420 | NO |
| PushNotification | `message, status` T:2871-2877 | `message, pushSent?, localSent?, disabledReason?, sentAt?` T:3791-3800 | NO |
| EnterWorktree | `name?, path?` T:2878-2887 | `worktreePath, worktreeBranch?, message` T:3587-3591 | NO — session-cwd change a consumer needs to know about |
| ExitWorktree | `action, discard_changes?` T:2888-2897 | `action, originalCwd, worktreePath, worktreeBranch?, tmuxSessionName?, discardedFiles?, discardedCommits?, message` T:3592-3601 | NO |

Count: 36 declared built-ins with no arm; 2 honestly unmodeled (Mcp, ToolSearch-as-undeclared), 1 borderline (ClaudeDesign), 33 would be producer defects by the arm's own rule.

---

## 15. CONSOLIDATED UNSUPPORTED (no home, no recorded reason)
1. `BashInput.description` T:546
2. `BashInput.dangerouslyDisableSandbox` T:554 / `BashOutput.dangerouslyDisableSandbox` T:2937
3. `BashOutput.persistedOutputPath` value (path itself) T:2953
4. `FileReadInput.pages` T:618
5. `FileReadOutput` `startLine` with offset>0 T:222 (range arm declined, but no honest arm remains)
6. `FileReadOutput` image arm (`base64,type,originalSize,dimensions.*`) T:236-268
7. `FileReadOutput` notebook arm (`cells`) T:270-284
8. `FileReadOutput` pdf arm T:286-300
9. `FileReadOutput` parts arm (`count,outputDir`) T:302-320
10. `FileReadOutput` file_unchanged arm + `source:"seeded"` T:322-332
11. `GrepInput.-i` T:680, `.type` T:688, `.multiline` T:700
12. `AgentOutput.agentType` T:102 (actual vs requested)
13. `AgentOutput.content[].citations` T:106
14. `AgentOutput.toolStats.frameCount` T:138
15. `AgentOutput.outputFile` T:159 (async)
16. `AgentOutput.sessionUrl` T:183 (remote) — `AgentSubagentIsolationRemote` is empty
17. `TaskCreateInput.metadata` T:2499, `TaskUpdateInput.metadata` T:2545
18. `TaskUpdateInput.addBlocks/addBlockedBy` T:2533,2537
19. `TaskUpdateInput.status:"deleted"` T:2529 — no arm
20. `TaskUpdateOutput.error` T:3622 — failure arm empty
21. `AskUserQuestionInput.annotations` T:2411
22. `AskUserQuestionOutput.annotations[].preview` T:3570
23. `WorkflowInput.args` T:2584
24. `WorkflowOutput.transcriptDir` T:3753, `.error` T:3769 (no workflow failure shape)
25. SendMessage `resumedAgentId` VALUE (only its presence is used)
26. Every Input/Output field of the 33 no-arm built-ins in §14.

## 16. PROTO FIELDS WITH NO PRODUCER (on the declared type surface)
1. `AgentSubagentStart.created_agent_id` — `agentId` arrives only on the RESULT (T:101,152); at start the only source is `task_started/task_progress.task_id` keyed by `tool_use_id`, which is a different record and (for awaited, non-background spawns) may never be emitted. P:982-990 demands it at start.
2. `AgentSubagentSuccess.report/totals/tool_stats/models_used` for a DETACHED spawn — `task_updated.patch` (S:4520-4535) carries none of them.
3. `AgentTaskFailed`, `AgentTaskKilled`, `AgentTaskPaused` — tracker status set is `pending|in_progress|completed|deleted` (T:2529,3613,3632); arms were lifted from the background-task type (S:4530).
4. `AgentTaskAct.state` on a `changed` act — `TaskUpdateOutput` returns `updatedFields`, not the task (T:3618-3627); requires shim-side accumulation.
5. `AgentTaskRunning.active_form` at create time — a created task is pending, so the create-time `activeForm` (T:2495) has no slot.
6. `AgentGrepContent.extent` / `AgentGrepFiles.extent` when `totalLines`/`totalFiles` absent (T:3149-3150 optional).
7. `AgentGlobPartial.omitted` when `totalMatches` absent (T:3137 optional).
8. `AgentWorkflowStart.script.path` ("ALWAYS SET") — `WorkflowOutput.scriptPath` is optional (T:3755), absent on syntax failure.
9. `AgentQuestionSelection.free_text` per question — `AskUserQuestionOutput.response` is per ask (T:3560).
10. `AgentReadHead` "cut on a line boundary" — `truncatedByTokenCap` (T:230) is a token cut, not a line cut.

## 17. NON-STATIC resolutions
1. Skill `allowed_tools` — the `command_permissions` attachment carries no `sourceToolUseID` (D:1535); correlation is positional.
2. AskUserQuestion `chosen[]` — comma-split of `answers[q]` then label matching; lossy for labels containing commas (D:1593-1594).
3. TaskUpdate `state` — requires a session-wide per-task map (item 16.4).
4. Subagent `created_agent_id` at start — cross-record join on `tool_use_id` to a system message that may arrive later or not at all.
5. Bash `bytes_omitted` — arithmetic over measured inline byte length (bounded, but derived, not a lookup).
6. Bash `image.data` — crosses from `tool_use_result` to the `tool_result` content block (keyed, first image block; static but two-record).

## 18. Record-vs-type discrepancies worth a design-record entry
- D:2424-2428 cites the wrong SDK type for task status (background task vs tracker).
- D:2429 conflates TaskStop (background task) with a tracker act.
- workflow.proto "ALWAYS SET" vs `scriptPath?` optional.
- P:1047 `AgentSubagentIsolationRemote` empty while `remote_launched.sessionUrl` exists; the workflow's remote placement DOES carry `session_url` — inconsistent treatment of the same fact.
- `DetachableWork` omits `monitor` although D:2044 acknowledges the vendor's background set includes it.
- SendMessage's typelessness is not recorded in D the way Skill's is (D:1522).
