Files scanned: 6363 (subagent files included). Records: 641428. Unparseable lines: 0.

Distinct collapsed key paths (dynamic map keys folded to `{file}`/`{question}`): 1080.

### Record types

| type | n |
|---|---|
| assistant | 231003 |
| attachment | 176570 |
| user | 149402 |
| queue-operation | 20379 |
| last-prompt | 18887 |
| mode | 12490 |
| system | 10806 |
| permission-mode | 7926 |
| ai-title | 7853 |
| file-history-snapshot | 3339 |
| pr-link | 1157 |
| atis-latch | 642 |
| file-history-delta | 611 |
| agent-name | 93 |
| started | 86 |
| result | 86 |
| relocated | 40 |
| worktree-state | 40 |
| custom-title | 18 |

### system subtypes

| subtype | n |
|---|---|
| system/stop_hook_summary | 5259 |
| system/turn_duration | 4435 |
| system/away_summary | 416 |
| system/local_command | 411 |
| system/compact_boundary | 209 |
| system/scheduled_task_fire | 37 |
| system/api_error | 27 |
| system/agents_killed | 8 |
| system/informational | 4 |

### Record type `queue-operation` — envelope paths (6)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 20379 | dict |  |
| `content` | 12445 | str | /create-or-update-workspace create\n\nGenerate a single workspace using the expl… / also, as an aside, let's f |
| `operation` | 20379 | str | enqueue / dequeue |
| `sessionId` | 20379 | str | 15d8ca17-35f0-431b-a895-bb3d2c1aa39d / 1b19a0f8-cba8-43cc-9535-8ff145e56f40 |
| `timestamp` | 20379 | str | 2026-07-28T19:54:49.432Z / 2026-08-10T03:26:58.650Z |
| `type` | 20379 | str | queue-operation |

### Record type `attachment` — envelope paths (144)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 176570 | dict |  |
| `agentId` | 4033 | str | a02c8311ad10cec6a / a0657fd27414aea9a |
| `attachment` | 176570 | dict |  |
| `attachment.addedLines` | 6660 | list |  |
| `attachment.addedLines[]` | 149984 | str | CronCreate / CronDelete |
| `attachment.addedNames` | 4367 | list |  |
| `attachment.addedNames[]` | 138631 | str | CronCreate / CronDelete |
| `attachment.addedTypes` | 2293 | list |  |
| `attachment.addedTypes[]` | 11922 | str | claude / Explore |
| `attachment.allowedTools` | 502 | list |  |
| `attachment.allowedTools[]` | 2011 | str | Agent / Read(~/**) |
| `attachment.autoModeConsentFlow` | 122 | bool | false |
| `attachment.banner` | 62 | str | [Truncated: PARTIAL view — /Users/dodgecoates/.config/doom/.claude/worktrees/age… / [Truncated: PARTIAL view — |
| `attachment.bashFirst` | 83 | bool | true / false |
| `attachment.blockingError` | 366 | dict |  |
| `attachment.blockingError.blockingError` | 366 | str | ["$CLAUDE_PROJECT_DIR"/.claude/run-subproject-tests.sh]: daemon test suite faile… / ["$CLAUDE_PROJECT_DIR"/.cl |
| `attachment.blockingError.command` | 366 | str | "$CLAUDE_PROJECT_DIR"/.claude/run-subproject-tests.sh / /Users/dodgecoates/.gns/sockets/hook.py |
| `attachment.bypass` | 59 | bool | false |
| `attachment.command` | 148117 | str | bash "${CLAUDE_PLUGIN_ROOT}/scripts/init.sh" / ~/.gns/sockets/hook.py |
| `attachment.commandMode` | 1063 | str | prompt / task-notification |
| `attachment.content` | 156689 | dict/list/str | IMPORTANT - DO NOT IGNORE — GNS Plugin Active:\nThis environment has GNS install… / - agent-token-usage: Repor |
| `attachment.content.content` | 146 | str | ## Workflow\n\n- Ask clarifying questions first if the scope is unclear.\n- When… / # CLAUDE.md\n\nThis file p |
| `attachment.content.contentDiffersFromDisk` | 146 | bool | false |
| `attachment.content.file` | 343 | dict |  |
| `attachment.content.file.content` | 343 | str | import { describe, expect, it } from "vitest";\nimport { readFileSync } from "no… / /**\n * The shim's exclusi |
| `attachment.content.file.filePath` | 343 | str | /Users/dodgecoates/.config/doom/modules/app/agent-repl/agent-shim/claude/shim/te… / /Users/dodgecoates/.config |
| `attachment.content.file.numLines` | 343 | int | 247 / 118 |
| `attachment.content.file.startLine` | 343 | int | 1 |
| `attachment.content.file.totalLines` | 343 | int | 247 / 118 |
| `attachment.content.parent` | 63 | str | /Users/dodgecoates/workspace/ChessCom/explanation-engine/explanation-engine/CLAU… / /Users/dodgecoates/workspa |
| `attachment.content.path` | 146 | str | /Users/dodgecoates/.claude/CLAUDE.md / /Users/dodgecoates/.config/doom/.claude/worktrees/agent-a2f4f5fb2c881d9 |
| `attachment.content.type` | 489 | str | Project / text |
| `attachment.content[]` | 1922 | dict |  |
| `attachment.content[].activeForm` | 1766 | str | Verifying daemon supersede coverage / Fixing create→id correlation |
| `attachment.content[].blockedBy` | 1922 | list |  |
| `attachment.content[].blockedBy[]` | 150 | str | 13 / 1 |
| `attachment.content[].blocks` | 1922 | list |  |
| `attachment.content[].blocks[]` | 150 | str | 14 / 3 |
| `attachment.content[].description` | 1922 | str | Read daemon/internal/server/supersede.go and its call sites; confirm every path … / CommandAck (or equivalent) |
| `attachment.content[].id` | 1922 | str | 1 / 2 |
| `attachment.content[].metadata` | 66 | dict |  |
| `attachment.content[].metadata.closure_note` | 34 | str | Go services e2e suite fully green at round 6 (every top-level test passes; only … |
| `attachment.content[].metadata.final_act` | 29 | str | After everything is green and closed out: launch the corpus-editing webpage for … |
| `attachment.content[].metadata.runsearch_timeout_fallback` | 37 | str | User directive: if the RunSearch timeout trace / mock-recording fix is not fruit… |
| `attachment.content[].status` | 1922 | str | in_progress / pending |
| `attachment.content[].subject` | 1922 | str | Verify daemon stand-down covers all supersede paths / Fix create→id correlation to use the ack instead of cwd  |
| `attachment.data` | 9 | dict |  |
| `attachment.data.reason` | 9 | str | New message countermands the stop instruction by explicitly saying to carry on i… / Clarifies how task (4) sho |
| `attachment.data.verdict` | 9 | str | interrupt / wait |
| `attachment.deltaSummary` | 48 | NoneType/str | null / Agent stalled: no progress for 600s (stream watchdog did not recover) |
| `attachment.description` | 48 | str | Stage-2 error consolidation writer / Bifurcate dormant into hibernated/severed |
| `attachment.displayPath` | 897 | str | Users/dodgecoates/.claude/CLAUDE.md / Users/dodgecoates/.claude/skills |
| `attachment.durationMs` | 148117 | int | 23 / 64 |
| `attachment.exitCode` | 148108 | int | 0 / 1 |
| `attachment.filename` | 1557 | str | /Users/dodgecoates/.config/doom/modules/app/agent-repl/frontend-client.el / /Users/dodgecoates/.config/doom/AG |
| `attachment.files` | 2426 | list |  |
| `attachment.files[]` | 6213 | dict |  |
| `attachment.files[].diagnostics` | 6213 | list |  |
| `attachment.files[].diagnostics[]` | 29126 | dict |  |
| `attachment.files[].diagnostics[].code` | 26853 | str | UnusedImport / default |
| `attachment.files[].diagnostics[].message` | 29126 | str | "errors" imported and not used / "path/filepath" imported and not used |
| `attachment.files[].diagnostics[].range` | 29126 | dict |  |
| `attachment.files[].diagnostics[].range.end` | 29126 | dict |  |
| `attachment.files[].diagnostics[].range.end.character` | 29126 | int | 9 / 16 |
| `attachment.files[].diagnostics[].range.end.line` | 29126 | int | 29 / 31 |
| `attachment.files[].diagnostics[].range.start` | 29126 | dict |  |
| `attachment.files[].diagnostics[].range.start.character` | 29126 | int | 1 / 22 |
| `attachment.files[].diagnostics[].range.start.line` | 29126 | int | 29 / 31 |
| `attachment.files[].diagnostics[].severity` | 29126 | str | Error / Info |
| `attachment.files[].diagnostics[].source` | 29126 | str | compiler / unusedfunc |
| `attachment.files[].uri` | 6213 | str | /Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/sessiondr… / /Users/dodgecoates/.config |
| `attachment.hookEvent` | 148491 | str | SessionStart / PreToolUse |
| `attachment.hookName` | 148491 | str | SessionStart:startup / PreToolUse:Read |
| `attachment.isInitial` | 6600 | bool | true / false |
| `attachment.isMeta` | 1 | bool | true |
| `attachment.isNew` | 2426 | bool | true |
| `attachment.isSubAgent` | 2 | bool | false |
| `attachment.itemCount` | 4916 | int | 0 / 3 |
| `attachment.names` | 4307 | list |  |
| `attachment.names[]` | 183422 | str | agent-token-usage / analyze-position |
| `attachment.needsAuthMcpServers` | 2274 | list |  |
| `attachment.needsAuthMcpServers[]` | 5484 | str | claude.ai Google Calendar / claude.ai Adobe for creativity |
| `attachment.newDate` | 140 | str | 2026-08-02 / 2026-07-26 |
| `attachment.origin` | 358 | dict |  |
| `attachment.origin.body` | 1 | str | Relaying a reply for a peer that asked for it (the requester "general-purpose" w… |
| `attachment.origin.from` | 1 | str | fork |
| `attachment.origin.kind` | 358 | str | human / peer |
| `attachment.origin.name` | 1 | str | fork |
| `attachment.origin.senderTaskId` | 1 | str | ab08147d85455fd29 |
| `attachment.outputFilePath` | 48 | str | /private/tmp/claude-501/-Users-dodgecoates--config-doom/648d0255-7e1c-462e-b3eb-… / /private/tmp/claude-501/-U |
| `attachment.path` | 146 | str | /Users/dodgecoates/.claude/CLAUDE.md / /Users/dodgecoates/.config/doom/.claude/worktrees/agent-a2f4f5fb2c881d9 |
| `attachment.pendingMcpServers` | 2363 | list |  |
| `attachment.pendingMcpServers[]` | 517 | str | claude.ai Gmail / claude.ai Google Drive |
| `attachment.planExists` | 3 | bool | false |
| `attachment.planFilePath` | 3 | str | /Users/dodgecoates/.claude/plans/jazzy-puzzling-hare.md / /Users/dodgecoates/.claude/plans/reply-tidy-chipmunk |
| `attachment.prompt` | 1063 | list/str | also, as an aside, let's fix two other things about workspace restoration: the "… / also, update the AGENTS.md |
| `attachment.prompt[]` | 8 | dict |  |
| `attachment.prompt[].text` | 8 | str | please immediately fix the turn_active bug as prescribed and reload everything. … / and cancel the adverserial |
| `attachment.prompt[].type` | 8 | str | text |
| `attachment.readdedNames` | 4367 | list |  |
| `attachment.readdedNames[]` | 569 | str | mcp__claude_ai_Gmail__apply_sensitive_message_label / mcp__claude_ai_Gmail__apply_sensitive_thread_label |
| `attachment.reminderType` | 2 | str | full |
| `attachment.removedNames` | 4367 | list |  |
| `attachment.removedNames[]` | 1430 | str | mcp__claude_ai_Google_Calendar__authenticate / mcp__claude_ai_Google_Calendar__complete_authentication |
| `attachment.removedTypes` | 2293 | list |  |
| `attachment.removedTypes[]` | 27 | str | claude-code-guide |
| `attachment.showConcurrencyNote` | 2293 | bool | true |
| `attachment.skillCount` | 4307 | int | 36 / 41 |
| `attachment.skillDir` | 12 | str | /Users/dodgecoates/.claude/skills / /Users/dodgecoates/.config/doom-worktrees/uds-ack-track-before-send-uko/.c |
| `attachment.skillNames` | 12 | list |  |
| `attachment.skillNames[]` | 275 | str | workspace-close / build-status |
| `attachment.skills` | 74 | list |  |
| `attachment.skills[]` | 317 | dict |  |
| `attachment.skills[].content` | 317 | str | Base directory for this skill: /Users/dodgecoates/.claude/skills/structural-inva… / Base directory for this sk |
| `attachment.skills[].name` | 317 | str | structural-invariants / debug-emacs-agent-repl |
| `attachment.skills[].path` | 317 | str | userSettings:structural-invariants / userSettings:debug-emacs-agent-repl |
| `attachment.snippet` | 825 | str | 1	;;; frontend-client.el --- HTTP session client for the claude-repld daemon -*-… / 1	# Agents \n2	\n3	Always  |
| `attachment.source_uuid` | 28 | str | ebe707f6-d286-4c8c-b5c7-a1e9fe674f60 / f7c156e3-395a-421d-a37b-65bcc209e48d |
| `attachment.status` | 48 | str | running / failed |
| `attachment.stderr` | 148108 | str |  / Failed to run: Hook "powershell -NoProfile -ExecutionPolicy Bypass -File ${CLAUD… |
| `attachment.stdout` | 148108 | str | IMPORTANT - DO NOT IGNORE — GNS Plugin Active:\nThis environment has GNS install… / {}\n |
| `attachment.steerOnly` | 83 | bool | true / false |
| `attachment.taskId` | 48 | str | a2f4f5fb2c881d95c / a3ca8252d1611361f |
| `attachment.taskType` | 48 | str | local_agent |
| `attachment.text` | 5948 | str | <total_tokens>14962655 tokens left</total_tokens> / <total_tokens>14961419 tokens left</total_tokens> |
| `attachment.timedOut` | 9 | bool | false |
| `attachment.timeoutMs` | 9 | int | 30000 |
| `attachment.timestamp` | 1063 | str | 2026-07-18T20:59:26.423Z / 2026-07-18T21:46:29.616Z |
| `attachment.toolUseID` | 148645 | str | 7483c4eb-2cf2-496b-9d74-9173b00aad5c / toolu_01Ab2ao5vuTfiRRjD2K5wyxT |
| `attachment.type` | 176570 | str | hook_success / deferred_tools_delta |
| `cwd` | 176570 | str | / / /private/tmp |
| `entrypoint` | 176570 | str | sdk-cli / cli |
| `gitBranch` | 176570 | str | HEAD / DWC/repo-first-authoring-uii |
| `isSidechain` | 176570 | bool | false / true |
| `parentUuid` | 176570 | NoneType/str | null / 80f2a8f2-c89a-4de2-a13c-25a93a9821e8 |
| `sessionId` | 176570 | str | 15d8ca17-35f0-431b-a895-bb3d2c1aa39d / 1b19a0f8-cba8-43cc-9535-8ff145e56f40 |
| `sessionKind` | 862 | str | bg |
| `session_id` | 85615 | str | 029627c4-6eb6-4049-a5bd-4450414ae5f6 / 52b90184-84bb-4d52-b452-a3436349646d |
| `slug` | 74422 | str | jazzy-puzzling-hare / snazzy-launching-gizmo |
| `timestamp` | 176570 | str | 2026-07-28T19:54:49.015Z / 2026-07-28T19:54:49.441Z |
| `type` | 176570 | str | attachment |
| `userType` | 176570 | str | external |
| `uuid` | 176570 | str | 634e087c-bf89-430f-a17f-8f1a871576b8 / 46ae4720-337d-45cb-b2bc-64048779bfa6 |
| `version` | 176570 | str | 2.1.220 / 2.1.226 |

### Record type `user` — envelope paths (41)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 149402 | dict |  |
| `agentId` | 90883 | str | a2ae1570083dc3acc / a02c8311ad10cec6a |
| `classifierMetaLines` | 738 | str | {"meta":{"gitStatus":{"staged":0,"modified":4,"untracked":1}}}\n / {"meta":{"gitStatus":{"clean":true}}}\n |
| `cwd` | 145416 | str | / / /private/tmp |
| `entrypoint` | 145385 | str | sdk-cli / cli |
| `gitBranch` | 145416 | str | HEAD / DWC/repo-first-authoring-uii |
| `interruptedByShutdown` | 150 | bool | true |
| `interruptedMessageId` | 149 | str | msg_011CdAGQhcs8MBY8e3ntpBsW / msg_011CdPVJ39ELWJuMWckf4pGR |
| `isCompactSummary` | 209 | bool | true |
| `isMeta` | 2060 | bool | true |
| `isSidechain` | 145416 | bool | false / true |
| `isVisibleInTranscriptOnly` | 209 | bool | true |
| `message` | 149402 | dict |  |
| `message.content` | 149402 | list/str | <command-message>create-or-update-workspace</command-message>\n<command-name>/cr… / what's the current default |
| `message.role` | 149402 | str | user |
| `origin` | 5368 | dict |  |
| `origin.body` | 36 | str | Commit df6e8eaf on DWC/store-publish-order — shim-store ingestAndFan now holds a… / Your previous run was cut  |
| `origin.from` | 36 | str | opus-medium / claude |
| `origin.kind` | 5368 | str | human / coordinator |
| `origin.name` | 36 | str | opus-medium / claude |
| `origin.senderTaskId` | 36 | str | ad61e0422d251f329 / a6aa8aaaf2f9e7858 |
| `parentUuid` | 149402 | NoneType/str | 634e087c-bf89-430f-a17f-8f1a871576b8 / bf80654a-a712-4c8b-ae2f-db82ed5c463f |
| `permissionMode` | 8420 | str | auto / default |
| `promptId` | 144751 | str | b7f02e48-9366-433c-a1aa-752b8c433ea1 / 1a0efa5d-b442-44c9-a5cf-a5f739653f00 |
| `promptSource` | 8420 | str | typed / sdk |
| `queuePriority` | 82 | str | later |
| `sessionId` | 149402 | str | 15d8ca17-35f0-431b-a895-bb3d2c1aa39d / 1b19a0f8-cba8-43cc-9535-8ff145e56f40 |
| `sessionKind` | 2082 | str | bg |
| `session_id` | 22018 | str | 029627c4-6eb6-4049-a5bd-4450414ae5f6 / 52b90184-84bb-4d52-b452-a3436349646d |
| `slug` | 88173 | str | jazzy-puzzling-hare / snazzy-launching-gizmo |
| `sourceToolAssistantUUID` | 131298 | str | 31f7a927-8089-48ab-ab33-e63354723ef5 / ac8a7f69-9d56-4024-bf67-c3f2411b2fbe |
| `sourceToolUseID` | 806 | str | toolu_01CxNUYo5Nvr7F4s1Ubi6Ews / toolu_01VaM71ymCYyyRRi79oYcF33 |
| `timestamp` | 149402 | str | 2026-07-28T19:54:49.441Z / 2026-07-28T19:54:52.958Z |
| `toolDenialKind` | 428 | str | user-rejected / automode-unavailable |
| `toolEndsTurn` | 9 | bool | true |
| `turnCompanion` | 39 | bool | true |
| `type` | 149402 | str | user |
| `userFeedback` | 10 | str | The user wants to clarify these questions.\n    This means they may have additio… |
| `userType` | 145416 | str | external |
| `uuid` | 149402 | str | bf80654a-a712-4c8b-ae2f-db82ed5c463f / 80f2a8f2-c89a-4de2-a13c-25a93a9821e8 |
| `version` | 145416 | str | 2.1.220 / 2.1.226 |

### Record type `last-prompt` — envelope paths (5)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 18887 | dict |  |
| `lastPrompt` | 18775 | str | /create-or-update-workspace create  Generate a single workspace using the explic… / what's the current default |
| `leafUuid` | 18887 | str | 8cb11e2b-1cbd-46c3-8d12-1b05a8b68dd7 / 8f442262-cb50-476d-8d7d-f3159d3ccfeb |
| `sessionId` | 18887 | str | 15d8ca17-35f0-431b-a895-bb3d2c1aa39d / 1b19a0f8-cba8-43cc-9535-8ff145e56f40 |
| `type` | 18887 | str | last-prompt |

### Record type `assistant` — envelope paths (67)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 231003 | dict |  |
| `agentId` | 139359 | str | a2ae1570083dc3acc / a02c8311ad10cec6a |
| `apiErrorStatus` | 96 | int | 404 / 429 |
| `attributionAgent` | 139323 | str | statusline-setup / Explore |
| `attributionPlugin` | 856 | str | gns-cowork |
| `attributionSkill` | 20802 | str | create-or-update-workspace / gns-cowork:gns-bootstrap |
| `cwd` | 226612 | str | / / /private/tmp |
| `effort` | 221509 | str | xhigh / medium |
| `entrypoint` | 226571 | str | sdk-cli / cli |
| `error` | 131 | str | server_error / model_not_found |
| `gitBranch` | 226612 | str | HEAD / DWC/repo-first-authoring-uii |
| `isAbortedMidStream` | 43 | bool | true |
| `isApiErrorMessage` | 446 | bool | true / false |
| `isSidechain` | 227098 | bool | false / true |
| `message` | 231003 | dict |  |
| `message.container` | 446 | NoneType | null |
| `message.content` | 231003 | list |  |
| `message.context_management` | 477 | NoneType/dict | null |
| `message.context_management.applied_edits` | 31 | list |  |
| `message.diagnostics` | 226162 | NoneType/dict | null |
| `message.diagnostics.cache_miss_reason` | 2634 | dict |  |
| `message.diagnostics.cache_miss_reason.cache_missed_input_tokens` | 1516 | int | 259191 / 311291 |
| `message.diagnostics.cache_miss_reason.type` | 2634 | str | unavailable / previous_message_not_found |
| `message.id` | 231003 | str | msg_011CdV3RCmcjkmfJNRP4vpiU / msg_011CdV3RSLSVemHjW72wdPgH |
| `message.model` | 231003 | str | claude-opus-5 / claude-opus-4-8 |
| `message.role` | 231003 | str | assistant |
| `message.stop_details` | 226611 | NoneType | null |
| `message.stop_reason` | 226612 | NoneType/str | tool_use / end_turn |
| `message.stop_sequence` | 226612 | NoneType/str | null /  |
| `message.type` | 231003 | str | message |
| `message.usage` | 226612 | dict |  |
| `message.usage.cache_creation` | 226612 | dict |  |
| `message.usage.cache_creation.ephemeral_1h_input_tokens` | 226612 | int | 18476 / 14011 |
| `message.usage.cache_creation.ephemeral_5m_input_tokens` | 226612 | int | 0 / 8728 |
| `message.usage.cache_creation_input_tokens` | 226612 | int | 18476 / 14011 |
| `message.usage.cache_read_input_tokens` | 226612 | int | 15185 / 33661 |
| `message.usage.inference_geo` | 226611 | NoneType/str | not_available / global |
| `message.usage.input_tokens` | 226612 | int | 2 / 1 |
| `message.usage.iterations` | 161074 | NoneType/list | null |
| `message.usage.iterations[]` | 160616 | dict |  |
| `message.usage.iterations[].cache_creation` | 160616 | dict |  |
| `message.usage.iterations[].cache_creation.ephemeral_1h_input_tokens` | 160616 | int | 18476 / 14011 |
| `message.usage.iterations[].cache_creation.ephemeral_5m_input_tokens` | 160616 | int | 0 / 8728 |
| `message.usage.iterations[].cache_creation_input_tokens` | 160616 | int | 18476 / 14011 |
| `message.usage.iterations[].cache_read_input_tokens` | 160616 | int | 15185 / 33661 |
| `message.usage.iterations[].input_tokens` | 160616 | int | 2 / 1 |
| `message.usage.iterations[].output_tokens` | 160616 | int | 94 / 1144 |
| `message.usage.iterations[].type` | 160616 | str | message |
| `message.usage.output_tokens` | 226612 | int | 94 / 1144 |
| `message.usage.output_tokens_details` | 41693 | NoneType/dict | null |
| `message.usage.output_tokens_details.thinking_tokens` | 41657 | int | 0 / 29 |
| `message.usage.server_tool_use` | 161075 | dict |  |
| `message.usage.server_tool_use.web_fetch_requests` | 161075 | int | 0 |
| `message.usage.server_tool_use.web_search_requests` | 161075 | int | 0 |
| `message.usage.service_tier` | 226612 | NoneType/str | standard / null |
| `message.usage.speed` | 161074 | NoneType/str | standard / null |
| `parentUuid` | 231003 | str | 8cb11e2b-1cbd-46c3-8d12-1b05a8b68dd7 / 8bad3a11-4287-4025-b80b-62fc83da94ad |
| `requestId` | 226262 | str | req_011CdV3RAbeLdrLddF6wZpC1 / req_011CdV3RR9kd9bDWmqEpxRF9 |
| `sessionId` | 231003 | str | 15d8ca17-35f0-431b-a895-bb3d2c1aa39d / 1b19a0f8-cba8-43cc-9535-8ff145e56f40 |
| `sessionKind` | 3223 | str | bg |
| `session_id` | 45769 | str | 029627c4-6eb6-4049-a5bd-4450414ae5f6 / 52b90184-84bb-4d52-b452-a3436349646d |
| `slug` | 135923 | str | jazzy-puzzling-hare / snazzy-launching-gizmo |
| `timestamp` | 231003 | str | 2026-07-28T19:54:51.524Z / 2026-07-28T19:54:52.595Z |
| `type` | 231003 | str | assistant |
| `userType` | 226612 | str | external |
| `uuid` | 231003 | str | 8bad3a11-4287-4025-b80b-62fc83da94ad / 31f7a927-8089-48ab-ab33-e63354723ef5 |
| `version` | 226612 | str | 2.1.220 / 2.1.226 |

### Record type `atis-latch` — envelope paths (4)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 642 | dict |  |
| `atis` | 642 | str |  |
| `sessionId` | 642 | str | 7ab938a1-f103-4a3c-8251-83ea025fecc3 / df3175f9-0a76-4751-bd8e-d35e45b15137 |
| `type` | 642 | str | atis-latch |

### Record type `mode` — envelope paths (4)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 12490 | dict |  |
| `mode` | 12490 | str | normal |
| `sessionId` | 12490 | str | 029627c4-6eb6-4049-a5bd-4450414ae5f6 / 096dbf91-dbc5-405c-bf52-bad3c0314af6 |
| `type` | 12490 | str | mode |

### Record type `permission-mode` — envelope paths (4)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 7926 | dict |  |
| `permissionMode` | 7926 | str | auto / bypassPermissions |
| `sessionId` | 7926 | str | 029627c4-6eb6-4049-a5bd-4450414ae5f6 / 096dbf91-dbc5-405c-bf52-bad3c0314af6 |
| `type` | 7926 | str | permission-mode |

### Record type `file-history-snapshot` — envelope paths (13)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 3339 | dict |  |
| `isSnapshotUpdate` | 3339 | bool | false |
| `messageId` | 3339 | str | d6d1fe90-9859-42e4-9362-175a11a18efd / 2546aa34-17de-4ee4-b2ce-2b16869a812f |
| `snapshot` | 3339 | dict |  |
| `snapshot.messageId` | 3339 | str | d6d1fe90-9859-42e4-9362-175a11a18efd / 2546aa34-17de-4ee4-b2ce-2b16869a812f |
| `snapshot.timestamp` | 3339 | str | 2026-08-06T16:24:02.982Z / 2026-08-06T16:25:05.886Z |
| `snapshot.trackedFileBackups` | 3339 | dict |  |
| `snapshot.trackedFileBackups.{file}` | 165911 | dict |  |
| `snapshot.trackedFileBackups.{file}.backupFileName` | 165911 | NoneType/str | 6f6339dfdc537381@v2 / 76095bdf9b42405d@v2 |
| `snapshot.trackedFileBackups.{file}.backupTime` | 165911 | str | 2026-08-16T14:43:49.139Z / 2026-08-17T03:44:57.110Z |
| `snapshot.trackedFileBackups.{file}.realParentDir` | 165676 | str | /Users/dodgecoates/workspace/ChessCom/explanation-engine-worktrees/cee-webapp/.g… / /Users/dodgecoates/workspa |
| `snapshot.trackedFileBackups.{file}.version` | 165911 | int | 2 / 10 |
| `type` | 3339 | str | file-history-snapshot |

### Record type `system` — envelope paths (70)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 10806 | dict |  |
| `agentId` | 1 | str | aa62db3d1bbedeb03 |
| `compactMetadata` | 209 | dict |  |
| `compactMetadata.cumulativeDroppedTokens` | 209 | int | 370263 / 1238304 |
| `compactMetadata.durationMs` | 209 | int | 170884 / 116605 |
| `compactMetadata.postTokens` | 209 | int | 7696 / 12687 |
| `compactMetadata.preCompactDiscoveredTools` | 110 | list |  |
| `compactMetadata.preCompactDiscoveredTools[]` | 425 | str | SendMessage / TaskStop |
| `compactMetadata.preTokens` | 209 | int | 377959 / 880728 |
| `compactMetadata.preservedMessages` | 209 | dict |  |
| `compactMetadata.preservedMessages.allUuids` | 209 | list |  |
| `compactMetadata.preservedMessages.allUuids[]` | 1084 | str | 0242a53b-d07f-4f3d-8dd2-aed74fe6b004 / 6f1137fd-8762-4a29-ad1f-54e28a6c621d |
| `compactMetadata.preservedMessages.anchorUuid` | 209 | str | 3d093489-e79f-481a-b496-d47775833d68 / 2f083f83-ba20-4c7f-b823-d579b377ba0a |
| `compactMetadata.preservedMessages.uuids` | 209 | list |  |
| `compactMetadata.preservedMessages.uuids[]` | 1031 | str | 0242a53b-d07f-4f3d-8dd2-aed74fe6b004 / 6f1137fd-8762-4a29-ad1f-54e28a6c621d |
| `compactMetadata.preservedSegment` | 209 | dict |  |
| `compactMetadata.preservedSegment.anchorUuid` | 209 | str | 3d093489-e79f-481a-b496-d47775833d68 / 2f083f83-ba20-4c7f-b823-d579b377ba0a |
| `compactMetadata.preservedSegment.headUuid` | 209 | str | 0242a53b-d07f-4f3d-8dd2-aed74fe6b004 / de64098b-3e6f-4eec-9789-64e9941c2217 |
| `compactMetadata.preservedSegment.tailUuid` | 209 | str | 1d8e95b7-be7f-47d7-85cb-ecba1f80842a / 419906fc-d27b-4e31-bf46-cb2724202c92 |
| `compactMetadata.trigger` | 209 | str | manual / auto |
| `content` | 1077 | str | <command-name>/model</command-name>\n            <command-message>model</command… / <local-command-stdout>Kept |
| `cronKind` | 13 | str | loop |
| `cwd` | 10806 | str | /Users/dodgecoates / /Users/dodgecoates/.claude |
| `durationMs` | 4435 | int | 12626 / 17961 |
| `entrypoint` | 10806 | str | cli / sdk-cli |
| `error` | 27 | dict |  |
| `error.connection` | 27 | NoneType/dict | null |
| `error.connection.code` | 26 | str | ECONNRESET / ENOTFOUND |
| `error.connection.isSSLError` | 26 | bool | false |
| `error.connection.message` | 26 | str | The socket connection was closed unexpectedly. For more information, pass `verbo… / getaddrinfo ENOTFOUND api. |
| `error.formatted` | 27 | str | Unable to connect to API (ECONNRESET) / 401 OAuth access token has been revoked. |
| `error.isNetworkDown` | 27 | bool | false / true |
| `error.message` | 27 | str | Connection error. / 401 {"type":"error","error":{"type":"authentication_error","message":"OAuth acce… |
| `error.rateLimits` | 27 | NoneType | null |
| `error.status` | 1 | int | 401 |
| `gitBranch` | 10806 | str | HEAD / DWC/cache-hit-observability |
| `hasOutput` | 5259 | bool | true |
| `hookAdditionalContext` | 5259 | list |  |
| `hookCount` | 5259 | int | 4 / 5 |
| `hookErrors` | 5259 | list |  |
| `hookErrors[]` | 204 | str | Failed to run: Hook "powershell -NoProfile -ExecutionPolicy Bypass -File ${CLAUD… / 📥 2 new Slack thread messa |
| `hookInfos` | 5259 | list |  |
| `hookInfos[]` | 17952 | dict |  |
| `hookInfos[].command` | 17952 | str | [ -z "$DOOM_SANDBOX" ] && ~/.gns/sockets/hook.py \|\| true / ~/.gns/sockets/hook.py |
| `hookInfos[].durationMs` | 16469 | int | 37 / 36 |
| `isMeta` | 5448 | bool | false |
| `isSidechain` | 10806 | bool | false / true |
| `level` | 5910 | str | suggestion / info |
| `logicalParentUuid` | 209 | str | 1d8e95b7-be7f-47d7-85cb-ecba1f80842a / 419906fc-d27b-4e31-bf46-cb2724202c92 |
| `maxRetries` | 27 | int | 10 |
| `messageCount` | 4435 | int | 25 / 41 |
| `parentUuid` | 10806 | NoneType/str | 4bb6f1d5-e309-4679-9a78-95417e2a8ab5 / 5234494a-8c08-4668-b61e-f5982ea6193f |
| `pendingBackgroundAgentCount` | 2007 | int | 2 / 1 |
| `pendingWorkflowCount` | 200 | int | 1 / 2 |
| `preventedContinuation` | 5259 | bool | false |
| `retryAttempt` | 27 | int | 1 / 2 |
| `retryInMs` | 27 | int | 547 / 584 |
| `sessionId` | 10806 | str | 029627c4-6eb6-4049-a5bd-4450414ae5f6 / 096dbf91-dbc5-405c-bf52-bad3c0314af6 |
| `sessionKind` | 139 | str | bg |
| `session_id` | 4344 | str | 029627c4-6eb6-4049-a5bd-4450414ae5f6 / 52b90184-84bb-4d52-b452-a3436349646d |
| `slug` | 7808 | str | jazzy-puzzling-hare / snazzy-launching-gizmo |
| `source` | 27 | str | request_retry |
| `stopReason` | 5259 | str |  |
| `subtype` | 10806 | str | stop_hook_summary / turn_duration |
| `timestamp` | 10806 | str | 2026-08-06T16:24:15.607Z / 2026-08-06T16:24:15.608Z |
| `toolUseID` | 5259 | str | 12e76d6c-563a-4fa3-abc0-e09ac1c601dd / 08f02d49-62b6-4cba-91d5-bbef7207c47f |
| `type` | 10806 | str | system |
| `userType` | 10806 | str | external |
| `uuid` | 10806 | str | 5234494a-8c08-4668-b61e-f5982ea6193f / cdf67999-e9be-4279-a0f4-17dabe9e658d |
| `version` | 10806 | str | 2.1.223 / 2.1.219 |

### Record type `ai-title` — envelope paths (4)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 7853 | dict |  |
| `aiTitle` | 7853 | str | Check default subagent model and effort configuration / Review agent configuration defaults |
| `sessionId` | 7853 | str | 029627c4-6eb6-4049-a5bd-4450414ae5f6 / 52b90184-84bb-4d52-b452-a3436349646d |
| `type` | 7853 | str | ai-title |

### Record type `file-history-delta` — envelope paths (11)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 611 | dict |  |
| `backup` | 611 | dict |  |
| `backup.backupFileName` | 611 | NoneType/str | null / 38aca12b18686dda@v1 |
| `backup.backupTime` | 611 | str | 2026-08-18T21:03:58.567Z / 2026-07-18T18:35:10.281Z |
| `backup.realParentDir` | 598 | str | /private/tmp/claude-501/-Users-dodgecoates/f933d4e9-45f2-4259-b293-3bc02fa17e91/… / /Users/dodgecoates/.config |
| `backup.version` | 611 | int | 1 |
| `messageId` | 611 | str | 0f7c358e-5805-4a1e-ac57-8b6e8f74cddc / fdd43ffc-5a7c-466d-83db-d9e1b605602d |
| `snapshotMessageId` | 611 | str | 09e64d52-15c2-4a2b-848a-14be345f16a7 / 6546d75d-a4bb-4228-8f9c-17ddec34442b |
| `timestamp` | 611 | str | 2026-08-18T21:03:58.567Z / 2026-07-18T18:35:10.281Z |
| `trackingPath` | 611 | str | /private/tmp/claude-501/-Users-dodgecoates/f933d4e9-45f2-4259-b293-3bc02fa17e91/… / modules/app/agent-repl/sta |
| `type` | 611 | str | file-history-delta |

### Record type `relocated` — envelope paths (4)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 40 | dict |  |
| `relocatedCwd` | 40 | str | /Users/dodgecoates/.config/doom/.claude/worktrees/single-session-id / /Users/dodgecoates/.config/doom |
| `sessionId` | 40 | str | 9b6a4f2d-6df9-4dd7-a30b-35dabff2ede3 |
| `type` | 40 | str | relocated |

### Record type `worktree-state` — envelope paths (12)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 40 | dict |  |
| `sessionId` | 40 | str | 9b6a4f2d-6df9-4dd7-a30b-35dabff2ede3 |
| `type` | 40 | str | worktree-state |
| `worktreeSession` | 40 | NoneType/dict | null |
| `worktreeSession.originalBranch` | 35 | str | master |
| `worktreeSession.originalCwd` | 35 | str | /Users/dodgecoates/.config/doom |
| `worktreeSession.originalHeadCommit` | 35 | str | 7a6ded8f71f5303e033e9e98cef333528af10bbc |
| `worktreeSession.preEnterOriginalCwd` | 35 | str | /Users/dodgecoates/.config/doom |
| `worktreeSession.sessionId` | 35 | str | 9b6a4f2d-6df9-4dd7-a30b-35dabff2ede3 |
| `worktreeSession.worktreeBranch` | 35 | str | worktree-single-session-id |
| `worktreeSession.worktreeName` | 35 | str | single-session-id |
| `worktreeSession.worktreePath` | 35 | str | /Users/dodgecoates/.config/doom/.claude/worktrees/single-session-id |

### Record type `started` — envelope paths (4)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 86 | dict |  |
| `agentId` | 86 | str | a1e9158618036abe6 / a6cc59e6a6f02f5df |
| `key` | 86 | str | v2:5d71a63b5848a8c1ada868464021e3c0b826d4ab33062f22930841cb1a8503ec / v2:4a4146636a6983b13f20bca03d29f4dafe518 |
| `type` | 86 | str | started |

### Record type `result` — envelope paths (5)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 86 | dict |  |
| `agentId` | 86 | str | a1e9158618036abe6 / a6cc59e6a6f02f5df |
| `key` | 86 | str | v2:5d71a63b5848a8c1ada868464021e3c0b826d4ab33062f22930841cb1a8503ec / v2:4a4146636a6983b13f20bca03d29f4dafe518 |
| `result` | 86 | str | **Branch** `fix/incremental-snapshot-apply` (worktree `/Users/dodgecoates/.confi… / **Branch** `fix/boot-order |
| `type` | 86 | str | result |

### Record type `pr-link` — envelope paths (7)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 1157 | dict |  |
| `prNumber` | 1157 | int | 7225 / 7234 |
| `prRepository` | 1157 | str | ChessCom/explanation-engine / ChessCom/definitions |
| `prUrl` | 1157 | str | https://github.com/ChessCom/explanation-engine/pull/7225 / https://github.com/ChessCom/explanation-engine/pull |
| `sessionId` | 1157 | str | fe97f7a9-f138-45ec-b3cb-e608fa2fceb2 / 0c236079-92f5-4077-8bfc-84e558093459 |
| `timestamp` | 1157 | str | 2026-08-02T21:11:11.060Z / 2026-08-02T21:11:25.462Z |
| `type` | 1157 | str | pr-link |

### Record type `custom-title` — envelope paths (4)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 18 | dict |  |
| `customTitle` | 18 | str | agent-repl-ab-iterm-20260804T1328 / agent-repl-ab-iterm-20260804T1352 |
| `sessionId` | 18 | str | 3548dee3-444c-4827-9b3d-4132f9293ef7 / 10bad6ca-7ceb-4658-b80a-f7171c4e97ee |
| `type` | 18 | str | custom-title |

### Record type `agent-name` — envelope paths (4)

| path | n | types | example |
|---|---|---|---|
| `(record)` | 93 | dict |  |
| `agentName` | 93 | str | agent-repl-ab-iterm-20260804T1328 / agent-repl-ab-iterm-20260804T1352 |
| `sessionId` | 93 | str | 3548dee3-444c-4827-9b3d-4132f9293ef7 / 10bad6ca-7ceb-4658-b80a-f7171c4e97ee |
| `type` | 93 | str | agent-name |

### `message.content[]` blocks: `user/text` (3)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 3449 | dict |  |
| `content[].text` | 3449 | str | Base directory for this skill: /Users/dodgecoates/.claude/skills/create-or-updat… / Base directory for this sk |
| `content[].type` | 3449 | str | text |

### `message.content[]` blocks: `assistant/text` (3)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 39378 | dict |  |
| `content[].text` | 39378 | str | I'll follow the `create` verb prescription. / Explicit-prompt mode. Building the dispatch and validating with  |
| `content[].type` | 39378 | str | text |

### `message.content[]` blocks: `assistant/tool_use/Read` (15)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 12514 | dict |  |
| `content[].caller` | 12514 | dict |  |
| `content[].caller.type` | 12514 | str | direct |
| `content[].id` | 12514 | str | toolu_01Ab2ao5vuTfiRRjD2K5wyxT / toolu_01MZhBW4FsdeVRLZhzs1oYvM |
| `content[].input` | 12514 | dict |  |
| `content[].input.__unparsedToolInput` | 18 | dict |  |
| `content[].input.__unparsedToolInput.len` | 18 | int | 122 / 118 |
| `content[].input.__unparsedToolInput.raw` | 18 | str | {"file_path": "/Users/dodgecoates/.config/doom/modules/app/agent-repl/frontend-c… / {"file_path": "/Users/dodg |
| `content[].input.file_offset` | 1 | str | 1 |
| `content[].input.file_path` | 12496 | str | /Users/dodgecoates/.claude/skills/create-or-update-workspace/create.md / /Users/dodgecoates/.zshrc |
| `content[].input.limit` | 6178 | int | 62 / 32 |
| `content[].input.offset` | 5940 | int | 270 / 14 |
| `content[].input.parameter` | 1 | str | 1 |
| `content[].name` | 12514 | str | Read |
| `content[].type` | 12514 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/Read` (11)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 12514 | dict |  |
| `content[].content` | 12514 | list/str | 1	# Workspace verb: `create`\n2	\n3	Generate practical git branch/worktree names… / 1	ZSH_THEME="robbyrussell" |
| `content[].content[]` | 45 | dict |  |
| `content[].content[].source` | 45 | dict |  |
| `content[].content[].source.data` | 45 | str | /9j/4AAQSkZJRgABAgAAAQABAAD/wAARCAQKBkADAREAAhEBAxEB/9sAQwADAgIDAgIDAwMDBAMDBAUI… / /9j/4AAQSkZJRgABAgAAAQABAA |
| `content[].content[].source.media_type` | 45 | str | image/jpeg / image/png |
| `content[].content[].source.type` | 45 | str | base64 |
| `content[].content[].type` | 45 | str | image |
| `content[].is_error` | 90 | bool | true |
| `content[].tool_use_id` | 12514 | str | toolu_01Ab2ao5vuTfiRRjD2K5wyxT / toolu_01MZhBW4FsdeVRLZhzs1oYvM |
| `content[].type` | 12514 | str | tool_result |

### `message.content[]` blocks: `assistant/thinking` (4)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 60144 | dict |  |
| `content[].signature` | 60144 | str | CAISsBUKhwEIEBgCKkCOcJQfCjhm4lQmXiHQLqvJQs6uk58OzoHBloWU6DRsUChTFLusN6lPYqDEtNux… / CAISpQ0KhwEIEBgCKkB5C02e1l |
| `content[].thinking` | 60144 | str |  / The user wants me to create a file called `probe-artifact.txt` with the single l… |
| `content[].type` | 60144 | str | thinking |

### `message.content[]` blocks: `assistant/tool_use/Bash` (17)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 94283 | dict |  |
| `content[].caller` | 94121 | dict |  |
| `content[].caller.type` | 94121 | str | direct |
| `content[].id` | 94283 | str | toolu_01BqVoJX79vHgAWsTcKvm8qv / toolu_01SVA3jzqmUvcJvptkxTWJwM |
| `content[].input` | 94283 | dict |  |
| `content[].input.cmd2` | 1 | str | x |
| `content[].input.command` | 94283 | str | git -C /Users/dodgecoates/workspace/ChessCom/explanation-engine rev-parse --abbr… / PROMPT=$(cat <<'PROMPTEOF' |
| `content[].input.command2` | 1 | str | true |
| `content[].input.command_type` | 2 | str |  |
| `content[].input.dangerouslyDisableSandbox` | 253 | bool | true / false |
| `content[].input.description` | 68746 | str | Resolve source workspace name, path, and HEAD / Dry-run the create dispatch |
| `content[].input.query2` | 2 | str | x / y |
| `content[].input.run_in_background` | 3006 | bool | true / false |
| `content[].input.timeout` | 9504 | int | 300000 / 600000 |
| `content[].input.timeout_ms` | 3 | str | 600000 |
| `content[].name` | 94283 | str | Bash |
| `content[].type` | 94283 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/Bash` (5)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 94279 | dict |  |
| `content[].content` | 94279 | str | fix/validate-game-for-analysis-active-gamepoint\n/Users/dodgecoates/workspace/Ch… / NOTICE: CLAUDE_WORKSPACE_P |
| `content[].is_error` | 94117 | bool | false / true |
| `content[].tool_use_id` | 94279 | str | toolu_01BqVoJX79vHgAWsTcKvm8qv / toolu_01SVA3jzqmUvcJvptkxTWJwM |
| `content[].type` | 94279 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/Skill` (9)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 791 | dict |  |
| `content[].caller` | 791 | dict |  |
| `content[].caller.type` | 791 | str | direct |
| `content[].id` | 791 | str | toolu_01CxNUYo5Nvr7F4s1Ubi6Ews / toolu_01VaM71ymCYyyRRi79oYcF33 |
| `content[].input` | 791 | dict |  |
| `content[].input.args` | 711 | str | merge / emacs livelocked at 100% CPU in tab-bar redisplay (ns_change_tab_bar_height osci… |
| `content[].input.skill` | 791 | str | gns-cowork:gns-bootstrap / create-or-update-workspace |
| `content[].name` | 791 | str | Skill |
| `content[].type` | 791 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/Skill` (5)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 791 | dict |  |
| `content[].content` | 791 | str | Launching skill: gns-cowork:gns-bootstrap / Launching skill: create-or-update-workspace |
| `content[].is_error` | 32 | bool | true |
| `content[].tool_use_id` | 791 | str | toolu_01CxNUYo5Nvr7F4s1Ubi6Ews / toolu_01VaM71ymCYyyRRi79oYcF33 |
| `content[].type` | 791 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/Edit` (11)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 16809 | dict |  |
| `content[].caller` | 16809 | dict |  |
| `content[].caller.type` | 16809 | str | direct |
| `content[].id` | 16809 | str | toolu_01QYe8Y23rCWARTCDeA79uJP / toolu_01TMfWJa2f9sgPMz1oRRZVWv |
| `content[].input` | 16809 | dict |  |
| `content[].input.file_path` | 16809 | str | /private/tmp/claude-501/-Users-dodgecoates/52b90184-84bb-4d52-b452-a3436349646d/… / /Users/dodgecoates/.claude |
| `content[].input.new_string` | 16809 | str | placeholder / #!/usr/bin/env bash\n# Claude Code status line script\n# Converted from the zsh … |
| `content[].input.old_string` | 16809 | str | placeholder / #!/usr/bin/env bash\n# Claude Code status line script\n\ninput=$(cat)\n\nmodel=$… |
| `content[].input.replace_all` | 16809 | bool | false / true |
| `content[].name` | 16809 | str | Edit |
| `content[].type` | 16809 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/Edit` (5)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 16809 | dict |  |
| `content[].content` | 16809 | str | <tool_use_error>No changes to make: old_string and new_string are exactly the sa… / The file /Users/dodgecoate |
| `content[].is_error` | 306 | bool | true |
| `content[].tool_use_id` | 16809 | str | toolu_01QYe8Y23rCWARTCDeA79uJP / toolu_01TMfWJa2f9sgPMz1oRRZVWv |
| `content[].type` | 16809 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/AskUserQuestion` (17)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 247 | dict |  |
| `content[].caller` | 247 | dict |  |
| `content[].caller.type` | 247 | str | direct |
| `content[].id` | 247 | str | toolu_01RDX9BYTo3wop1Eae9AypEv / toolu_01FVGbQv1E9P6bbHK8N6vNAP |
| `content[].input` | 247 | dict |  |
| `content[].input.questions` | 247 | list |  |
| `content[].input.questions[]` | 333 | dict |  |
| `content[].input.questions[].header` | 333 | str | Scope / Approach |
| `content[].input.questions[].multiSelect` | 332 | bool | false / true |
| `content[].input.questions[].options` | 333 | list |  |
| `content[].input.questions[].options[]` | 918 | dict |  |
| `content[].input.questions[].options[].description` | 918 | str | Defaults for .claude/agents/*.md frontmatter — model, tools, isolation, backgrou… / Defaults when building age |
| `content[].input.questions[].options[].label` | 918 | str | Claude Code subagents / Agent SDK / API agents |
| `content[].input.questions[].options[].preview` | 41 | str | 0      20k     50k    100k    100k+\n\|-------\|-------\|-------\|------->\ngreen  g… / 0      20k     50k    100k |
| `content[].input.questions[].question` | 333 | str | What does "agent configuration" refer to here? / Which tradeoff do you want for pinning subagents to Opus + me |
| `content[].name` | 247 | str | AskUserQuestion |
| `content[].type` | 247 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/AskUserQuestion` (5)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 245 | dict |  |
| `content[].content` | 245 | str | Your questions have been answered: "What does "agent configuration" refer to her… / The user answered: "Which  |
| `content[].is_error` | 51 | bool | true |
| `content[].tool_use_id` | 245 | str | toolu_01RDX9BYTo3wop1Eae9AypEv / toolu_01FVGbQv1E9P6bbHK8N6vNAP |
| `content[].type` | 245 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/ToolSearch` (9)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 508 | dict |  |
| `content[].caller` | 508 | dict |  |
| `content[].caller.type` | 508 | str | direct |
| `content[].id` | 508 | str | toolu_01Jncf63jSVp7otWMPQGzuLN / toolu_01KgdXKAcowY7zD3fNDevAqG |
| `content[].input` | 508 | dict |  |
| `content[].input.max_results` | 505 | int | 2 / 5 |
| `content[].input.query` | 508 | str | select:WebFetch,WebSearch / select:WebSearch,WebFetch |
| `content[].name` | 508 | str | ToolSearch |
| `content[].type` | 508 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/ToolSearch` (7)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 508 | dict |  |
| `content[].content` | 508 | list/str | No matching deferred tools found |
| `content[].content[]` | 601 | dict |  |
| `content[].content[].tool_name` | 601 | str | WebFetch / WebSearch |
| `content[].content[].type` | 601 | str | tool_reference |
| `content[].tool_use_id` | 508 | str | toolu_01Jncf63jSVp7otWMPQGzuLN / toolu_01KgdXKAcowY7zD3fNDevAqG |
| `content[].type` | 508 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/WebFetch` (9)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 98 | dict |  |
| `content[].caller` | 98 | dict |  |
| `content[].caller.type` | 98 | str | direct |
| `content[].id` | 98 | str | toolu_01X3WrcyvF7ofL5e4iEiGZy7 / toolu_011sapwubVBW1xfUp3wBTSTn |
| `content[].input` | 98 | dict |  |
| `content[].input.prompt` | 98 | str | What does the documentation say about the `model` field in subagent configuratio… / List all environment varia |
| `content[].input.url` | 98 | str | https://docs.claude.com/en/docs/claude-code/sub-agents / https://code.claude.com/docs/en/sub-agents |
| `content[].name` | 98 | str | WebFetch |
| `content[].type` | 98 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/WebFetch` (4)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 98 | dict |  |
| `content[].content` | 98 | str | REDIRECT DETECTED: The URL redirects to a different host.\n\nOriginal URL: https… / <persisted-output>\nOutput |
| `content[].tool_use_id` | 98 | str | toolu_01X3WrcyvF7ofL5e4iEiGZy7 / toolu_011sapwubVBW1xfUp3wBTSTn |
| `content[].type` | 98 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/Agent` (13)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 2016 | dict |  |
| `content[].caller` | 2016 | dict |  |
| `content[].caller.type` | 2016 | str | direct |
| `content[].id` | 2016 | str | toolu_01PMGdDCBScon1Q2TvGDbMMf / toolu_012EV7cR2ytkLuCyCjxDEoqa |
| `content[].input` | 2016 | dict |  |
| `content[].input.description` | 2016 | str | Configure statusline from PS1 / Research Claude Code JSONL schema |
| `content[].input.isolation` | 475 | str | worktree |
| `content[].input.model` | 821 | str | opus / sonnet |
| `content[].input.prompt` | 2016 | str | Configure my statusLine from my shell PS1 configuration / READ-ONLY research task. Do NOT modify or create any |
| `content[].input.run_in_background` | 454 | bool | false / true |
| `content[].input.subagent_type` | 1801 | str | statusline-setup / general-purpose |
| `content[].name` | 2016 | str | Agent |
| `content[].type` | 2016 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/Agent` (8)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 2016 | dict |  |
| `content[].content` | 2016 | list/str | Agent type 'opus-medium' not found. Available agents: claude, claude-code-guide,… / Agent type 'opus-high' not |
| `content[].content[]` | 2103 | dict |  |
| `content[].content[].text` | 2103 | str | [harness: subagent output matched instruction-shaped pattern(s): settings-json. … / Configured.\n\n- Source: z |
| `content[].content[].type` | 2103 | str | text |
| `content[].is_error` | 58 | bool | true |
| `content[].tool_use_id` | 2016 | str | toolu_01PMGdDCBScon1Q2TvGDbMMf / toolu_012EV7cR2ytkLuCyCjxDEoqa |
| `content[].type` | 2016 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/Write` (9)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 3017 | dict |  |
| `content[].caller` | 3017 | dict |  |
| `content[].caller.type` | 3017 | str | direct |
| `content[].id` | 3017 | str | toolu_016GfC8vRkCDjW6vWA5AfzVf / toolu_01Kf4wwyTGdYTracf3Db9jVx |
| `content[].input` | 3017 | dict |  |
| `content[].input.content` | 3017 | str | package main\n\nimport "fmt"\n\nfunc main() {\n	sum := 0\n	for i := 1; i <= 10; … / #!/usr/bin/env bash\n# tes |
| `content[].input.file_path` | 3017 | str | /private/tmp/claude-501/-Users-dodgecoates/f933d4e9-45f2-4259-b293-3bc02fa17e91/… / /Users/dodgecoates/workspa |
| `content[].name` | 3017 | str | Write |
| `content[].type` | 3017 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/Write` (5)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 3017 | dict |  |
| `content[].content` | 3017 | str | File created successfully at: /private/tmp/claude-501/-Users-dodgecoates/f933d4e… / The file /Users/dodgecoate |
| `content[].is_error` | 64 | bool | true |
| `content[].tool_use_id` | 3017 | str | toolu_016GfC8vRkCDjW6vWA5AfzVf / toolu_01Kf4wwyTGdYTracf3Db9jVx |
| `content[].type` | 3017 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/WebSearch` (10)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 56 | dict |  |
| `content[].caller` | 56 | dict |  |
| `content[].caller.type` | 56 | str | direct |
| `content[].id` | 56 | str | toolu_01K5j4WfAnbv1SAsKmA7wfSC / toolu_01RwLhFQLpS3ZLaADginbaaE |
| `content[].input` | 56 | dict |  |
| `content[].input.allowed_domains` | 3 | list |  |
| `content[].input.allowed_domains[]` | 9 | str | en.wikipedia.org / chess.com |
| `content[].input.query` | 56 | str | claude-agent-sdk-typescript SDKMessage union types.ts SDKSystemMessage subtype / Claude Code session transcrip |
| `content[].name` | 56 | str | WebSearch |
| `content[].type` | 56 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/WebSearch` (4)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 56 | dict |  |
| `content[].content` | 56 | str | Web search results for query: "claude-agent-sdk-typescript SDKMessage union type… / Web search results for que |
| `content[].tool_use_id` | 56 | str | toolu_01K5j4WfAnbv1SAsKmA7wfSC / toolu_01KuuVEDs2FM4T1i1MSqUkvo |
| `content[].type` | 56 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/TaskStop` (8)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 137 | dict |  |
| `content[].caller` | 137 | dict |  |
| `content[].caller.type` | 137 | str | direct |
| `content[].id` | 137 | str | toolu_01NtpPC7h8HiSf8zbN5Qv5pe / toolu_013CJcnvpSeNra6vt9g2FMFr |
| `content[].input` | 137 | dict |  |
| `content[].input.task_id` | 137 | str | bjmbsdtrj / a90275e96f96c2fa9 |
| `content[].name` | 137 | str | TaskStop |
| `content[].type` | 137 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/TaskStop` (5)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 137 | dict |  |
| `content[].content` | 137 | str | {"message":"Successfully stopped task: bjmbsdtrj (F=/private/tmp/claude-501/-Use… / {"message":"Successfully s |
| `content[].is_error` | 13 | bool | true |
| `content[].tool_use_id` | 137 | str | toolu_01NtpPC7h8HiSf8zbN5Qv5pe / toolu_013CJcnvpSeNra6vt9g2FMFr |
| `content[].type` | 137 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/SendMessage` (13)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 337 | dict |  |
| `content[].caller` | 337 | dict |  |
| `content[].caller.type` | 337 | str | direct |
| `content[].id` | 337 | str | toolu_01Xm18kcch19fLd2STWBwxfc / toolu_01T4Jb5abGWjoD4gi8mvdhUD |
| `content[].input` | 337 | dict |  |
| `content[].input.content` | 337 | str | Your final message delivered only the addendum — … / SCOPE UPDATE ONLY — do NOT proceed yet. You are s… |
| `content[].input.message` | 337 | str | Your final message delivered only the addendum — the main report (with §1B, §1C,… / SCOPE UPDATE ONLY — do NOT |
| `content[].input.recipient` | 337 | str | a80a1269d5bfca0d8 / a37ceb6a4d3f8ed2a |
| `content[].input.summary` | 335 | str | Write full report to a file / Scope update: add passthrough arms |
| `content[].input.to` | 337 | str | a80a1269d5bfca0d8 / a37ceb6a4d3f8ed2a |
| `content[].input.type` | 337 | str | message |
| `content[].name` | 337 | str | SendMessage |
| `content[].type` | 337 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/SendMessage` (7)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 337 | dict |  |
| `content[].content` | 337 | list |  |
| `content[].content[]` | 337 | dict |  |
| `content[].content[].text` | 337 | str | {"success":true,"message":"Agent \"a80a1269d5bfca0d8\" had no active task; resum… / {"success":true,"message": |
| `content[].content[].type` | 337 | str | text |
| `content[].tool_use_id` | 337 | str | toolu_01Xm18kcch19fLd2STWBwxfc / toolu_01T4Jb5abGWjoD4gi8mvdhUD |
| `content[].type` | 337 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/TaskCreate` (12)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 84 | dict |  |
| `content[].caller` | 84 | dict |  |
| `content[].caller.type` | 84 | str | direct |
| `content[].id` | 84 | str | toolu_01DYr4osVjtg31aSFDtU4zE5 / toolu_017hu4wysgw24hbK2b9o4uTT |
| `content[].input` | 84 | dict |  |
| `content[].input.activeForm` | 72 | str | Verifying daemon supersede coverage / Fixing create→id correlation |
| `content[].input.description` | 82 | str | Read daemon/internal/server/supersede.go and its call sites; confirm every path … / CommandAck (or equivalent) |
| `content[].input.prompt` | 1 | str | Investigate streamed MoveClassification: 3-depth hardcoded interval, continuatio… |
| `content[].input.subject` | 82 | str | Verify daemon stand-down covers all supersede paths / Fix create→id correlation to use the ack instead of cwd  |
| `content[].input.tasks` | 1 | str | [{"title":"gitenv package + tests (env-stripping git boundary)","status":"in_pro… |
| `content[].name` | 84 | str | TaskCreate |
| `content[].type` | 84 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/TaskCreate` (5)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 84 | dict |  |
| `content[].content` | 84 | str | Task #1 created successfully: Verify daemon stand-down covers all supersede path… / Task #2 created successful |
| `content[].is_error` | 2 | bool | true |
| `content[].tool_use_id` | 84 | str | toolu_01DYr4osVjtg31aSFDtU4zE5 / toolu_017hu4wysgw24hbK2b9o4uTT |
| `content[].type` | 84 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/TaskUpdate` (18)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 167 | dict |  |
| `content[].caller` | 167 | dict |  |
| `content[].caller.type` | 167 | str | direct |
| `content[].id` | 167 | str | toolu_01PyCWyagXAa7R88ZYGXhoVq / toolu_01EUZUPiMfvZEXLX9E7DHssw |
| `content[].input` | 167 | dict |  |
| `content[].input.activeForm` | 4 | str | Finishing CreateGameForAnalysis refactor via subagent / Reviewing, sweeping, pushing, and updating the PR |
| `content[].input.addBlockedBy` | 3 | list |  |
| `content[].input.addBlockedBy[]` | 4 | str | 13 / 1 |
| `content[].input.description` | 4 | str | Generalize supersedeResumeConflicts to also stand down same-cwd non-terminal rec… / Full verifier + close out  |
| `content[].input.metadata` | 3 | dict |  |
| `content[].input.metadata.closure_note` | 1 | str | Go services e2e suite fully green at round 6 (every top-level test passes; only … |
| `content[].input.metadata.final_act` | 1 | str | After everything is green and closed out: launch the corpus-editing webpage for … |
| `content[].input.metadata.runsearch_timeout_fallback` | 1 | str | User directive: if the RunSearch timeout trace / mock-recording fix is not fruit… |
| `content[].input.status` | 160 | str | in_progress / completed |
| `content[].input.subject` | 1 | str | Make create supersede same-cwd records and push terminal views |
| `content[].input.taskId` | 167 | str | 1 / 2 |
| `content[].name` | 167 | str | TaskUpdate |
| `content[].type` | 167 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/TaskUpdate` (4)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 167 | dict |  |
| `content[].content` | 167 | str | Updated task #1 status / Updated task #2 subject, description, status |
| `content[].tool_use_id` | 167 | str | toolu_01PyCWyagXAa7R88ZYGXhoVq / toolu_01EUZUPiMfvZEXLX9E7DHssw |
| `content[].type` | 167 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/TaskList` (7)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 4 | dict |  |
| `content[].caller` | 4 | dict |  |
| `content[].caller.type` | 4 | str | direct |
| `content[].id` | 4 | str | toolu_011U522RV1UXm8LugG6UUSAr / toolu_015a95ptLHNfvejzyk3PyDTh |
| `content[].input` | 4 | dict |  |
| `content[].name` | 4 | str | TaskList |
| `content[].type` | 4 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/TaskList` (4)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 4 | dict |  |
| `content[].content` | 4 | str | #1 [completed] Proto: add EventOrigin enum + field\n#2 [completed] Find and stam… / No tasks found |
| `content[].tool_use_id` | 4 | str | toolu_011U522RV1UXm8LugG6UUSAr / toolu_015a95ptLHNfvejzyk3PyDTh |
| `content[].type` | 4 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/Monitor` (17)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 192 | dict |  |
| `content[].caller` | 192 | dict |  |
| `content[].caller.type` | 192 | str | direct |
| `content[].id` | 192 | str | toolu_01D5CDCaDeqyEpVmYGofJ5y4 / toolu_01VU3CAdYat17j5WFwoYTSDt |
| `content[].input` | 192 | dict |  |
| `content[].input.bash_id` | 1 | str | b4mibviu6 |
| `content[].input.command` | 188 | str | until ! ps -p 90540 > /dev/null 2>&1; do sleep 5; done; echo "e2e run finished" / until ! ps -p 11762 > /dev/n |
| `content[].input.description` | 188 | str | wait for verbose e2e go test run to finish / wait for full e2e rerun after accounting fix |
| `content[].input.persistent` | 187 | bool | false / true |
| `content[].input.target` | 1 | str | buvq2gi28 |
| `content[].input.timeout` | 2 | str | 3600000 |
| `content[].input.timeout_ms` | 187 | int | 360000 / 300000 |
| `content[].input.timeout_seconds` | 1 | str | 360 |
| `content[].input.until` | 1 | str | The background agent ac7582312178a0584 (server package rewrite) has completed, O… |
| `content[].input.wait_for_completion` | 1 | str | true |
| `content[].name` | 192 | str | Monitor |
| `content[].type` | 192 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/Monitor` (5)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 192 | dict |  |
| `content[].content` | 192 | str | Monitor started (task bnjfsz0ev, timeout 360000ms). You will be notified on each… / Monitor started (task bqb9 |
| `content[].is_error` | 25 | bool | true |
| `content[].tool_use_id` | 192 | str | toolu_01D5CDCaDeqyEpVmYGofJ5y4 / toolu_01VU3CAdYat17j5WFwoYTSDt |
| `content[].type` | 192 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/EnterWorktree` (9)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 21 | dict |  |
| `content[].caller` | 21 | dict |  |
| `content[].caller.type` | 21 | str | direct |
| `content[].id` | 21 | str | toolu_01Kun2KLFyUnJekyFsom3D2R / toolu_01F9kL23n3NkEPV9DNLdzio6 |
| `content[].input` | 21 | dict |  |
| `content[].input.name` | 4 | str | single-session-id / proto-cursor-move |
| `content[].input.path` | 17 | str | /Users/dodgecoates/.config/doom/.claude/worktrees/prompt-identity / /Users/dodgecoates/.config/doom/.claude/wo |
| `content[].name` | 21 | str | EnterWorktree |
| `content[].type` | 21 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/EnterWorktree` (5)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 21 | dict |  |
| `content[].content` | 21 | str | Created worktree at /Users/dodgecoates/.config/doom/.claude/worktrees/single-ses… / Entered worktree at /Users |
| `content[].is_error` | 11 | bool | true |
| `content[].tool_use_id` | 21 | str | toolu_01Kun2KLFyUnJekyFsom3D2R / toolu_01F9kL23n3NkEPV9DNLdzio6 |
| `content[].type` | 21 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/ExitWorktree` (9)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 2 | dict |  |
| `content[].caller` | 2 | dict |  |
| `content[].caller.type` | 2 | str | direct |
| `content[].id` | 2 | str | toolu_01S6KMEg7LLSGZv5PmtvPTDz / toolu_01Xqgu6MDB3ShD4CpBzbDryt |
| `content[].input` | 2 | dict |  |
| `content[].input.action` | 2 | str | remove |
| `content[].input.discard_changes` | 1 | bool | true |
| `content[].name` | 2 | str | ExitWorktree |
| `content[].type` | 2 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/ExitWorktree` (5)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 2 | dict |  |
| `content[].content` | 2 | str | <tool_use_error>Worktree has 393 commits on worktree-single-session-id. Removing… / Exited and removed worktre |
| `content[].is_error` | 1 | bool | true |
| `content[].tool_use_id` | 2 | str | toolu_01S6KMEg7LLSGZv5PmtvPTDz / toolu_01Xqgu6MDB3ShD4CpBzbDryt |
| `content[].type` | 2 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/TaskOutput` (10)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 7 | dict |  |
| `content[].caller` | 7 | dict |  |
| `content[].caller.type` | 7 | str | direct |
| `content[].id` | 7 | str | toolu_01JCaNM2HAshCpBaV3JjaiQy / toolu_018NDeBmsBxBP1p2jJZ8mDvC |
| `content[].input` | 7 | dict |  |
| `content[].input.block` | 7 | bool | false / true |
| `content[].input.task_id` | 7 | str | a89164f0264ae8b3e / bpjf9y5y0 |
| `content[].input.timeout` | 7 | int | 5000 / 420000 |
| `content[].name` | 7 | str | TaskOutput |
| `content[].type` | 7 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/TaskOutput` (5)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 7 | dict |  |
| `content[].content` | 7 | str | <retrieval_status>success</retrieval_status>\n\n<task_id>a89164f0264ae8b3e</task… / <retrieval_status>success< |
| `content[].is_error` | 1 | bool | true |
| `content[].tool_use_id` | 7 | str | toolu_01JCaNM2HAshCpBaV3JjaiQy / toolu_018NDeBmsBxBP1p2jJZ8mDvC |
| `content[].type` | 7 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/ListAgents` (7)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 23 | dict |  |
| `content[].caller` | 23 | dict |  |
| `content[].caller.type` | 23 | str | direct |
| `content[].id` | 23 | str | toolu_0156H6J7BCoA2NAPSBhcSSHX / toolu_018uPQ2Cd5beR87Dt1sKmqMP |
| `content[].input` | 23 | dict |  |
| `content[].name` | 23 | str | ListAgents |
| `content[].type` | 23 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/ListAgents` (4)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 23 | dict |  |
| `content[].content` | 23 | str | Peer sessions (7):\n  slack-cee-ceac-integration-shj-11 [17730d]  ·  interactive… / Peer sessions (2):\n  doom |
| `content[].tool_use_id` | 23 | str | toolu_0156H6J7BCoA2NAPSBhcSSHX / toolu_018uPQ2Cd5beR87Dt1sKmqMP |
| `content[].type` | 23 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/ScheduleWakeup` (12)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 103 | dict |  |
| `content[].caller` | 103 | dict |  |
| `content[].caller.type` | 103 | str | direct |
| `content[].id` | 103 | str | toolu_016Bxd2k4s5wvXKjND9rVRyk / toolu_01Lam59WM4hMyTZpKXbdwzDx |
| `content[].input` | 103 | dict |  |
| `content[].input.delaySeconds` | 94 | int | 180 / 900 |
| `content[].input.noop` | 29 | bool | false |
| `content[].input.prompt` | 93 | str | Verify the selective-compaction-options webview recovered after the webapp dist … / /loop iteration — agent-re |
| `content[].input.reason` | 94 | str | Waiting out the wedged webview's ~90s self-reload cycle plus one full 45s lease … / 15-minute merge-queue prog |
| `content[].input.stop` | 9 | bool | true |
| `content[].name` | 103 | str | ScheduleWakeup |
| `content[].type` | 103 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/ScheduleWakeup` (5)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 103 | dict |  |
| `content[].content` | 103 | str | Next wakeup scheduled for 13:09:00 (in 238s). Nothing more to do this turn — the… / Loop stopped — cancelled 1 |
| `content[].is_error` | 2 | bool | true |
| `content[].tool_use_id` | 103 | str | toolu_016Bxd2k4s5wvXKjND9rVRyk / toolu_01Lam59WM4hMyTZpKXbdwzDx |
| `content[].type` | 103 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/Workflow` (8)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 83 | dict |  |
| `content[].caller` | 83 | dict |  |
| `content[].caller.type` | 83 | str | direct |
| `content[].id` | 83 | str | toolu_01DakEvV5StvKFoZXPAWC5DK / toolu_018Bgu7dF7ReYUfBzydfKNXj |
| `content[].input` | 83 | dict |  |
| `content[].input.script` | 83 | str | export const meta = {\n  name: 'bad-state-remediation-fanout',\n  description: '… / export const meta = {\n  n |
| `content[].name` | 83 | str | Workflow |
| `content[].type` | 83 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/Workflow` (5)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 83 | dict |  |
| `content[].content` | 83 | str | Workflow launched in background. Task ID: w6n5yorg1\nSummary: Parallel opus low-… / Workflow launched in backg |
| `content[].is_error` | 83 | bool | false |
| `content[].tool_use_id` | 83 | str | toolu_01DakEvV5StvKFoZXPAWC5DK / toolu_018Bgu7dF7ReYUfBzydfKNXj |
| `content[].type` | 83 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/StructuredOutput` (9)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 9 | dict |  |
| `content[].caller` | 9 | dict |  |
| `content[].caller.type` | 9 | str | direct |
| `content[].id` | 9 | str | toolu_01Sm4eR1uhyjfqGE3DmZ9Xyb / toolu_01GirF2xDgft5f5LYo2rPuRM |
| `content[].input` | 9 | dict |  |
| `content[].input.reason` | 9 | str | New message countermands the stop instruction by explicitly saying to carry on i… / Clarifies how task (4) sho |
| `content[].input.verdict` | 9 | str | interrupt / wait |
| `content[].name` | 9 | str | StructuredOutput |
| `content[].type` | 9 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/StructuredOutput` (4)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 9 | dict |  |
| `content[].content` | 9 | str | Structured output provided successfully |
| `content[].tool_use_id` | 9 | str | toolu_01Sm4eR1uhyjfqGE3DmZ9Xyb / toolu_01GirF2xDgft5f5LYo2rPuRM |
| `content[].type` | 9 | str | tool_result |

### `message.content[]` blocks: `assistant/tool_use/BashOutput` (8)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 5 | dict |  |
| `content[].caller` | 5 | dict |  |
| `content[].caller.type` | 5 | str | direct |
| `content[].id` | 5 | str | toolu_01CKNxP7re5eFa4FTz5TCeD2 / toolu_01MxjF1xXBTrsk1tXYjxDWEo |
| `content[].input` | 5 | dict |  |
| `content[].input.bash_id` | 5 | str | 22a9fe |
| `content[].name` | 5 | str | BashOutput |
| `content[].type` | 5 | str | tool_use |

### `message.content[]` blocks: `user/tool_result/BashOutput` (4)

| path | n | types | example |
|---|---|---|---|
| `content[]` | 5 | dict |  |
| `content[].content` | 5 | str | <status>running</status>\n\n<stdout>\n1787338889.354687000\nd+CdHPsjM+Rv8+xiawdw… |
| `content[].tool_use_id` | 5 | str | toolu_01CKNxP7re5eFa4FTz5TCeD2 / toolu_01MxjF1xXBTrsk1tXYjxDWEo |
| `content[].type` | 5 | str | tool_result |

### `toolUseResult` for tool `Read` (17)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 10481 | dict/str | Error: File does not exist. Note: your current working directory is /Users/dodge… / InputValidationError: JSON |
| `toolUseResult.file` | 10391 | dict |  |
| `toolUseResult.file.base64` | 45 | str | /9j/4AAQSkZJRgABAgAAAQABAAD/wAARCAQKBkADAREAAhEBAxEB/9sAQwADAgIDAgIDAwMDBAMDBAUI… / /9j/4AAQSkZJRgABAgAAAQABAA |
| `toolUseResult.file.content` | 10247 | str | # Workspace verb: `create`\n\nGenerate practical git branch/worktree names for o… / ZSH_THEME="robbyrussell"\n |
| `toolUseResult.file.dimensions` | 45 | dict |  |
| `toolUseResult.file.dimensions.displayHeight` | 45 | int | 1034 / 900 |
| `toolUseResult.file.dimensions.displayWidth` | 45 | int | 1600 / 2000 |
| `toolUseResult.file.dimensions.originalHeight` | 45 | int | 1034 / 900 |
| `toolUseResult.file.dimensions.originalWidth` | 45 | int | 1600 / 4112 |
| `toolUseResult.file.filePath` | 10346 | str | /Users/dodgecoates/.claude/skills/create-or-update-workspace/create.md / /Users/dodgecoates/.zshrc |
| `toolUseResult.file.numLines` | 10247 | int | 261 / 265 |
| `toolUseResult.file.originalSize` | 45 | int | 2357597 / 1980049 |
| `toolUseResult.file.startLine` | 10247 | int | 1 / 270 |
| `toolUseResult.file.totalLines` | 10247 | int | 261 / 265 |
| `toolUseResult.file.truncatedByTokenCap` | 51 | bool | true |
| `toolUseResult.file.type` | 45 | str | image/jpeg / image/png |
| `toolUseResult.type` | 10391 | str | text / file_unchanged |

### `toolUseResult` for tool `Bash` (30)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 77927 | dict/str | Error: Exit code 1\n=== /Users/dodgecoates/.claude/settings.json\n  "model": "fa… / User rejected tool use |
| `toolUseResult.backgroundCwdHint` | 466 | str | Session cwd remains /Users/dodgecoates/.config/doom/.claude/worktrees/agent-a37c… / Session cwd remains /Users |
| `toolUseResult.backgroundEndsWithFinalResponse` | 9 | bool | true |
| `toolUseResult.backgroundTaskId` | 2810 | str | b6vxdsjz4 / bh7sprmo3 |
| `toolUseResult.backgroundedByUser` | 76 | bool | true |
| `toolUseResult.dangerouslyDisableSandbox` | 216 | bool | true / false |
| `toolUseResult.gitOperation` | 782 | dict |  |
| `toolUseResult.gitOperation.branch` | 401 | dict |  |
| `toolUseResult.gitOperation.branch.action` | 401 | str | rebased / merged |
| `toolUseResult.gitOperation.branch.ref` | 401 | str | "$target" / master |
| `toolUseResult.gitOperation.commit` | 276 | dict |  |
| `toolUseResult.gitOperation.commit.branch` | 114 | str | agent/proto-ui-element-messages-shared-fact / proto/message-pagination |
| `toolUseResult.gitOperation.commit.kind` | 276 | str | committed / cherry-picked |
| `toolUseResult.gitOperation.commit.sha` | 276 | str | 24b31031 / 2a0790bf |
| `toolUseResult.gitOperation.pr` | 25 | dict |  |
| `toolUseResult.gitOperation.pr.action` | 25 | str | auto-merge-disabled / created |
| `toolUseResult.gitOperation.pr.number` | 25 | int | 7458 / 7234 |
| `toolUseResult.gitOperation.pr.url` | 19 | str | https://github.com/ChessCom/explanation-engine/pull/7234 / https://github.com/ChessCom/explanation-engine/pull |
| `toolUseResult.gitOperation.push` | 89 | dict |  |
| `toolUseResult.gitOperation.push.branch` | 89 | str | protobuf-skill-comments-context / proto-skill-sketch-gate-6254 |
| `toolUseResult.interrupted` | 74331 | bool | false |
| `toolUseResult.isImage` | 74331 | bool | false |
| `toolUseResult.noOutputExpected` | 74324 | bool | false / true |
| `toolUseResult.persistedOutputPath` | 408 | str | /Users/dodgecoates/.claude/projects/-/7ab938a1-f103-4a3c-8251-83ea025fecc3/tool-… / /Users/dodgecoates/.claude |
| `toolUseResult.persistedOutputSize` | 408 | int | 30334 / 60488 |
| `toolUseResult.returnCodeInterpretation` | 1107 | str | No matches found / Some directories were inaccessible |
| `toolUseResult.staleReadFileStateHint` | 10 | str | [This command modified 1 file you've previously read: test-render-colors.el. Cal… / [This command modified 1 f |
| `toolUseResult.stderr` | 74331 | str |  / \nShell cwd was reset to /Users/dodgecoates/.claude-chesscom/skills-worktrees/re… |
| `toolUseResult.stdout` | 74331 | str | fix/validate-game-for-analysis-active-gamepoint\n/Users/dodgecoates/workspace/Ch… / NOTICE: CLAUDE_WORKSPACE_P |
| `toolUseResult.timedOutAfterMs` | 134 | int | 120000 / 600000 |

### `toolUseResult` for tool `Skill` (5)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 746 | dict/str | Error: Unknown skill: check-cicd / Error: Unknown skill: create-or-update-pr |
| `toolUseResult.allowedTools` | 498 | list |  |
| `toolUseResult.allowedTools[]` | 4329 | str | Read / Write |
| `toolUseResult.commandName` | 714 | str | gns-cowork:gns-bootstrap / create-or-update-workspace |
| `toolUseResult.success` | 714 | bool | true |

### `toolUseResult` for tool `Edit` (17)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 13648 | dict/str | Error: No changes to make: old_string and new_string are exactly the same. / Error: String to replace not foun |
| `toolUseResult.filePath` | 13342 | str | /Users/dodgecoates/.claude/statusline-command.sh / /Users/dodgecoates/.claude/settings.json |
| `toolUseResult.memdirStamped` | 190 | bool | true |
| `toolUseResult.newString` | 13342 | str | #!/usr/bin/env bash\n# Claude Code status line script\n# Converted from the zsh … /     "command": "bash /User |
| `toolUseResult.oldString` | 13342 | str | #!/usr/bin/env bash\n# Claude Code status line script\n\ninput=$(cat)\n\nmodel=$… /     "command": "bash /home |
| `toolUseResult.originalFile` | 13342 | NoneType/str | #!/usr/bin/env bash\n# Claude Code status line script\n\ninput=$(cat)\n\nmodel=$… / {\n  "$schema": "https://j |
| `toolUseResult.replaceAll` | 13342 | bool | false / true |
| `toolUseResult.staleRecovered` | 167 | bool | true |
| `toolUseResult.structuredPatch` | 13342 | list |  |
| `toolUseResult.structuredPatch[]` | 14544 | dict |  |
| `toolUseResult.structuredPatch[].lines` | 14544 | list |  |
| `toolUseResult.structuredPatch[].lines[]` | 344354 | str |  #!/usr/bin/env bash /  # Claude Code status line script |
| `toolUseResult.structuredPatch[].newLines` | 14544 | int | 16 / 7 |
| `toolUseResult.structuredPatch[].newStart` | 14544 | int | 1 / 103 |
| `toolUseResult.structuredPatch[].oldLines` | 14544 | int | 18 / 7 |
| `toolUseResult.structuredPatch[].oldStart` | 14544 | int | 1 / 103 |
| `toolUseResult.userModified` | 13342 | bool | false |

### `toolUseResult` for tool `AskUserQuestion` (16)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 245 | dict/str | User rejected tool use / Error: The user doesn't want to proceed with this tool use. The tool use was rej… |
| `toolUseResult.annotations` | 171 | dict |  |
| `toolUseResult.annotations.{question}` | 9 | dict |  |
| `toolUseResult.annotations.{question}.preview` | 9 | str | // PrevalidateGame: ply branch removed\n\n// advancedstats/feature.go\n// now ca… / static constexpr int max_g |
| `toolUseResult.answers` | 194 | dict |  |
| `toolUseResult.answers.{question}` | 263 | str | Fold payloads.proto into message.proto / Use "Sol" as written |
| `toolUseResult.questions` | 194 | list |  |
| `toolUseResult.questions[]` | 263 | dict |  |
| `toolUseResult.questions[].header` | 263 | str | Scope / Approach |
| `toolUseResult.questions[].multiSelect` | 263 | bool | false / true |
| `toolUseResult.questions[].options` | 263 | list |  |
| `toolUseResult.questions[].options[]` | 722 | dict |  |
| `toolUseResult.questions[].options[].description` | 722 | str | Defaults for .claude/agents/*.md frontmatter — model, tools, isolation, backgrou… / Defaults when building age |
| `toolUseResult.questions[].options[].label` | 722 | str | Claude Code subagents / Agent SDK / API agents |
| `toolUseResult.questions[].options[].preview` | 26 | str | 0      20k     50k    100k    100k+\n\|-------\|-------\|-------\|------->\ngreen  g… / 0      20k     50k    100k |
| `toolUseResult.questions[].question` | 263 | str | What does "agent configuration" refer to here? / Which tradeoff do you want for pinning subagents to Opus + me |

### `toolUseResult` for tool `ToolSearch` (5)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 424 | dict |  |
| `toolUseResult.matches` | 424 | list |  |
| `toolUseResult.matches[]` | 514 | str | WebFetch / WebSearch |
| `toolUseResult.query` | 424 | str | select:WebFetch,WebSearch / select:WebSearch,WebFetch |
| `toolUseResult.total_deferred_tools` | 424 | int | 49 / 34 |

### `toolUseResult` for tool `WebFetch` (7)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 86 | dict |  |
| `toolUseResult.bytes` | 86 | int | 729 / 94956 |
| `toolUseResult.code` | 86 | int | 301 / 200 |
| `toolUseResult.codeText` | 86 | str | Moved Permanently / OK |
| `toolUseResult.durationMs` | 86 | int | 132 / 233 |
| `toolUseResult.result` | 86 | str | REDIRECT DETECTED: The URL redirects to a different host.\n\nOriginal URL: https… / > ## Documentation Index\n |
| `toolUseResult.url` | 86 | str | https://docs.claude.com/en/docs/claude-code/sub-agents / https://code.claude.com/docs/en/sub-agents |

### `toolUseResult` for tool `Agent` (53)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 1864 | dict/str | Error: Agent type 'opus-medium' not found. Available agents: claude, claude-code… / Error: Agent type 'opus-hi |
| `toolUseResult.agentId` | 1806 | str | a2ae1570083dc3acc / a56962b594391f27c |
| `toolUseResult.agentType` | 153 | str | statusline-setup / Explore |
| `toolUseResult.canReadOutputFile` | 1653 | bool | true |
| `toolUseResult.content` | 153 | list |  |
| `toolUseResult.content[]` | 163 | dict |  |
| `toolUseResult.content[].text` | 163 | str | [harness: subagent output matched instruction-shaped pattern(s): settings-json. … / Configured.\n\n- Source: z |
| `toolUseResult.content[].type` | 163 | str | text |
| `toolUseResult.description` | 1653 | str | Research Claude Code JSONL schema / Vet pre-existing bug claims |
| `toolUseResult.isAsync` | 1653 | bool | true |
| `toolUseResult.outputFile` | 1653 | str | /private/tmp/claude-501/-Users-dodgecoates--config-doom/648d0255-7e1c-462e-b3eb-… / /private/tmp/claude-501/-U |
| `toolUseResult.prompt` | 1806 | str | Configure my statusLine from my shell PS1 configuration / READ-ONLY research task. Do NOT modify or create any |
| `toolUseResult.resolvedModel` | 1806 | str | claude-sonnet-5 / claude-opus-5[1m] |
| `toolUseResult.status` | 1806 | str | completed / async_launched |
| `toolUseResult.toolStats` | 138 | dict |  |
| `toolUseResult.toolStats.bashCount` | 138 | int | 0 / 30 |
| `toolUseResult.toolStats.editFileCount` | 138 | int | 3 / 0 |
| `toolUseResult.toolStats.linesAdded` | 138 | int | 18 / 0 |
| `toolUseResult.toolStats.linesRemoved` | 138 | int | 20 / 0 |
| `toolUseResult.toolStats.otherToolCount` | 138 | int | 0 / 9 |
| `toolUseResult.toolStats.readCount` | 138 | int | 3 / 14 |
| `toolUseResult.toolStats.searchCount` | 138 | int | 0 |
| `toolUseResult.totalDurationMs` | 153 | int | 46372 / 212341 |
| `toolUseResult.totalTokens` | 153 | int | 23418 / 86120 |
| `toolUseResult.totalToolUseCount` | 153 | int | 6 / 44 |
| `toolUseResult.usage` | 153 | dict |  |
| `toolUseResult.usage.cache_creation` | 153 | dict |  |
| `toolUseResult.usage.cache_creation.ephemeral_1h_input_tokens` | 153 | int | 0 |
| `toolUseResult.usage.cache_creation.ephemeral_5m_input_tokens` | 153 | int | 1333 / 661 |
| `toolUseResult.usage.cache_creation_input_tokens` | 153 | int | 1333 / 661 |
| `toolUseResult.usage.cache_read_input_tokens` | 153 | int | 21709 / 80970 |
| `toolUseResult.usage.inference_geo` | 153 | str | not_available / global |
| `toolUseResult.usage.input_tokens` | 153 | int | 2 / 8 |
| `toolUseResult.usage.iterations` | 153 | list |  |
| `toolUseResult.usage.iterations[]` | 153 | dict |  |
| `toolUseResult.usage.iterations[].cache_creation` | 153 | dict |  |
| `toolUseResult.usage.iterations[].cache_creation.ephemeral_1h_input_tokens` | 153 | int | 0 |
| `toolUseResult.usage.iterations[].cache_creation.ephemeral_5m_input_tokens` | 153 | int | 1333 / 661 |
| `toolUseResult.usage.iterations[].cache_creation_input_tokens` | 153 | int | 1333 / 661 |
| `toolUseResult.usage.iterations[].cache_read_input_tokens` | 153 | int | 21709 / 80970 |
| `toolUseResult.usage.iterations[].input_tokens` | 153 | int | 2 / 8 |
| `toolUseResult.usage.iterations[].output_tokens` | 153 | int | 374 / 4487 |
| `toolUseResult.usage.iterations[].type` | 153 | str | message |
| `toolUseResult.usage.output_tokens` | 153 | int | 374 / 4487 |
| `toolUseResult.usage.output_tokens_details` | 26 | dict |  |
| `toolUseResult.usage.output_tokens_details.thinking_tokens` | 26 | int | 75 / 0 |
| `toolUseResult.usage.server_tool_use` | 153 | dict |  |
| `toolUseResult.usage.server_tool_use.web_fetch_requests` | 153 | int | 0 |
| `toolUseResult.usage.server_tool_use.web_search_requests` | 153 | int | 0 |
| `toolUseResult.usage.service_tier` | 153 | str | standard |
| `toolUseResult.usage.speed` | 153 | str | standard |
| `toolUseResult.worktreeBranch` | 6 | str | worktree-agent-a6aa32c2b7b7e6895 / worktree-agent-aef9c924e1bcd4fc4 |
| `toolUseResult.worktreePath` | 6 | str | /Users/dodgecoates/.config/doom/.claude/worktrees/agent-a6aa32c2b7b7e6895 / /Users/dodgecoates/.config/doom/.c |

### `toolUseResult` for tool `Write` (15)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 2409 | dict/str | Error: File has not been read yet. Read it first before writing to it. / Error: This agent is isolated in the  |
| `toolUseResult.content` | 2345 | str | package main\n\nimport "fmt"\n\nfunc main() {\n	sum := 0\n	for i := 1; i <= 10; … / #!/usr/bin/env bash\n# tes |
| `toolUseResult.filePath` | 2345 | str | /private/tmp/claude-501/-Users-dodgecoates/f933d4e9-45f2-4259-b293-3bc02fa17e91/… / /Users/dodgecoates/workspa |
| `toolUseResult.memdirStamped` | 43 | bool | true |
| `toolUseResult.originalFile` | 2345 | NoneType/str | null / #!/usr/bin/env bash\n# test-run.sh — validate run.sh's default-home symlink verb… |
| `toolUseResult.structuredPatch` | 2345 | list |  |
| `toolUseResult.structuredPatch[]` | 999 | dict |  |
| `toolUseResult.structuredPatch[].lines` | 999 | list |  |
| `toolUseResult.structuredPatch[].lines[]` | 62096 | str |  #!/usr/bin/env bash / -# test-run.sh — validate run.sh's default-home symlink verbs |
| `toolUseResult.structuredPatch[].newLines` | 999 | int | 14 / 28 |
| `toolUseResult.structuredPatch[].newStart` | 999 | int | 1 / 24 |
| `toolUseResult.structuredPatch[].oldLines` | 999 | int | 11 / 17 |
| `toolUseResult.structuredPatch[].oldStart` | 999 | int | 1 / 21 |
| `toolUseResult.type` | 2345 | str | create / update |
| `toolUseResult.userModified` | 2345 | bool | false |

### `toolUseResult` for tool `WebSearch` (11)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 37 | dict |  |
| `toolUseResult.durationSeconds` | 37 | float | 4.8961673330003395 / 6.253811999999918 |
| `toolUseResult.query` | 37 | str | claude-agent-sdk-typescript SDKMessage union types.ts SDKSystemMessage subtype / claude-code-log daaain Pydant |
| `toolUseResult.results` | 37 | list |  |
| `toolUseResult.results[]` | 76 | dict/str | Based on the search results, I found information about the `SDKSystemMessage` ty… / Based on the search result |
| `toolUseResult.results[].content` | 38 | list |  |
| `toolUseResult.results[].content[]` | 313 | dict |  |
| `toolUseResult.results[].content[].title` | 313 | str | claude-agent-sdk: SDKMessage type resolves to any due to missing type declaratio… / Agent SDK reference - Type |
| `toolUseResult.results[].content[].url` | 313 | str | https://github.com/anthropics/claude-code/issues/27834 / https://code.claude.com/docs/en/agent-sdk/typescript |
| `toolUseResult.results[].tool_use_id` | 38 | str | srvtoolu_01PzKuYQRHtpAT7CVznuiD4m / srvtoolu_015zJKGX8Jztn6xkR7WTmqae |
| `toolUseResult.searchCount` | 37 | int | 1 / 2 |

### `toolUseResult` for tool `SendMessage` (9)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 294 | dict |  |
| `toolUseResult.display` | 3 | str | Not sent — no agent named 'general-purpose' is reachable. |
| `toolUseResult.message` | 294 | str | Agent "a80a1269d5bfca0d8" had no active task; resumed from transcript in the bac… / Agent "a37ceb6a4d3f8ed2a"  |
| `toolUseResult.pin` | 279 | dict |  |
| `toolUseResult.pin.id` | 279 | str | a80a1269d5bfca0d8 / a37ceb6a4d3f8ed2a |
| `toolUseResult.pin.name` | 279 | str | a80a1269d5bfca0d8 / a37ceb6a4d3f8ed2a |
| `toolUseResult.pin.ref` | 279 | str | d756ec / 1adaea |
| `toolUseResult.resumedAgentId` | 154 | str | a80a1269d5bfca0d8 / a37ceb6a4d3f8ed2a |
| `toolUseResult.success` | 294 | bool | true / false |

### `toolUseResult` for tool `TaskStop` (5)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 110 | dict/str | Error: No task found with ID: b1zvzs714. Running background agents: a9f05bf87603… / Error: Task bk81bxkko is n |
| `toolUseResult.command` | 97 | str | Vet pre-existing hook claim / Flake origin: code-path proof |
| `toolUseResult.message` | 97 | str | Successfully stopped task: a90275e96f96c2fa9 (Vet pre-existing hook claim) / Successfully stopped task: ab1a39 |
| `toolUseResult.task_id` | 97 | str | a90275e96f96c2fa9 / ab1a398aa72d0fbde |
| `toolUseResult.task_type` | 97 | str | local_agent / local_bash |

### `toolUseResult` for tool `TaskCreate` (4)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 84 | dict/str | InputValidationError: [\n  {\n    "expected": "string",\n    "code": "invalid_ty… |
| `toolUseResult.task` | 82 | dict |  |
| `toolUseResult.task.id` | 82 | str | 1 / 2 |
| `toolUseResult.task.subject` | 82 | str | Verify daemon stand-down covers all supersede paths / Fix create→id correlation to use the ack instead of cwd  |

### `toolUseResult` for tool `TaskUpdate` (8)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 167 | dict |  |
| `toolUseResult.statusChange` | 160 | dict |  |
| `toolUseResult.statusChange.from` | 160 | str | pending / in_progress |
| `toolUseResult.statusChange.to` | 160 | str | in_progress / completed |
| `toolUseResult.success` | 167 | bool | true |
| `toolUseResult.taskId` | 167 | str | 1 / 2 |
| `toolUseResult.updatedFields` | 167 | list |  |
| `toolUseResult.updatedFields[]` | 175 | str | status / subject |

### `toolUseResult` for tool `TaskList` (7)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 4 | dict |  |
| `toolUseResult.tasks` | 4 | list |  |
| `toolUseResult.tasks[]` | 8 | dict |  |
| `toolUseResult.tasks[].blockedBy` | 8 | list |  |
| `toolUseResult.tasks[].id` | 8 | str | 1 / 2 |
| `toolUseResult.tasks[].status` | 8 | str | completed / pending |
| `toolUseResult.tasks[].subject` | 8 | str | Proto: add EventOrigin enum + field / Find and stamp all producers |

### `toolUseResult` for tool `Monitor` (4)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 155 | dict/str | Error: This agent is isolated in the worktree /Users/dodgecoates/.config/doom/.c… / InputValidationError: [\n  |
| `toolUseResult.persistent` | 130 | bool | false / true |
| `toolUseResult.taskId` | 130 | str | bnjfsz0ev / bqb924qom |
| `toolUseResult.timeoutMs` | 130 | int | 360000 / 300000 |

### `toolUseResult` for tool `EnterWorktree` (4)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 21 | dict/str | Error: EnterWorktree cannot create a worktree from a subagent with a cwd overrid… / Error: Permission for this |
| `toolUseResult.message` | 10 | str | Created worktree at /Users/dodgecoates/.config/doom/.claude/worktrees/single-ses… / Entered worktree at /Users |
| `toolUseResult.worktreeBranch` | 10 | str | worktree-single-session-id / fix/prompt-identity-and-echo-ordering |
| `toolUseResult.worktreePath` | 10 | str | /Users/dodgecoates/.config/doom/.claude/worktrees/single-session-id / /Users/dodgecoates/.config/doom/.claude/ |

### `toolUseResult` for tool `ExitWorktree` (8)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 2 | dict/str | Error: Worktree has 393 commits on worktree-single-session-id. Removing will dis… |
| `toolUseResult.action` | 1 | str | remove |
| `toolUseResult.discardedCommits` | 1 | int | 393 |
| `toolUseResult.discardedFiles` | 1 | int | 0 |
| `toolUseResult.message` | 1 | str | Exited and removed worktree at /Users/dodgecoates/.config/doom/.claude/worktrees… |
| `toolUseResult.originalCwd` | 1 | str | /Users/dodgecoates/.config/doom |
| `toolUseResult.worktreeBranch` | 1 | str | worktree-single-session-id |
| `toolUseResult.worktreePath` | 1 | str | /Users/dodgecoates/.config/doom/.claude/worktrees/single-session-id |

### `toolUseResult` for tool `TaskOutput` (12)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 7 | dict/str | User rejected tool use |
| `toolUseResult.retrieval_status` | 6 | str | success / not_ready |
| `toolUseResult.task` | 6 | dict |  |
| `toolUseResult.task.description` | 6 | str | POC handrolled resumeSessionAt / Run the unified verifier |
| `toolUseResult.task.exitCode` | 4 | NoneType/int | 0 / null |
| `toolUseResult.task.isRawTranscript` | 2 | bool | false |
| `toolUseResult.task.output` | 6 | str | Done. Everything ran under the scratch dir with `cwd=.../rewind-poc/work`, so al… / \n\n[safe-test-run] ert ex |
| `toolUseResult.task.prompt` | 2 | str | Build and run a proof-of-concept that hand-rolls "resumeSessionAt" (conversation… / You are a weekly-report re |
| `toolUseResult.task.result` | 2 | str | Done. Everything ran under the scratch dir with `cwd=.../rewind-poc/work`, so al… / All searches complete.\n\n |
| `toolUseResult.task.status` | 6 | str | completed / running |
| `toolUseResult.task.task_id` | 6 | str | a89164f0264ae8b3e / bpjf9y5y0 |
| `toolUseResult.task.task_type` | 6 | str | local_agent / local_bash |

### `toolUseResult` for tool `ScheduleWakeup` (6)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 103 | dict/str | Error: `prompt` is required when `stop` is not true. / Error: `noop` is required when `stop` is not true. |
| `toolUseResult.cancelledWakeups` | 9 | int | 1 / 0 |
| `toolUseResult.clampedDelaySeconds` | 101 | int | 180 / 0 |
| `toolUseResult.scheduledFor` | 101 | int | 1786295340000 / 0 |
| `toolUseResult.stopped` | 9 | bool | true |
| `toolUseResult.wasClamped` | 101 | bool | false |

### `toolUseResult` for tool `Workflow` (9)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 83 | dict |  |
| `toolUseResult.runId` | 83 | str | wf_0de21b19-007 / wf_897e6d78-ec1 |
| `toolUseResult.scriptPath` | 83 | str | /Users/dodgecoates/.claude/projects/-Users-dodgecoates--config-doom-modules-app-… / /Users/dodgecoates/.claude |
| `toolUseResult.status` | 83 | str | async_launched |
| `toolUseResult.summary` | 83 | str | Parallel opus low-effort remediation of merge-failed pinning and workspace-key p… / Max-parallel remediation:  |
| `toolUseResult.taskId` | 83 | str | w6n5yorg1 / w84n2qwin |
| `toolUseResult.taskType` | 83 | str | local_workflow |
| `toolUseResult.transcriptDir` | 83 | str | /Users/dodgecoates/.claude/projects/-Users-dodgecoates--config-doom/df0e52f2-693… / /Users/dodgecoates/.claude |
| `toolUseResult.workflowName` | 83 | str | bad-state-remediation-fanout / startup-loop-iteration-0 |

### `toolUseResult` for tool `ListAgents` (2)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 22 | dict |  |
| `toolUseResult.listing` | 22 | str | Peer sessions (2):\n  doom-c9 [e172ed]  ·  interactive  ·  started 2d ago\n  doo… / Subagents (1):\n  a08abdbf |

### `toolUseResult` for tool `StructuredOutput` (1)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 9 | str | Structured output provided successfully |

### `toolUseResult` for tool `BashOutput` (10)

| path | n | types | example |
|---|---|---|---|
| `toolUseResult` | 5 | dict |  |
| `toolUseResult.command` | 5 | str | bash -c 'for i in $(seq 1 200); do date +%s.%N; head -c 6000 /dev/urandom \| base… |
| `toolUseResult.exitCode` | 5 | NoneType | null |
| `toolUseResult.shellId` | 5 | str | 22a9fe |
| `toolUseResult.status` | 5 | str | running |
| `toolUseResult.stderr` | 5 | str |  |
| `toolUseResult.stderrLines` | 5 | int | 1 |
| `toolUseResult.stdout` | 5 | str | 1787338889.354687000\nd+CdHPsjM+Rv8+xiawdwW5Hhs1kEDYuowww49/HvqpOCm216nBUs4nbcI5… |
| `toolUseResult.stdoutLines` | 5 | int | 12 / 64 |
| `toolUseResult.timestamp` | 5 | str | 2026-08-21T19:01:31.685Z / 2026-08-21T19:01:42.472Z |