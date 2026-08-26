# GAPS-consolidated — vendor surfaces vs `conversation.v1`

**What this is.** One consolidated register of everything a vendor data surface
carries that `conversation.v1` does not mirror anywhere, by any spelling.

**Contract snapshot.** Taken against `53b0bdb3b` (*"audit finding 1 —
AgentResponseFailureReason stop-taxonomy arms land"*), branch
`proto/message-pagination`. Thirteen files in
`proto/src/conversation/v1/`: `agent.proto`, `agent_activity.proto`,
`api.proto`, `content_blocks.proto`, `detached_work.proto`, `history.proto`,
`permission.proto`, `question.proto`, `session.proto`, `slash_command.proto`,
`turn.proto`, `user.proto`, `workflow.proto`. The contract is moving under this
audit — remediation is landing concurrently — so every citation is at that SHA.

**What counts as a gap.** A vendor fact with **no home and no recorded
reason**. A fact dropped WITH a reason in
`docs/protobuf-design/figma-to-idl-redesign.md` is not a gap; those are
collected in [Appendix A](#appendix-a--recorded-drops-not-gaps) so the survey is
not re-purchased. Three further classes are called out rather than silently
counted:

- **CARRIED-OPAQUELY** — the fact reaches a consumer, but only as
  `AgentUnmodeled` / `UnsupportedBlock` / `google.protobuf.Struct`, i.e. with its
  structure destroyed. The design record calls a recognizable built-in landing
  there a *producer defect* (`figma-to-idl-redesign.md:1465,2712`), so these are
  gaps with a soft landing, not non-gaps.
- **RESIDUE-ONLY** — the fact lands in `store.v1`'s
  `StoreUnservedItem{vendor_specific|unknown|unparsed}`
  (`store/v1/store.proto:139-176`). That package is **structurally invisible to
  the daemon** (`store.proto:5-19`), so a residue landing is durable storage,
  never delivery. Still a gap.
- **NO-PRODUCER (inverse gap)** — `conversation.v1` declares a field that **no**
  vendor surface can fill. Not a fidelity gap but a contract defect, and
  recorded here because the same audit answers it.

**Evidence tiers.**

- **OBSERVED** — present in real data. Counts are first-hand unless attributed.
  Two first-hand samples back most counts:
  - **S1** — 300 session transcripts under `~/.claude/projects` and
    `~/.claude-chesscom/projects` modified since 2026-07-01, **40,279 lines**.
  - **S2** — 150 subagent sidechain transcripts under
    `~/.claude/projects/*/*/subagents/agent-*.jsonl`, **11,915 lines**.
  - **S3** — the in-repo golden corpus, `testdata/corpus/` (harvest 2026-07-23,
    real anonymized artifacts, provenance in `MANIFEST.md`). Cited when a shape
    is rare enough that S1/S2 missed it.
  - **S4** — all 57 `wf_*.json`, all 57 `journal.jsonl`, all 1,771
    `agent-*.meta.json`, and 190 `/tmp/claude-501/**/tasks/*.output` spools.
  - **S5** — a deep aggregate over 95 transcript files spanning binary versions
    `2.0.36`-`2.1.236` across all three config roots, **92,211 records**, with
    tool results joined back to their calling tool name. Where S5 and S1
    disagree, S5's larger number is quoted.
  - **S6** — presence/absence over **all 14,349** `.jsonl` files in
    `~/.claude/projects`, `~/.claude-backup/projects` and
    `~/.claude-chesscom/projects`. Used only for **absence** proofs, which is
    the one claim a sample cannot make.
A methodological warning worth carrying: `jq`'s `paths(scalars)` silently drops
every `false` and `null` leaf, so a naive extractor loses
`toolUseResult.interrupted`, `.isImage`, `.userModified` and
`message.stop_details` entirely. S5 used
`paths(type=="boolean" or type=="number" or type=="string" or type=="null")`.

- **DECLARED-ONLY** — present in the SDK type surface
  (`agent-shim/claude/shim/node_modules/@anthropic-ai/claude-agent-sdk/sdk.d.ts`)
  or in vendor typings, with no occurrence found in real data.

**Prior partitions.** `A-sdk-stream.md`, `B-tool-io.md`, `C-jsonl.md`,
`D-sidecar-files.md`, `E-control-surface.md`, `F-ssm-comb.md`,
`G-statedb-architecture.md`, `inventory.md`, `classify.md`. They are the
starting inventory; every claim carried forward here was re-verified against the
current sources and the current protos. Findings the redesign has since
superseded are listed in [Appendix B](#appendix-b--prior-audit-findings-this-pass-retires)
rather than repeated.

---

## Summary

| # | Theme | Gaps | OBSERVED | DECLARED-ONLY |
|---|---|---|---|---|
| 1 | [Stop, refusal and termination taxonomy](#1-stop-refusal-and-termination-taxonomy) | 11 | 5 | 6 |
| 2 | [Retraction, supersede, interruption, rewind](#2-retraction-supersede-interruption-rewind) | 10 | 4 | 6 |
| 3 | [Identities, lineage and replay fidelity](#3-identities-lineage-and-replay-fidelity) | 19 | 17 | 2 |
| 4 | [Usage, cost and accounting](#4-usage-cost-and-accounting) | 14 | 9 | 5 |
| 5 | [Tool I/O fidelity](#5-tool-io-fidelity) | 28 | 20 | 8 |
| 6 | [Tool failure evidence](#6-tool-failure-evidence) | 6 | 5 | 1 |
| 7 | [Session inventory and environment](#7-session-inventory-and-environment) | 15 | 9 | 6 |
| 8 | [Hooks](#8-hooks) | 6 | 4 | 2 |
| 9 | [Attachments and injected context](#9-attachments-and-injected-context) | 9 | 9 | 0 |
| 10 | [Compaction and context cuts](#10-compaction-and-context-cuts) | 9 | 7 | 2 |
| 11 | [Subagents and detached work](#11-subagents-and-detached-work) | 13 | 8 | 5 |
| 12 | [Journals, meta files and spools](#12-journals-meta-files-and-spools) | 14 | 13 | 1 |
| 13 | [Control surface and session-level vendor events](#13-control-surface-and-session-level-vendor-events) | 16 | 2 | 14 |
| 14 | [Inverse gaps: declared but unfillable](#14-inverse-gaps-declared-but-unfillable) | 12 | — | — |
| | **Total** | **182** | **112** | **58** |

Theme 14's twelve are counted separately: they are contract defects, not vendor
facts, so they carry no evidence tier.

---

## 1. Stop, refusal and termination taxonomy

Finding 1 (`53b0bdb3b`) landed `AgentResponseFailureReason { max_tokens |
refused | context_window_exceeded | aborted }` at
`agent_activity.proto:409-433`. That closes the response-level half. What
remains is the vendor's *other* stop vocabularies, none of which map onto those
four arms.

| ID | Source · field | Semantics | Tier | Home today | Minimal modeling |
|---|---|---|---|---|---|
| STOP-1 | `sdk.d.ts:6909` `TerminalReason` — 19 literals | Why the **turn** ended, as the CLI rules it: `blocking_limit`, `rapid_refill_breaker`, `prompt_too_long`, `image_error`, `model_error`, `api_error`, `malformed_tool_use_exhausted`, `aborted_streaming`, `aborted_tools`, `stop_hook_prevented`, `hook_stopped`, `tool_deferred`, `max_turns`, `background_requested`, `completed`, `budget_exhausted`, `structured_output_retry_exhausted`, `tool_deferred_unavailable`, `turn_setup_failed` | OBSERVED — `terminal_reason` present on `testdata/corpus/stream/result_success.jsonl` (S3) | None. `AgentSuccess{completed\|interrupted}` and `AgentFailure{api_request_failed}` cover 2 of 19 | Arms on `AgentFailure`; 14 of the 19 have no equivalent under any existing arm |
| STOP-2 | `sdk.d.ts:4271` `SDKResultError.subtype` | `error_during_execution`, `error_max_turns`, `error_max_budget_usd`, `error_max_structured_output_retries` | DECLARED-ONLY (corpus `COVERAGE.md` records the error variant as not-found-on-machine) | None — `AgentFailure` has one arm | Four arms on `AgentFailure`; budget and turn ceilings are product-visible limits |
| STOP-3 | `sdk.d.ts:4281` `SDKResultError.errors: string[]` | The accumulated error strings for a failed run | DECLARED-ONLY | None | A repeated string on the failure arm |
| STOP-4 | `sdk.d.ts:2901` `SDKAssistantMessageError` — 10 literals | Per-response API error class: adds `oauth_org_not_allowed`, `billing_error`, `model_not_found`, `max_output_tokens`, `unknown` beyond what `ApiRequestFailed` names | OBSERVED — `error` on assistant lines, **7 in S1** (`server_error` 2, `rate_limit` 4 with `apiErrorStatus: 429`, …) | Partial. `api.proto:36-55` covers rate-limit/overload/auth/permission/invalid/too-large/not-found/internal; `billing_error`, `model_not_found`, `oauth_org_not_allowed` fall to `ApiUnmodeledError{type}` | Three arms, or an explicit ruling that `ApiUnmodeledError` is their home |
| STOP-5 | `messages.d.ts` (beta) `:2094` `BetaStopReason` — value `stop_sequence` | The model stopped on a stop sequence | **OBSERVED — 62 in S5, 50 in S1** (`message.stop_reason`) | None | The design record states at `agent_activity.proto:407` that `stop_sequence` is "unused by this product". **That is falsified by real data.** Either an arm or an amended reason |
| STOP-6 | beta `:1561` `BetaMessage.stop_details` → `:1849` `BetaRefusalStopDetails.category` — `cyber\|bio\|frontier_llm\|reasoning_extraction\|general_harms` | *Which* safety category a refusal fell under | **OBSERVED — S5 finds `message.stop_details` non-null twice, both `{"type":"refusal","category":"bio","explanation":…}`; and `apiRefusalCategory` on all 3 `system/model_refusal_*` lines** | None. `AgentResponseRefused` carries only free-text `explanation` | An enum beside `AgentResponseRefusalExplanation` |
| STOP-7 | beta `:1896,1918,1925` `BetaRefusalStopDetails.{fallback_credit_token, fallback_has_prefill_claim, recommended_model}` | The vendor's own remediation hint for a refusal | DECLARED-ONLY | None | Fields on `AgentResponseRefused`; `recommended_model` is user-actionable |
| STOP-8 | `sdk.d.ts:4090-4114` `SDKModelRefusalFallbackMessage` — whole message | The vendor **refused and silently switched model**: `trigger`, `direction: retry\|revert\|sticky`, `original_model`, `fallback_model`, `request_id`, `api_refusal_category`, `api_refusal_explanation`, `content` | **OBSERVED — `system/model_refusal_fallback` 2 in S5, carrying `direction: retry`, `trigger: refusal`, `originalModel: claude-fable-5`, `fallbackModel: claude-opus-4-8`, `apiRefusalCategory: reasoning_extraction`. A matching content block `{"type":"fallback","from":{"model":…},"to":{"model":…}}` appears twice** | None. `SessionModelChanged` names a *deliberate* change, not a refusal-driven fallback | A `SessionUpdate` arm, or an `AgentActivity` arm — the user's answer came from a different model than the one the session says is effective |
| STOP-9 | `sdk.d.ts:4122-4130` `SDKModelRefusalNoFallbackMessage` | The same refusal with no fallback available: `original_model`, `api_refusal_category`, `api_refusal_explanation`, `refused_user_message_uuid`, `content` | **OBSERVED — 1 in S5** | Partial — the *explanation* now has `AgentResponseRefused.explanation`; `original_model` and `refused_user_message_uuid` do not | Extend `AgentResponseRefused`, or a distinct session-level arm |
| STOP-10 | `sdk.d.ts:3957` `SDKInformationalMessage.prevent_continuation` | An informational line that **ends the turn** | DECLARED-ONLY | None | An `AgentFailure` arm — a turn-ending fact with no representation is a stuck-looking feed |
| STOP-11 | S1 `system/stop_hook_summary.stopReason` and `.preventedContinuation` | A Stop hook's own verdict on whether the agent may continue | **OBSERVED — 953 in S5** (every one carries `stopReason`, and in all 953 it is the **empty string** — the field exists and is never populated) | None | See [HOOK-3](#8-hooks); the stop verdict is the load-bearing half |

**Note on `pause_turn` and `compaction`.** Both are `BetaStopReason` values with
no arm, and both are **recorded drops** with reasons at
`agent_activity.proto:406-408`. Not gaps. See [Appendix A](#appendix-a--recorded-drops-not-gaps).

---

## 2. Retraction, supersede, interruption, rewind

`conversation.v1` is **upsert-only**: a unit is identified by `AgentActivityId`
and a later frame replaces it (`agent_activity.proto:111-118`). There is no
frame that says *this unit should no longer exist*, and no frame that says *the
conversation was rewound past here*. The vendor has both, and uses them.

| ID | Source · field | Semantics | Tier | Home today | Minimal modeling |
|---|---|---|---|---|---|
| RETRACT-1 | `sdk.d.ts:2869` `SDKAssistantMessage.supersedes?: UUID[]` | This message **replaces** the listed wire messages — the refusal-fallback path emits a second answer that voids the first | DECLARED-ONLY — and S6 confirms **no retraction marker of any kind exists on disk**: zero occurrences of `rewind`, `checkpoint`, `supersede`, `retract`, `isRewind`, `restoreCheckpoint` or `"deleted": true` across all 14,349 files, and **zero duplicate `uuid`s within any file** — the transcript is strictly append-only | None. Nothing on the wire carries a vendor uuid, so the list would be unaddressable even if carried | Requires [IDENT-1](#3-identities-lineage-and-replay-fidelity) first: a vendor-uuid-addressable identity, then a retraction frame |
| RETRACT-2 | `sdk.d.ts:4109` `SDKModelRefusalFallbackMessage.retracted_message_uuids?: string[]` | The messages the fallback **withdrew** from the transcript | DECLARED-ONLY | None | Same as RETRACT-1 |
| RETRACT-3 | `sdk.d.ts:2873` `SDKAssistantMessage.aborted?: true` | The response was truncated by an interrupt and carries **no** `stop_reason` | **OBSERVED — 7 in S5, 8 in S1** (disk twin `isAbortedMidStream`) | Partial — `AgentResponseAborted` (`agent_activity.proto:433`) now exists. But it is an arm of the *response*; the disk flag also appears on messages carrying tool calls | Confirm the arm covers the tool-call case, or widen |
| RETRACT-4 | S1 `interruptedByShutdown` | The record was cut because the **process** went down, not because the user stopped it | **OBSERVED — 17 in S5, 20 in S1** | None. `AgentInterrupted` (`agent.proto:144`) is explicitly the *user's* stop | An arm distinguishing user-stop from host-shutdown; the daemon's restart re-drive (`PROMPT_ORIGIN_RESUME_AFTER_RESTART`) exists precisely for this case and cannot correlate to it |
| RETRACT-5 | S1 `interruptedMessageId` | Which assistant message the interrupt cut | **OBSERVED — 32 in S5, 4 in S1** | None | A field on the interrupted arm; needs [IDENT-2](#3-identities-lineage-and-replay-fidelity) |
| RETRACT-6 | `sdk.d.ts:2486-2488` `Query.rewindFiles(userMessageId, {dryRun})` → `:2693` `RewindFilesResult{canRewind, error, filesChanged[], insertions, deletions, skippedLinks[]}` | The vendor's **checkpoint rewind**: restore the working tree to a prior user message | DECLARED-ONLY | None | The `/rewind` command literal exists (`slash_command.proto:119`) with **no result vocabulary at all**; a rewind that changed 40 files is invisible |
| RETRACT-7 | S1 line type `file-history-snapshot` / `file-history-delta` — `snapshot.messageId`, `snapshot.timestamp`, `backup.{backupFileName, backupTime, version}`, `trackingPath`, `isSnapshotUpdate` | The on-disk **backing store for rewind**: per-message file snapshots and deltas | **OBSERVED — S5: `file-history-snapshot` 870, `file-history-delta` 57.** The snapshot's `snapshot.trackedFileBackups` is an object keyed by arbitrary file path, each value `{backupFileName, backupTime, realParentDir, version}` | None | This is the evidence a rewind UI needs; nothing carries it |
| RETRACT-8 | `sdk.d.ts:1815` `Options.resumeSessionAt` | Resume a conversation **at a chosen message**, discarding what followed | DECLARED-ONLY | None. `SessionColdRemediation` offers `pay\|clear\|compact` (`session.proto:98-109`), none of which is a rewind | An arm; `E-control-surface.md:250` calls this "the largest NOROUTE in the partition" and it is still open |
| RETRACT-9 | `sdk.d.ts:3843` `SDKConversationResetMessage.new_conversation_id` | The conversation was reset and **re-identified** | DECLARED-ONLY | Partial — `SessionIdentityRotated` (`session.proto:178-185`) carries previous/new ids plus a `reason` string, but nothing produces a reset-specific reason | Verify the reset maps onto identity rotation; if so, record it |
| RETRACT-10 | `sdk.d.ts:3002` control `cancel_async_message{message_uuid}` | Cancel one queued message by uuid | DECLARED-ONLY | None | The design record (`figma-to-idl-redesign.md:2072-2075`) rules the daemon the only queue, so this may be a **recorded drop** — but the reason is recorded for `CancelQueuedPrompt`, not for this control verb. Flagged as needing a ruling, not modeling |

---

## 3. Identities, lineage and replay fidelity

The contract's identity story is deliberate: `AgentActivityId` is producer-minted
and opaque, ancestry is never on the wire, and lineage is a denormalized root
stamp (`figma-to-idl-redesign.md:297-304`). Those are recorded rulings. What is
**not** recorded is that a large set of vendor identity and provenance facts —
several of which downstream surfaces already declare fields for — reach no
consumer at all.

| ID | Source · field | Semantics | Tier | Home today | Minimal modeling |
|---|---|---|---|---|---|
| IDENT-1 | Every transcript line and every `SDKMessage` arm: `uuid` (`sdk.d.ts:2859`, `4154`, `4320`, …) | The vendor's own per-record identity — the key `supersedes`, `retracted_message_uuids`, `preserved_messages.uuids`, `refused_user_message_uuid`, `interruptedMessageId` and `preceding_tool_use_ids` are all expressed in | **OBSERVED — on every `assistant`/`user`/`attachment`/`system` record: 74,856 in S5** | None. `AgentActivityId` is shim-minted and explicitly opaque | Either carry the vendor uuid as an additional identity, or accept that **six** other vendor fields are permanently unconsumable. This is the single highest-leverage gap in the register |
| IDENT-2 | assistant `message.id` (`msg_…`), beta `:1493` | The **API response** identity — the natural key for "which units share one response", which `AgentActivity.usage` (`agent_activity.proto:52`) depends on ("exactly one unit per API response carries it") | **OBSERVED — on all 20,582 assistant records in S5.** S5 also proves the hazard: one API response is written as **many** records (3,821 `requestId` groups of 2, 2,528 of 3, up to one of 12), each carrying exactly one content block, and **usage is repeated identically on every record in the group** — naive summation over-counts by 2-3x | None | `classify.md:379-386` records that a response is written one record per content block and that in 38,109 sampled cases a later record's usage **differs** from the first. Without `message.id` the "one unit per response" rule has no static producer |
| IDENT-3 | `sdk.d.ts:2861` `SDKAssistantMessage.request_id`; disk twin `requestId` | The vendor's request correlation id, quotable in a support report | **OBSERVED — 20,528 in S5** | None in `conversation.v1`. **`frontend/v1/failure.proto:64` declares `VendorFailureContext.api_request_id`** and nothing can fill it | A string on the activity envelope or on `ApiRequestFailed` |
| IDENT-4 | beta `:1493` `BetaMessage.id`, surfaced as `frontend/v1/failure.proto:68` `api_message_id` | Which response a failure names | OBSERVED (same records as IDENT-2) | None | Same as IDENT-3 |
| IDENT-5 | S1 `timestamp` (ISO), `sdk.d.ts:2886`/`4603` | When the record was written | **OBSERVED — on every record type in S5.** S5 also finds timestamps are **not monotonic** — the worst files carry 262, 200 and 168 out-of-order transitions, so file order and the `parentUuid` chain are the only authoritative ordering | Partial. `AgentActivityStartedAt` (`agent_activity.proto:507`) rides **only** `*Start` arms. No settled or terminal frame carries an instant | The drop is recorded for prose/thinking (`turn.proto`, `AgentThinkingStart`) but **not** for tool settle instants or for any `system`-line timestamp |
| IDENT-6 | S1 `cwd` | The working directory the record was produced in | **OBSERVED — 74,856 in S5** | None. `workspace.v1 WorkspaceRef.dir` is the *daemon's* notion, and `frontend/v1/failure.proto:468` declares a `cwd` nothing fills | A field on `SessionStarted` at minimum; `SDKSystemMessage.cwd` (`sdk.d.ts:4419`) is the live producer |
| IDENT-7 | S1 `gitBranch` | The branch the record was produced on | **OBSERVED — 74,856 in S5** (may be `""`) | None | A field on `SessionStarted`; a workspace that moved branch mid-session is invisible |
| IDENT-8 | S1 `version` (e.g. `2.1.215`), `sdk.d.ts:4418` `claude_code_version` | The **agent binary's** version | **OBSERVED — 74,856 in S5, spanning 21 distinct versions from `2.0.36` to `2.1.236`** | None. `SessionRuntime` (`session.proto:53-58`) carries `shim_build_sha` and `sdk_version` — the SDK npm package, not the binary | A third field on `SessionRuntime` |
| IDENT-9 | S1 `entrypoint` (`sdk-cli` 25,418 / `cli` 7,142) | Whether the record came from the SDK-driven CLI or an interactive one | **OBSERVED — S5: `cli` 44,486, `sdk-cli` 30,257** | None | A `SessionStarted` field; it is how a record produced outside agent-repl is told apart |
| IDENT-10 | S1 `userType` | Producer class (`external` in 32,560/32,560) | **OBSERVED — 74,856 in S5, value `external` every time** | None | Low value at one observed value; recorded for completeness |
| IDENT-11 | S1 `slug`, `aiTitle`, `customTitle`, `agentName`; `sdk.d.ts:4347` `SDKSessionInfo.customTitle`, `:4335` `.summary` | The conversation's **name** — vendor-generated and user-set | **OBSERVED — S5: `slug` 16,508 (20 distinct human-readable slugs, e.g. `tingly-rolling-river`), `ai-title` 1,914, `last-prompt` 3,513 (each carrying `leafUuid`), plus `custom-title` and `agent-name`** | None | `TopbarTitle.text` (`frontend/v1/topbar.proto:49`) is daemon-composed from the workspace, so a vendor-titled conversation cannot show its own title |
| IDENT-12 | S1 `promptId`, `promptSource` (`sdk` 423 / `typed` 130 / `system` 45 / `queued` 7) | The vendor's prompt identity, and **whether a prompt was typed by a person or injected** | **OBSERVED — S5: `promptId` 11,950; `promptSource` 1,210 (`typed` 611 / `sdk` 289 / `system` 284 / `queued` 26); `origin.kind` 936 (`human` 631 / `task-notification` 304 / `coordinator` 1); `queuePriority` 6** | None. `shim.v1 PromptOrigin` is agent-repl's own attribution and never the vendor's | `promptSource: system` is the only structural way to tell an injected prompt from a typed one; `UserSaid` cannot express it |
| IDENT-13 | S1/S2 `attributionAgent`, `attributionSkill`, `attributionPlugin` | Which agent / skill / plugin a record is attributable to | **OBSERVED — S5: `attributionSkill` 2,316, `attributionAgent` 1,654, `attributionPlugin` 176** | None | `attributionSkill` is exactly the skill-scope delimiter `figma-to-idl-redesign.md:2822-2837` says does not exist — that recorded drop rests on a premise the data contradicts |
| IDENT-14 | S1 `effort` (`medium` 5,133 / `low` 1,485 / `xhigh` 28); `sdk.d.ts:553` `EffortLevel` | The reasoning-effort level the response ran at | **OBSERVED — 12,212 in S5: `medium` 5,283 / `low` 3,325 / `xhigh` 2,368 / `high` 1,236** | None anywhere in any package | A field on the response unit or the session; the shim already passes `effort` to the SDK and cannot report what it got |
| IDENT-15 | S1 `sourceToolAssistantUUID`, `sourceToolUseID` | Back-references from a tool result to the assistant record and the tool call that caused it | **OBSERVED — S5: `sourceToolAssistantUUID` 10,286, `sourceToolUseID` 102** | Partial — `sourceToolUseID` is the recorded producer for the skill-document join, but is undeclared on the SDK surface (`A-sdk-stream.md:101`), so the live path has no equivalent | Confirm the live join; the file plane and stream plane must agree |
| IDENT-16 | `sdk.d.ts:4577` `SDKToolUseSummaryMessage.preceding_tool_use_ids: string[]` + `.summary` | A vendor-composed **summary of a run of tool calls** | DECLARED-ONLY | None | A unit kind; the list is uuid-addressed, so needs IDENT-1 |
| IDENT-17 | `sdk.d.ts:4739` `SessionMessage.parent_agent_id` | The spawning agent, on the replay surface | DECLARED-ONLY | None. Ancestry off the wire is a **recorded ruling** (`figma-to-idl-redesign.md:297-304`), and a denormalized `parent_agent_id` was proposed and withdrawn (`:2503-2540`) | Not a gap for the live stream. Listed because the **replay** surface offers it and history reconstruction is the one place the withdrawal's justification (request-scoped paging) does not obviously apply |
| IDENT-18 | S5 `session_id` (snake-case), beside `sessionId` (camel) | The **runtime** session id after a resume, which is not the transcript's | **OBSERVED — 26,410 in S5, and it DIFFERS from `sessionId` in 5,836 of them** | None. `SessionStarted.vendor_session_id` is one value | This is exactly the hazard `shim-sidecar/internal/discover/discover.go:9-21` warns about, and the contract has one field where the vendor has two |
| IDENT-19 | S5 line types `relocated{relocatedCwd}` and `worktree-state{worktreeSession{originalCwd, preEnterOriginalCwd, worktreePath, worktreeName, worktreeBranch, originalBranch, originalHeadCommit}}` | The session **moved directory**, or entered/left a git worktree | **OBSERVED — `relocated` 40, `worktree-state` 40 in S5** | None. `AgentSubagentWorktree{path, branch}` (`agent_activity.proto:1338-1343`) describes a *subagent's* worktree, never the session's own | A session event. `EnterWorktree`/`ExitWorktree` are real tools with real results (`sdk-tools.d.ts:3587-3600`, 4 calls in S5) and no arm |

---

## 4. Usage, cost and accounting

`TokenUsage` (`api.proto:170-189`) carries four figures: `input_hits.read`,
`input_misses.written`, `input_misses.unwritten`, `output_tokens`, plus
`output_thinking_tokens`. The design record accepts several vendor usage fields
as **observed and not modelled** at `figma-to-idl-redesign.md:2251-2254` — those
are in [Appendix A](#appendix-a--recorded-drops-not-gaps) and not counted here.
What follows is what that ruling did **not** cover.

| ID | Source · field | Semantics | Tier | Home today | Minimal modeling |
|---|---|---|---|---|---|
| USAGE-1 | `sdk.d.ts:4310`/`4277` `total_cost_usd`; `:1271` `ModelUsage.costUSD` | **What the turn cost in money** | OBSERVED — `total_cost_usd` and per-model `costUSD` on S3 `stream/result_success.jsonl` | **None, anywhere in any package.** `figma-to-idl-redesign.md:4799` asserts `total_cost_usd` "already has its drawn home" — it does not; `FooterTokensCell` (`frontend/v1/footer.proto:363-376`) draws tokens and a verdict badge, never a currency figure | A field, or an explicit ruling that cost is derived by the daemon from tokens and model — but nothing carries the per-model split needed to derive it either (see USAGE-2) |
| USAGE-2 | `sdk.d.ts:4312` `modelUsage: Record<string, ModelUsage>` → `:1265-1281` `{inputTokens, outputTokens, cacheReadInputTokens, cacheCreationInputTokens, webSearchRequests, costUSD, contextWindow, maxOutputTokens, canonicalModel, provider}` | **Per-model** accounting for a turn that used more than one model | OBSERVED — S3 `stream/result_success.jsonl` carries `modelUsage."claude-haiku-4-5-20251001".*` with all eight core fields | None. `TokenUsage` is a single undifferentiated bucket | A repeated per-model breakdown; a fallback or a subagent on a different model makes the single bucket a lie |
| USAGE-3 | `sdk.d.ts:1272` `ModelUsage.contextWindow` | The model's context-window **size** | OBSERVED — same fixture | **None.** `SessionCold.context_tokens` (`session.proto:67`) is the current fill; there is no capacity to compare it against | A field; a "you are at 82% of context" surface is unbuildable without it, and `figma-to-idl-redesign.md:4799` claims a drawn home that does not exist |
| USAGE-4 | `sdk.d.ts:1273` `ModelUsage.maxOutputTokens` | The output ceiling that produced a `max_tokens` stop | OBSERVED — same fixture | None | A field; `AgentResponseStoppedAtMaxTokens` (`agent_activity.proto:421`) is empty and cannot say what the ceiling was |
| USAGE-5 | `sdk.d.ts:4295-4296` `duration_ms`, `duration_api_ms`; S1 `system/turn_duration.{durationMs, messageCount}` | How long the turn took, and how much of that was API time | **OBSERVED — `system/turn_duration` 840 in S5, each carrying `durationMs` and `messageCount`** | None. `AgentSubagentTotals.duration_ms` (`agent_activity.proto:1306`) exists for subagents only; the main turn has no duration anywhere | The drop at `figma-to-idl-redesign.md:541` ("derived from frame instants") is defeated by [IDENT-5](#3-identities-lineage-and-replay-fidelity): no terminal frame carries an instant to derive from |
| USAGE-6 | `sdk.d.ts:4297-4304` `ttft_ms`, `ttft_stream_ms`, `time_to_request_ms`, `request_sent_wall_ms`, `time_to_request_from_spawn_ms`, `warm_spare_claimed`, `time_origin_ms` | Latency telemetry for the turn | OBSERVED — `ttft_ms`, `ttft_stream_ms`, `time_to_request_ms` on S3 `stream/result_success.jsonl`; `ttft_ms` also on `stream/stream_event-message_start.jsonl` | None | Low product value; recorded because the design record's "no time estimates" of latency is nowhere stated as a drop |
| USAGE-7 | `sdk.d.ts:4307` `num_turns` | How many model round-trips the turn took | OBSERVED — S3 `stream/result_success.jsonl` | None | A field; it is the denominator for `error_max_turns` |
| USAGE-8 | beta `:2867` `BetaUsage.fallback_credit` → `:1115-1078` `{status: redeemed \| not_applied{reason: 12 literals, remove_to_redeem[]}}` | Whether a refusal-fallback response was **billed at a discount**, and if not, why | DECLARED-ONLY — post-dates the `figma-to-idl-redesign.md:2251` survey, so it is **not** covered by that ruling | None. The shim's `api-usage.ts:23` already validates and carries `fallbackCredit` | A field, or an amendment extending the 2251 ruling to cover it |
| USAGE-9 | beta `:934-941` `BetaDiagnostics.cache_miss_reason` — 6 variants | **Why** the prompt cache missed (`model_changed`, `system_changed`, `tools_changed`, `messages_changed`, `previous_message_not_found`, `unavailable`) plus `cache_missed_input_tokens` | **OBSERVED on disk — S5: `message.diagnostics.cache_miss_reason.type` 411 (`previous_message_not_found` 143, `system_changed` 100, `messages_changed` 64, `unavailable` 60, `model_changed` 24, `tools_changed` 20) and `cache_missed_input_tokens` 208** | None | Listed as a **partial** gap: the 2251 ruling names `cache_missed_input_tokens` + `cache_miss_reason` as accepted-unmodelled, so this is a recorded drop for the file plane. Flagged because a cold-context gate that cannot say *why* it went cold is the one place the cost of that ruling lands |
| USAGE-10 | `sdk.d.ts:4544-4548` `SDKThinkingTokensMessage{estimated_tokens, estimated_tokens_delta}` | The **live estimate** of thinking tokens, as they accrue | OBSERVED — S3 `stream/thinking_tokens.jsonl` and `stream/stream_event-content_block_delta-thinking.jsonl` (beta `:2254` `BetaThinkingDelta.estimated_tokens`). The **billed** twin `usage.output_tokens_details.thinking_tokens` is OBSERVED 3,166 times in S5 | None. `TokenUsage.output_thinking_tokens` is the *billed* figure at settle | Recorded as **OWED** at `figma-to-idl-redesign.md:2658-2662` ("needs its own type, not `TokenUsage`"). Owed, not dropped — counted |
| USAGE-11 | `sdk.d.ts:3063-3152` `SDKControlGetContextUsageResponse` | The **context breakdown** the `/context` command draws: `categories[]`, `totalTokens`, `maxTokens`, `percentage`, `memoryFiles[]`, `mcpTools[]`, `systemTools[]`, `systemPromptSections[]`, `agents[]`, `slashCommands`, `skills`, `autoCompactThreshold`, `isAutoCompactEnabled`, `messageBreakdown{toolCallTokens, toolResultTokens, attachmentTokens, assistantMessageTokens, userMessageTokens, redirectedContextTokens, unattributedTokens, toolCallsByType[], attachmentsByType[]}` | DECLARED-ONLY | None. **`frontend/v1/topbar.proto:219-254` declares `TokenBreakdownView{sections[], rows{label, tokens, share_permille, emphasized, depth}}`** — a rendered tree whose only plausible source is this response, and nothing in `conversation.v1` carries it | `SESSION_COMMAND_CONTEXT` (`slash_command.proto:97`) exists as a literal with no result vocabulary. Either a result shape or a ruling that the daemon calls the control channel directly |
| USAGE-12 | `sdk.d.ts:3186-3196` `SDKControlGetUsageResponse.session{total_cost_usd, total_api_duration_ms, total_duration_ms, total_lines_added, total_lines_removed, model_usage}` | Session-lifetime accounting behind `/cost` and `/usage` | DECLARED-ONLY | None | `SESSION_COMMAND_COST` / `SESSION_COMMAND_USAGE` (`slash_command.proto:94-95`) are literals with no result vocabulary |
| USAGE-13 | `sdk.d.ts:3220-3277` `rate_limits.{seven_day, seven_day_oauth_apps, seven_day_opus, seven_day_sonnet, model_scoped[], extra_usage}` | Every quota window **other than** the five-hour one | DECLARED-ONLY | None. `SessionAccountUsageAvailable` (`session.proto:272-275`) carries `five_hour` and nothing else | `figma-to-idl-redesign.md:1818-1820` records the five-hour-only choice with the reason "only it was ever sampled" and **flags the weekly allowance as having no producer**. That flag is still open — counted |
| USAGE-14 | `sdk.d.ts:3282-3393` `GetUsageResponse.behaviors{day, week}` — `request_count`, `session_count`, `behaviors[{key: cache_miss\|long_context\|subagent_heavy\|high_parallel\|cron, pct, count}]`, `agents[]`, `skills[]`, `plugins[]`, `mcp_servers[]` | The vendor's own **cost-behaviour attribution** | DECLARED-ONLY | None | Directly serves the topbar's accounting warning (`frontend/v1/topbar.proto:130-133`), which today has only daemon-composed prose lines |

**One structural note that outranks any individual row.** `AgentActivity.usage`
(`agent_activity.proto:52`) rests on the rule *"exactly one unit per API
response carries it"*. S5 shows the vendor writes one API response as **many
records** — 3,821 `requestId` groups of two, 2,528 of three, up to one group of
twelve — each carrying exactly one content block, and **repeating the identical
`usage` object on every record in the group**. The producer therefore needs
`message.id` or `requestId` to know which record is "the first", and
[IDENT-2](#3-identities-lineage-and-replay-fidelity)/[IDENT-3](#3-identities-lineage-and-replay-fidelity)
say neither reaches the wire. A producer that stamps every unit over-counts the
bill by two to three times; one that guesses positionally is not static. This is
the accounting theme's load-bearing gap, and it is an identity gap, not a usage
gap.

---

## 5. Tool I/O fidelity

`AgentActivity.item` (`agent_activity.proto:67-106`) models eleven kinds:
thinking, response, skill_use, read, write, edit, grep, glob, bash, subagent,
send_message, task_act — everything else falls to `AgentUnmodeled`
(`:1597-1662`), whose input is `google.protobuf.Struct` and whose output is
`ToolResultContent` blocks. The design record is explicit that a *recognizable*
built-in landing there is a **producer defect**
(`figma-to-idl-redesign.md:1465,2712`), so a structured vendor tool result that
can only be squashed into text is a gap, marked **CARRIED-OPAQUELY**.

Observed tool results in S5, joined back to their calling tool name: Bash 6,750 ·
Edit 1,025 · Read 890 · Agent 295 · Write 210 · Skill 96 · TaskUpdate 87 ·
ToolSearch 66 · TaskCreate 46 · AskUserQuestion 45 · SendMessage 39 ·
WebFetch 24 · TaskStop 16 · Monitor 13 · WebSearch 11 · Glob 10 ·
ScheduleWakeup 9 · Workflow 8 · BashOutput 5 · EnterWorktree 3 · TaskOutput 3 ·
Artifact 2 · ExitWorktree 1 · ListAgents 1.

**S6 absence proofs, over all 14,349 files.** `Grep`: zero. `TodoWrite`
(`oldTodos`/`newTodos`): zero. `NotebookEdit`: zero. `ExitPlanMode`: zero.
`KillShell`: zero. Any `mcp__*` tool: zero. Content blocks
`redacted_thinking`, `server_tool_use`, `web_search_tool_result`,
`web_fetch_tool_result`, `document`: zero. `type: "summary"`: zero — it has been
replaced by `ai-title` / `custom-title`, and `leafUuid` now rides `last-prompt`.
So `AgentGrep` and `AgentGlob`, which the contract models in full detail
(`agent_activity.proto:664-903`), have **no observed producer at all**, while
half the tools that do run have no arm.

| ID | Source · field | Semantics | Tier | Home today | Minimal modeling |
|---|---|---|---|---|---|
| TOOLIO-1 | `sdk-tools.d.ts:236-266` `FileReadOutput` `type:"image"` arm — `file.{base64, type, originalSize, dimensions{originalWidth, originalHeight, displayWidth, displayHeight}}` | A `Read` of an image file | **OBSERVED — 8 in S5** (`toolUseResult.type: "image"`, carrying `file.base64`, `file.type`, `file.originalSize` and all four `dimensions.*`); `classify.md:132` counts 45 on the harvest machine | **None.** `AgentReadSuccess` (`agent_activity.proto:483`) offers `whole{contents: string}` or `head{contents, total_lines}` — both string. An image Read can only be stuffed into a string | An image arm on the read extent. `AgentBashOutputImage` (`:1079`) already exists for shells; Read has no twin |
| TOOLIO-2 | `sdk-tools.d.ts:288-296` `FileReadOutput` `type:"pdf"` arm — `file.{filePath, base64, originalSize}`; input `:618` `pages?: string` | A `Read` of a PDF, with a page range | DECLARED-ONLY | None — no arm, and `pages` has no input field | An arm; a PDF read is currently unrepresentable in either direction |
| TOOLIO-3 | `sdk-tools.d.ts:275-279` `FileReadOutput` `type:"notebook"` — `file.cells: unknown[]` | A `Read` of a Jupyter notebook | DECLARED-ONLY | None | An arm |
| TOOLIO-4 | `sdk-tools.d.ts:305-317` `FileReadOutput` `type:"parts"` — `{filePath, originalSize, count, outputDir}` | A read too large to inline, split to a directory | DECLARED-ONLY | None | An arm |
| TOOLIO-5 | `sdk-tools.d.ts:326-331` `FileReadOutput` `type:"file_unchanged"` + `source?: "seeded"` | "Unchanged since you last read it" — the vendor elided the content | **OBSERVED — 1 in S5** (`toolUseResult.type: "file_unchanged"`) | None | An arm; without it an elided read looks like an empty file |
| TOOLIO-6 | `sdk-tools.d.ts:610` `FileReadInput.offset` → `:220` `FileReadOutput.file.startLine` | A read that starts **partway through** a file | **OBSERVED — S5 counts 632 `input.offset` and 645 `input.limit` among assistant tool arguments; `classify.md:125` counts 5,940 offset reads on the harvest machine** | **None, and the existing arm is dishonest.** `AgentReadHead` (`agent_activity.proto:479-488`) means "the first N lines"; a middle slice reported as `head` is a false claim | `figma-to-idl-redesign.md:3531-3542` records that a third `range` arm was **offered and declined**, reasoning that a short read is a head. That reason does not survive 5,940 real offset reads. Re-open, or amend the recorded reason |
| TOOLIO-7 | `sdk-tools.d.ts:228` `FileReadOutput.file.truncatedByTokenCap` | The read was cut by a **token** cap, not a line cap | **OBSERVED — 4 in S5** | None. `AgentReadHead.total_lines` implies a line-boundary cut | A flag or a distinct arm; a token-cap cut need not land on a line boundary |
| TOOLIO-8 | `sdk-tools.d.ts:2954-2958` `BashOutput.persistedOutputPath`, `.persistedOutputSize` | Where the shell's full output was spilled, and how big it is | **OBSERVED — 14 in S5** (`persistedOutputPath` and `persistedOutputSize` together) | Partial — the **arm** it selects is modeled (`AgentBashOutputPartial.bytes_omitted`, `:1071-1076`), the **path** is dropped with no stated reason (`B-tool-io.md:58`) | A field, or a recorded reason. A consumer offered "2.3 MB omitted" and no way to reach it is a dead end |
| TOOLIO-9 | `sdk-tools.d.ts:2914` `BashOutput.interrupted`; `:2930` `timedOutAfterMs`; `:2926` `backgroundedByUser` | How a shell ended, in the vendor's own words | **OBSERVED — `interrupted` is present on all 6,750 Bash results in S5 but `C-jsonl.md:2002` records it `false` in all 74,416 it sampled; `timedOutAfterMs` 9, `backgroundedByUser` 1** | Partial — `AgentBashInterrupted` (`:1023`) exists but has no observed producer; the timeout duration and the by-user attribution have no field | Fields on the interrupted arm; `DetachedCauseTimedOut.timeout_ms` (`detached_work.proto:83`) is the same fact one level up and equally unproduced |
| TOOLIO-10 | Detached shell spool terminator `EXIT=<code>` (`shim-sidecar/internal/handler/shell.go:15`) and the real terminators `[exited with code N]` / `[killed]` | The **exit code** of a background shell | **OBSERVED — `D-sidecar-files.md:234` measures `[exited with code N]` on 1,526 of 1,570 spools and `[killed]` on 4; my own sample finds a line-start `EXIT=` in 1 of 102 `b*` spools** | **None.** `agent_activity.proto:1020-1026` records the drop with the reason *"no producer states one for a foreground command"* — which is true of foreground and **false of detached shells**. `frontend/v1/footer.proto`'s sibling `FeedShellExit{code: int32}` (`frontend/v1/feed.proto:805-808`) declares the field and nothing fills it | Extend the recorded reason to cover detached, or add the code to the detached path. Also a live defect: the parser matches a marker that essentially no longer occurs |
| TOOLIO-11 | `sdk-tools.d.ts:2970-2996` `BashOutput.gitOperation{commit{sha,kind}, push{branch}, branch{ref,action}, pr{number,url,action}}` | The vendor **parsed the shell's git activity** into structure | **OBSERVED — S5: `gitOperation.commit` 60, `.push` 53, `.branch` 41, `.pr` 13**; corpus `COVERAGE.md` census: 696 bash results, `GitCommit` 356, `GitBranch` 222, `GitPush` 113, `GitPullRequest` 28 | None | **Recorded drop** at `figma-to-idl-redesign.md:3348-3353` ("nothing drawn consumes it"). Listed as a non-gap in [Appendix A](#appendix-a--recorded-drops-not-gaps); noted here only because the census is large enough that the reason may want revisiting |
| TOOLIO-12 | `sdk-tools.d.ts:544` `BashInput.description`; `:552` `dangerouslyDisableSandbox` | The model's own description of what a command does; and whether it ran **outside the sandbox** | **OBSERVED — S5: `input.description` 6,456, `dangerouslyDisableSandbox` 3 (input) and 1 (result); `classify.md:141` counts 68,746 and 469 on the harvest machine** | None. `AgentBashCommand` (`:946-950`) carries the command line alone | `dangerouslyDisableSandbox` is consent-relevant: a command that escaped the sandbox is indistinguishable from one that did not |
| TOOLIO-13 | `sdk-tools.d.ts:3060-3071` `FileEditOutput.gitDiff{filename, status, additions, deletions, changes, patch, repository}`; `:3041` `originalFile` | The repo-level view of an edit | **OBSERVED — S5: `originalFile`, `replaceAll` and `userModified` on all 1,025 Edit results; `staleRecovered` 19, `memdirStamped` 1** | None | **Recorded drop** at `figma-to-idl-redesign.md:3523-3526`. Non-gap; in Appendix A |
| TOOLIO-14 | `sdk-tools.d.ts:3143-3153` `GrepOutput.{mode, totalFiles, totalLines, appliedLimit, appliedOffset}` | Whether a grep result is complete | DECLARED-ONLY — **zero Grep calls in S6's 14,349 files, and zero in the 641,428-record census (`classify.md:394`)** | Partial | `B-tool-io.md:148`: `totalLines`/`totalFiles` are optional, so when absent the producer cannot honestly choose `all` vs `partial` — the extent oneof has an undetermined case. Structural, not fidelity |
| TOOLIO-15 | `sdk-tools.d.ts:3137-3141` `GlobOutput.{totalMatches, countIsComplete}` | Whether a glob result is complete | **OBSERVED and worse than declared: all 10 Glob results in S5 are a BARE STRING** — there is no structured Glob result on disk at all, so `AgentGlobSuccess.paths[]` (`:853`) has no producer either | Partial | Same defect as TOOLIO-14: with `truncated: true` and no `totalMatches`, neither `AgentGlobOmittedExact` nor `AgentGlobOmittedAtLeast` (`:890-899`) can be filled and the `omitted` oneof would be unset |
| TOOLIO-16 | `sdk-tools.d.ts:680,688,700` `GrepInput.{-i, type, multiline}` | Case-insensitivity, file-type filter, multiline mode | DECLARED-ONLY | None. `AgentGrepQuery` (`:685-696`) carries `pattern`, `path`, `glob` | Fields; case-insensitivity is invisible in rendered output |
| TOOLIO-17 | `sdk-tools.d.ts:3327-3355` `WebFetchOutput{bytes, code, codeText, result, durationMs, url, artifactRead}` | A URL fetch's HTTP status, size and body | **OBSERVED — 24 in S5** | CARRIED-OPAQUELY via `AgentUnmodeled` | An arm, or an accepted ruling. A `WebFetch` that 404'd is currently a success carrying prose |
| TOOLIO-18 | `sdk-tools.d.ts:3357-3394` `WebSearchOutput{query, results[{tool_use_id, content[{title, url}]}], durationSeconds, searchCount}` | Search results with titles and URLs | **OBSERVED — 11 in S5**, and the `results[]` array is heterogeneous: raw strings mixed with `{tool_use_id, content[{title, url}]}` objects | CARRIED-OPAQUELY | An arm; the URLs are the whole product of the call |
| TOOLIO-19 | `sdk-tools.d.ts:814-822` `TodoWriteInput.todos[]` / `:3309-3325` `TodoWriteOutput{oldTodos[], newTodos[]}` | The other task-tracker vocabulary | DECLARED-ONLY — corpus records "no TodoWrite tool on the machine"; this harness uses TaskCreate/TaskUpdate/TaskList | Partial — `AgentTaskAct` (`:154-174`) models acts, not whole-list replaces, and TodoWrite items carry **no ids** | `B-tool-io.md:285`: TodoWrite is N acts with no identities, so `AgentTaskId` cannot be minted honestly |
| TOOLIO-20 | `sdk-tools.d.ts:2509-2545` `TaskUpdateInput.status` includes `"deleted"`; `:3618-3626` `TaskUpdateOutput{success, taskId, updatedFields[], error, statusChange{from,to}}` | A task **deleted** from the tracker; and the update's own error | **OBSERVED — 87 TaskUpdate results in S5, all carrying `statusChange{from,to}` and `updatedFields[]`; `C-jsonl.md:2013` records `statusChange.to` = completed 82 / in_progress 67 / `deleted` 10 / pending 1** | None for `deleted` — `AgentTaskState.status` (`:200-216`) has pending/running/completed/failed/killed/paused and no deleted arm. `TaskUpdateOutput.error` has no home (`AgentTaskAct` has no failure) | A `deleted` arm; and an error path |
| TOOLIO-21 | `sdk-tools.d.ts:2483-2548` `TaskCreateInput`/`TaskUpdateInput.{metadata, addBlocks, addBlockedBy}`; `:3608` `TaskGetOutput.task.{blocks[], blockedBy[]}` | Task **dependency edges** and arbitrary metadata | **OBSERVED — S5 finds the whole tracker inlined in an attachment 119 times: `attachment.content[].{id, subject, description, activeForm, status, blocks[], blockedBy[]}`** | None. `AgentTaskState` (`:185-217`) has subject/description/owner/status | Repeated task ids; the tracker models a DAG and the contract models a list |
| TOOLIO-22 | `sdk-tools.d.ts:3396-3581` `AskUserQuestionOutput.{answers: map<string,string>, response?, annotations{preview, notes}, afkTimeoutMs}` | The answer map is keyed by **question text**, and the free-text `response` is **per-ask, not per-question** | **OBSERVED — 45 in S5**, `answers` keyed by the verbatim question text (dynamic keys — a schema hazard in its own right) | Partial. `AgentQuestionSelection` (`question.proto:174-195`) is per-question with `chosen[]`, `free_text` and `note` | `B-tool-io.md:254`: nothing decides which question a single `response` attaches to in a multi-question batch — the per-question `free_text` has no static producer. `chosen[]` is recovered by comma-splitting a joined string, which is lossy for labels containing commas (`classify.md:253`) |
| TOOLIO-23 | `sdk-tools.d.ts:2998-3023` `ExitPlanModeOutput{plan, isAgent, filePath, hasTaskTool, planWasEdited, awaitingLeaderApproval, requestId}`; S1 attachment `plan_mode{reminderType, isSubAgent, planFilePath, planExists}` | **Plan mode**: the plan the agent wrote and the user's approval of it | **OBSERVED — attachment `plan_mode` 1 in S1, `plan_mode_exit` in S3. The `ExitPlanMode` TOOL itself is zero across S6's 14,349 files**, so the plan surfaces only as an injected attachment | None. `AgentPermissionModePlan` (`permission.proto:240`) names the mode; nothing carries the plan or its approval | An activity kind. A plan is a first-class artifact the user reads and approves, and it is currently invisible |
| TOOLIO-24 | `sdk-tools.d.ts:727-747` `NotebookEditInput{notebook_path, cell_id, new_source, cell_type, edit_mode}` / `:3173-3213` `NotebookEditOutput` | Notebook cell edits | DECLARED-ONLY | CARRIED-OPAQUELY | `B-tool-io.md:288`: it could only ride `AgentEdit` by fabricating hunks |
| TOOLIO-25 | `sdk-tools.d.ts:712-767` MCP tool family — `ListMcpResourcesInput/Output`, `ReadMcpResourceInput/Output`, `RefreshMcpToolsOutput`, `McpInput/Output`; beta `:1318-1316` `BetaMCPToolUseBlock{server_name}` / `BetaMCPToolResultBlock{is_error}` | Any MCP tool call, and **which server** it went to | DECLARED-ONLY — corpus records no MCP servers configured during harvest, and S1 has none | CARRIED-OPAQUELY via `AgentUnmodeled` (`tool_name` would be `mcp__server__tool`) | `figma-to-idl-redesign.md` (`content_blocks.proto:4-9`) records the *content-block* collapse with a reason. The `server_name` axis is not covered by it |
| TOOLIO-26 | `sdk-tools.d.ts:3735-3770` `WorkflowOutput.{error, warning, transcriptDir}`; `:2564` `WorkflowInput.args` | A workflow that **failed to launch**, and where its transcripts went | OBSERVED — `scriptPath`, `runId`, `summary`, `transcriptDir`, `status` on S3 `tool-results/workflow_launch.jsonl` | Partial. `AgentWorkflowScriptRejected.error` (`agent.proto:258-261`) exists but `B-tool-io.md:263` notes `scriptPath` is `optional` upstream while `workflow.proto:58-63` says "ALWAYS SET" — a syntax-failed workflow has no honest shape. `transcriptDir` and `args` have no field | Make `scriptPath` optional; add `transcriptDir` |

---

## 6. Tool failure evidence

This is the register's most concentrated defect. **Every** modeled tool's
failure arm is an empty message:

`AgentThinkingFailure {}` `:330` · `AgentReadFailure {}` `:520` ·
`AgentWriteFailure {}` `:632` · `AgentEditFailure {}` `:683` ·
`AgentGrepFailure {}` `:835` · `AgentGlobFailure {}` `:931` ·
`AgentBashFailure {}` `:1117` · `AgentSubagentFailure {}` `:1376` ·
`AgentSkillUseFailure {}` `:1476` · `AgentSendMessageFailure {}` `:1581`
(all `agent_activity.proto`), plus `AgentQuestionFailure {}`
(`question.proto:222`) and `AgentPermissionFailure {}` (`permission.proto:326`).

Each carries the comment *"Arms are DERIVED from the producer's real refusal
sites at the implementation wave; empty until then."* That is a deferral, not a
drop — so the gap is open, and this audit is the pass that was meant to close
it. Only `AgentUnmodeledFailure` (`:1657-1662`) carries anything, and it carries
the full `ToolResultContent`.

| ID | Source · field | Semantics | Tier | Home today | Minimal modeling |
|---|---|---|---|---|---|
| TOOLFAIL-1 | `message.content[].is_error: true` on a `tool_result` block (beta `:2608` `BetaToolResultBlockParam.is_error`) plus the block's `content` | **The error text of a failed tool call** | **OBSERVED — S5: `is_error` is `true` on 408 tool_result blocks, `false` on 6,880, absent on 3,011** | **None for any modeled tool.** The text exists on the wire and every modeled failure arm is empty | One text field per failure arm, or a shared `AgentToolFailure{ToolResultContent}`. `frontend/v1/feed.proto:466-470` already declares `FeedSkillFailed{text}` and nothing can fill it |
| TOOLFAIL-2 | S1 `toolDenialKind` — `user-rejected`, `automode-blocked`, `permission-rule`, `automode-unavailable` | Why a tool call was refused before running | **OBSERVED — S5: 56 total, all four values — `user-rejected` 28, `automode-blocked` 15, `automode-unavailable` 10, `permission-rule` 3** | Partial. `AgentPermissionDeniedByUser` and `…ByPolicy` (`permission.proto:294-320`) cover the first three by free-text. **`automode-unavailable` ("the model is temporarily unavailable") is neither user nor policy** and has no arm | A third denial arm, or an enum |
| TOOLFAIL-3 | `sdk.d.ts:4168-4188` `SDKPermissionDeniedMessage{tool_name, tool_use_id, agent_id, decision_reason_type, decision_reason, message}`; `:3610` `decision_reason_type` — 11 literals (`rule`, `mode`, `subcommandResults`, `permissionPromptTool`, `hook`, `asyncAgent`, `sandboxOverride`, `workingDir`, `safetyCheck`, `classifier`, `other`) | **Which mechanism** denied a call | DECLARED-ONLY on the live surface | Partial and mismatched. `AgentPermissionDeniedByPolicy.decider` (`permission.proto:315`) is a **required** string while the SDK field is optional — a producer must synthesize an empty-string sentinel (`A-sdk-stream.md:357`) | Make `decider` optional, and make it an enum over the 11 literals |
| TOOLFAIL-4 | `sdk.d.ts:4313` `SDKResultSuccess.permission_denials: SDKPermissionDenial[]` → `:4159-4162` `{tool_name, tool_use_id, tool_input}` | The turn's roll-up of everything that was denied | OBSERVED — `permission_denials` on S3 `stream/result_success.jsonl` | None | A repeated field on the turn's conclusion; today a denial is only visible if its own unit was seen |
| TOOLFAIL-5 | `sdk-tools.d.ts:3618-3624` `TaskUpdateOutput.error`; `:3735-3770` `WorkflowOutput.error`; `sdk.d.ts:4534` `SDKTaskUpdatedMessage.patch.error` | The error string a task or workflow update returned | OBSERVED for the disk twin (`classify.md:248`); `TaskUpdate` results are 87 in S5 | None. `AgentTaskAct` has no failure arm at all; `AgentWorkflowFailure` has `script_rejected` and `run_ended` only | An error path on task acts; `patch.error` is the natural `AgentSubagentFailure` payload and that arm is empty |
| TOOLFAIL-6 | S1 `isApiErrorMessage` + the synthesized text | An assistant message that is really an **error notice** ("API Error: Connection closed mid-response", "You've hit your monthly spend limit") | **OBSERVED — S5: `isApiErrorMessage` 63 (12 of them `true`), `apiErrorStatus` 5.** A true one is unmistakable in shape — `message.model: "<synthetic>"`, zeroed usage, `stop_reason: "stop_sequence"` — and indistinguishable in *type* from a real answer | None. It arrives as ordinary assistant prose and would be drawn as the agent's answer | A discriminator. `sdk.d.ts:7025-7043` gives the vendor's own prefix taxonomies (`USAGE_LIMIT_ERROR_PREFIXES` 12 entries, `USAGE_TRANSITION_PREFIXES` 6, `USAGE_WARNING_PREFIXES` 2) — string-prefix matching is the vendor's own mechanism and there is nothing to match into |
| TOOLIO-27 | `BashOutput` tool result — `shellId`, `command`, `status`, `stdout`, `stderr`, `stdoutLines`, `stderrLines`, `timestamp` | **Polling a running background shell** | **OBSERVED — 5 in S5** | None. `AgentBashUpdate` (`:975-990`) carries `new_output` + `from_offset` and is fed from the spool, not from this tool | An arm, or a ruling that the poll is invisible and only the spool speaks. `classify.md:290` notes `toolUseResult.exitCode` exists here, **contradicting** the record's premise at `agent_activity.proto:1024` that no producer states an exit code |
| TOOLIO-28 | `sdk-tools.d.ts:3646-3666` `ScheduleWakeupOutput{scheduledFor, clampedDelaySeconds, wasClamped, stopped, cancelledWakeups}`; `:3668` `MonitorOutput{taskId, timeoutMs, persistent}`; `:3155-3171` `TaskStopOutput`; `:3215-3260` MCP resource outputs; `sdk-tools.d.ts:393-420` `ArtifactOutput` | The long tail of built-ins with structured results and no arm | **OBSERVED — S5: Monitor 13, ScheduleWakeup 9, TaskStop 16, ToolSearch 66, TaskOutput 3, EnterWorktree 3, Artifact 2, ExitWorktree 1, ListAgents 1** | CARRIED-OPAQUELY | `B-tool-io.md:275-313` enumerates 36 declared built-ins with no arm and rules 33 of them producer defects by the arm's own standard. **`Monitor` is the sharpest**: it is detached work with a `taskId`, and `DetachableWork` (`detached_work.proto:105-117`) has no `monitor` arm, so it can never be announced as detached |

---

## 7. Session inventory and environment

`SessionStarted` (`session.proto:23-50`) carries seven things:
`vendor_session_id`, `runtime`, `effective_model`, `permission_mode`,
`model_catalog`, `turn_in_flight`, `live_work`. `SDKSystemMessage`
(`sdk.d.ts:4412-4455`) — the vendor's own handshake, and the first thing every
session emits — carries seventeen. The overlap is three.

| ID | Source · field | Semantics | Tier | Home today | Minimal modeling |
|---|---|---|---|---|---|
| SESS-1 | `sdk.d.ts:4420` `SDKSystemMessage.tools: string[]` | **Which tools this session actually has** | OBSERVED — S3 `stream/system_init.jsonl` | None | A repeated string. `AgentUnmodeled` exists to catch tools the contract does not model; nothing says which tools were even available |
| SESS-2 | `sdk.d.ts:4432` `.skills: string[]`; S1 attachment `skill_listing{names[], skillCount, content, isInitial}` | The session's skill inventory | **OBSERVED — `skill_listing` attachment 184 in S5, 333 in S1**; also `stream/system_init.jsonl` | None | A repeated string; `AgentSkillUseFailure` ("commonly an unknown name, or one filtered out of the session's enabled set", `:1474`) names a set nothing carries |
| SESS-3 | `sdk.d.ts:4415` `.agents: string[]`; `:3450` `SDKControlInitializeResponse.agents: AgentInfo[]` → `:105-117` `{name, description, model}`; S1 attachment `agent_listing_delta{addedTypes[], addedLines[], isInitial, showConcurrencyNote}` | The **subagent catalog** | **OBSERVED — `agent_listing_delta` 76 in S5, 324 in S1** | None. `AgentSubagentPrompt.subagent_type` (`:1171`) is a bare string with no catalog to validate against | A repeated `{name, description, model}`; the frontend has no way to offer a subagent picker |
| SESS-4 | `sdk.d.ts:4430` `.slash_commands: string[]`; `:2935-2938` `SDKCommandsChangedMessage.commands: SlashCommand[]` → `:6641-6657` `{name, description, argumentHint, aliases[]}` | The session's **command catalog**, and changes to it | OBSERVED — `slash_commands[]` on S3 `stream/system_init.jsonl` | Partial and contradicted. `SessionCommand` (`slash_command.proto:84-121`) is a closed enum of 30 hardcoded literals. It cannot express a plugin-supplied command, a `description`, an `argumentHint`, or **aliases** — and `A-sdk-stream.md:348` notes `SlashCommand.aliases` directly contradicts the one-literal-per-command design | A catalog message; the enum is a static guess at a dynamic set |
| SESS-5 | `sdk.d.ts:4433-4441` `.plugins: {name, path, version}[]`; `:3688-3698` `SDKControlReloadPluginsResponse{plugins[], mcpServers[], error_count}`; `sdk.d.ts:4214-4219` `SDKPluginInstallMessage{status, name, error}` | Installed plugins and their install/reload outcomes | OBSERVED — `plugins[]` on S3 `stream/system_init.jsonl`; `attributionPlugin` 176 in S5 | None | A repeated `{name, version}`; a plugin that failed to install is silent |
| SESS-6 | `sdk.d.ts:4431` `.output_style: string`; `:3451-3452` `output_style` + `available_output_styles[]` | The active output style | OBSERVED — `output_style` on S3 `stream/system_init.jsonl` | None. `SESSION_COMMAND_OUTPUT_STYLE` (`slash_command.proto:108`) is a literal with no state | A field on `SessionStarted` and an arm on `SessionUpdate` |
| SESS-7 | `sdk.d.ts:4416` `.apiKeySource: ApiKeySource` (`:124` — `user\|project\|org\|temporary\|oauth`); `:23-32` `AccountInfo{email, organization, subscriptionType, tokenSource, apiKeySource, apiProvider}` (`:32` `apiProvider` — 8 literals) | **Which account and which provider** the session is billed to | OBSERVED — `apiKeySource` on S3 `stream/system_init.jsonl` | Partial. `SessionAccountUsage.subscription_type` (`session.proto:263`) carries one of the six | `E-control-surface.md:244`: `apiProvider` determines whether account usage can *ever* be available — it would explain `SessionUsageServiceUnavailable` (`session.proto:300`) structurally instead of leaving it a bare arm |
| SESS-8 | `sdk.d.ts:4450` `.capabilities?: string[]` — documented values `interrupt_receipt_v1`, `interrupt_cancel_queued_v1` | **What this CLI build can do** | OBSERVED — `capabilities[]` on S3 `stream/system_init.jsonl` | None | `E-control-surface.md:240`: the design record relies on these capabilities (`figma-to-idl-redesign.md:2860,2872`) and no field carries what the binary reports |
| SESS-9 | `sdk.d.ts:4417` `.betas?: string[]`; `:2930` `SdkBeta` (`context-1m-2025-08-07`) | Which beta features are on — including the **1M context window** | OBSERVED | None | A field; the active context window is a beta flag away from a different number, and [USAGE-3](#4-usage-cost-and-accounting) already has no producer |
| SESS-10 | `sdk.d.ts:1224-1260` `ModelInfo.{resolvedModel, supportsEffort, supportedEffortLevels[], supportsAdaptiveThinking, supportsFastMode, supportsAutoMode}` | Per-model **capability flags** | DECLARED-ONLY | Partial. `ModelOption` (`api.proto:115-122`) carries `model`, `display_name`, `description` and nothing else | `supportsFastMode` and `supportsAutoMode` gate `SessionFastMode` (`session.proto:220`) and `AgentPermissionModeAuto` (`permission.proto:244`) per model. Offering a mode the selected model cannot do is a UI defect the contract makes unavoidable |
| SESS-11 | `sdk.d.ts:1075-1114` `McpServerStatus{name, status, serverInfo, error, config, scope, tools[]}`; `:1083` status — `connected\|failed\|needs-auth\|pending\|disabled` | MCP server health, in five states | DECLARED-ONLY (no MCP servers configured during harvest); the *disk* twins are **OBSERVED — S5: `attachment.needsAuthMcpServers[]` 69, `attachment.pendingMcpServers[]` 129** | Partial. `SessionMcpServer` (`session.proto:237-245`) has `connected` and `failed` only. `needs-auth`, `pending`, `disabled` have no arm, and **`SessionMcpServerFailed.error` has no producer** — `init` carries no error text | Three arms; and `needs-auth` is user-actionable (it means "go authenticate"), so its absence is a functional hole |
| SESS-12 | `sdk.d.ts:640-635` `FastModeState` (`off\|cooldown\|on`) and `FastModeDisabledReason` (10 literals) | Fast mode's third state, and why it is off | OBSERVED — `fast_mode_state` on S3 `stream/system_init.jsonl` and `stream/result_success.jsonl` | Partial. `SessionFastMode` (`session.proto:220-234`) has `on` and `off{reason: string}` | **`cooldown` has no arm.** The `reason` is kept as a verbatim vendor string by a recorded ruling (`figma-to-idl-redesign.md:1822`), so only the missing third state is a gap |
| SESS-13 | S5 line types `mode` and `permission-mode` | The session's mode, written to disk as its own line kind | **OBSERVED — S5: `mode` 2,669 (always `normal`), `permission-mode` 1,718 (`auto` 1,664 / `bypassPermissions` 53 / `default` 1)** | Partial. `permissionMode` maps to `AgentPermissionMode`; the separate `mode` line has no home | A ruling on what `mode` is; it may be vendor-specific residue |
| SESS-14 | `sdk.d.ts:4384-4396` `SDKSettingsParseError{file, path, message}`; `:2267` `ProvenanceEntry{source, path, policyOrigin}`; `:2628` `ResolvedSettingSource` | A **broken settings file**, and where a setting came from | DECLARED-ONLY | None. `AgentPermissionAskRule.source` (`permission.proto:116`) is a bare string | A session diagnostic; `SessionFault` (`session.proto:340-347`) is the shim's own health, not the vendor's config |
| SESS-15 | `sdk.d.ts:3997-4014` `SDKMemoryRecallMessage{mode: select\|synthesize, memories[{path, scope: personal\|team\|organization, content}]}`; S1 attachment `nested_memory{path, displayPath, content}` | **Which memory files were pulled into context** | **OBSERVED — `nested_memory` attachment 41 in S5**, carrying `content.contentDiffersFromDisk` | None | See [ATTACH-6](#9-attachments-and-injected-context); the live channel and the disk channel are both unmodelled |

---

## 8. Hooks

Hooks are the single most voluminous thing on disk — **35,622 of 92,211 S5
records (39%) are `hook_success` attachments** — and `conversation.v1` mentions
hooks exactly once, as the `/hooks` command literal (`slash_command.proto:107`).
`frontend/v1/footer.proto:301-303` declares `FooterStatusActivityHook{name}`,
which nothing can fill.

| ID | Source · field | Semantics | Tier | Home today | Minimal modeling |
|---|---|---|---|---|---|
| HOOK-1 | S1 attachment `hook_success{hookName, hookEvent, command, exitCode, durationMs, stdout, stderr, content, toolUseID}` | A hook ran and succeeded, with its full output | **OBSERVED — 35,622 in S5** | None | An activity kind or a session event. This is the largest single unmodelled record class in the corpus |
| HOOK-2 | S1 attachments `hook_blocking_error{blockingError{blockingError, command}, hookName, hookEvent, toolUseID}`, `hook_non_blocking_error{command, exitCode, stdout, stderr, durationMs, …}`, `hook_cancelled{hookName, hookEvent, toolUseID}` | A hook **blocked a tool call**, failed without blocking, or was cancelled | **OBSERVED — S5: `hook_non_blocking_error` 278, `hook_blocking_error` 103, `hook_cancelled` 12, plus a 24th attachment kind `hook_system_message` 1** | None | A blocking hook is a *refusal the user must understand*; it is currently indistinguishable from the tool simply not running |
| HOOK-3 | S1 `system/stop_hook_summary{hookCount, hookInfos[{command, durationMs}], stopReason, hasOutput, preventedContinuation, hookErrors[], hookAdditionalContext[], toolUseID, level}` | The **Stop hook roll-up** — including whether a hook prevented the agent from continuing | **OBSERVED — 953 in S5, each carrying all ten fields; `hookErrors[]` is non-empty in 25 of them** | None | See also [STOP-11](#1-stop-refusal-and-termination-taxonomy). `TerminalReason` has `stop_hook_prevented` and `hook_stopped` (`sdk.d.ts:6909`), so the vendor models this as a turn-ending cause |
| HOOK-4 | `sdk.d.ts:3929-3934` `SDKHookStartedMessage{hook_id, hook_name, hook_event}`; `:3901-3909` `SDKHookProgressMessage{…, stdout, stderr, output}`; `:3914-3924` `SDKHookResponseMessage{…, exit_code, outcome: success\|error\|cancelled}` | The **live** hook lifecycle | OBSERVED — S3 `stream/hook_started.jsonl`, `stream/hook_response.jsonl` | None | The live twin of HOOK-1/2. `FooterStatusActivityHook{name}` exists and is unfillable |
| HOOK-5 | `sdk.d.ts:816-835` `HOOK_EVENTS` / `HookEvent` — 31 literals (`PreToolUse` … `MessageDisplay`) | The hook event vocabulary | **OBSERVED for six of the 31 — S5 `attachment.hookEvent`: `PreToolUse` 31,147, `Stop` 3,779, `PostToolUse` 951, `SessionStart` 70, `PostToolUseFailure` 43, `UserPromptSubmit` 26** | None | An enum; `hookEvent` is currently a free string with no home to be free in |
| HOOK-6 | `sdk.d.ts:841` `HookPermissionDecision` (`allow\|deny\|ask\|defer`); `Options.hooks` includes a **`PermissionRequest`** hook | A hook that **answers a permission prompt** — a second permission path beside `canUseTool` | DECLARED-ONLY | None | `E-control-surface.md:240` flags this as a path the shim must not wire. Recorded here because if it is ever wired, `AgentPermissionDeniedByPolicy.decider` is the only place it could surface and `decision_reason_type: 'hook'` (`sdk.d.ts:3610`) is its name |

---

## 9. Attachments and injected context

**Attachments are the largest single record class** — 40,254 of 92,211 S5
records (44%) — and
`conversation.v1` has no attachment concept at all. `UserContentBlock`
(`user.proto:36-46`) is `text | image | unsupported`; an attachment is a
distinct top-level line type (`"type": "attachment"`), not a content block, so
even `UnsupportedBlock` is not its home. Every one of these lands in
`store.v1 StoreUnknown` — **RESIDUE-ONLY**, and therefore invisible to the
daemon by construction (`store/v1/store.proto:5-19`).

The complete `attachment.type` set in S5 is **24 values**: `hook_success`
35,622 · `total_tokens_reminder` 1,519 · `task_reminder` 1,127 ·
`diagnostics` 467 · `hook_non_blocking_error` 278 · `edited_text_file` 224 ·
`skill_listing` 184 · `deferred_tools_delta` 179 · `queued_command` 173 ·
`command_permissions` 107 · `hook_blocking_error` 103 ·
`agent_listing_delta` 76 · `compact_file_reference` 47 · `nested_memory` 41 ·
`file` 39 · `date_change` 29 · `hook_cancelled` 12 · `context_tip` 11 ·
`read_truncation_notice` 5 · `invoked_skills` 4 · `auto_mode` 4 ·
`ultrathink_effort` 1 · `task_status` 1 · `hook_system_message` 1. S1 and the
corpus add five more: `dynamic_skill`, `plan_mode`, `plan_mode_exit`,
`structured_output`, `ultra_effort_enter`/`ultra_effort_exit`,
`silent_turn_reminder`, `auto_mode_exit` — **29 distinct kinds, none modelled**.

The hook attachments are counted under [theme 8](#8-hooks). The rest:

| ID | Source · field | Semantics | Tier | Home today | Minimal modeling |
|---|---|---|---|---|---|
| ATTACH-1 | `command_permissions` attachment | **The tool allowances a skill was granted** | **OBSERVED — 107 in S5**, carrying `allowedTools[]` | Partial and positional. It is the recorded producer for `AgentSkillAllowedTools` (`:1436-1444`) but carries **no `sourceToolUseID`**, so the link to its skill is positional — `B-tool-io.md:218` and `classify.md:381` both flag it NON-STATIC | Either the vendor adds the correlation, or the contract acknowledges the allowance list cannot be attributed |
| ATTACH-2 | `diagnostics` attachment — `files[{uri, diagnostics[{code, message, severity, source, range{start{line,character}, end{…}}}]}], isNew` | **LSP diagnostics** injected after an edit | **OBSERVED — 467 in S5**, carrying full LSP ranges and `severity` | None | An activity kind. This is the compiler telling the agent it broke the build, and it is invisible to the user watching |
| ATTACH-3 | `edited_text_file{filename, snippet}` | The user edited a file **out of band**, mid-turn | **OBSERVED — 224 in S5** | None. `AgentEditSuccess.user_modified` (`:649`) is a bool about a *tool* edit | A session event; a user edit that changed what the agent is working on is a first-class event |
| ATTACH-4 | `file{filename, displayPath, content{file{content, filePath, numLines, startLine, totalLines}}}` | **A file the user attached** with `@` | **OBSERVED — 39 in S5** | None. `UserContentBlock` has `text \| image \| unsupported`, and `content_blocks.proto:79-91` forbids `UnsupportedBlock` for a knowable kind | A file arm on `UserContentBlock` |
| ATTACH-5 | `queued_command{prompt (string or blocks), commandMode, origin{kind}, timestamp}` | A prompt the user typed **while the agent was busy** | **OBSERVED — 173 in S5**, carrying `commandMode` and `origin.kind`; both `prompt` arms occur (string 171, blocks 2) | None | agent-repl's own held-prompt machinery (`agentrepl/v1/endpoint_update_held_prompt.proto`) is a *different* queue; the vendor's own queued commands are unrepresented |
| ATTACH-6 | `nested_memory{path, displayPath, content{path, content}}`, `dynamic_skill{skillDir, displayPath, skillNames[]}`, `invoked_skills{skills[{name, path, content}]}` | Context **silently pulled in**: memory files, dynamically discovered skills, skills invoked without a tool call | **OBSERVED — S5: `nested_memory` 41, `invoked_skills` 4; `dynamic_skill` 1 in S1** | None. `AgentSkillUse` requires a tool call; `invoked_skills` records skills that had none | An arm. `figma-to-idl-redesign.md:2822-2837` reasons that nothing delimits a skill's scope; `invoked_skills` and `attributionSkill` are both evidence against that premise |
| ATTACH-7 | `total_tokens_reminder{text}` | The vendor's own **context-budget warning**, injected into the prompt | **OBSERVED — 1,519 in S5** | None | A session event; it is the vendor's own signal that the window is filling |
| ATTACH-8 | `auto_mode`, `auto_mode_exit`, `ultrathink_effort`, `ultra_effort_enter`, `ultra_effort_exit`, `plan_mode`, `plan_mode_exit`, `context_tip{tip{tip, action, featureId}}`, `date_change{newDate}`, `read_truncation_notice{banner, toolUseID}`, `compact_file_reference{filename, displayPath}`, `task_reminder{itemCount}`, `structured_output{data, toolUseID}`, `silent_turn_reminder`, `frame-link{frameUrl, path}`, `pr-link{prNumber, prRepository, prUrl}` | **Mode transitions and injected notices** — entering/leaving auto mode, effort escalation, plan mode, a date rollover, a truncation banner, a PR the agent opened | **OBSERVED — S5: `date_change` 29, `context_tip` 11, `read_truncation_notice` 5, `auto_mode` 4 (carrying `autoModeConsentFlow`, `bashFirst`, `bypass`, `steerOnly`), `ultrathink_effort` 1, `task_status` 1; `pr-link` line type 1,007; `frame-link` 2** | None for any of them | Mode transitions are the ones that matter: `SessionPermissionModeChanged` (`session.proto:208-211`) covers the permission mode, but effort escalation ([IDENT-14](#3-identities-lineage-and-replay-fidelity)) and auto-mode entry/exit have no arm |
| ATTACH-9 | `deferred_tools_delta{addedNames[], addedLines[], removedNames[], readdedNames[], needsAuthMcpServers[]}` | **The tool set changed mid-session** — tools deferred out and back in | **OBSERVED — 179 in S5**, carrying `addedNames[]`, `removedNames[]`, `readdedNames[]`, `addedTypes[]`, `removedTypes[]` | None | Compounds [SESS-1](#7-session-inventory-and-environment): not only is the tool set uncarried, its *changes* are too |

---

## 10. Compaction and context cuts

`ContextCompacted` (`slash_command.proto:160-168`) carries a summary and a
`ContextTokenDelta{tokens_before, tokens_after}`. The vendor's own compaction
record carries nine more facts, and the design record already owes a vetting
item on exactly this: *"whether our bookkeeping survives the vendor's
`compact_boundary` format"* (`figma-to-idl-redesign.md:1861-1865`). It does not.

| ID | Source · field | Semantics | Tier | Home today | Minimal modeling |
|---|---|---|---|---|---|
| COMPACT-1 | `sdk.d.ts:2947` `compact_metadata.trigger: 'manual' \| 'auto'` | Whether the user asked for the compaction or the vendor did it **on its own** | **OBSERVED — 24 in S5: `manual` 21, `auto` 3** | None. `ContextCompacted` has no trigger | An arm. An auto-compaction is something that *happened to* the user and a manual one is something they did; drawing them identically is wrong |
| COMPACT-2 | `sdk.d.ts:2951` `.duration_ms` | How long the compaction took | **OBSERVED — 24 in S5** | None | A field; compaction is the slowest thing a session does |
| COMPACT-3 | S5 `compactMetadata.cumulativeDroppedTokens` | How much context has been **discarded across the session's whole life** | **OBSERVED — 24 in S5** | None. `ContextTokenDelta` is per-cut | A field; it is the only figure that says how much of the conversation is gone |
| COMPACT-4 | `sdk.d.ts:2959-2971` `.preserved_segment{head_uuid, anchor_uuid, tail_uuid}` and `.preserved_messages{anchor_uuid, uuids[], allUuids[]}` | **Exactly which messages survived** the cut | **OBSERVED — 24 in S5** | None | Needs [IDENT-1](#3-identities-lineage-and-replay-fidelity). Without it a consumer cannot tell a surviving message from a dropped one, so a feed cannot grey out what the model can no longer see |
| COMPACT-5 | S5 `compactMetadata.preCompactDiscoveredTools[]` | The tool set as it stood before the cut | **OBSERVED — 11 in S5** | None | Compounds [SESS-1](#7-session-inventory-and-environment) |
| COMPACT-6 | S5 `logicalParentUuid` | The **real** pre-compaction parent — a `compact_boundary` deliberately nulls `parentUuid` and moves the link here | **OBSERVED — 24 in S5, appearing on `compact_boundary` records and nowhere else** | None | The one place the vendor *does* put ancestry on the record, and the flat model has nowhere to receive it. Without it a compacted session's history is two disconnected components |
| COMPACT-7 | S5 `isCompactSummary` + `isVisibleInTranscriptOnly` | Marks the user record that **is** the summary | **OBSERVED — 24 each in S5, co-occurring 1:1** | Partial — recorded as the producer for `ContextCompacted.summary`, but `A-sdk-stream.md:101` finds `isCompactSummary` is **undeclared on the SDK surface**, so the live plane has no equivalent and the sidecar must resolve it by reading the *next* line (`convert.go:179`) | Either a live discriminator or an accepted file-plane-only rule |
| COMPACT-8 | `sdk.d.ts:4406-4407` `SDKStatusMessage.compact_result: 'success' \| 'failed'`, `.compact_error: string` | A compaction that **failed** | DECLARED-ONLY | None. `ContextCut` (`slash_command.proto:126-133`) has `cleared \| compacted` and no failure arm | A failure arm. Compaction is daemon-directed by design (`figma-to-idl-redesign.md:590-596`), which makes a failed one the daemon's problem to report and it has no vocabulary |
| COMPACT-9 | beta `:918-941` `BetaContextManagementResponse.applied_edits[]` → `clear_tool_uses_20250919{cleared_input_tokens, cleared_tool_uses}` and `clear_thinking_20251015{cleared_input_tokens, cleared_thinking_turns}`; beta `:777-786` `BetaCompactionBlock{content, encrypted_content}`; beta `:812-818` `BetaCompactionContentBlockDelta` | **Server-side context editing** — the API silently dropping tool uses and thinking turns from the context, reported per response | **OBSERVED — `message.context_management` present on 63 assistant records in S5** (`applied_edits[]` empty in the 8 sampled); `classify.md:74` counts 31 non-empty on the harvest machine | None. `ContextCut` models *our* cuts; this is a cut nobody asked for | An arm. Tokens vanish from context between turns and no surface can say so |

---

## 11. Subagents and detached work

| ID | Source · field | Semantics | Tier | Home today | Minimal modeling |
|---|---|---|---|---|---|
| AGENT-1 | `sdk-tools.d.ts:146-176` `AgentOutput` `status: "async_launched"` arm — `outputFile`, `canReadOutputFile`, `isAsync` | Where a **backgrounded** subagent's output is being written | **OBSERVED — S5: 295 Agent results, `status: async_launched` 280 of them, all carrying `outputFile` and `canReadOutputFile`** | None. `AgentSubagentStart` (`:1161-1196`) carries the prompt and the created agent id; `AgentDetachedWork` (`detached_work.proto:21-37`) carries an id and an origin. Neither carries the path | A field. The spool is the *only* place a detached subagent's work exists, and nothing names it |
| AGENT-2 | `sdk-tools.d.ts:178-199` `AgentOutput` `status: "remote_launched"` arm — `taskId`, `sessionUrl` | A subagent running **on another machine**, reachable by URL | DECLARED-ONLY | None. `AgentSubagentIsolationRemote` (`:1209`) is an **empty message**, while `AgentWorkflowPlacementRemote.session_url` (`workflow.proto:79-85`) carries exactly this fact one level up | Add `session_url`; the asymmetry is unexplained and `B-tool-io.md:210` flags it |
| AGENT-3 | `sdk.d.ts:4476-4492` `SDKTaskProgressMessage{usage{total_tokens, tool_uses, duration_ms}, last_tool_name, summary}` vs `AgentSubagentTotals.usage: TokenUsage` (`:1309`) | Mid-run and final subagent accounting | **OBSERVED — S5: 23 completed Agent results carry a full `usage.*` block mirroring `message.usage`; the async ones carry only `totalTokens`** | Mismatched. `AgentSubagentTotals.usage` is a full `TokenUsage` breakdown; the async path supplies **one scalar** | `A-sdk-stream.md:305` and `classify.md:381`: `AgentSubagentTotals.usage` **cannot be filled** for an async spawn. Either make it optional or add a scalar arm |
| AGENT-4 | `sdk-tools.d.ts:139` `AgentOutput.toolStats.frameCount`; `:102` `.agentType` (the *actual* type, vs the requested one) | Two subagent totals with no field | DECLARED-ONLY for `frameCount`; **`agentType` OBSERVED on all 23 completed results in S5** | Partial. `AgentSubagentToolStats` (`:1318-1335`) has seven counters, not `frameCount`. `AgentSubagentPrompt.subagent_type` is the *requested* type | A field; a spawn that resolved to a different agent type than asked for is silently wrong |
| AGENT-5 | `sdk.d.ts:4529-4536` `SDKTaskUpdatedMessage.patch{status: pending\|running\|completed\|failed\|killed\|paused, description, end_time, total_paused_ms, error, is_backgrounded}` | The **background-task** state machine | DECLARED-ONLY on the live surface | Mismatched. `AgentTaskState.status` (`:200-216`) took its six arms from *this* type — but `AgentTaskAct` models the **task tracker**, a different thing. `B-tool-io.md:239` shows the consequence: `AgentTaskFailed`, `AgentTaskKilled` and `AgentTaskPaused` have **no tracker producer**, and the tracker's own `deleted` has no arm | Separate the two state machines, or record why they share arms |
| AGENT-6 | `sdk.d.ts:2915-2925` `SDKBackgroundTasksChangedMessage.tasks[{task_id, task_type, description}]` | The **current set of live background tasks**, relayed whole | OBSERVED — S3 `stream/background_tasks_changed.jsonl` (2 tasks) | None. `SessionUpdate` (`session.proto:152-174`) has seven arms and none of them is this | `figma-to-idl-redesign.md:306-318` Ruling 2 says the level is "relayed verbatim as a session fact" — **and no arm exists to relay it into**. `A-sdk-stream.md:324` flags the same contradiction. This is a recorded ruling with no implementation surface |
| AGENT-7 | S5 `system.pendingBackgroundAgentCount` (306) and `.pendingWorkflowCount` (11) | How many detached things are outstanding, as a **count** | **OBSERVED — 306 and 11 in S5** | None, and a count cannot fill what exists: `TurnLive.live_work` (`turn.proto:71-74`) and `SessionLive.live_work` (`session.proto:405-410`) want **ids** | Either ids from another producer, or accept the count is unusable |
| AGENT-8 | S5 `system/agents_killed` (2) | Subagents were killed | **OBSERVED — 2 in S5** | None, and it carries **no ids**, so it cannot fill `TurnKilledForced.stopped_work` (`turn.proto:65-68`) or `SessionKilledForced.stopped_work` (`session.proto:400`) either | The record exists and the contract's field is unfillable from it |
| AGENT-9 | `sdk.d.ts:4564-4571` `SDKToolProgressMessage.subagent_retry{agent_id, attempt, max_retries, retry_delay_ms, error_status, error_category}` | A subagent that **failed and is being retried** | DECLARED-ONLY | None. `AgentSubagentUpdate` (`:1214-1229`) carries progress, a note and an activity label. There is **no retry count anywhere in the contract** | Fields; `wf_*.json`'s `workflowProgress[].attempt` (S4) is the same fact from the file plane and equally unmodelled |
| AGENT-10 | `sdk-tools.d.ts:2425-2439` `SendMessage` result — `pin{id, name, ref}`, `resumedAgentId` | Which agent a message was pinned to and which was resumed | **OBSERVED — S5: 39 SendMessage results, `pin.{id,name,ref}` on 34, `resumedAgentId` on 23** | Partial. `AgentSendMessageSuccess.recipient_agent_id` (`:1523`) plus a `resumed_recipient` arm. `pin.name` and `pin.ref` are **recorded drops** (`figma-to-idl-redesign.md:2766-2771`); `pin.id` vs `resumedAgentId` divergence is not — `B-tool-io.md:228` flags that if they differ, the difference is lost | Confirm they are the same id space |
| AGENT-11 | S5 `origin{kind: peer, from, name, fromSession, senderTaskId, body}` on a user record (`sdk.d.ts:4030-4052`) | A `SendMessage` **arriving** at the recipient | **OBSERVED — S5: `attachment.origin.{body, from, name, senderTaskId}` 1; `origin.kind` 936 overall with `coordinator` 1** | None. `UserSaid` (`user.proto:22-26`) has content and nothing else — no sender | The send side is modelled in detail (`AgentSendMessage`, `:1463-1581`) and the receive side is anonymous prose |
| AGENT-12 | `sdk.d.ts:4471`/`4517` `skip_transcript` on task-started and task-notification | The vendor says **do not show this subagent's transcript** | DECLARED-ONLY | None | `figma-to-idl-redesign.md:2931` rules it "rides the stream as a RENDERING property" and `A-sdk-stream.md:284` finds **no field carries it**. A recorded ruling with no field |
| AGENT-13 | `sdk.d.ts:3438` control `initialize.forwardSubagentText` | Whether subagent prose reaches us at all | DECLARED-ONLY | N/A — an option, not data | Recorded **OWED** at `figma-to-idl-redesign.md:3057-3059`: *"without which a subagent's prose and reasoning never reach us at all."* Counted because every subagent-content gap above is downstream of it |

---

## 12. Journals, meta files and spools

The sidecar knows four file kinds (`shim-sidecar/internal/tail/context.go:9-13`).
Three findings here are not modeling gaps at all but **files that exist and are
never read**, which is a stronger form of the same problem: the fact never
reaches `conversation.v1` because nothing opens the file.

| ID | Source · field | Semantics | Tier | Home today | Minimal modeling |
|---|---|---|---|---|---|
| WFLOW-1 | `projects/<p>/<s>/workflows/wf_<id>.json` — `runId`, `taskId`, `workflowName`, `scriptPath`, `script` (the full JS source), `status`, `summary`, `result`, `startTime`, `timestamp`, `durationMs`, `agentCount`, `totalTokens`, `totalToolCalls`, `defaultModel`, `phases[{title, detail}]`, `logs[]` | The **workflow run manifest** | **OBSERVED — all 57 files on disk (S4)** | **None — and it is not even discovered.** `discover.go:83-89` globs `subagents/workflows/wf_*/journal.jsonl` and never `workflows/wf_*.json`. No Go, TS or elisp path reads it | Discovery first, then fields. `AgentWorkflowCompleted` (`agent.proto:232-236`) carries an optional summary and **no totals at all** |
| WFLOW-2 | `wf_<id>.json` → `workflowProgress[]` `type: "workflow_agent"` — `label`, `agentId`, `model`, `state`, `startedAt`, `queuedAt`, `attempt`, `lastToolName`, `lastToolSummary`, `promptPreview`, `lastProgressAt`, `tokens`, `toolCalls`, `durationMs`, `resultPreview`, `isolation`, `phaseIndex`, `phaseTitle`, `index` | **Per-subagent live state inside a workflow run** — 86 entries across the 57 files | **OBSERVED (S4)** | Almost none. `AgentWorkflowUpdate.all_subagents` (`agent.proto:195-210`) carries `AgentSubagentStart` plus `live \| ended` — no state, no model, no attempt, no phase, no tokens, no last tool | `D-sidecar-files.md:293-302` shows this file **contradicts three "no producer" claims** in the design record (`:1062-1066` description, `:1070-1074` run-finished, `:1310-1313` phase grouping). Those record claims need amending, not just fields |
| WFLOW-3 | `wf_<id>.json` → `phases[{title, detail}]` and `workflowProgress[]` `type: "workflow_phase"` | A workflow's **phase structure** | **OBSERVED — 57 phase entries (S4)** | None | `figma-to-idl-redesign.md:1310-1313` records phase grouping as having no producer. It has one |
| WFLOW-4 | `journal.jsonl` — `agentId` | Which agent a journal record belongs to | **OBSERVED — on 172 of 172 rows (S4); 246 journal records in S5** | **None — the field is read by nothing.** `internal/convert/journal.go` reads `type`, `key` and `result` and renders them to a **lossy string** `"result v2:…: <text>\n"` (conceded at `journal.go:45-49`) | The journal's only join key is discarded at the converter |
| WFLOW-5 | `journal.jsonl` — `result` as an **object** | The agent's structured report | **OBSERVED — 120 `result` rows in S5, and the object is free-form and agent-authored**: `.reason`/`.refuted` 69, `.caseName`/`.eventCount`/`.fragmentPath`/`.notes` 24, `.confusions[]`/`.guesses[]` 12, `.findings[]` 7 | None. `AgentSubagentReport.prose` (`:1290-1295`) is markdown | Either a `Struct` arm or an accepted flattening. Today it is flattened by string concatenation with no recorded decision |
| WFLOW-6 | `journal.jsonl` — no timestamps at all | — | **OBSERVED (S4)** | N/A | `D-sidecar-files.md:278`: `AgentActivityStartedAt` has **no journal producer**, so a workflow step can never carry a start instant |
| WFLOW-7 | Daemon + webapp journal parsers key on `label`, `agent`, `phase`, `error`, `prompt` (`daemon/internal/frontend/asyncjournal.go:61-77`; `webapp/src/async-stream.ts:414-420`) | — | **OBSERVED: 0 of 172 real journal rows carry any of those keys (S4)** | N/A | A live defect: `ParseJournalRows` skips 100% of real rows. The keys it wants live in `wf_<id>.json`'s `workflowProgress[]`, which nothing reads ([WFLOW-1](#12-journals-meta-files-and-spools)) |
| META-1 | `agent-<id>.meta.json` — `agentType`, `description`, `toolUseId`, `spawnDepth`, `model`, `parentAgentId`, `worktreePath`, `worktreeBranch`, `spawnedWithWorktree`, `inheritedWorktreePath`, `isFork`, `stoppedByUser`, `cwd`, `worktreeCleanlyRemoved` | Everything durable about a spawned subagent | **OBSERVED — 1,771 real files (S4), with counts: `agentType`/`spawnDepth` 1,771, `toolUseId`/`description` 1,685, `model` 796, `parentAgentId` 443, `worktreePath` 380, `spawnedWithWorktree` 336, `worktreeBranch` 301, `inheritedWorktreePath` 65, `isFork` 34, `stoppedByUser` 22, `cwd` 9, `worktreeCleanlyRemoved` 4** | **None — the file is never opened.** `discover.go:141` constructs `Target.MetaPath` and `discover.go:44` declares it; grep finds no reader anywhere in the sidecar | Six of these have no field even if it were read: `spawnDepth`, `inheritedWorktreePath`, `isFork`, `cwd`, `worktreeCleanlyRemoved`, and `stoppedByUser` (which is the only durable record that a subagent was **stopped by hand** — `AgentSubagentFailure {}` is empty) |
| META-2 | `agent-<id>.meta.json` under `subagents/workflows/wf_*/` | Workflow-spawned subagents' metas | **OBSERVED — 86 files (S4)** | None — **not globbed**: `discover.go:85` requires exactly `projects/*/*/subagents/agent-*.jsonl` (`len(segs)==4`) and these have six segments | Their 86 companion **transcripts** are equally unreachable, and `D-sidecar-files.md:247` finds zero overlap with the agent spools — *"row B's transcripts are reachable by no path at all"* |
| META-3 | Old-layout `projects/<project>/agent-<7hex>.jsonl` | Pre-`2.1` subagent transcripts at project top level | **OBSERVED — 96 files (S5)** | Mis-classified: the `projects/*/*.jsonl` glob (`discover.go:84`) calls them `KindSessionTranscript` with `SessionID` = the filename, while their records carry a real `sessionId`, `isSidechain: true` and an `agentId` | A path rule. 17 of 70 sampled "session" files had filename ≠ record `sessionId`, all of this form |
| SPOOL-1 | Shell spool terminator | See [TOOLIO-10](#5-tool-io-fidelity) | **OBSERVED** | — | The exit code, and a parser that matches a marker essentially absent from disk |
| SPOOL-2 | Shell spool interleaving | The spool is **one byte stream**; stdout and stderr are interleaved and unrecoverable | **OBSERVED — 102-104 `b*` spools (S4/S5)** | Mismatched. `AgentBashOutputText` (`:1045-1065`) has **separate** `stdout` and `stderr` fields, and `agent_activity.proto:1018-1021` reasons that no producer states an interleaving — the spool is exactly such a producer | `D-sidecar-files.md:237`: for a detached shell the split cannot be recovered, so one of the two fields must be a lie or empty |
| SPOOL-3 | `w*.output` workflow spools | The third spool kind | **DECLARED-ONLY — `discover.go:192-193` classifies it; 0 of 190 spools on disk (S4), 0 of 104 in S5, and the store holds 0 `spool w*` cursors against 1,538 `b*` and 30 `a*`** | N/A | A dead arm; recorded so it is not mistaken for coverage |
| SPOOL-4 | Spool owner path index | `OpenTaskState` carries only `last_activity_at_ms` — *"this message says WHEN a task was last active, but not WHICH task it is"* (`proto/gen/go/store/v1/cursor.pb.go:154-158`) | **OBSERVED as a live defect** | Broken. `seedOwners` (`shim-sidecar/owner.go:216-241`) is now a no-op that logs an error, so after a restart a live task's spool is **held and never read** | The retired `TaskStarted.output_path` seeded this index; `DetachedWorkStarted` has no such field. Same root as [AGENT-1](#11-subagents-and-detached-work) |

---

## 13. Control surface and session-level vendor events

`SDKMessage` (`sdk.d.ts:4019`) has **38 arms**, not the six a conversation model
usually assumes: 24 are `type: 'system'` distinguished by `subtype`, eight are
other top-level types, and the real stdout union `StdoutMessage`
(`sdk.d.ts:6762`) adds five more (`active_goal`, `keep_alive`,
`control_request`, `control_response`, `control_cancel_request`).
`SessionUpdate` (`session.proto:152-174`) has seven arms.

| ID | Source · field | Semantics | Tier | Home today | Minimal modeling |
|---|---|---|---|---|---|
| CTRL-1 | `sdk.d.ts:2842-2849` `SDKAPIRetryMessage{attempt, max_retries, retry_delay_ms, error_status, error}`; disk twin `system/api_error{error{message, formatted, connection{code, message, isSSLError}, isNetworkDown, rateLimits}, retryInMs, retryAttempt, maxRetries, source}` | **The vendor is retrying** — attempt N of M, in X ms | **OBSERVED — 5 `system/api_error` records in S5, e.g. `ECONNRESET`, `retryInMs: 561`, `retryAttempt: 1`, `maxRetries: 10`, `level: "error"`** | **None.** `ApiRequestFailed` (`api.proto:28-56`) is terminal — a retry is neither a failure nor an activity. **`frontend/v1/footer.proto:304-309` declares `FooterStatusActivityRetrying{attempt, status}`** and nothing can fill it | A `SessionUpdate` arm. Note also that `retry_delay_ms` is the vendor's **only** retry hint and it rides the *retry*, while `ApiRateLimited.retry_after_ms` (`api.proto:62`) rides the *failure* — which the SDK never pairs with a delay (`A-sdk-stream.md:210`) |
| CTRL-2 | `sdk.d.ts:4903-4907` `SDKAuthStatusMessage{isAuthenticating, output[], error}` | An auth prompt is up | DECLARED-ONLY | None. `frontend/v1/footer.proto` declares an `authenticating` activity arm with no feeder | A `SessionUpdate` arm |
| CTRL-3 | `sdk.d.ts:4401-4407` `SDKStatusMessage.status: 'compacting' \| 'requesting'` | What the session is doing right now | OBSERVED — S3 `stream/status.jsonl` | None | `FooterSubStatusThinking{submitting, thinking, clearing, compacting}` (`frontend/v1/footer.proto:148-162`) is the drawn form and has no source |
| CTRL-4 | `sdk.d.ts:4672-4678` `SDKWorkerShuttingDownMessage.reason: string` | **Why** the worker is going down | DECLARED-ONLY | None. `SessionQueryDied` (`session.proto:189-196`) has `unexpected_eof` and `iterator_failure{cause}` — neither is an announced shutdown with a reason | An arm; an announced shutdown drawn as an unexpected EOF is a wrong accusation |
| CTRL-5 | `sdk.d.ts:4373-4376` `SDKSessionStateChangedMessage.state: 'idle' \| 'running' \| 'requires_action'` | The session's own state machine | DECLARED-ONLY | None | `figma-to-idl-redesign.md:2796,2877-2881` records the drop: liveness is structural and `requires_action` is implied by an open permission or question. **Recorded drop — in [Appendix A](#appendix-a--recorded-drops-not-gaps)**, listed here only for completeness of the arm walk |
| CTRL-6 | `sdk.d.ts:4237-4266` `SDKRateLimitEvent.rate_limit_info` → `SDKRateLimitInfo{status: allowed\|allowed_warning\|rejected, rateLimitType: 6 literals, utilization, resetsAt, overageStatus, overageResetsAt, overageDisabledReason: 13 literals, isUsingOverage, overageInUse, surpassedThreshold, errorCode, canUserPurchaseCredits, hasChargeableSavedPaymentMethod}` | **A rate-limit push event** with the full quota picture | OBSERVED — S3 `stream/rate_limit_event.jsonl` carries `status`, `rateLimitType`, `utilization`, `resetsAt`, `overageInUse`, `surpassedThreshold` | Barely. `SessionAccountUsage` (`session.proto:258-269`) is a **poll** carrying `five_hour` utilization and reset only. There is no push arm and no overage vocabulary | A `SessionUpdate` arm. `canUserPurchaseCredits` and `errorCode: 'credits_required'` are the difference between "you are blocked" and "you are blocked and here is the fix" |
| CTRL-7 | `sdk.d.ts:4138-4145` `SDKNotificationMessage{key, text, priority: low\|medium\|high\|immediate, color, timeout_ms}` | A vendor notification addressed to the user | OBSERVED — S3 `stream/notification.jsonl` | None | An arm; `immediate` priority is by definition something that must be shown |
| CTRL-8 | `sdk.d.ts:3972-3975` `SDKLocalCommandOutputMessage.content`; disk twin `system/local_command{content, level}` | The **output of a slash command** — what `/cost`, `/status`, `/doctor` actually printed | **OBSERVED — `system/local_command` 85 in S5; `away_summary` 79; `informational` 2; `scheduled_task_fire` 4** | None. `SessionCommand` (`slash_command.proto:84-121`) names 30 commands and only `/clear` and `/compact` have outcome vocabulary (`ContextCut`) | Either a generic command-output arm or per-command results. 28 of 30 modelled commands produce output with nowhere to go |
| CTRL-9 | `sdk.d.ts:4074-4082` `SDKMirrorErrorMessage{error, key{projectKey, sessionId, subpath}}` | The vendor **failed to write its own transcript** | DECLARED-ONLY | None. `SessionFault` (`session.proto:340-347`) is the shim's own health | An arm. The sidecar's entire file plane depends on that transcript existing; silent data loss upstream is invisible |
| CTRL-10 | `sdk.d.ts:4227-4229` `SDKPromptSuggestionMessage.suggestion`; `:2826-2834` `SDKActiveGoalMessage.value{condition, iterations, set_at, tokens_at_start, last_reason}` | Prompt suggestions; and the session's **active goal / loop state** | DECLARED-ONLY | None | `active_goal` is outside `SDKMessage` and rides `StdoutMessage` — a loop's own termination condition and iteration count, entirely unmodelled |
| CTRL-11 | `sdk.d.ts:3866-3877` `SDKFilesPersistedEvent{files[{filename, file_id}], failed[{filename, error}], processed_at}` | Files uploaded to the API, and which uploads **failed** | DECLARED-ONLY | None | An arm |
| CTRL-12 | `sdk.d.ts:3857-3861` `SDKElicitationCompleteMessage{mcp_server_name, elicitation_id}`; `:3016-3035` control `elicitation{message, mode: form\|url, url, requested_schema, title, display_name, description}`; `:3753-3763` control `request_user_dialog{dialog_kind, payload, tool_use_id}` | **Two more kinds of blocking user input** beside permission and question: an MCP elicitation and a generic user dialog | DECLARED-ONLY | None. `AgentUpdate` (`agent.proto:98-115`) has `activity \| question \| permission` | `E-control-surface.md:240` calls `UserDialogRequest` a *third* blocking-input kind with no `AgentUpdate` arm. A session blocked on one of these looks idle |
| CTRL-13 | `sdk.d.ts:3485-3493` `SDKControlInterruptResponse{still_queued: string[], cancelled: string[]}` | What an interrupt **could not** cancel | DECLARED-ONLY | None | `figma-to-idl-redesign.md:2072-2075` rules `still_queued` "defensive evidence… a fault to surface", and `E-control-surface.md:115,261` finds **no arm and no `SessionFault` kind exists to surface it**. A ruling with no landing site |
| CTRL-14 | `sdk.d.ts:2317` `Query.setMcpPermissionModeOverride(serverName, mode)`; `:1125-1129` `McpServerToolPolicy{permission_policy: always_allow\|always_ask\|always_deny, org_max_permission: allow\|ask\|blocked}` | **Per-MCP-server permission policy**, including an org ceiling | DECLARED-ONLY | None. `AgentPermissionChange` (`permission.proto:137-156`) has rules, mode, and directories — no per-server scope | An arm; an org-imposed ceiling the user cannot override is exactly what a permission UI must show |
| CTRL-15 | `sdk.d.ts:3597-3635` control `can_use_tool` — `blocked_path`, `decision_reason`, `decision_reason_type`, `matched_ask_rule`, `permission_suggestions[]`, `classifier_approvable`, `suppress_always_allow_rule`, `requires_user_interaction`, `agent_id`, `description` | The permission prompt's **full evidence** | DECLARED-ONLY on the live surface; the **transcript records no permission prompt at all** (`classify.md:394`) — only denial outcomes | Partial and lossy. `AgentPermissionTrigger` (`permission.proto:94-105`) is a **oneof** over `blocked_path \| ask_rule \| note`, but the SDK can carry `blocked_path` **and** `decision_reason` **and** `matched_ask_rule` together — `E-control-surface.md:82`: one of three is lost when two coincide | Make the trigger repeated, or a message with three optional fields. Also unmodelled: `classifier_approvable`, `suppress_always_allow_rule`, `requires_user_interaction` |
| CTRL-16 | `sdk.d.ts:2455-2584` — 13 `Query` methods with no route: `setMcpPermissionModeOverride`, `applyFlagSettings`, `reinitialize`, `supportedAgents`, `readFile`, `reloadPlugins`, `reloadSkills`, `rewindFiles`, `seedReadState`, `reconnectMcpServer`, `toggleMcpServer`, `setMcpServers`, `WarmQuery`/`startup` | Vendor capabilities the contract cannot reach | DECLARED-ONLY | None | Not all should be reachable; `E-control-surface.md:236` enumerates them, and `rewindFiles` ([RETRACT-6](#2-retraction-supersede-interruption-rewind)) and `reconnectMcpServer` (the fix for [SESS-11](#7-session-inventory-and-environment)'s `needs-auth`) are the two whose absence is user-visible |

---

## 14. Inverse gaps: declared but unfillable

Fields `conversation.v1` (or a surface that depends on it) declares, that **no**
vendor surface can supply. Each is a contract defect the same audit answers, so
they are recorded here rather than filed separately. No evidence tier applies —
the evidence is the absence.

| ID | Declared field | Why nothing can fill it |
|---|---|---|
| NOPROD-1 | `AgentGrep` and every arm below it (`agent_activity.proto:664-807`) | **Zero Grep calls across S6's 14,349 files** and zero in the 641,428-record census. The tool is not used in this harness — grep runs through Bash |
| NOPROD-2 | `AgentGlobSuccess.paths[]` and the `all \| partial` extent (`:847-899`) | All 10 observed Glob results are a **bare string**. There is no structured Glob result on disk. Additionally the type surface's `totalMatches`/`countIsComplete` are optional, so even a structured one could leave the `omitted` oneof unset (`B-tool-io.md:174`) |
| NOPROD-3 | `AgentPermissionStart` entire — `prompt`, `trigger`, `offered_standing` — plus `AgentPermissionAllowed{once, standing}`, `AgentPermissionStanding`, all six `AgentPermissionChange` arms (`permission.proto:62-198`) | **The transcript records no permission prompt at all** (`classify.md:394`) — only denial outcomes. An *allowed* call is indistinguishable from one that never asked. The live `can_use_tool` control request is the only producer, and it is a request/response, not a stream frame |
| NOPROD-4 | `AgentBashOutputImage` (`:1079-1085`) | `toolUseResult.isImage` is present on all 6,750 Bash results in S5 and `C-jsonl.md:2001` finds it `false` in all 74,416 it sampled. No producer |
| NOPROD-5 | `AgentBashInterrupted` (`:1023-1027`) | `toolUseResult.interrupted` is present on every Bash result and `C-jsonl.md:2002` finds it `false` in all 74,416 |
| NOPROD-6 | `AgentSubagentTotals.usage: TokenUsage` (`:1309`) for an async spawn | The async path supplies `totalTokens`, one scalar. A four-field breakdown cannot be derived from it ([AGENT-3](#11-subagents-and-detached-work)) |
| NOPROD-7 | `AgentSubagentSuccess.models_used` "in the order they were used" (`:1306`) | Only ever one `resolvedModel` is observed (`classify.md:215`). Ordering has no producer |
| NOPROD-8 | `AgentSubagentPrompt.requested_name` (`:1176`); `AgentSubagentUpdate.note` (`:1224`) | Neither appears in any meta file, journal or transcript (`D-sidecar-files.md:262-278`) |
| NOPROD-9 | `AgentWorkflowStart.resumed_from` (`workflow.proto:35`); `AgentWorkflowPlacementRemote.session_url`; `AgentWorkflowNotice`; `AgentWorkflowScriptRejected.error`; `AgentWorkflowInterrupted` | No key anywhere on disk (`D-sidecar-files.md:262-268`). Conversely `AgentWorkflowCompleted` **does** have a producer — `wf_*.json.status` 57/57 — and it is unwired |
| NOPROD-10 | `AgentQuestionSelection.note` (`question.proto:194`) and the `free_text`/`note` distinction | `classify.md:256` finds no producer for `note`; `figma-to-idl-redesign.md:2927-2934` records the distinction as **inferred from field descriptions and never observed**, with verification owed |
| NOPROD-11 | `SessionIdentityRotated.reason` (`session.proto:184`); `SessionColdLapsed.cache_ttl_ms` (`session.proto:89`); `SessionColdRemediation.clear` (`session.proto:104`) | No vendor reason exists on any message; the SDK never states the prompt-cache TTL tier; and no SDK primitive keeps a session id with an empty context — `forkSession` mints a new id, `deleteSession` destroys (`E-control-surface.md:246-252`) |
| NOPROD-12 | `SessionMcpServerFailed.error` (`session.proto:253`); `DetachedCauseTimedOut.timeout_ms` (`detached_work.proto:83`); `DetachedCauseByUser` | `init.mcp_servers` carries `{name, status}` and **no error text**. No file or stream distinguishes a timed-out detachment from a user-requested one (`D-sidecar-files.md:275`) |

---

## The ten most consequential

Ranked by how much else depends on them, not by field count.

1. **[IDENT-1](#3-identities-lineage-and-replay-fidelity) — the vendor `uuid` reaches no consumer.** OBSERVED on 74,856 records. Six other vendor facts are expressed *only* in that address space — `supersedes`, `retracted_message_uuids`, `preserved_messages.uuids`, `refused_user_message_uuid`, `interruptedMessageId`, `preceding_tool_use_ids` — so they are permanently unconsumable until it lands. Nothing else in this register unblocks as much.

2. **[TOOLFAIL-1](#6-tool-failure-evidence) — every modeled tool's failure arm is empty.** Twelve `*Failure {}` messages, and 408 real `is_error: true` tool results whose text has nowhere to go. `frontend/v1/feed.proto:466` already declares `FeedSkillFailed{text}`. The proto comments call this a deferral to "the implementation wave"; this audit is that wave.

3. **[IDENT-2](#3-identities-lineage-and-replay-fidelity)/[USAGE](#4-usage-cost-and-accounting) — `AgentActivity.usage`'s central rule has no static producer.** One API response is written as many records (up to twelve), each repeating identical usage. Without `message.id` or `requestId` on the wire, a producer either over-counts the bill two-to-threefold or guesses positionally. The accounting theme's biggest problem is an identity problem.

4. **[USAGE-1](#4-usage-cost-and-accounting)/[USAGE-3](#4-usage-cost-and-accounting) — cost and context-window size have no home at all.** `total_cost_usd` and `contextWindow` are both asserted at `figma-to-idl-redesign.md:4799` to "already have their drawn home". Neither does. A cost surface and a context-fill surface are both unbuildable, and `AgentResponseStoppedAtMaxTokens` cannot say what ceiling it hit.

5. **[HOOK-1](#8-hooks)…[HOOK-3](#8-hooks) — hooks are 39% of the transcript and are absent from the contract.** 35,622 `hook_success` records, 103 blocking errors, 953 stop-hook summaries carrying `preventedContinuation`. `frontend/v1/footer.proto:301` declares `FooterStatusActivityHook{name}` and nothing fills it. A hook that blocks a tool call is a refusal the user must understand and it is currently invisible.

6. **[ATTACH](#9-attachments-and-injected-context) — 29 attachment kinds, 44% of records, and no attachment concept exists.** `UserContentBlock` is `text | image | unsupported` and an attachment is not a content block, so even the `unsupported` escape does not apply. Every one lands in `store.v1 StoreUnknown`, which is invisible to the daemon by construction. `diagnostics` (LSP errors after an edit) and `file` (a user-attached file) are the sharpest.

7. **[COMPACT-1](#10-compaction-and-context-cuts)/[COMPACT-6](#10-compaction-and-context-cuts) — compaction drops its trigger and its ancestry bridge.** `trigger: auto` vs `manual` (3 vs 21 observed) is the difference between something the user did and something that happened to them. `logicalParentUuid` is the *only* place the vendor puts ancestry on a record, appearing on `compact_boundary` and nowhere else — without it a compacted session's history is two disconnected components. The design record already owes this vetting item at `:1861-1865`.

8. **[WFLOW-1](#12-journals-meta-files-and-spools)/[META-1](#12-journals-meta-files-and-spools) — 1,828 real files that nothing opens.** 57 `wf_*.json` run manifests are not globbed; 1,771 `agent-*.meta.json` have their path constructed and never read. Between them they hold every durable fact about a spawned subagent, and `D-sidecar-files.md:293-302` shows they **contradict three "no producer" claims** the design record relies on. This is the one theme where the fix is discovery, not modeling.

9. **[STOP-1](#1-stop-refusal-and-termination-taxonomy) — `TerminalReason`'s 19 values map onto two arms.** Finding 1 closed the *response*-level taxonomy; the *turn*-level one is untouched. `stop_hook_prevented`, `budget_exhausted`, `prompt_too_long`, `max_turns` and eleven more all resolve to "the turn ended" with no account. `stop_sequence` (62 observed) additionally falsifies the recorded reason at `agent_activity.proto:407` that it is "unused by this product".

10. **[TOOLIO-10](#5-tool-io-fidelity) — the detached shell exit code, and a parser watching for a marker that no longer occurs.** The recorded drop at `agent_activity.proto:1020-1026` is scoped to *foreground* commands and detached shells do carry an exit code. `frontend/v1/feed.proto:805` declares `FeedShellExit{code}`. Meanwhile `handler/shell.go:15` matches `EXIT=<code>` against a population where the real terminators are `[exited with code N]` (1,526 of 1,570) and `[killed]` — so `DetachedExited` never fires and every background shell ends via the staleness LOST sweep, the exact verdict that code exists to prevent.

---

## Appendix A — recorded drops (NOT gaps)

Collected so the survey is not re-purchased, and so a future pass can tell a
decided question from an open one. Each is dropped **with a reason** and is
excluded from the register's counts.

**Usage and accounting** — `figma-to-idl-redesign.md:2251-2254`: `cache_creation`'s
5m/1h split, `cache_missed_input_tokens` + `cache_miss_reason`, `iterations[]`,
`server_tool_use`, `service_tier`, `inference_geo`, `speed`, all "observed and
NOT modelled, by the user's ruling that `api.proto` is fine". `:2638` tool calls
carry no usage. `:2641-2649` exactly one unit per response carries it.
`:2651-2656` `AgentThinkingSuccess.token_usage` deleted — thinking tokens are an
estimate, not a bill. `:1818-1820` `sample_latency_ms`, `query_instance_id`,
`turn_id`, the turn-boundary oneof. `:541` `response_timing` — derived.

**Stop taxonomy** — `agent_activity.proto:406-408`: `pause_turn` (the vendor
resumes it itself), `compaction` (the context cut is that fact's home).
`:2374-2378` thinking `signature` (withheld = block + signature, nothing to
draw). `:2234-2236` `TurnEnded.unexplained`. `:3620-3639` finality is not a wire
fact. `:801-820` the feed's response error arm carries no reason on purpose.

**Bash** — `:3339-3346`: no exit code on the *foreground* `BashOutput`;
`rawOutputPath`; `backgroundCwdHint`, `returnCodeInterpretation`,
`noOutputExpected`, `staleReadFileStateHint`, `ghRateLimitHint` (model-facing
notes); `structuredContent` (untyped). `:3348-3353` `gitOperation` flagged, not
modelled. `:3198-3225` foreground shell output is observable nowhere, verified
empirically. `:692-705` no `failed` outcome — a non-zero exit still completed.

**Read / Write / Edit / Grep / Glob** — `:3531-3542` a `range` arm offered and
declined; a short read is a head. `:3523-3526` `originalFile` and `gitDiff`.
`:3512-3513` `replace_all` (its effect is the hunk count). `:3505-3510`
`old_string`/`new_string`. `:3450-3457` `appliedLimit`/`appliedOffset`,
`durationMs`; an empty search is a success.

**Subagents** — `:3008-3019` the subagent's lifecycle and its work are separate;
the producer's own awaited-vs-backgrounded split is deliberately not reflected.
`:3029-3034` the activity label is a bare string. `:3036-3040` a remote spawn is
modelled and explicitly unobservable. `:2503-2540` recursion retracted, ancestry
never on the wire, `parent_agent_id` proposed and withdrawn. `:2542-2558`
pagination is for agents only; omitted counts are display-only.

**Skills** — `:2822-2837` nothing delimits a skill's scope (see
[IDENT-13](#3-identities-lineage-and-replay-fidelity), where the data disagrees).
`:2816-2820` the tool's return is worthless; success settles on the document.
`:2839-2844` `allowedTools` are bare strings.

**SendMessage** — `:2766-2771` the prose `message`, `pin.name`, `pin.ref`.
`:2747-2757` the "why the recipient needed resuming" distinction.

**Questions** — `:2923-2925` the echoed `multiSelect` flag and the idle-timeout
duration. `:2866-2907` position and minted tokens rejected as keys; the failure
arm is scoped to asking; an unanswered ask is a success.

**Task tracker** — `:3686-3708` both act arms empty; `stopped` is not an act;
the `skip_transcript` presentation oneof dropped. `:730-738` no act history in
the bubble.

**Workflow** — `:2468-2481` the `source` oneof. `workflow.proto:60-62` the
script body. `:1273-1310` the workflow pipe collapses; no history or pagination.
`:984-988` workflows deferred wholesale.

**Unmodeled tools** — `:2664-2714` `Struct` for arguments, blocks for results,
and **no update arm**, "because this contract holds no knowledge of any
unmodeled tool's streaming behavior".

**Heartbeats and keepalives** — `:4060-4074` the heartbeat concept leaves
`shim.v1` entirely; liveness is the stream being open. `:4707-4735` no keepalive
frames anywhere. `:790-794` the "beats stopped, shim has not ruled" in-between
state is deliberately unrepresented. `:2081-2089` the keep-alive leaves the shim
API; `PROMPT_ORIGIN_CACHE_KEEP_ALIVE` retired.

**Session** — `:1878-1883` `from_seq`, `query_created_seq`, `protocol_version`,
`daemon_version`, `query_instance_id`, `turn_in_flight` bool, `active_turn_ids`.
`:1856-1857` `continue` mode. `:1663-1667` `SetSessionModel` deliberately
departs from mid-turn `setModel`. `:2072-2075` the daemon is the only queue;
`CancelQueuedPrompt` never exists. `:2796,2877-2881` liveness is structural and
`requires_action` is implied ([CTRL-5](#13-control-surface-and-session-level-vendor-events)).
`E-control-surface.md:106` `setModel(undefined)`.

**Content blocks** — `content_blocks.proto:4-9`: the vendor's seventeen block
kinds collapse to "a tool was called, a tool returned"; the tool's own name
carries the rest. This covers the *shape* of MCP / server-tool / web-search /
code-execution / container-upload blocks, though not the `server_name` axis
([TOOLIO-25](#5-tool-io-fidelity)).

**Frontend** — `:904-905` thinking dropped from the feed (footer only);
unmodeled dropped from the feed (topbar dropdown). `:5537-5549`
`FooterStatusActivityNote` and its free-text escape deleted: *"an activity the
daemon can name but has no arm for is a modeling gap; the fix is the arm. Not
even a guarded escape."* `:718-728` identifier-only subagent rows weighed and
declined. `:5386-5419` `workspace` and `fence` leave every view.

**Hibernation** — `:420-477`: the whole family left the contract, with the
committed consequence stated plainly (a parked workspace is indistinguishable
from an idle one). Not a gap; a decision.

---

## Appendix B — prior audit findings this pass retires

Findings in `A`–`G` that the redesign has since superseded, so they are not
counted above.

1. **`G-statedb-architecture.md` entire.** Stage 6 deleted `state/v1/durable.proto` and the whole `state.v1` package (`figma-to-idl-redesign.md:1049-1057`). Its architectural conclusions survive; its proto content does not.

2. **`F-ssm-comb.md`'s shed list.** Declared implementation-wave work carried with stage 6 (`:1081-1082`), not contract work.

3. **Everything keyed to `SessionStarted.main_agent_id`, `WatchTurn`, `UpdateTurn`, `WatchSubagent`, `UpdateSubagent`, `HistoryPrompt`, `HistoryContinuation`** — `E:19-24`, `E:112-121`, `A:99`, `A:547`. All deleted by the agent consolidation (`:1114-1148`) and the `AgentPrompt` landing (`:1513`).

4. **`E:37`, `E:104` — `SessionColdCompact`'s four-arm scope oneof.** Replaced by the three-value `SessionCompactScope` enum; `PROMPTS_AND_RESPONSES` is dead (`:536-544`).

5. **Every hibernation-keyed item** — `HibernationDetail`, `HostHibernation*`, `FooterStatusAsleep`, `RosterRowStatusHibernated`, `ReviveWorkspace`, `HibernateWorkspace` (`:420-477`).

6. **`A:154`, `A:159`, `A:261` and `E`'s "tool progress has no home"** — discharged by `AgentToolCallProgress` (`:552-567`), which gives nine tool kinds a `progress` arm. Still **not** covered, by explicit boundary: task acts (no arm, by design) and the subagent (keeps its richer channel).

7. **`A:189`, `A:246-248`, `A:271`'s "footer StatusActivity arms with no feeder"** — the footer's activity family was rebuilt at `:957-1023`. Re-checked against the current footer: the *arms* still exist and still have no feeder, so these are re-raised as [CTRL-1](#13-control-surface-and-session-level-vendor-events), [CTRL-2](#13-control-surface-and-session-level-vendor-events) and [HOOK-4](#8-hooks) rather than retired.

8. **`B:9`'s framing.** Still true that the shim produces no `conversation.v1` activities, but the proto files it names (`content_pb`, `message_pb`, `payloads_pb`, `tokens_pb`) were deleted at `:2261`/`:6097`. Shim-implementation debt, not a contract gap.

**Not retired — live discrepancies in the design record itself**, each surfaced
by this pass and left standing:

- `:3332` "terminated by its `EXIT=<code>` line" — contradicted by 1,570 real spools ([TOOLIO-10](#5-tool-io-fidelity)).
- `:1062-1066`, `:1070-1074`, `:1310-1313` — three workflow "no producer" claims contradicted by `wf_<id>.json` ([WFLOW-2](#12-journals-meta-files-and-spools), [WFLOW-3](#12-journals-meta-files-and-spools)).
- `:249-250` — "`KillSession`'s every live task is the vendor's own `backgroundTasks()` answer". `backgroundTasks(toolUseId?)` returns `Promise<boolean>` (`sdk.d.ts:2575`).
- `:2858-2859` — `cancel_queued` and `cancel_async_message` cited as available; neither is reachable from the public `Query`.
- `:306-318` Ruling 2 — "relays the level verbatim as a session fact" with no `SessionUpdate` arm to relay into ([AGENT-6](#11-subagents-and-detached-work)).
- `:3694-3699` — the tracker's six status arms are taken from the SDK's *background-task* patch type, a different state machine ([AGENT-5](#11-subagents-and-detached-work)).
- `:4799` — `total_cost_usd` and `context_window` said to "already have their drawn home"; neither does ([USAGE-1](#4-usage-cost-and-accounting), [USAGE-3](#4-usage-cost-and-accounting)).
- `agent_activity.proto:407` — `stop_sequence` "unused by this product"; 62 observed ([STOP-5](#1-stop-refusal-and-termination-taxonomy)).
