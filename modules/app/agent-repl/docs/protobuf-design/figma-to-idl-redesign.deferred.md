# DEFERRED: vendor surfaces learned about, deliberately not modeled now

Companion to `figma-to-idl-redesign.md` (the decision record) and
`figma-to-idl-redesign.vetting.md` (the investigations owed). This document
holds a third class: vendor capabilities SURVEYED AND UNDERSTOOD during the
redesign that the user chose NOT to implement in this pass. Each may return
as its own later PR; the notes exist so that pass starts from evidence
rather than re-surveying.

These are NOT gaps (each was seen and deliberately set aside), NOT exempt-set
members (exemption is a shim behavior for tool calls; these are mostly
control/stream surfaces), and NOT vetting debt (nothing landed rests on them).

## The deferred items

### CTRL-6 — the rate-limit push event, full quota picture
`SDKRateLimitEvent.rate_limit_info` (sdk.d.ts:4237-4266): status
(allowed|allowed_warning|rejected), 6 rateLimitType literals, utilization,
resetsAt, the whole OVERAGE story (overageStatus, overageResetsAt, 13
overageDisabledReason literals, isUsingOverage, surpassedThreshold,
canUserPurchaseCredits, hasChargeableSavedPaymentMethod). Today the footer's
FooterStatusActivityRateLimited/FooterAllowance draws a thinner slice fed by
SessionAccountUsage; a later PR could type the full picture.

### CTRL-12 — two more blocking-input kinds
MCP ELICITATION (control `elicitation`: message, mode form|url, url,
requested_schema, title, display_name, description;
SDKElicitationCompleteMessage) and the generic USER DIALOG (control
`request_user_dialog`: dialog_kind, payload, tool_use_id). Both are blocking
user input beside permission and question; declared-only. A later PR would
give each its own feed row kind + answer verb, per the permission/question
precedent (different item kind, different answer shape, own verb).

### CTRL-16 — the unreachable control verbs
13 Query methods with no route: setMcpPermissionModeOverride,
applyFlagSettings, reinitialize, supportedAgents, readFile, reloadPlugins,
reloadSkills, rewindFiles, seedReadState, reconnectMcpServer,
toggleMcpServer, setMcpServers, WarmQuery/startup. Not all SHOULD be
reachable; a later PR is a rulings pass (which earn agentrepl/shim verbs),
not a modeling task.

### TOOLIO-23 — plan mode's product
ExitPlanModeOutput{plan, isAgent, filePath, hasTaskTool, planWasEdited,
awaitingLeaderApproval, requestId} + the plan_mode attachment family. The
plan document and its approval flow have no drawn treatment. Related:
ATTACH-8's mode transitions were DROPPED (see the decision record) — a later
plan-mode PR should draw CURRENT state only, never edge relay.

### IDENT-16 — the tool-run summary
SDKToolUseSummaryMessage{preceding_tool_use_ids[], summary}: a
vendor-composed summary of a run of tool calls. Declared-only, and
uuid-addressed — needs a vendor-uuid-addressable identity (IDENT-1), which
the identity ruling (vendor ids never cross the contract) deliberately
refuses; any later PR must map uuids to AgentActivityIds shim-side.

### USAGE-11 — the /context breakdown
SDKControlGetContextUsageResponse: categories[], totalTokens, maxTokens,
percentage, memoryFiles[], mcpTools[], systemTools[], systemPromptSections[],
agents[], slashCommands, skills, autoCompactThreshold, isAutoCompactEnabled,
messageBreakdown{toolCallTokens, toolResultTokens, attachmentTokens, ...}.
SESSION_COMMAND_CONTEXT today is a literal with no result vocabulary; a
later PR would give the /context panel a typed result (the /status panel
precedent: a command_panel arm on SubmitPrompt's success).

### USAGE-12 — session-lifetime accounting (/cost, /usage)
SDKControlGetUsageResponse.session{total_cost_usd, total_api_duration_ms,
total_duration_ms, total_lines_added, total_lines_removed, model_usage}.
Same treatment as USAGE-11 when it returns.

### USAGE-14 — cost-behaviour attribution
GetUsageResponse.behaviors{day, week}: request_count, session_count,
behaviors[{key: cache_miss|long_context|subagent_heavy|high_parallel|cron,
pct, count}], agents[], skills[], plugins[], mcp_servers[]. Directly serves
the topbar's accounting warning, which today has only daemon-derived
evidence; a later PR could feed the warning from the vendor's own
attribution.

### SESS-15 — memory recall, the live channel
SDKMemoryRecallMessage{mode: select|synthesize, memories[{path, scope:
personal|team|organization, content}]}. Largely covered by the landed
AgentContextInjected.memory (fed from the nested_memory attachment); the
deferred remainder is reconciling the LIVE channel with the attachment
channel (contentDiffersFromDisk, the scope vocabulary) if memory surfacing
ever grows beyond the footer's loading status.

### IDENT-1 bucket — vendor-uuid-addressed facts (deferred 2026-08-26)

The vendor stamps every transcript record with a `uuid`, and a family of
facts is expressible only by referencing records that way. The standing
identity ruling (vendor identity spaces never cross the contract; the shim
translates uuid to AgentActivityId/TurnId where a unit exists) keeps these
permanently uncarried:

- COMPACT-4 — which messages a compaction PRESERVED (the preserved-segment
  reference points at raw records, not units).
- COMPACT-6 — `logicalParentUuid`, the vendor's one ancestry pointer across
  a compaction; without it a compacted session's history is two
  disconnected components.
- IDENT-3/4 residue — the vendor's request_id / API message.id on failure
  evidence (frontend/v1 failure.proto:VendorFailureContext declares
  api_request_id/api_message_id with no producer); support-ticket material
  only.
- IDENT-2 — the "exactly one unit per response carries usage" rule keeps an
  adjacency producer key shim-side rather than a wire key.

A future PR reopening any of these must first re-open the identity ruling
itself (a typed vendor-record reference type), which is why they travel as
one bucket.

## SIMPLE-ADD wave groups reverted at orchestrator review (deferred 2026-08-26)

The subagent-landed SIMPLE-ADD wave (c7814df86) was audited by the
orchestrator; every group below is NEW feature/support with no prescribed
frontend UI change, so it was reverted from the protos (tags retired at each
site) and parked here. The wave's own comment text — the field semantics,
the "why" prose — is preserved in that commit and is the starting evidence
for any later PR.

### Vendor handshake
`SessionVendorHandshake` on SessionStarted (tag 8 retired): entrypoint,
user_type, betas, capabilities, cwd, git_branch, tool/skill/subagent/plugin
catalogs, output_style, api_key_source, api_provider, conversation names —
plus the SessionEntrypoint/SessionApiKeySource enums and the
SessionSubagentDefinition/SessionPlugin/SessionConversationName leaves.
Fourteen fields of session baseline with no drawn surface yet.

### Run accounting
`AgentRunAccounting` on AgentSuccess (tag 4) and AgentFailure (tag 31):
RunDuration (wall vs api ms), per-model ModelUsage (provider, usage,
web-search count, context window, output ceiling), 7-field RunLatency
telemetry, RunPermissionDenial roll-up with Struct arguments. Money was
already deleted by ruling; the rest follows the same "no accounting surface"
fate for now. Non-optional context_window_tokens/max_output_tokens need the
optional treatment if this returns.

### Workflow phases and progress; workflow run totals
`AgentWorkflowUpdate.phases` (tag 2), `AgentWorkflowSubagent.phase`/
`.progress` (tags 4-5, the 12-field AgentWorkflowSubagentProgress grab-bag),
`AgentWorkflowCompleted.totals` (tag 2, AgentWorkflowTotals). Workflow
rendering today draws spawn order and liveness only.

### Prompt provenance
`UserSaid.provenance` (tag 2): the two-axis UserPromptProvenance
(source: typed|sdk|injected|queued × origin: human|peer|task_notification|
coordinator, with sender identity on the peer arm). No feed treatment for
non-human prompts exists yet.

### Model fallback
`SessionUpdate.model_fallback` (in the retired 8-23 block):
SessionModelFallback with the retry|revert|sticky direction oneof and the
refusal echo. Returns together with "refusal detail" below.

### Token fallback credit; cache-miss diagnostics
`TokenUsage` tags 5-6 retired: TokenFallbackCredit (redeemed|not_applied
with remove_to_redeem) and TokenCacheMissDiagnostics (missed tokens + 6-arm
invalidation reason). No cost/usage surface consumes either.

### MCP permission policy
`SessionUpdate.mcp_permission_policy`: per-server policy oneof plus the
org-ceiling oneof. The MCP panel draws health only.

### Server-side context edits
`ContextCut.server_edited` (tag 4 retired): ContextEditedByServer with
per-edit typed counts (tool uses vs thinking turns). The API-initiated cut
has no drawn treatment; compaction_failed (tag 3) was KEPT.

### Refusal detail
`AgentResponseRefused` tags 2-6 retired: AgentRefusalCategory enum,
original/recommended model pair, fallback credit token and prefill claim.
If it returns, revisit the enum-of-unsettled-vendor-vocabulary choice.

### Auth status
`SessionUpdate.auth_status`: SessionAuthStatus. Also carries a known schema
defect to fix on return: `output`/`error` sit beside the state oneof but
only mean anything in the authenticating arm (adjacent-exclusivity).

### Vendor session events (the remaining SessionUpdate arms)
Tags 8-23 retired on SessionUpdate: api_retrying, busy periods,
worker_shutting_down, notifications (with priority enum), transcript write
failure, active_goal, prompt_suggestion, files_persisted,
interrupt_incomplete, background_tasks, location/worktree state, tool-set
churn, settings_fault. Each is real vendor evidence with no UI story;
context_budget_warning (tag 24) was KEPT.

### Non-text read extents; attached-file block
`AgentReadSuccess` tags 4-8 retired (image, pdf, notebook,
split-to-directory, unchanged extents plus AgentReadStart.requested_pages);
`UserContentBlock.file` (tag 4) and content_blocks' FileBlock family.
Rendering non-text reads and attachment chips is a feature of its own.

### get_plan — the path-less plan read (deferred 2026-08-27)

`SDKControlGetPlanRequest` (subtype 'get_plan', sdk.d.ts:3156): reads the
session's current plan-mode plan without knowing the plan file's path (the
worker resolves its own plan slug; never creates one). Not modeled — the
landed FeedPlan bubble gets its document from the ExitPlanMode output. This
verb is the enabler if the planning state ever becomes LIVE-UPDATING (the
daemon polling the plan as it grows instead of waiting for the exit).
