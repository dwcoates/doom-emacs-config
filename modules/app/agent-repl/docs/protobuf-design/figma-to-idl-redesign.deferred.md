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
