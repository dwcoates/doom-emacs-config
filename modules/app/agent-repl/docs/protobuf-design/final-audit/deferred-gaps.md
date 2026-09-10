# Deferred-item gaps in the overhaul plans

Audit scope: `docs/protobuf-design/figma-to-idl-redesign.deferred.md` (the
deferred metadocument) read against EVERY document under `docs/overhaul/`
(README, ORCHESTRATION-META, daemon, shim, sidecar, store, elisp, webapp,
prompts/PROJECTLEAD, prompts/TEAMLEAD, reports/merge-variants-2026-08-27),
with the frozen contract under `proto/src/` consulted to settle whether a
producer exists.

Two questions asked of every deferred item: (1) does an overhaul plan
promise or depend on it; (2) does its absence leave a flow that silently
needs it. Deliberate absence acknowledged anywhere in docs/overhaul is
treated as fine and appears in the checked-and-clear list at the end.

---

## 1. The `/agents` and `/help` panels have NO PRODUCER — the vendor-handshake reversion took their only source

DEFERRED ITEM: "Vendor handshake" (`SessionVendorHandshake` on
SessionStarted, tag 8 retired) — which carried the "tool/skill/subagent/
plugin catalogs" and the conversation names; plus CTRL-16, which parks
`supportedAgents` as an unreachable control verb.

OVERHAUL TEXT IMPLICATED:
- `webapp.md` — "**Command panels** (`status_panel.proto`,
  `context_panel.proto`, `mcp_panel.proto`, `todos_panel.proto`,
  `agents_panel.proto`, `help_panel.proto`) — daemon-resolved rows for
  programmatically answered slash commands; the webapp owns only the
  rendering. They arrive as `SubmitPrompt` success arms."
- `daemon.md` 4 (THE PROMPT HANDLER) — "recognize session commands and
  answer the read-only panel class inline."
- `daemon.md` contract context — "`SubmitPrompt`'s success arm forks into
  'a minted turn' vs 'a recognized command's panel'."

THE SILENT DEPENDENCY: `frontend/v1/agents_panel.proto`'s header states the
panel is filled "from the vendor's structured catalog answer", and
`help_panel.proto`'s states "from the vendor's structured supported-commands
answer". Neither answer exists anywhere on the frozen wire: `SessionStarted`
carries only vendor_session_id, runtime, effective_model, permission_mode,
`model_catalog`, turn_in_flight and live_work — the subagent/skill/plugin
catalogs died with tag 8, and no `shim.v1` verb or `SessionUpdate` arm
replaces them. `conversation/v1/slash_command.proto`'s `SessionCommand` enum
holds our own literals but no descriptions, so it cannot fill
`HelpPanelRow.description` either.

WHY IT IS SILENT RATHER THAN ACKNOWLEDGED: `SubmitPromptCommandPanel`
explicitly retires tags 2-3 with a stated reason for `/cost` and `/usage`
falling through to the vendor — so the reader sees the design KNOWS how to
say "this command is not daemon-handled", and infers that the six arms that
remain all have sources. The daemon teamlead will implement `/agents` and
`/help` by dispatching an implementer against a message with no input, and
the failure mode is a well-formed empty panel (a `AgentsPanelView` with zero
rows reads as "no agents configured", not as "unimplementable").

REMEDY DIRECTION (not a ruling): either restore the handshake's two catalogs,
or retire the `agents` and `help` arms the way `/cost` and `/usage` were
retired, with the reason stated at the tag.

---

## 2. The `/context` panel is declared "un-stranded" but the landed fact cannot fill it

DEFERRED ITEM: USAGE-11 — "the /context breakdown"
(`SDKControlGetContextUsageResponse`: categories[], memoryFiles[], mcpTools[],
systemTools[], systemPromptSections[], agents[], slashCommands, skills,
autoCompactThreshold, isAutoCompactEnabled, messageBreakdown, percentage).
Its own text says "SESSION_COMMAND_CONTEXT today is a literal with no result
vocabulary; a later PR would give the /context panel a typed result."

OVERHAUL TEXT IMPLICATED:
- `ORCHESTRATION-META.md` STATUS — "shim.v1 GetSessionContextUsage and
  conversation SessionContextUsage landed (pulled, never derived; **/context
  panel un-stranded**)."
- `daemon.md` 10a — "the /context panel resolves from the SAME fact."
- `webapp.md` topbar — "hover shows the session-scoped breakdown."

THE CONTRADICTION: the deferral says the panel is parked; the ledger says it
is un-stranded. The contract sides with the deferral on DEPTH.
`conversation/v1.SessionContextUsage` carries `total_tokens`, `max_tokens`
and a flat `repeated SessionContextCategory {label, tokens}`. But
`frontend/v1/context_panel.proto` is a TREE: `ContextPanelSection {heading,
rows}` with headings exampled as "Memory files" and "MCP tools", and
`ContextPanelRow {label ("webapp/CLAUDE.md"), tokens, share_permille,
depth}`. Per-item labels, nesting depth and window share are exactly
USAGE-11's `memoryFiles[]` / `mcpTools[]` / `percentage` — the parked half.

The flat category list can populate ONE section with depth 0 rows and no
shares. Nothing in docs/overhaul says the panel is meant to draw only that,
so the webapp teamlead will build a tree renderer the daemon can never fill,
and the daemon teamlead will either invent a heading or escalate a
"prescription gap" that is really a deliberate deferral nobody told it about.

---

## 3. Detached WORKFLOW work is watched and stored but has nowhere to be drawn — and the deferral's own wording hides it

DEFERRED ITEM: "Workflow phases and progress; workflow run totals"
(`AgentWorkflowUpdate.phases`, `AgentWorkflowSubagent.phase`/`.progress`,
`AgentWorkflowCompleted.totals`), whose text asserts: "Workflow rendering
today draws spawn order and liveness only."

OVERHAUL TEXT IMPLICATED:
- `daemon.md` 10b (THE SESSIONWATCHER) — "it owns every shim watch for its
  session — WatchSession, the turn's WatchAgent, one per live detached item
  (WatchAgent / WatchBash / **WatchWorkflow**) … detached-work announcements
  to the feed's bubble head plus opening that item's own watch."
- `shim.md` — `GetWorkflow` / `WatchWorkflow` / `StopWorkflow`, and
  `DetachableWork {subagent | bash | workflow | monitor}`.
- `store.md` — a whole `workflow` table, "one row per run", plus the derived
  subagent level.
- `sidecar.md` — Owed E, workflow journals AND per-agent transcripts with
  `agent-<id>.meta.json`, listed as prescribed discovery scope and a
  replacement integration-test subject.

THE HOLE: `frontend/v1` has no workflow surface at all. `feed.proto`'s async
arms are exactly `FeedDetachedSubagent` and `FeedDetachedShell`; there is no
workflow row, no workflow bubble, and no workflow chip —
`FooterLiveWorkChips` is agents / tasks / shells / monitors / crons.
Monitors' absence from the feed is EXPLICIT and deliberate
(`footer.proto`: "monitors have no feed bubble, unlike the agent and shell
rows"), which proves the schema states this kind of decision when it makes
one. For workflow it states nothing.

So the sessionwatcher is prescribed to open a `WatchWorkflow` per live run
and route its announcements "to the feed's bubble head", and there is no
bubble head to route to. The deferral's sentence — "workflow rendering today
draws spawn order and liveness only" — is the thing that makes this
invisible: it tells any reader that basic workflow drawing exists and only
the phase detail was parked, when in fact nothing is drawn.

(A workflow's constituent agents can each surface as `FeedDetachedSubagent`
via the per-agent `WatchAgent` fan-out `daemon.md` 10b describes. That
covers the AGENTS, never the RUN — its description, script path, placement
local/remote, notice, summary and terminal, all modeled in
`conversation/v1/workflow.proto`, have no consumer.)

---

## 4. Prompt origin has no durable home, so replayed history cannot say why a daemon-submitted turn exists

DEFERRED ITEM: "Prompt provenance" — `UserSaid.provenance` tag 2, reverted
("No feed treatment for non-human prompts exists yet").

OVERHAUL TEXT IMPLICATED:
- `daemon.md` 5 (THE PROMPT QUEUE) — "the ONE path for ALL session-bound
  deliveries — prompts from every origin".
- `daemon.md` 6 — the merge orchestrator's "remediation prompts route through
  the prompt queue like any origin"; `before_ws_merge` and
  `postprocessing_prompt` run as prompts; "the displaced user turn is
  captured durably and resubmitted exactly once at lease release, **across a
  daemon bounce**".
- `daemon.md` session lifecycle — "opening late or after a daemon restart
  misses nothing"; `ReadHistory` replays `AgentPrompt` entries.

THE SILENT DEPENDENCY: `PromptOrigin` (shim/v1/prompt_origin.proto, 28
values including `MERGE_CONFLICT_REPAIR`, `MERGE_BEFORE_ACTION`,
`WORKSPACE_CREATED`, `MERGE_DISPLACED_TURN_RESUME`,
`RESUME_AFTER_RESTART`) rides ONLY `shim.v1.StartTurn.origin`. It is not on
`AgentPrompt`, not on `UserSaid` (tag 2 retired), and not in any `store.v1`
column. Its own file comment asserts the opposite — "a closed, durable
attribution vocabulary … shared by the turn's request and the session's
durable bookkeeping … a stored TurnStarted can therefore be traced back to
the exact editor situation that caused it" — and
`PROMPT_ORIGIN_RESUME_AFTER_RESTART`'s comment goes further: "A status
surface reads this to report 'resumed after restart' instead of presenting
work the user did not just ask for as though they had."

No status surface can read it. Live, the daemon knows the origin because it
made the submission; on any REPLAY (history page, cold repaint, post-restart
catch-up) a daemon-generated prompt returns as an indistinguishable
`user_prompt` row. Two concrete consequences neither daemon.md nor webapp.md
addresses:
- the merge orchestrator's configured/remediation prompts are routed into the
  merge sub-feed by the OUTPUT ADDRESS, which `daemon.md` 6 says is "set on
  lease acquisition, updated per tab, **cleared on release**" — after the
  merge settles there is no address and no persisted origin, so
  reconstructing which historical prompts belonged to which merge tab has no
  input, while `daemon.md` insists "merge phase history is not stored state —
  it is feed content the daemon synthesizes on the fly";
- the restart re-drive turn is drawn as though the user had just typed it.

---

## 5. The deferred metadocument is unreadable by everyone the retired tags address

DEFERRED ITEM: all of them, structurally.

OVERHAUL TEXT IMPLICATED:
- `prompts/TEAMLEAD.md` — "**NEVER look inside `docs/protobuf-design/`** —
  that is the design-process directory, the PROJECT LEAD's context
  exclusively … Your world is `docs/overhaul/` plus the code and proto
  directories." All five teamleads receive this, and relay it downward.
- `prompts/TEAMLEAD.md` — "THE PROTOBUF COMMENTS ARE RICH DOCUMENTATION …
  More information is available there whenever you need it."

THE CONTRADICTION: the frozen protos repeatedly discharge their explanation
by pointing at the forbidden document. `SessionStarted`: "Tag 8 is RETIRED:
the vendor handshake block is deferred — **see the deferred-work
metadocument**". `SessionUpdate`: "Tags 8-23 are RETIRED … deferred — see the
deferred-work metadocument ('vendor session events')". `UserSaid`: "Tag 2 is
RETIRED: prompt provenance is deferred — see the deferred-work
metadocument". `ContextCut` tag 4, `TokenUsage` tags 5-6, and the rest of the
reverted SIMPLE-ADD wave do the same.

An implementer told the comments are authoritative, and told the pointer's
target is off-limits, gets "this was deliberately removed" with no way to
learn WHAT was removed or WHY. Under the standing "surface any concern around
unexpected or undefined UX — missing protobuf fields … rather than improvise"
rule, every retired tag becomes an escalation the project lead must answer
from a document only it may read. No `docs/overhaul/` document restates the
deferred inventory.

---

## 6. The project lead is told the deferred inventory is superseded history

DEFERRED ITEM: all of them, structurally (companion to finding 5).

OVERHAUL TEXT IMPLICATED — `prompts/PROJECTLEAD.md`:
"`docs/protobuf-design/` EXISTS and is YOURS ALONE to access — but it is
HISTORICAL: the design-era record, registers and digests. It is NOT
necessarily a source of truth — where it conflicts with `docs/overhaul` or
the protos, the `docs/overhaul` version settles it. **Do not routinely read
it**; be aware it exists and consult a specific file only when a specific
question genuinely demands the history."

THE CONTRADICTION: the deferred metadocument is not history and is not
superseded — it is the live, forward-looking inventory of what deliberately
does not exist, and it is the ONLY place several of those decisions are
recorded (`docs/overhaul/` restates none of them). The instruction's
conflict-resolution rule actively inverts the truth for findings 1-4: in each
of those, `docs/overhaul/` promises a capability the deferral parked, and the
lead is told docs/overhaul wins.

The one place the lead is pointed back at the directory is webapp-specific
and narrow: "audit reports in the historical directory enumerate known gaps
and unsketched surfaces, available if a specific webapp question demands
them" — which will not surface the daemon-side producer gaps in findings 1-3.

---

## 7. `/status` degrades to near-empty, and the overhaul never says so

DEFERRED ITEM: "Vendor handshake" again — the reverted block carried
entrypoint, user_type, betas, capabilities, **cwd**, git_branch, the
catalogs, output_style, **api_key_source**, api_provider.

OVERHAUL TEXT IMPLICATED: `webapp.md` lists `status_panel.proto` among the
"daemon-resolved rows" the webapp renders; `daemon.md` names /status as the
precedent the other panels follow ("the /status panel precedent" is also how
the deferred doc itself frames USAGE-11's future).

THE GAP: `status_panel.proto`'s header names its content as "version,
working directory, auth, plugins, memory" resolved "from the session's init
facts (conversation.v1 SessionBegan)". `SessionBegan` no longer exists;
`SessionStarted` survives with the handshake block retired. Of the five named
rows, only "version" has a producer (`SessionRuntime`'s three version
fields). Working directory, auth, plugins and memory were all handshake
fields.

This is the SOFTEST of the panel findings — `StatusPanelView` is explicitly
tolerant ("EMPTY rows means no init has landed yet"; "the daemon OMITS a row
it has no value for"), and the panel splices account, model and permission
mode from elsewhere. So it will not fail; it will quietly ship three spliced
rows and a version, which no overhaul document predicts and which a
playtest screenshot review would flag as a bug rather than as the settled
consequence of a deferral.

---

## Checked and clear

Each below was chased to the contract and found either sourced, deliberately
absent with the absence acknowledged in `docs/overhaul/`, or genuinely
independent of the deferred item.

- **CTRL-6 (rate-limit push, overage story)** — `FooterStatusActivityRateLimited`
  carries only `FooterAllowance session` + `weekly`, fed by
  `SessionAccountUsage`'s windows; a live 429 has its own home in
  `ApiRequestFailed.rate_limited`. No overhaul text promises overage,
  credit-purchase or reset-threshold surfacing.
- **CTRL-12 (MCP elicitation, generic user dialog)** — the blocking-input set
  is closed everywhere and consistently: `FooterStatusWaiting`'s substatuses
  are wakeup / permission / question; `shim.md`'s "The permission gate"
  section states "ONE vendor gate exists (canUseTool)"; `webapp.md` lists
  only permission/question cards. No plan assumes a third kind.
- **CTRL-16 (unreachable control verbs), for internal uses** — the verbs are
  parked as UNROUTED, not as unusable inside the shim. The keep-alive YIELD
  OBLIGATION (`shim.md`: "a real prompt rolls context back to just after the
  last real prompt") is wholly shim-internal and needs no agentrepl/shim verb,
  so it is not a dependency on the parked routing pass. (`supportedAgents` is
  the one exception and is finding 1.)
- **TOOLIO-23 (plan mode's product)** — the deferral text is STALE, not
  depended upon: `AgentPlanModeExited` fully carries `plan`,
  `plan_was_edited`, `file_path`, `is_agent`, `has_task_tool` and
  `awaiting_leader_approval`, so `FeedPlan`/`FeedPlanEditTarget` and
  elisp.md's "plan bubble's ✎ edit button (opens the plan file …)" are all
  fed. The approval flow rides the ordinary permission gate. ATTACH-8's
  dropped mode transitions are promised nowhere.
- **get_plan (path-less plan read)** — its stated enabling condition is "if
  the planning state ever becomes LIVE-UPDATING". No overhaul document
  promises a live-updating plan; `FeedPlan`'s states are planning / planned /
  failed, and the plan arrives with the exit.
- **IDENT-16 (tool-run summary)** — no overhaul text promises a run summary;
  the feed's units are per-activity throughout.
- **USAGE-12 (/cost, /usage)** — acknowledged AT THE CONTRACT:
  `SubmitPromptCommandPanel` tags 2-3 retired with the reason stated, the
  commands falling through to the vendor. "Money/cost is deliberately absent
  from the entire API" appears in daemon.md and webapp.md alike.
- **USAGE-14 (cost-behaviour attribution)** — the deferral itself says the
  topbar warning "today has only daemon-derived evidence", and daemon.md 10a
  matches ("warnings (accounting + unmodeled + pulled diagnostics)") with
  `TopbarAccountingWarningDetail` carrying daemon-composed lines. No plan
  claims vendor attribution.
- **SESS-15 (memory recall live channel)** — covered by the landed
  `AgentContextInjected.memory`; the footer's skill/memory injection line is
  the only surface any overhaul doc names.
- **IDENT-1 bucket / COMPACT-4 / COMPACT-6** — our compaction is
  daemon-directed and shim-implemented "via a throwaway summarizing session"
  (shim.md), and the store never discards records, so history paging does not
  traverse a vendor compaction boundary and needs no `logicalParentUuid`.
  `store.md`'s "logical-session scoping" invariant handles the one real
  splitting risk (identity rotation) without a vendor-uuid reference type.
- **Run accounting (AgentRunAccounting)** — the footer's turn figures and the
  topbar's session figures are explicitly per-resolver in-memory accumulation
  off `AgentActivity.usage` (daemon.md 15), not a vendor roll-up; `TokenUsage`
  survives and `FooterTokensCell` has its own verdict arms for incomplete
  evidence.
- **Token fallback credit / cache-miss diagnostics; model fallback; refusal
  detail** — no overhaul document draws any of them; the model selector and
  `FeedTurnErrorRefusal` are arm-level only.
- **MCP permission policy** — the deferral says "the MCP panel draws health
  only", and `McpPanelRow` is exactly a five-arm health badge fed by
  `SessionUpdate.mcp_server`. Producer present, scope matched.
- **Server-side context edits (`ContextCut.server_edited`)** —
  `compaction_failed` was kept and is what the plans reference; no
  API-initiated cut is drawn anywhere.
- **Auth status (`SessionAuthStatus`)** — daemon.md 10a supplies an
  independent mechanism ("the resolver reads that root's `.claude.json` for
  the email; logged-out is a drawn state, never blank"), matching
  `TopbarAccountLoggedOut`. Not a dependency.
- **Vendor session events, `interrupt_incomplete` specifically** — a failed
  interrupt IS detectable without it: `AgentSuccess.interrupted`
  (`AgentInterruptedByUser` / `ByHostShutdown`) is the acknowledged-stop arm,
  so a turn concluding on any other arm after an interrupt request is the
  evidence daemon.md 5's "a failed interrupt strips the jump and stamps the
  classification error" needs.
- **Vendor session events, the rest** — `api_retrying`, busy periods,
  `worker_shutting_down`, notifications, transcript write failure,
  active_goal, prompt_suggestion, files_persisted, background_tasks,
  location/worktree state, tool-set churn, settings_fault: none is drawn or
  depended on. Upstream silence is covered by daemon.md's layered-liveness
  rule ("shim-degraded arms", `SessionDiagnostics`), not by vendor retry
  events; `context_budget_warning` was kept and is the one arm the footer uses.
- **Non-text read extents; `UserContentBlock.file` / FileBlock family** —
  webapp.md's feed row taxonomy names no attachment chip and no image/pdf/
  notebook read rendering; `content_blocks.proto` keeps text + image + an
  explicitly non-fallback `UnsupportedBlock`. Nothing promised.
- **Rollout controller vs `reloadSkills` / `reloadPlugins` / `reinitialize`
  (CTRL-16)** — `daemon.md` 10 classifies the landed range by subsystem
  prefix into daemon / shim / elisp / webapp only, and shim changes are
  handled by a full process relaunch, which subsumes any in-process reload.
  No rollout path needs a vendor reload verb.
- **`reports/merge-variants-2026-08-27.md`** — old-tree code-reading
  evidence; touches no deferred surface.
