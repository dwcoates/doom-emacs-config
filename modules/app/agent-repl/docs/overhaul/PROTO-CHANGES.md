# Protobuf change ledger — the project lead's running tally

Every contract edit since the kickoff foundation (e1eb8ca18), classified by
WHO decided it. "OWN ACCORD" entries are the project lead's allowed class
(a field or arm planning clearly forgot, plain data with an obvious
producer, or arms the contract said would be derived at the wave).
"USER-RULED" entries were put to the user with options. Updated at every
landing; the newest landing is last.

## Landing 1 — overhaul/integration 80a7a0322 (2026-08-29)

USER-RULED (Q1–Q4):
- /agents and /help: recognized daemon-side, never forwarded, answered as a
  refusal card (no catalog increment).
- OpenInEditor{workspace, path, optional line} (WEB LINK) + WatchHostWorkspace
  push arm `open_in_editor{path, optional line}`.
- FeedRow arms `command_panel` (panel oneof status|todos|mcp|context) and
  `command_refused{command, reason, optional add_support}`;
  SubmitPromptSuccess.command_refused{command};
  RequestCommandSupport{workspace, command} → {WorkspaceRef}.
- UpdateHeldPrompt.accept.

OWN ACCORD:
- SubmitPromptRequest.origin (conversation.v1.PromptOrigin, required) —
  planning's own rule says every Emacs send site chooses a value; the request
  had no field to carry it.
- WatchLoginTerminal reshaped to a SERVER stream (WatchLoginTerminalRequest
  {workspace} → LoginTerminalOutput) + unary SendLoginInput{workspace,
  keystrokes|resize} — a WKWebView cannot speak Connect bidi over cleartext;
  same semantics, transport shape only. LoginTerminalInput/LoginTerminalAttach
  deleted.
- TopbarView.permission_mode_picker {current, options[{mode, display_name}]}
  — the kept picker had no served data (its "only modes it was served" rule
  needs a carrier).
- AgentUpdate.context_cut (ContextCut) and AgentUpdate.api_error
  (ApiRequestFailed, mid-turn) — both facts were specified with no carrier.
- shim.v1 derived failure arms (the contract's "DERIVED at the wave" class),
  from the shim's real refusal sites: StartSessionFailure.cause +
  {vendor_start_failed, unknown_session, already_started, conversation_owned};
  SetSessionModelFailure.cause + {model_not_in_catalog, no_session,
  vendor_refused}; SetSessionPermissionModeFailure.kind {no_session,
  vendor_refused}; HibernateError.kind {turn_in_flight,
  compaction_failed{error}, no_session}; KillSessionFailure.cause +
  {no_session, query_refused_to_end}; StartTurnFailure.kind
  {turn_already_open, no_session, vendor_refused, query_dead};
  UpdateAgentFailure.kind {unknown_agent, no_open_ask, answer_mismatch,
  nothing_running, no_session}; KillTurnFailure.cause + {not_the_open_turn,
  no_turn_open, no_session}; StopBashFailure.kind {unknown_work,
  already_ended}; DetachForegroundFailure.kind {unknown_unit,
  already_concluded, not_detachable, no_session}; ReadHistoryFailure.kind
  {unknown_agent, stale_pointer, store_unavailable}; SessionFault.kind
  {store_unreachable, converter_defect, log_sink_poisoned, keepalive_failed,
  vendor_query_failed}.
- Comment-only: TopbarConnectivity.tone cites render-colors.json's topbar
  tones (teal gone); FeedCodeSpan.paint_class cites paint-classes.json.
- proto/gen/go/go.mod: require connectrpc.com/connect v1.17.0.

## Landing 2 — overhaul/integration d2e59721b (2026-08-29)

OWN ACCORD:
- SubmitPromptRequest.workspace (workspace.v1.WorkspaceRef) — the busiest
  per-workspace verb named no workspace when `feed` was unset.
- proto/gen/go/go.mod `go 1.23.0` (x/net v0.43.0 declares it).

## Between landings — a65714f32

OWN ACCORD (hygiene, no schema change): deleted generated bindings whose
.proto no longer exists (shim/v1 endpoint_get_session_context_usage,
endpoint_get_session_diagnostics, prompt_origin — Go and TS).

## Landing 3 — overhaul/integration (2026-08-29; protos 29d147b79 + de58dda4b)

OWN ACCORD:
- DetachedLost {file_vanished | went_silent | swept_up} (agent_activity.proto)
  threaded as `lost` arms on AgentBashInterrupted.cause,
  AgentSubagentFailure.cause and AgentFailure.failure — the feed's
  FeedShellLost/FeedSubagentLost had no producer path.
- HostNotificationKind.question_asked{header} — a blocking question fires
  attention like a permission ask (the triage ruling's "…" anticipated it).
- store.v1 derived failure arms from the store's real refusal sites:
  WriteBatchFailure.kind {invalid_request{field}, storage_failure};
  OpenAgentSessionFailure.kind / ReadAgentPageFailure.kind {invalid_request
  {field}, stale_pointer, storage_failure}; GetLiveWorkFailure.kind
  {storage_failure}; GetSidecarCursorsFailure.kind {invalid_request{field},
  storage_failure}; GetWorkflowFailure.kind {not_implemented,
  invalid_request{field}, unknown_run}.
- FeedToolCallReturned.form gains `none` (FeedToolCallNoOutput) — a returned
  call with nothing to draw had no representation (an empty text would be a
  sentinel).
- FeedToolCallInput.form oneof {command | path | query} (UNSET = plain) — the
  existing look styles a shell line, a muted path and a query differently and
  the client holds no per-tool knowledge, so the daemon states the form.
- FeedTurnEndedErrored.headline (FeedTurnErrorHeadline, required) — the
  client owned sixteen per-arm sentences, a server-driven-UI violation.
- Comment-only: FeedPageErrorHeadline.tone cites render-colors.json's colors.
- FeedColdGateResolvedCompact.scope — the resolved trace named the summarizer
  but not the chosen scope, which the answer verb carries.
- shim.v1 StartTurnSuccess.page (conversation.v1.HistoryPage, required) —
  the request already carried page_size/known_through and every comment
  said the opening page rides the response; the field was missing.
- shim.v1 UpdateAgentFailure.kind gains `not_deliverable` — the pinned SDK
  has no route to prompt an existing subagent, and the nearest landed arm
  (nothing_running) would have lied.
- shim.v1 DetachForegroundFailure.kind gains `unsupported` — the pinned SDK
  offers no verb to initiate a detachment; `not_detachable` (wrong kind)
  would have lied.
- Comment-only: conversation.v1.AgentId carries the CROSS-PLANE MINTING RULE
  (main = original vendor session id; subagent = the spawning call's
  tool_use_id, the one id both the stream and meta.json carry). The
  previous wording implied a vendor agent-id space the stream never exposes.
- store.v1 ReadAgentPageSuccess.lines: `repeated StorePageLine` →
  `repeated StoreLineAt` — a continuation page carried no pointers while
  HistoryEntryAt.at is required, so the shim was minting placeholder marks.
- store.v1 rpc WatchBashRun {run} → stream {row: StoreAgentBash}, replay
  then follow, ends after the terminal. BORDERLINE for the allowed class
  (a new rpc, not a field): the bash table had a write path (the sidecar's
  spool rows) and NO read path, so "every byte of detached shell output
  comes from the sidecar" was unreachable. Reversible if the user prefers
  folding bash rows into WatchAgentSession.
- Comment-only: conversation.v1 AgentFrame.detached_work IS a page line
  (upsert key `detached:<work id>`) — agent.proto said "never as a page
  line" while history.proto required it for replay and GetLiveWork's
  live_detached had no other source.
- Deferred to landing 4: the daemon's derived error arms (every empty
  `<Rpc>Error` in agentrepl.v1, `transferring_away`, `not_yet_adopted`, the
  DaemonFault/SessionFault/HostFault kind oneofs).

## Landing 4 — overhaul/integration 78479349f (2026-08-29; protos e226e9d4f, 882e1208b, 662bb16ad, 7983d2601, 728c3e016)

OWN ACCORD:
- AgentUpdate.context_budget_warning = 7 (ContextBudgetWarning{text}) and
  SessionUpdate tag 24 RETIRED — the warning is a transcript attachment with
  no live-stream producer; a SessionUpdate arm had no home on the agent plane
  where the sidecar delivers it.
- Comment-only: DetachedWorkId carries the MINTING RULE (value == the unit's
  AgentActivityId, i.e. the spawning call's tool_use_id) so a `created`-origin
  monitor/bash/subagent is retired by its own terminal frame.
- agentrepl.v1 error arms, DERIVED from the daemon lead's refusal-site
  batch (settled sites landed as-is; sites still prescribed in unlanded
  briefs landed too, to spare consumers a later re-work — any that end up
  unused get retired): cross-cutting unknown_workspace /
  workspace_ref_mismatch{registry_dir} / transferring_away{address} /
  not_yet_adopted on 30 per-workspace rpcs (CreateWorkspace and
  RegisterWorkspace excluded: no existing workspace is their subject);
  per-rpc arms on every unary endpoint (see the endpoint files);
  DaemonFault.kind (6 arms), SessionFault.kind (8 arms), HostFault.kind
  (same 8, reusing SessionFault's messages, tags 3–10). Typed in by an
  opus-low writer from the lead's table; reviewed by the project lead.
- SessionUpdate.rate_limit_status = 28 (SessionRateLimitStatus: typed status
  allowed|allowed_warning|rejected, optional window/utilization/overage
  fields) — the SDK's stream-only `rate_limit_event`, evidenced by the shim
  lead's probe corpus; the footer's allowance cell had no producer.
- FooterAllowance.status: verbatim string (tag 4) RETIRED → typed oneof
  allowed|allowed_warning|rejected (tags 5–7), the vocabulary now in
  evidence.

## Landing 5 — STAGED on overhaul/landing-5 (not yet landed)

OWN ACCORD:
- AgentBashOutput.form gains `not_observed` — a lost/reconciled run had to
  lie with `partial{bytes_omitted: 0}`.
- FeedResponse.notice{heading} — vendor-synthesized notices were drawn via
  a daemon-composed heading prepended to the prose (an unmarked register).
- Comment-only: WebWorkspaceTransferred = notice + host reload, no in-page
  redial (the origin changes with the port); FooterAllowance.status UNSET
  is legal ("no vendor verdict yet").
- PENDING: server-derived daemon arms from wave 3; retirement of any
  landed-but-unused arms; FeedPermissionArguments' wire source.

## Explicitly NOT changed (rulings recorded instead)

- WatchAgentSession / WatchFeed refusals: no failure frame — a refused open
  closes at the transport (the contract's own convention).
- FeedMergeTabLabel: no badge counts (label + state only).
- FailureKind: no carrier this wave.
- HibernateError: no free-text detail beyond compaction_failed.error.
