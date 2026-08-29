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

## Landing 3 — STAGED on overhaul/landing-3 (not yet landed)

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
- PENDING before landing: the daemon's derived error arms (every empty
  `<Rpc>Error` in agentrepl.v1, `transferring_away`, `not_yet_adopted`, the
  DaemonFault/SessionFault/HostFault kind oneofs).

## Explicitly NOT changed (rulings recorded instead)

- WatchAgentSession / WatchFeed refusals: no failure frame — a refused open
  closes at the transport (the contract's own convention).
- FeedMergeTabLabel: no badge counts (label + state only).
- FailureKind: no carrier this wave.
- HibernateError: no free-text detail beyond compaction_failed.error.
