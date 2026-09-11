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

## Landing 5 — overhaul/integration (2026-08-29; protos e6cb62dbd, 480d8f75b, 01851abf7, 5e7a4bf70)

OWN ACCORD:
- AgentBashOutput.form gains `not_observed` — a lost/reconciled run had to
  lie with `partial{bytes_omitted: 0}`.
- FeedResponse.notice{heading} — vendor-synthesized notices were drawn via
  a daemon-composed heading prepended to the prose (an unmarked register).
- Comment-only: WebWorkspaceTransferred = notice + host reload, no in-page
  redial (the origin changes with the port); FooterAllowance.status UNSET
  is legal ("no vendor verdict yet").
- UpdateMergeQueuePause/Resume gain `optional workspace.v1.RepositoryRef
  repository` (UNSET = every repository) — the queue is per repository
  but the request carried no scope, so pause was daemon-wide.
- Deferred to landing 6: server-derived daemon arms from wave 3;
  retirement of any landed-but-unused arms; FeedPermissionArguments' wire
  source.

## Landing 6 — overhaul/integration (2026-09-01; protos d46e601e7)

OWN ACCORD (all plain-data arms with a real refusal/answer site, the
"derived at the wave" class; raised by the daemon's wave-3 wiring):
- SubmitPromptSuccess.command_acted (empty) — a recognized session act that
  mints no turn (/model <arg>, the picker path) had no success arm.
- SubmitPromptError.duplicate_submission (empty) — the client-minted
  idempotency_key was already accepted for the workspace.
- UpdateMergeQueueError.unknown_repository (empty) — pause/resume named a
  RepositoryRef the registry does not hold.
- RETIRED SubmitPromptError.turn_already_open (tag 8 reserved) — no producer
  anywhere: a busy subagent is refused by the SHIM (UpdateAgentFailure) and
  relayed; the daemon never judges a subagent's turn.

ALREADY LANDED, no change (proposals answered by reading the contract):
- SubmitPromptCommandPanel.status exists (tag 1); the thin /status panel has
  its wire home. AnswerQuestionError already splits ask_not_standing (5) from
  unserved_value (6).

RULED, no change:
- Watch* rpcs carry no <Rpc>Error by design; a refused open (unknown or
  unowned workspace, unknown/expired FeedWatchToken, no login pty) closes at
  the transport per the refused-open convention. The daemon's ERROR-ARMS
  records these as transport-closed, not unlanded.
- OpenWorkspaceTranscriptMissing.searched_paths stays a bare list this wave;
  the webapp renders the count. A composed sentence is deferred.

## Landing 7 — overhaul/integration (2026-09-02; protos ab7e681f2, bindings c10714a41)

OWN ACCORD (plain-data fields and arms, each with a recorded producer or
refusal site; raised by the leads' final reports and the daemon handoff):
- agentrepl SubmitPromptError.bubble_refused (tag 11) = SubmitPromptBubbleRefused
  {detail; kind: not_deliverable (2) | agent_busy (3)} — the shim's refusal of
  a bubble-addressed prompt, relayed by kind (the daemon never judges a
  subagent's turn; why tag 8 retired).
- shim.v1 StartSessionFresh.model is now `optional` — UNSET = SDK default;
  SessionStarted.effective_model states what took effect.
- shim.v1 WatchSessionResponse is now `oneof frame { update = 1;
  session_started = 2 }` — the original SessionStarted re-announced ONCE per
  watch, right after the opening diagnostics, on EVERY new watch, so an
  adopting daemon (crash boot, handover) attaches purely. Tag 1 unchanged.
- shim.v1 UpdateAgentFailure.agent_busy (tag 8, empty) — prompt to a subagent
  whose own turn is running; the daemon relays it as bubble_refused{agent_busy}.
- agentrepl CloseWorkspaceBlocked gains {bool turn_in_flight=1; uint32
  live_work=2; uint32 held_prompts=3; bool merge_queued=4; string summary=5}
  — the footer's close-blocked evidence, so a caller with no footer can say why.
- store.v1 OpenAgentSessionFailure.unknown_agent (tag 5, empty) — a well-formed
  agent id naming no book is refused, not served empty; shim maps to NotFound.
  Cross-plane; store lead agreed at park.
- frontend.v1 FeedMergeAbandoned.summary (string, tag 1) — the resolved
  sentence for the collapsed line, as FeedMergeFailed carries.

DEFERRED (no producer pressure yet): AgentToolFailure denied marker (only if
the permission-id join proves awkward); OpenWorkspaceTranscriptMissing
composed text.

## Landing 8 — overhaul/integration (2026-09-02; protos 1fdf85e63, bindings 3791cd630; USER-APPROVED)

Raised by the rebuilt e2e suite's writers reading the contract; both are wire
homes for behavior already ruled:
- frontend.v1 FeedSessionSeparation.kind.compaction_failed (tag 7) =
  FeedContextCutCompactionFailed{error} — a compaction that was offered
  (/compact, the cold gate's compact remedy) and did not happen, drawn in the
  slot the compacted divider would have taken; `tokens` UNSET. Relays
  conversation.v1.ContextCut.compaction_failed. Previously: nothing drawn.
- frontend.v1 FeedTurnEndedErrored.error gains the run's own terminals,
  importing failure.proto's evidence messages as that file prescribes:
  max_turns (19, FailureVendorMaxTurns), max_budget (20, FailureVendorMaxBudget),
  execution_error (21, FailureVendorExecutionError), turn_failed (22,
  FailureVendorTurnFailed, also carries structured_output_retry_exhausted via
  stop_reason), stop_hook_prevented (23, new empty FeedTurnErrorStopHookPrevented).
  Previously: AgentFailure.max_turns / budget_exhausted / execution_error /
  structured_output_retry_exhausted / stop_hook_prevented had NO wire path to
  any frontend stream.

RULED, no proto: context_budget_warning gets no feed row (footer only).
Ctrl-b was removed from the inventory on 2026-09-04 by owner ruling and may
be added back later.

## Landing 10 — overhaul/integration (2026-09-04; USER-APPROVED)

Four forgotten plain-data shapes the rebuilt e2e coverage exposed (tests
written to the proto found no arm to assert). Fast mode was explicitly NOT
given a frontend surface (owner ruling: "no fast mode").

- frontend.v1 FeedAgentPrompt.delivery oneof (tags 3-4):
  FeedAgentPromptQueuedToLive | FeedAgentPromptResumedRecipient — mirrors
  conversation.v1 AgentSendMessageSuccess's queued_to_live / resumed_recipient
  arms on the SENDER's row; UNSET on the recipient's copy.
- frontend.v1 FeedPermissionAnswered.denied_undecidable (tag 6) =
  FeedPermissionDeniedUndecidable{text} — the shim's
  AgentPermissionDenied.undecidable relayed as its own arm instead of being
  folded onto denied_by_policy.
- frontend.v1 FeedTurnErrorQueryDied.cause oneof (tags 1-2):
  FeedTurnErrorQueryUnexpectedEof | FeedTurnErrorQueryIteratorFailure —
  mirrors conversation.v1 SessionQueryDied.
- agentrepl.v1 SetModelError.cold (tag 8) = SetModelCold{} — the shim's
  SetSessionModelFailure.cold relayed by name; the remediation menu is the
  cold gate row's (AnswerColdGate). Closes the ERROR-ARMS row.

## Landing 9 — overhaul/integration (2026-09-03; USER-APPROVED)

- agentrepl.v1 OpenWorkspaceError.vendor_start_failed (tag 8) =
  OpenWorkspaceVendorStartFailed{detail} — the shim's
  StartSessionFailure.vendor_start_failed relayed by name. Previously the
  daemon relayed it through the unlanded-arm convention (failed_precondition
  "intended arm ..."), which the rebuilt e2e suite exposed as untyped.

USER RULING 2026-09-03 (no proto): "the proto always wins" — where the
daemon's drawing disagrees with the proto's framing, the proto governs.
First application: an INTERRUPTED Bash run is a SUCCESS arm carrying the
interrupted marker (conversation.v1 AgentBashInterrupted sits inside
AgentBashSuccess), so FeedToolCallReturned's verdict is `succeeded` with the
interrupted text, not `failed`. The daemon's deliberate `failed` drawing
(resolve/feed/toolcall.go) is a defect.

## Cross-system ruling, no proto (2026-09-02)

- KERNEL LOCKS: the shim takes the WORKSPACE lock inside StartSession (beside
  the session lock), not at process startup. Reason: the rollout's shim
  relaunch prelaunches an inert shim beside the live one; a startup flock
  blocked it forever. An inert shim holds no lock; probe semantics unchanged.
  Supersedes the kickoff text "shim-held from startup" in every plan doc.

## Explicitly NOT changed (rulings recorded instead)

- WatchAgentSession / WatchFeed refusals: no failure frame — a refused open
  closes at the transport (the contract's own convention).
- FeedMergeTabLabel: no badge counts (label + state only).
- FailureKind: no carrier this wave.
- HibernateError: no free-text detail beyond compaction_failed.error.

## Landing 11 (2026-09-04): a cause on the feed's lost arms

- frontend.v1 FeedShellLost and FeedSubagentLost gain `oneof how
  {file_vanished | went_silent | swept_up}` (empty markers), mirroring
  conversation.v1 DetachedLost.how one-to-one. Previously both were empty
  messages, so the frontend could not say which lost it was, and the e2e
  suite had to pin the arm on the sidecar's own terminal reason instead of
  the feed. Owner ruling 2026-09-04: "yes, carry it". Daemon relays the arm
  by name; webapp draws it; Emacs draws no feed (no change).

## Landing 12 (2026-09-05): a settled spawn can name the agent it created

OWNER-DELEGATED LEAD RULING (the project lead ruled this in on the owner's
behalf; protos 974f5356e).

- conversation.v1 AgentSubagentSuccess.created_agent_id (tag 8, AgentId) —
  the same join key AgentSubagentStart states, restated on the conclusion.
  UNSET stays legal and means the producer could not name the agent on this
  frame; a consumer that already saw the start is unaffected.

WHY. The success arm is the ONLY frame some deliveries ever carry. Every frame
of one spawn shares one store upsert key, so a history replay hands a consumer
the terminal and nothing else; a transcript-only session the sidecar read with
no shim watching does the same, since the sidecar produces a start only for an
async launch. The daemon already held a settled frame that outran its start
(resolve/feed/subagent.go, retireHeldSpawns), but a start that is never coming
cannot be waited for: the bubble was drawn warned and its OpenFeed then refused
as feed_undecodable, because the row named no agent.

NOT A DERIVATION, AND THIS IS THE POINT. detached_work.proto forbids deriving
one identity from another. This field makes that derivation UNNECESSARY rather
than permitted — the producer states what it knows, and no consumer has to
reconstruct it from the unit id, the calling agent, or a spool's file name.

PRODUCERS. All three fill it, with the minting rule's value (the spawning
call's tool_use_id):
- shim convert/tools/subagent.ts — the awaited spawn's tool result.
- shim convert/detached.ts — a background task's settling notification.
- shim-sidecar internal/convert/subagent.go — the transcript's settled spawn,
  the delivery the field exists for. Its helper answers nil for an empty id,
  so a call whose own id is unknown leaves the field UNSET.

CONSUMER. daemon resolve/feed/subagent.go takes the created agent from
whichever frame states it. A naming success draws at once, mints its sub-feed
and earns no subagent_without_start warning; a success without one keeps the
hold-then-warn path untouched. Held frames released by a non-start naming
frame fold BEFORE it, since the terminal is later in the run than everything
it outran.

## Landing 13 (2026-09-09): a standing state the strip never drew, and a read nobody could tell had failed

OWNER-DELEGATED LEAD RULING (the project lead ruled this in on the owner's
behalf). Seven scenarios stood in the matrix as WEAK BY CONTRACT: the fake
produced them, the shim converted them, the daemon received them on
conversation.v1 SessionUpdate, and then nothing could be asserted at any
user-visible surface because frontend/v1 carried no field for them. A mock
the product cannot draw is a product gap.

- frontend.v1 TopbarView.fast_mode (tag 10, TopbarFastMode with arms
  on / off{reason} / cooldown) — the permission mode's sibling, the other
  session-level standing state a reader has to know before sending. UNSET
  stays legal and means the vendor has stated none.
- frontend.v1 FooterStatusActivityRateLimited.sample (tag 3,
  FooterAllowanceSample with arms available / service_unavailable /
  window_unavailable / utilization_unavailable / sampling_failure{cause}) —
  arm for arm with conversation.v1 SessionAccountUsage's own outcome oneof.

WHY FAST MODE IS THE TOPBAR'S. It is a session-level standing state, not a
turn fact and not an activity, which is the permission mode's own shape and
the permission mode's own home. THE ARM IS THE STATE, never a bool: cooldown
is neither on nor off, and a bool forces the client to draw it as one of them
— offering a switch that cannot take effect, or claiming a speed the session
does not have. SessionFastModeCooldown carries no reset instant, so neither
does ours; a countdown would be invented rather than reported. It is a LABEL
and not a picker, because nothing on the contract sets fast mode.

WHY THE OUTCOME RIDES BESIDE THE FIGURES. The allowance figures are the last
ones READ; the outcome says whether the last ATTEMPT read anything. Without
it a sample that failed is indistinguishable from one that never happened,
and the strip goes on drawing yesterday's percentage as though it were
today's. It is carried ALONGSIDE the standing figures and NEVER by clearing
them — the footer's own sourcing rule ("a sample that could read no figure
leaves the figures on hand standing") is unchanged, and now has a
drawn-surface test.

ONE BEHAVIOR CHANGE, STATED. The rate line's newsworthiness gate now opens on
an unread sample as well as on a newsworthy allowance. Without that the
outcome cell is unreachable from any session whose figures sit under the 0.8
threshold — which is every session this mock produces. An unremarkable
allowance is not news; an allowance nobody could read is.

CONSUMERS. daemon resolve/topbar (the empty fast_mode branch fills the view
arm for arm) and resolve/footer (every sample files its outcome, readable or
not; unreadability is kept in state rather than sniffed back out of the drawn
arm, so a producer's reasonless unavailable is not read as a success).
webapp draws a read-only `.topbar-fast` cell after the mode picker and a
`.footer-allowance-unread` caveat beside the allowance figures; `available`
draws nothing, being the outcome that reports nothing is wrong.

EMACS IS UNTOUCHED, deliberately: `lisp/` draws neither the permission mode
nor the allowance line, so there is no sibling surface for either field to
join.

A DEAD TRIGGER FOUND AND CLOSED. `!usage-sampling-failure` answered
`behaviors: null`, a shape `accountUsageUpdate` never reads — it branches on
`rate_limits_available`, `rate_limits` and the five-hour window only — so the
scenario produced the AVAILABLE outcome and named an arm it could not reach.
The shim's one producer of `sampling_failure` is the catch around the usage
probe, so the mock now RAISES there.

Claude-Session: https://claude.ai/code/session_01MydqUQAkLfSwBAz9wL5scJ

## Landing 14 (2026-09-09): a refused send is not a send that stated nothing

OWNER-DELEGATED LEAD RULING (the project lead ruled this in on the owner's
behalf; protos 7660044a2).

- frontend.v1 FeedAgentPrompt.delivery gains `refused` (tag 5,
  FeedAgentPromptRefused), whose sole field is
  `optional FeedAgentPromptRefusalReason reason` (tag 1, one `string text`).
  The two existing arms are unchanged; the oneof's own comment now reads "how
  the send FARED" rather than "was delivered".

WHY. The oneof carried only arms that say how a send LANDED, so a REFUSED send
was drawn exactly as one whose producer merely observed nothing — and the
oneof's documented meaning for unset ("the producer observed nothing; the
row's presence already says the attempt happened") is precisely what "not yet
delivered" looks like. A reader was left assuming a message was still on its
way to an agent that will never receive it. Two independent e2e agents
recorded the same gap, and TestSendMessageRefused could only prove the refusal
by asserting a negative that is also its own opposite.

A REASON AND NO KIND, because a reason is all the producer has. The vendor
answers a refused send with `success: false` and a sentence written for the
model to read; it declares no refusal code, and the distinction between a
recipient the user stopped and one that never existed lives only inside that
prose. Recovering it would mean parsing the sentence — the same rule
AgentSendMessageResumedRecipient states for itself. The reason is UNSET when
the refusal carried no content at all: the arm is the refusal, the reason only
its detail.

NO CONVERSATION-TIER CHANGE. AgentSendMessage.failure has carried this fact
since the foundation, and both producers already fill it — shim
convert/tools/send-message.ts (`failureOf(outcome)`) and shim-sidecar
internal/convert/settled_items.go (kindSendMessage, failed). The gap was
entirely in the frontend tier; this landing is where the fact reaches a
surface. Both producers gained a test pinning the refusal PROSE into
AgentToolFailure.content, which is now load-bearing.

CONSUMERS. daemon resolve/feed/sendmessage.go's failure arm sets `refused`
ALWAYS — a contentless refusal is still a refusal and must not fall back to
the unset oneof — reading the reason with failureText, the same reading every
failed tool call's account gets. webapp feed/rows/agent-prompt.ts draws it on
the SAME delivery marker the two landings wear, with a `refused` class in the
feed's error color and the producer's words in an element of their own, so
this client's wording and the vendor's stay tellable apart. Emacs draws no
feed (no change).

## Landing 15 (2026-09-09): the page learns which session its logs belong to

OWNER-DELEGATED LEAD RULING (the project lead ruled this in on the owner's
behalf).

- agentrepl.v1 WatchWebWorkspaceResponse.push gains `session_identity` (tag 2,
  WebWorkspaceSessionIdentity), a new message carrying
  `agent_repl_session_id` (tag 1) and `claude_session_id` (tag 2). Both may be
  empty: a workspace with no session yet, and a session with no vendor
  conversation yet, are legitimate states.

WHY. Four of the five runtimes stamp a correlation identity on their records —
the daemon `agent_repl_session_id`, the shim the SDK `request_id`, Emacs
`claude_session_id` off HostSessionLive, the sidecar its own. The webapp was
the exception: `bindLogContext` knew only `connection_id`, so a browser-side
ClientLog record could not be joined to the daemon session it was logged
against and a user-visible fault in the page could not be traced to the vendor
session behind it. The webapp logger was already built for this — its
RuntimeLogContext has both identity fields and `restampRecordIdentity` restamps
both — and nothing ever bound them.

WHY THIS FRAME. failure.proto states the rule that "an agent-repl session
identity is a vocabulary a rendering frontend does not have", which bars
frontend/v1 (the topbar's session line included). WatchWebWorkspace is the
page's standing, per-workspace, explicitly NOT DRAWN link stream — the
webview's analog of the host stream Emacs binds its own identity from.

STATE, NOT AN EVENT. `session_identity` is published on its own topic and
merged onto the wire beside the `transferred` event, exactly as the host
stream carries `host` beside its four event arms and for the same reason: a
Topic replays only its latest value, so one topic carrying both would hand a
late subscriber whichever came last. The daemon composes it before subscribing
(so every fresh stream opens with one) and republishes it on every edge that
republishes the host view, so a restart or a handover that mints a new identity
is followed rather than remembered from boot.

NO OTHER RUNTIME CHANGES. The daemon already promotes `agent_repl_session_id`
and `claude_session_id` out of a forwarded ClientLog record's context into the
persisted record's own fields (dlog.promote), so the field names the page
stamps are the ones the other four runtimes are already grepped by.

## Landing 16 (2026-09-09): a tool call can return a picture, and a shell says how it exited

OWNER-DELEGATED LEAD RULING (the project lead ruled this in on the owner's
behalf). Two plain-data fields, both found by owner 13 in the real webview,
both on the ONE message the whole grey tool card settles into.

- frontend.v1 FeedToolCallReturned.form gains `image` (tag 11), carrying the
  EXISTING FeedImageBlock. FeedToolCallReturned also gains
  `optional FeedShellExit exit` (tag 12), the EXISTING chip element the detached
  shell's settled shape already carries.

WHY THE IMAGE ARM. The form oneof offered succeeded / failed / text / code /
diff / lines / links / none and no image, so `!bash-image` — a shell whose
stdout IS a screenshot — settled to `none` and the card drew NOTHING: a picture
and a command that printed nothing were the same drawing. The conversation tier
has carried the fact from the start (AgentBashOutput.form = image); this is
where it reaches a surface, which is the same story landing 14 tells about a
refused send.

REUSED, NOT REINVENTED. FeedImageBlock is the block a prompt body draws an image
with, and its documented contract — "an image the webview can fetch; the daemon
RESOLVES the record's reference into a src a browser can load" — is exactly what
a tool's image needs. A second image message would have given the same picture
two resolutions that could disagree, so the arm carries that one.

WHY THE EXIT FIELD, AND WHY THE SAME ELEMENT. A failing foreground `Bash` drew
`failed` and no number: the reader was told the command went wrong and never
told how. A foreground shell and a detached one are the SAME COMMAND TOLD TWICE,
so the field sits where a reader of FeedShellSettled would look for it and
carries FeedShellExit itself. The site states the parallel as a rule: a field
added to, retyped on, or reworded for one of the two belongs on the other in the
same edit. Unset is the honest state (a foreground call ordinarily states no
status, and a KILLED command never has a number) and, as FeedShellSettled.exit
already says for itself, absence draws no chip, NEVER a zero.

TWO PRODUCERS WERE STARVING THE TIER, AND BOTH ARE FIXED. `isImage` on the
vendor's Output object states only THAT the output was an image; the bytes and
the media type arrive on the answering `tool_result`'s own image content block,
which is the only place either is stated.

- shim convert/tools/bash.ts refused EVERY image output and produced no terminal
  frame at all, so an image-producing shell's unit stayed open forever. It now
  reads the image block back out of the data url the shim carries an inlined
  image by. The loud refusal is unchanged for a result carrying no loadable
  bytes: a fetchable url names no bytes, and a payload that does not decode is
  half an image.
- shim-sidecar internal/convert/results.go read `mediaType` off the Output
  object — a field that is not there — and set no bytes, so its image arm
  carried neither half. It now reads the result block and decodes the payload.

NO CONVERSATION-TIER CHANGE. AgentBashOutputImage{data, media_type} and
AgentBashCompleted.termination.exited.code both predate this landing; the
foreground exit code has been filled by both producers since the two `!bash-fail`
fixes (shim 14df2e44a, sidecar's statedExitCode), which is what makes
`termination` legitimately SET for a foreground command that exited non-zero
even though the field's own comment describes it as a detached-shell fact.

CONSUMERS. daemon resolve/feed/toolcall.go composes the src as
`data:<media_type>;base64,<bytes>` and captions it with the command line — the
only caption the record affords — and refuses loudly under
`daemon.feed.bash_image_unresolved` when either half is missing, falling back to
`none` rather than serving a src that renders as a broken image on every client.
bashExit relays the stated termination onto the new field. webapp
feed/cards/tool-call.ts delegates BOTH drawings rather than writing new ones:
the image goes through drawFeedImageBlock (the prompt body's own drawing) and
the chip through drawFeedShellExit (the detached shell's own chip), so neither
pair can drift.

EMACS IS UNTOUCHED, deliberately: `lisp/` draws no feed.

## Landing 17 (2026-09-09): a fork mints a new AgentId and ports the history under it

OWNER-DELEGATED LEAD RULING. A comment correction on conversation.v1 AgentId's
cross-plane minting rule, no field change: a resume or rotation keeps the
original vendor session id; a FORK mints a new one and ports the history under
it, re-minting every record identity the file plane derives upsert keys from
(record uuid and every *uuid link, message.id, tool_use ids, sessionId, prompt
and task ids, sidecar file names). A byte-copied fork re-keyed identical
content under a second book and the store refused the move ("would move the
row from book A to book B"), parking the fork's file plane. Built in
daemon/internal/remint and account.PortTranscript.

## Landing 18 (2026-09-10): a tool call whose work moved to the background says so

OWNER-DELEGATED LEAD RULING, found by owner 13 (F43). frontend.v1
FeedSimpleToolCall.outcome gains `moved` (tag 6, FeedToolCallMoved, empty). A
backgrounded shell drew two rows: the detached shell row that settles, and the
Bash card above it stuck on `running` forever, because the card had no arm for
work that MOVED. The arm carries no verdict and no output; the detached shell
row is the record of the run.

## Two plain-data presence fields (2026-09-10)

- store.v1 OpenAgentSessionRequest gains `bool page_only` (tag 4). A one-shot
  read says so at open, the store mints no watch token, and `watch` in the
  success is unset. Why: OpenAgentSession is unary with no close, so a token
  minted for a read that never watched lived for the store's process lifetime
  (two per turn, measured). Five shim one-shot sites now open page-only.
- conversation.v1 AgentTaskState `subject` and `description` become
  `optional`. Presence lets a consumer tell "the act named no subject" from
  "the subject is empty", as `owner` already could; the daemon's checklist
  keeps what it holds when an act does not name one.

Also this session, comment-only: frontend.v1 RosterRow.attention states both
clears (selection, and the last open ask settling).

## Logging plain-data fields (2026-09-10/11)

- agentrepl.v1 ClientLogRecord gains `timestamp` (8, the client's own RFC 3339
  instant; unset = daemon arrival clock), `verbose` (9, the record's own
  verbosity class), and `oneof runtime { webapp = 10; sidecar = 11 }` (which
  client wrote it, so the daemon files it in that runtime's per-workspace
  sink; unset = webapp). Why: every forwarded record persisted with the
  arrival clock and a recomputed class, and `sidecar.log` was never written
  because the daemon hardcoded the webapp.
- agentrepl.v1 DaemonHealth gains `DaemonIdentity identity` (instance_id, pid,
  build_sha). Why: Emacs's restart verb answered "completed" on a link cycle
  with no new daemon; completion now requires observing a different instance.

Claude-Session: https://claude.ai/code/session_01MydqUQAkLfSwBAz9wL5scJ
