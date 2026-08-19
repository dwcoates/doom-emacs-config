# DESIGN: the figma→idl redesign of the agent-repl contract

## The problem, in the user's terms

`frontend.v1` is the figma→idl specification for the frontend: one file per UI
component, each component's props one message, resolved by the server and
rendered verbatim. `agentrepl.v1` is the API the webapp actually talks to.
Therefore `agentrepl.v1` should be COMPOSED of `frontend.v1` messages, passed
along component-dedicated endpoints — the sidebar talks to an endpoint that
ships `frontend.v1` sidebar messages, likewise the topbar, the footer, the feed
— not a god-endpoint that handles all shapes.

That is NOT how it works today. `service AgentRepl` is a pure command surface:
34 unary RPCs, 30 of them returning an empty `Success`, none returning a
component view. With the one multiplexed push stream deleted (see the
superseded record), no `frontend.v1` component data has any path to the webapp
at all. The composition rule is nowhere in force.

The redesign walks every surface, ONE `.proto` FILE AT A TIME, in the order
settled under "The iteration sequence, as walked" below (`conversation.v1`
first, then `frontend.v1`, `agentrepl.v1`, `shim.v1`, `store.v1`, `state.v1`). For `agentrepl.v1` the RPC inventory is hashed out explicitly first,
then the shapes.

## The record this supersedes

`proto/DESIGN-protobuf-surfaces.superseded.md` is the previous design record,
kept as a backup. Every decision it records STANDS unless an entry below
reopens it by name. In particular the following are settled and are NOT
re-litigated here:

- The six surfaces and the package-boundary-is-surface-boundary rule.
- Package names encode ownership, not routing; the drawn/called test between
  `frontend` and `agentrepl`.
- `agentrepl.v1` is a Connect service, not gRPC, because of the clients (an
  xwidget WebKit view and elisp).
- Every endpoint owns its request and response, one `endpoint_` file each.
- Every response is a two-arm `oneof { success; error }`; errors are typed
  messages in band, never status codes; the async boundary keeps
  `FailureCardView` as a push.
- One canonical form per message; import the encompassing message; re-spelling
  in its four forms is a defect; depth is not synthesis.
- The one multiplexed push stream is REVERSED: one server-streaming endpoint
  per UI component, with the two surviving ordering failures (typing-cut vs
  conversation-delta; cross-stream workspace references) accepted by the user
  as the price and owed a conventions-stage answer.

## The iteration sequence, as walked

**Settled.** Six stages, one per surface, walked ONE `.proto` FILE AT A TIME
within each. A later stage opens only after every earlier one is settled; any
stage may be reopened at any time, and reopening reopens every decision
downstream of it, enumerated by name.

1. **`conversation.v1`** — COMPLETE at `363323f1b`. Walked `message` →
   `tool_call` → `agent` → `user` → `content_blocks` → `context_cut` → `api`
   → `detached_work` → `session_command` (the per-concern file set after the
   folds and split recorded under Landed changes; `tokens` folded into `api`
   on its turn; originally `message` → `payloads` → `content` → `tokens` →
   `session_command`).
2. **`frontend.v1`** — `sidebar` → `topbar` → `footer` → `failure` → `feed`.
3. **`agentrepl.v1`**, from the empty service, in three sub-stages:
   - 3a. RPC inventory — every method by name and one-line purpose, no
     shapes, each ruled on one at a time.
   - 3b. Cross-endpoint conventions — including the invariants the deleted
     `service.proto` header carried and this record did not carry forward.
   - 3c. Per-endpoint shapes — one new `endpoint_*.proto` at a time.
4. **`shim.v1`** — `core` → `entry-delivery` → `message-page` → `external` →
   `bookkeeping`.
5. **`store.v1`** — `write` → `entry` → `cursor` → `unsupported`.
6. **`state.v1`** — `durable.proto`.

**Within a stage, files are walked TOP-DOWN BY CONTAINMENT.** The user's
second amendment, made after the sequence was first accepted: the file
declaring the highest-level (outermost) message goes first, then the files it
imports, down to the leaves, so a leaf is never designed before the message
that embeds it has said what it needs. The concrete order is MECHANICAL — read
off the intra-package import graph, importer before imported — and was
recomputed for stages 1, 4 and 5 above (`message.proto` imports `payloads`,
which imports `content` and `tokens`; `core`/`entry-delivery`/`message-page`
all import `external`, which imports `bookkeeping`; `write` imports `entry`
and `cursor`, `entry` imports `unsupported`). Stage 2's component files have
no containment relation, so their order stays as the user accepted it.
`tokens` before `session_command` was arbitrary between two leaves (moot once
`tokens` folded into `api`). This convention was added to the
`/create-or-update-protobufs` skill by a one-shot subagent — PR "walk one
package's proto files top-down by containment" (explanation-engine #7447,
MERGED). Two sibling conventions from this session landed the same way:
"add group-by-concern convention" (#7449, MERGED) and "add
adjacent-exclusivity enum-to-oneof test" (#7448, open — CI green, merge-queue
add pending a GitHub outage).

**Retracted by this amendment.** The `content.proto` increment sketched
before the amendment (ThinkingBlock as a two-arm oneof, ImageBlock's location
split into path/url arms, UnsupportedBlock.raw's exception stated at the
field, ToolCallBlock.arguments deferred to its own increment) is WITHDRAWN
unagreed and returns when `content.proto` comes up in the top-down order — by
then `payloads.proto` will have said what it needs from it. (It did return, as
`content_blocks.proto`, and landed at `3b55689e3` with the same three
decisions.)

**The amendments the user made, and why.** The first proposal put
`frontend.v1` before `conversation.v1`, and the file order within stages 3–6
was proposed by the orchestrator. The user moved `conversation.v1` FIRST
because the consumer-first order would have had every `frontend.v1` message
that embeds a `conversation.v1` type reopened by the leaf changing underneath
it — the leaf every other surface imports is settled before its importers.
The user also ordered the `agentrepl.v1` clean slate BEFORE the sequence was
settled (see the landed change below), so stage 3 begins from
`service AgentRepl {}` rather than from the old method table. The remaining
orders were accepted as proposed. The stage-1 transport decision from the
superseded record — Connect, one server-streaming endpoint per component —
is carried in, not re-walked; the sequence's stage 3 designs endpoints on
that transport.

## Landed changes

### 3c: SessionHealth lands

**What changed ("looks good").** endpoint_session_health.proto: request
{ WorkspaceRef }; response result { success { healthy | unhealthy{ repeated
SessionFault } } | error {} }. SessionFault = DaemonFault's discipline
({detail} now, kind oneof added with derived arms at the wave, from the
session controller's real fault sites) but DELIBERATELY ITS OWN TYPE — two
fault vocabularies with different producers, not one shared one.

### 3c: DaemonHealth lands — typed fault classes + dynamic detail

**What changed ("okay").** endpoint_daemon_health.proto: DaemonHealthRequest
{}; response result { success { oneof health { healthy | unhealthy{ repeated
DaemonFault } } } | error {} }. UNHEALTHY IS AN ANSWER, never an error.

**The user's ruling on fault representation.** Typed fault arms, "with
supported string for dynamic error details as needed — general classes of
failures represented with messages (recursively) as far as readily doable,
but always supporting detailed dynamic information by way of strings."
DaemonFault = { string detail } + a kind oneof ADDED WITH its first derived
arms at the wave (empty oneofs are illegal; inventing arms would violate
derived-not-invented). The pending file's RuntimeFault (string component/
fault_type/impact/cause) is this message's ancestor and is judged INTO those
arms at the host stream's turn.

### 3c: UpdateMergeQueue lands

**What changed ("looks good").** endpoint_update_merge_queue.proto: request
oneof action { pause | resume | evict{WorkspaceRef} }; success {} | error {}
(arms derived: already paused, not paused, no such queued merge).
Consolidates PauseMergeQueue/ResumeMergeQueue/EvictMerge. Purely inbound —
the queue's visible state rides the merge bubbles' queue tabs and the
roster's status arms.

### 3c: UpdateShutdownSchedule lands — DAEMON ADMIN section opens

**What changed ("okay these look good").** endpoint_update_shutdown_schedule
.proto: request oneof action { schedule{at_ms} | cancel | now }; success {} |
error {} (arms derived: nothing scheduled to cancel, a newer schedule
stands). Consolidates ScheduleShutdown/CancelScheduledShutdown/Shutdown.

**Clarified for the record (the user asked what it is for and why elisp).**
No UX motivation exists — it is DEPLOY TOOLING's drain-and-exit control,
purely inbound; elisp is merely today's plumbing to reach it, and nothing
about the schedule is pushed outward on this verb. The user-visible
consequences ride surfaces already modeled: held prompts in the tray during
the drain, footer status. Kept in agentrepl.v1 (typed, one service) rather
than a side mechanism. Review discipline correction, also applied from here
on: an RPC is presented with BOTH its request and response, one RPC at a
time unless RPCs genuinely share messages.

### 3c: WatchFooter, WatchDaemonHolds, UpdateHeldPrompt, AnswerHeldOffer land — FOOTER and DAEMON-HOLD TRAY sections complete

**What changed ("makes sense", after the UX walk).** Four endpoint files +
four rpcs (19 total). WatchFooter / WatchDaemonHolds: { WorkspaceRef } →
stream <Rpc>Response wrapping the view whole. UpdateHeldPrompt:
{ WorkspaceRef; TurnId turn; oneof action { release | drop } } — the TurnId
is the echo-token loop closed: minted at submission
(SubmitPromptSuccess.turn), served inside the tray's HeldPrompt entry,
handed back unchanged; release = deliver NOW (interrupting the running turn
when that is what delivery takes), drop = discard (the composer takes the
text back); doing nothing is the normal path. AnswerHeldOffer: addressed by
OFFER KIND (a workspace has at most one offer of a kind standing — a token
would over-address); merge_dequeue { keep | release }. All success arms are
"done — the tray's new state arrives on its stream"; error arms empty until
derived. The echo-token skill convention merged as explanation-engine #7492.

### CONVENTION (3b addition): every rpc returns `<RpcName>Response`; CORRECTION: ten rpcs had silently missed the service block

**The user's ruling.** No rpc returns a foreign type directly — a stream of a
frontend view returns `stream <RpcName>Response` wrapping the view as its
single field, declared in that rpc's endpoint file. Landed by a sonnet
subagent for the three violations (WatchFeedResponse{row},
WatchWorkspaceRosterResponse{roster}, WatchTopbarResponse{topbar}); every
later endpoint follows it directly.

**CORRECTION, KEPT VISIBLE.** The subagent's report exposed that
service.proto held only FIVE rpcs: the orchestrator's earlier service-block
edits for the sidebar and topbar sections (WatchWorkspaceRoster,
CreateWorkspace, the six workspace verbs, WatchTopbar, SetModel) had
SILENTLY NO-OP'D — python str.replace anchored on text that was not in the
file (it assumed AnswerPermission closed the block; SubmitPrompt did), and
nothing asserted the replacement took. The endpoint files and design entries
were always right; only the service block lagged. Rebuilt whole with all 15
rpcs in section order, compiles. ROOT CAUSE: unasserted textual replaces on
a file whose shape had drifted; the fix discipline is asserting every
replace (as the feed.proto edits did) or rebuilding the block wholesale.

### 3c: WatchTopbar + SetModel land; `AgentModel` — the typed-echo-token pattern (stage-1 api.proto reopened additively)

**The user's pattern, stated and codified.** "We should have a message
encapsulating fields that are dynamically determined by the backend and
reused by the frontend in future requests — the frontend can't/shouldn't
know the supported values — wrapped so the reuse is obvious." Named the
TYPED ECHO TOKEN: the provider mints a wrapper message, embeds it in what it
serves, and the request field is the SAME type echoed back unchanged — the
round-trip becomes a schema fact, and a client that invents a value is typed
as wrong rather than merely told not to. Dispatched to the skill as a
one-shot subagent (worktree proto-echo-token).

**What changed ("your agent model protobuf suggestion looks good").**
conversation/v1/api.proto: new `AgentModel { name }`; `ModelOption.value`
(string) becomes `AgentModel model = 1` — the token inside each served
option. endpoint_watch_topbar.proto: WatchTopbarRequest { WorkspaceRef } →
stream frontend.v1.TopbarView. endpoint_set_model.proto: SetModelRequest
{ WorkspaceRef; conversation.v1.AgentModel model } → success {} | error {}
(arms derived at the wave: model not among served options, no session). The
TOPBAR service section is complete.

### 3c: the six remaining sidebar verbs land — the SIDEBAR section is complete

**What changed ("those all look simple, let's land them").** Six endpoint
files, each `{ WorkspaceRef } → { success {} | error {} }` with errors
derived later: OpenWorkspace, CloseWorkspace (the only teardown),
MergeWorkspace (success = ENQUEUED; the merge's life from there is the
feed's bubble; it targets the workspace's parent by definition),
HibernateWorkspace, ReviveWorkspace, and RestartWorkspace.

**RestartWorkspace, per the user's spec.** It bounces ONLY the workspace's
shim (rebuild if out of date + restart the process), with `bool force`:
false = GRACEFUL — wait until no turn is in flight and no async/background
tasks run, holding incoming prompts via the daemon hold (tray-visible)
meanwhile; true = FORCED — interrupt the shim (current turn + background
tasks), bounce immediately, and do NOT resume the agent afterwards —
continuing is the user's, with a subsequent prompt. (`force` is a bool, not
a two-arm oneof, consistent with the new skill convention's scope: no
adjacent data changes interpretation under it. The graceful hold implies a
FIFTH daemon-hold reason — restart pending — for the tray's hold oneof:
flagged for the tray shapes at the wave.)

**Consequences.** No MergeWorkspace parameters exist (no target override);
no RestartWorkspace webapp/daemon rebuild semantics — never the webapp,
never the daemon. The sidebar section's eight RPCs are all on the service.

### 3c: CreateWorkspace lands — the daemon names AND creates; the package file model settled (`workspace.proto` shared vocabulary)

**File model (the user's ruling).** agentrepl.v1 is exactly: service.proto
(all RPC signatures), endpoint_<rpc>.proto (that RPC's request/response), and
<shared-package>.proto files for what MORE THAN ONE endpoint needs. So
workspace_ref.proto is FOLDED into a new workspace.proto (WorkspaceRef +
RepositoryRef), and no repository_ref.proto exists.

**CreateWorkspace ("i agree with the protos").** THE DAEMON DOES THE
CREATING — the user: it should name the workspace "because it's the thing
actually creating the workspace worktree/branch/etc". It derives the slug
(from the initial prompt when present) → branch → worktree dir, runs the git
itself, registers the workspace; NO host materialization round-trip exists —
Emacs sees the workspace on the roster and opens its buffer. This supersedes
the orchestrator's host-report-back guess. "Name" was clarified as the
coupled branch name / worktree dir / display name, no abstract identifier;
the dir IS the identity and returns as CreateWorkspaceSuccess.workspace.
Request: RepositoryRef + optional UserSaid initial_prompt + optional
base_ref (both optionals per the new skill convention — presence, never
sentinels; skill PRs #7490 bool→oneof and #7491 optional-null both MERGED).
RepositoryRef is an ADDRESS like WorkspaceRef — base_ref stays a request
parameter, per adjacent-exclusivity, and ref types stay pure addresses.
Error arms empty until derived.

### 3c: WatchWorkspaceRoster lands — the one global stream

**What changed (the user approved the workspace-management RPC prototypes).**
`endpoint_watch_workspace_roster.proto`: WatchWorkspaceRosterRequest {} —
empty on purpose, the roster is global — and the stream's message is
frontend.v1.WorkspaceRoster itself, whole. The SIDEBAR service section opens.
The bool→oneof skill convention is PR "create-or-update-protobufs:
mode-selecting bool is a two-arm oneof" (explanation-engine #7490, queued).

### 3c: AnswerPermission lands; kind ④ reopened — the permission card gains a QUESTIONS body (AskUserQuestion); SubmitPrompt gains its idempotency key

**The user's probe that found the gap.** "I'm not seeing how multiple
selection, user-input, etc are being handled — this is the AskUserQuestion
feature, right? Do we need to research the SDK api?" Researched in the shim's
own contract rather than asserted: the SDK has ONE gate (canUseTool → the
shim's PermissionRequest/PermissionResponse round-trip, core.proto:1043, with
allow-with-edits via updated_input), and AskUserQuestion is a TOOL riding
that same gate — its input is questions[]{question, header, options{label,
description}, multiSelect}, its answer is an allow whose updated_input
carries the selections. The sketched verdict-only endpoint could not carry
answers; the drawn card could not draw a question.

**feed.proto (stage-2 reopen).** FeedPermission gains `oneof body { tool
{headline, arguments} | questions }`; FeedPermissionQuestions is the batch
(1–4); each FeedPermissionQuestion is text + header + `oneof options
{ single_select | multi_select }` — the user's shape: THE ARM IS THE
SELECTION MODE (radios vs checkboxes), replacing the orchestrator's bool,
with dedicated arm messages so a mode-specific prop later (a "pick at most
N") lands on its arm. The mode is PER QUESTION because the SDK puts
multiSelect on each question — one batch can mix a radio and a checkbox
question, so lifting it to the body was unrepresentable. Free-text "Other"
is always offered by the drawn card, never an option. FeedPermissionSuccess
gains the `answered_questions` arm: given answers, one per question, each
{header, chosen[], other_text}, drawn as the verdict line.

**endpoint_answer_permission.proto.** Request { WorkspaceRef; ToolCallId;
oneof answer { allow_once | allow_for_session | deny{reason} | answers } };
AnswerPermissionAnswers = one AnswerPermissionAnswer per question in batch
order {chosen[], other_text}; the daemon translates to
allow-with-updated_input. Body/answer mismatch is a refusal. Success is
"delivered" — the card's state change arrives on the feed stream. Error
empty until derived.

**SubmitPromptRequest** gains `idempotency_key = 2` (the 3b retrofit).

**Flagged, not modeled:** general allow-with-edits (editing a Bash command
before allowing) — the shim wire supports it; no UI exists today.

**Skill update dispatched** (one-shot subagent, worktree
proto-bool-mode-oneof): a boolean that selects the INTERPRETATION of
adjacent data is a two-arm oneof of dedicated (possibly identically-shaped)
arm messages, never an adjacent bool — genericized, per the user's
instruction.

### 3c: Interrupt lands — the folded stop verb; the pending file sheds its ack payload

**What changed ("okay").** `endpoint_interrupt.proto`: InterruptRequest
{ WorkspaceRef; oneof target { turn | detached(MessageId) } }; success
outcome { interrupted_turn | interrupted_detached{count} | nothing_running };
error empty until derived. rpc Interrupt on the service.

**Absorbed.** The pending file's DetachedCancelOutcome/DetachedAgentsCancelled
are DELETED (its shim.v1 import too): `nothing_running` moves from the old
"refused with ok=false" to a SUCCESS ANSWER per the domain-outcome rule — the
old comment's worry ("a stop that missed looks like one that worked") is
answered by the arm being distinct, not by mis-classing a quiet session as a
failure; `count` keeps its frontend-vocabulary rationale (shim task ids stay
on the shim wire); shim.v1's DetachedCancelUnsupported becomes a derived
ERROR arm at the wave.

### 3c: GetFeedPage lands — no cursor exists on the wire

**The user's ruling.** "Cursor should be totally unknown to frontend. It
should only be able to get the first page, and the next page, for the given
source." The DAEMON holds each container's walk position (one webview per
workspace = one reader per container); `first` resets the walk to the newest
page, `next` continues older, and a `next` with no walk standing is a
refusal, not an empty page.

**What changed ("okay").** `endpoint_get_feed_page.proto`:
GetFeedPageRequest { WorkspaceRef; oneof container { top_level |
parent(MessageId) }; oneof page { first | next } }; response
{ frontend.v1.FeedPage success | GetFeedPageError error } — TWO error layers
on purpose (response error = not served: unknown workspace/container,
next-with-no-walk; page error = served with a hole). Error arms empty until
derived. NO page-size parameter — the daemon picks. And in feed.proto,
FeedPageHasMore LOSES its cursor field — an empty arm, purely "older rows
exist".

### 3c: WatchFeed lands; `WorkspaceRef` declared

**What changed ("okay let's continue").** `workspace_ref.proto` — the 3b
shared identity, `WorkspaceRef { string dir }` (one shared type here, unlike
frontend.v1's per-element wrappers, because requests are called, not drawn).
`endpoint_watch_feed.proto` — `WatchFeedRequest { WorkspaceRef workspace }`;
the stream's message is `frontend.v1.FeedRow` ITSELF, whole (an upsert by id).
`rpc WatchFeed(WatchFeedRequest) returns (stream frontend.v1.FeedRow)`.

**Settled with it.** A refused open closes the stream at the transport (no
in-band error arm until a real need shows); NO resume token — a reconnect
re-opens and re-pulls pages, the stream is "now" never "since" (a resume
token would rebuild the fence machinery stage 2 deleted).

### 3b REOPENED: the keepalive convention is RETRACTED — no keepalive frames anywhere

**The user's challenge, at WatchFeed's 3c turn.** An in-band keepalive arm
"bleeds implementation details"; the shim already heartbeats from the SDK, so
source-of-truth liveness should manifest from the actual source of truth; and
keepalive only fires when nothing is happening, when staleness is not a
user-facing fact.

**Retracted, with the layering that replaces it (each layer already modeled):**

1. Source-of-truth silence (SDK/shim quiet mid-turn): the DAEMON observes it —
   FailureShimDegraded (window-shaped) + footer status arms. Real facts from
   the party that can see the silence, not pings.
2. Daemon↔client pipe death: the connection fails; the client library sees it
   and unary calls fail loudly. No frame detects a dead pipe better.
3. A wedged publisher behind a healthy connection is a DAEMON-INTERNAL fault:
   the daemon's own watchdog surfaces it (RuntimeFault / DaemonHealth), not N
   clients timing frame cadence per stream.

ROOT CAUSE of the error: the orchestrator carried the stage-2 "keepalive
convention" forward from the HeartbeatView deletion without re-asking WHO
should detect each silence; putting the detector in every client was the same
client-derivation failure the redesign removes elsewhere.

**Consequences.** Stream messages carry views only: WatchFeed's stream message
is plain FeedRow (the oneof wrapper returns only if a real second arm — e.g.
an in-band error — is ever agreed). No stream defined at 3c gets a keepalive
arm. The stage-2 footer entry's "liveness is the keepalive" sentence is
superseded by this layering.

### STAGE 3b SETTLED: the cross-endpoint conventions

**Settled ("the conventions are sound yes" for 1–2; "kay proceed" on the
orchestrator's recommendations for 3–6).**

1. RESPONSE SPELLING: every response is `oneof result { <Method>Success
   success = 1; <Method>Error error = 2; }` — the standing convention, the
   package's one spelling (already in force on SubmitPrompt).
2. ERROR DERIVATION: every `<Method>Error`'s arms are DERIVED from the
   daemon's real refusal sites, never invented; entry-less failures name
   failure.proto evidence types.
3. KEEPALIVE: an EXPLICIT IN-BAND FRAME, not a transport ping — every
   component stream's message is `oneof { <view> | keepalive {} }` at cadence
   C; a client hearing nothing for T marks the component stale. An h2 ping
   proves the connection lives, not that THIS component's resolver lives;
   the guarded failure is a wedged publisher, which only an in-band frame
   disproves. C and T are implementation-wave constants, not schema.
4. WORKSPACE ADDRESSING: a shared typed identity `WorkspaceRef { string dir }`
   in agentrepl.v1, a field on every per-workspace request and stream-open.
   Requests are CALLED, not drawn, so a shared identity type is right here
   (the stage-2 flag answered); the URL-param approach left addressing
   outside the schema.
5. IDEMPOTENCY: a client-minted `idempotency_key` on SubmitPrompt ONLY — the
   one verb where a duplicate is costly and undetectable (a retried
   answer/interrupt/close is naturally idempotent by its target). The daemon
   refuses duplicates by key — the guarantee that replaced
   newQueueEntryID()/promptreceipt's request-id refusal.
6. OLD HEADER RESIDUE: "no paint attestation" carries forward as a
   service-level comment (responses never claim anything was rendered); the
   request_id/client_id envelope DIES (Connect's unary response IS the
   correlation; no client identity is load-bearing — ClientLog names its
   caller in-band); protojson note carries forward as fact (Connect serves
   binary or JSON per client; elisp uses JSON).

**Consequences.** Every stream message defined at 3c is a two-arm oneof
(view | keepalive). SubmitPromptRequest gains `idempotency_key` at its 3c
turn. WorkspaceRef is declared once in agentrepl.v1 when the first
per-workspace endpoint lands at 3c.

### Stage 3, HOST and DAEMON-ADMIN sections settled — the section walk is COMPLETE (seven sections)

**Settled ("sounds fine, let's continue"), on the orchestrator's stated
recommendations, each confirmable below.**

1. The former Host section SPLITS: **Host** (RegisterWorkspace,
   SelectWorkspace, WatchHost, ReportHostAction) and **Daemon admin**
   (UpdateShutdownSchedule, UpdateMergeQueue, DaemonHealth, SessionHealth,
   ClientLog) — the admin verbs are not host-natured; Emacs is just today's
   caller. Sections: feed, sidebar, topbar, footer, daemon-hold tray, host,
   daemon admin.
2. Consolidations per the established pattern: UpdateShutdownSchedule
   { schedule | cancel | now }; UpdateMergeQueue { pause | resume |
   evict{workspace} }; WorkspaceMaterialized + HostActionCompleted fold into
   ReportHostAction { oneof action } with arms derived at 3c from what Emacs
   actually reports.
3. COMPOSER GATING IS A RESOLVED ELEMENT on the host stream ("open" /
   "blocked: merging" / "blocked: reviving" as arms), never raw bools
   (merge_lease_held, hibernated) Emacs maps — the figma→idl gray-zone
   ruling: what Emacs DRAWS arrives resolved; what it coordinates with
   (session_id, generation, backfill, lifecycle booleans it does not draw)
   stays coordination residue, exempt.
4. Facts with no Emacs need LEAVE the host surface (each already has its
   drawn home): model/model_options, total_tokens/total_cost_usd/
   context_window, permission_mode/pending_permissions, merge_status,
   merged_at_ms. Also dead: fence (stage 2 deleted fencing), the
   merge_dequeue_offer (the tray's HeldOffer).
5. BackfillState (the never-blue signal Emacs reads) converts enum → oneof;
   `failed` carries typed evidence, shaped at 3c. Its known limitation (a
   sidecar read error that is not a malformed line manifests as PENDING
   forever) is carried in the pending file's comment and stays owed.
6. DELETE-WITHOUT-CLOSE IS DEAD: CloseWorkspace is the only teardown; no
   DeleteSession returns.
7. DetachedCancelOutcome/DetachedAgentsCancelled are not stream output: they
   become the folded Interrupt{detached}'s success arms at 3c.

**The full fact-by-fact enumeration of the host stream's contents** (what is
carried vs re-homed vs deleted) was walked with the user in conversation and
is the 3c worksheet for WatchHost; the pending file is deleted when 3c has
placed or deleted every message in it.

### COMMAND PANELS section dissolves into the feed's SubmitPrompt; the endpoint lands (the first agentrepl.v1 RPC)

**The user's model.** "The client sends normal user requests, and the daemon
MIGHT respond with a 'this is a programmatically handled command' message —
the webapp shouldn't know what's programmatically handled; that's up to the
daemon, transparently." So RunCommandPanel never exists, the recognition
table lives only in the daemon, and the SECTION LIST DROPS TO SIX: feed,
sidebar, topbar, footer, daemon-hold tray, host.

**What landed ("looks good").** `endpoint_submit_prompt.proto` + the first
rpc on the empty service. SubmitPromptRequest { UserSaid }; response
result { success | error }; success outcome { turn { TurnId } | command_panel
{ oneof panel { frontend.v1.StatusPanelView status } } } — both ANSWERS per
the domain-outcome rule; a HELD prompt is a `turn` success (the tray shows
the hold, not an error). agentrepl.v1 composing frontend.v1 is the
composition rule in force. The error arm set is DELIBERATELY EMPTY until this
endpoint's 3c turn (derived from the daemon's real refusal sites, spelled per
3b). Deliberately absent: a client idempotency key (3b question), workspace
(the connection's), any echo of the prompt (the feed pushes the row).

**Consequences.** A new panel command (/context, /login) is a new panel arm
deployed daemon-side; old clients fail to match loudly instead of mis-sending
it as a prompt. StatusPanelView stays a frontend.v1 component; only its
transport is settled (unary, in the submit response — no stream, no fetch).

### Stage 3, DAEMON-HOLD TRAY section: three RPCs

**Settled ("looks good").**

1. WatchDaemonHolds — per-workspace DaemonHoldTray stream, whole-replaced.
2. UpdateHeldPrompt — ONE verb per the user's consolidation ("all held prompt
   updates along one channel"), the AnswerPermission precedent: `{ TurnId;
   oneof action { release (deliver now — today's "force") | drop (discard) } }`.
   A later action is a new arm; the refusal vocabulary (no such hold, already
   delivered) is declared once.
3. AnswerHeldOffer — its own verb, NOT folded into UpdateHeldPrompt: an offer
   is a different item kind with answers of its own shape (merge-dequeue
   keep/release today), and one RPC whose arms half-apply per item kind would
   reintroduce adjacent-exclusivity.

**The old table's third queue verb ("accept") is DEAD: accept is the default.**
A held prompt delivers itself when its hold clears; waiting requires no verb.

### Stage 3, FOOTER section: one RPC

**Settled ("sounds good").** WatchFooter — per-workspace stream of FooterView,
whole-replaced. Nothing else: the strip's states are daemon-resolved, the
tokens cell rides the view, and an expanded row's click is client-side
navigation to a feed bubble by MessageId — no command exists in this section.

### Stage 3, TOPBAR section: two RPCs; the breakdown menu nests INTO TopbarView

**Settled.** WatchTopbar — per-workspace stream of TopbarView, WHOLE; and
SetModel. Nothing else.

**The walk that got there, in the user's terms.** The orchestrator proposed
fetch-on-open for the breakdown menu; the user: "shouldn't token updates be
streamed? I don't see a pull architecture there being correct" — right, a
token figure changes while you look at it. The orchestrator then proposed a
separate WatchTokenBreakdown stream; the user: just WatchTopbar, tokens along
the same channel. The orchestrator then proposed a two-arm partial-replace
oneof (topbar | token_breakdown); the user: "the token breakdown is PART of
topbar — one level deeper" — so TopbarView gains `token_breakdown = 6` and
the stream is the plain view, whole. The partial-replace oneof is RETRACTED
as a delta creeping back in; the topbar is a few hundred bytes and whole-view
replace is the convention. Confirmed semantics: no oneofs among the view's
elements (none are mutually exclusive — a push can carry a new warning AND
fresher tokens, because a push is the whole topbar as it now stands, never an
event naming what changed); the breakdown is ALWAYS POPULATED, like a folded
section still carrying its rows, so opening the menu needs no round-trip.

**Presence ruling (the user asked about `optional` on warnings).** Message
fields carry presence natively; the convention is: EVERY element of a view is
always set, and "nothing to show" is expressed INSIDE the element (an empty
warnings list is the daemon saying nothing is wrong; the selector's unset
`selected` is the documented no-selection). An optional strip would spell "no
warnings" two ways. An unset element is a malformed frame, not a state.

**Consequences.** The TokenBreakdown* messages keep their names (the menu is
its own family). The daemon's topbar resolver owns session accounting
composition on every push.

### Stage 3, SIDEBAR section: the RPC inventory — nine RPCs; roster UI prefs go webview-local (stage-2 sidebar shape reopened by name)

**Settled.** Names and purposes only; shapes are 3c:

1. WatchWorkspaceRoster — the GLOBAL roster stream (the one stream with no
   workspace). Renamed from WatchRoster by the user.
2. CreateWorkspace, 3. OpenWorkspace, 4. CloseWorkspace, 5. MergeWorkspace,
   6. HibernateWorkspace — one verb each; EACH IS ONE RPC SHAPE FOR BOTH
   CALLERS (the webapp's roster clicks AND Emacs — Emacs speaks the same
   messages as protojson over its transport; Connect serves JSON natively, so
   one schema, two codecs). The user's instruction.
7. ReviveWorkspace, 8. RestartWorkspace — renamed from *Session by the user:
   the workspace is the address; one live session per workspace is the rule.
9. (none) — SetWorkspaceRosterView NEVER EXISTS, see below.

**Roster UI preferences: WEBVIEW-LOCAL (the user's selection, recommended).**
The stage-2 sidebar shape is REOPENED BY NAME and re-landed: grouping mode,
section folds and the nav cursor leave WorkspaceRoster. Folds and cursor are
per-client by nature (a shared fold would fold every webview; the cursor is
where YOUR keyboard is, and daemon-holding it makes every keystroke a
round-trip echoed to all webviews). Consequence of local grouping: the roster
carries BOTH groupings fully resolved (`repository` and `task` as siblings,
no longer a oneof) and the client draws the pane its local preference picks —
a selection between resolved views, like folding, never a derivation.
DELETED: RosterNavCursor, RosterFold, WorkspaceRoster.nav, the view oneof;
headers lose their fold element (task header renumbers).

**Flagged, still open for this section's 3c turn:** whether
delete-session-without-close is real (the old CreateSession/DeleteSession
pair), or CloseWorkspace is the only teardown.

### Stage 3, FEED section: the RPC inventory — five RPCs

**Settled ("okay i'm sold").** Names and purposes only; shapes are 3c:

1. WatchFeed — ONE server stream of FeedRow upserts per workspace, top-level
   and nested rows alike; the client routes by `parent`. One stream, not one
   per container: nesting is data, not topology, and reconnect is one re-open.
2. GetFeedPage — unary; a FeedPage for a container (top-level, a bubble, a
   merge phase) from a cursor. Replaces FirstPage/NextPage/Resync.
3. SubmitPrompt — a UserSaid; returns the daemon-minted TurnId or a typed
   refusal.
4. Interrupt — "stop that", with a TARGET ONEOF: the running turn, or a
   detached bubble by MessageId. The user's fold: CancelDetachedWork "is
   essentially an interrupt" — one user intent, one verb, the difference
   lives in the target arm, the refusal vocabulary is shared. The old
   Interrupt's second job (the merge-dequeue trigger) is already dead — that
   moved to the held-offer answer in stage 2.
5. AnswerPermission — allow once / allow for session / deny, by ToolCallId.

**Consequences.** CancelDetachedAgents does not return; the daemon routes the
target arm to the vendor query interrupt or the task stop respectively.

### STAGE 3a SETTLED: `agentrepl.v1`'s seven sections, the walk order

**Settled ("looks good, let's settle").** The service is organized by
COMPONENT, not by mechanism — each drawn component owns its stream, its pulls
and its commands together (the figma→idl file rule applied to the API), plus
two non-drawn sections. The RPC names below are SCOPING ONLY; each section's
actual RPC set is settled one RPC at a time at its turn:

1. Feed — stream, paging, prompt/interrupt/permission/cancel-detached.
2. Sidebar — roster stream, workspace-lifecycle clicks, view preferences.
3. Topbar — stream, model selector, session token-breakdown delivery.
4. Footer — stream.
5. Daemon-hold tray — stream, held-prompt/offer actions.
6. Command panels — programmatically-rendered slash commands (/status today;
   /context, /login later — the section is named for the CLASS, so a later
   command is a new panel kind, not a new section). Generalized from the
   orchestrator's too-specific "status panel".
7. Host — register/select workspace, report-backs, host stream, shutdown
   scheduling, health, merge-queue control, client log relay (that one's
   caller is the webapp, noted; Host = the non-drawn operational surface).

**The transport reasoning re-verified with the user on the way (Connect
stands, not reopened).** The user probed whether commands should ride the
feed's connection ("one connection for feed, along which commands are sent
and updates received") and whether Connect forgoes bidirectional RPC. Answers
recorded: browser fetch cannot do gRPC bidi (no duplex request in WebKit, no
trailers, no half-close — the page never owns the connection); the
"protobuf-powered WebSocket" is what agent-repl HAS today, and its cost is
the hand-rolled correlation layer (requestId, pending-ack map, ack-vs-push
races) that Connect deletes by making commands ordinary unary requests; all
endpoints multiplex over the ONE HTTP/2 connection, so one-connection is true
at the socket level and endpoint-per-component at the API level. The user:
"ah got it, that makes a lot of sense."

**Dropped from the old table at this level** (each re-judged, not silently):
PublishWorkspaceRoster (Emacs no longer authors the roster);
FirstPage/NextPage/Resync (→ feed paging + streams); CreateSession/
DeleteSession (subsumed by workspace open/close — whether delete-without-
close is real is FLAGGED for the sidebar section's turn).

### `token_breakdown.proto` folds into `topbar.proto`

**What changed.** The file is deleted; `TokenBreakdownView`, `TokenBreakdownSection`,
`TokenBreakdownHeading`, `TokenBreakdownRow` move verbatim into `topbar.proto`
under a banner. The banner states the uncoupling: the breakdown is the
SESSION's accounting, opened by the topbar; the footer's turn-level tokens are
a different fact and a different component, never a shared type.

**Why, in the user's terms.** "Token_breakdown should be in topbar" — the menu
is the topbar's subcomponent, so its messages live in the topbar's component
file; a separate file wrongly presented it as a sibling component. The
orchestrator's suggestion to re-home it under the footer is RETRACTED: the
topbar's token figure is the SESSION's, the footer's is the TURN's — different
values, architecturally uncoupled (checked: TokenBreakdownView's only consumer
is the topbar menu; the footer already has its own FooterTokens* messages).

**Consequences.** Stage-3 sections: no token-breakdown section; its delivery
(pushed vs fetched on open) is settled in the TOPBAR section's turn. If the
footer ever grows a turn-breakdown menu it gets footer.proto-family messages.

### `feed.proto`: `FeedApiFailure` folds into `FeedAgent.error` — SIX row kinds

**What changed.** `FeedApiFailure*` deleted; `FeedRow` is user, agent,
detached, permission, context_cut, merge (renumbered 4–9).
`FeedAgentError.reason` gains `api_request_failed { FeedAgentApiFailureMessage
{text}; FeedAgentApiFailureWhen {at_ms} }` — the vendor's recorded
`conversation.v1.ApiRequestFailed`, drawn as the failure of the response it
refused. When the vendor refused before a single token, the row is CREATED BY
the failure: its id is the `ApiRequestFailed` record's, `partial` is empty,
the head still names the author.

**Why, in the user's terms.** "FeedApiFailure should be folded into the agent
oneof, yes" — closing the flag from the previous entry. An API failure IS the
response failing; a separate row drew one fact as two kinds.

**Consequences.** The daemon's feed resolver keys a refused-before-first-token
response's row on the `ApiRequestFailed` record id, and on the `AgentSaid`'s
id otherwise (the two never coexist for one response). No row kind is
error-only any more; the one-arm kinds are `FeedUser {success}` and
`FeedContextCut {success}`.

### `feed.proto`: EVERY row kind carries `oneof result { update | success | error }`; kinds ⑤ `FeedFailure` and ⑨ `FeedPreview` are DELETED; `FailureKind` shrinks to the entry-less residue

**Why, in the user's terms — the conversation, in order.**

- Asked what the live preview IS in the UI (the type-out of the agent's
  response before it settles, at the tail, same id as the settled row), the
  user: "shouldn't that be WITHIN the streamable messages themselves, rather
  than adjacent them?" — i.e. a state arm on `FeedAgent`, not a ninth kind.
  Agreed: one drawn box in two states, and it surfaced an on-disk
  adjacent-exclusivity defect — the usage stamp sat on the head but means
  nothing while arriving.
- The user then generalized: `update` and `success` over `arriving`/
  `settled`, PLUS an `error` arm, and "all feed arms should have their own
  error representation represented in internal arms" — `oneof result {
  update (if applicable) | success | error }` on every kind. Rulings: (1)
  flatten to ONE level everywhere (option b) rather than keep `state →
  outcome` — "rewrite approved"; (2) kinds that cannot update or fail carry a
  ONE-ARM oneof; (3) `FeedFailure` is not needed "unless the error
  specifically and inherently does not correlate to any feed entry";
  everything else is that entry's own error.

**What changed.**

- `FeedRow` is SEVEN kinds: user, agent, detached, permission, api_failure,
  context_cut, merge (renumbered 4–10). `FeedFailure` (+ headline/detail/
  evidence/lifecycle) and `FeedPreview` deleted.
- `FeedAgent { head{author}; result { update{prose-so-far blocks: text |
  thinking} | success{usage stamp; blocks; stop notice} | error{partial
  blocks; headline; oneof reason — the failure.proto evidence messages
  imported whole: query_termination, vendor_max_turns, vendor_max_budget,
  vendor_execution_error, vendor_turn_failed, vendor_network_down,
  turn_undriven, keep_alive_window_unclosed/_inverted } } }`. The row exists
  from the first `ContentArriving` fragment under its future `AgentSaid` id.
- `FeedTool.result { update | success{ returned{blocks} | detached{bubble} }
  | error{ failed{blocks} | denied{reason} } }`.
- `FeedDetached.result { update | success{ended_at_ms, summary, exit} |
  error{ended_at_ms, exit, reason { failed | cancelled | lost }} }`; the head
  keeps only constant props; steps → `update|success|error`.
- `FeedPermission.result { update (open) | success{ allowed_once |
  allowed_for_session | denied; at_ms } | error{ abandoned; at_ms } }` — a
  DENIAL IS AN ANSWER (domain outcome, not a failure).
- `FeedApiFailure.result { error{headline, message, when} }` — one arm, and it
  is `error`: the row IS a failure.
- `FeedContextCut.result { success{tokens; cut{cleared|compacted}} }` — one
  arm; `compacted` gains a `cold_read` NOTICE (`FailureCompactionColdRead`
  imported) — a compaction that read cold still compacted.
- `FeedMerge.result { update | success{ended_at_ms, commit} |
  error{ended_at_ms, reason { failed | abandoned }} }`; each
  `FeedMergePhase.result { update | success{ended_at_ms, summary} |
  error{ended_at_ms, summary} }`.
- `FeedPage.result { success{rows, edge, breadcrumbs} | error{headline;
  history_replay_truncated} }` — a page that could not be completed is the
  PAGE's error, not a row.
- `failure.proto`: `FailureKind` shrinks 28 → 17 arms (renumbered): machinery
  session/shim/internal residue + client-local. The entry-correlated arms
  (query_termination, turn_undriven, keep_alive_window_*,
  history_replay_truncated, compaction_cold_read, the five vendor arms) leave
  the oneof; their EVIDENCE MESSAGES stay, imported by feed.proto's error
  arms. Header rewritten to say so. `host_surface_pending.proto`'s
  `SessionView.death` repoints to `FailureKind` (holding file, stage 3).

**Two corrections to what the orchestrator said in the conversation, kept
visible.** (a) It first mapped `history_replay_truncated` and
`compaction_cold_read` to `FeedContextCut.error`; on writing them, the first
is a PAGE failure (the re-pull, not a cut) and the second is a cost notice on
a compaction that happened, so they landed as `FeedPage.error` and a
`cold_read` notice on `compacted`. (b) `shim_store_write_rejected` was listed
as entry-correlated ("the entry being written"); the daemon has no row for a
write that never landed, so it stays in `FailureKind`.

**Consequences.**

- The webapp's separate `BubbleTyping` line under a bubble
  (`async-render.ts:336`) goes away — a subagent's streaming is a nested
  `FeedAgent` in `update`; `smooth.ts` paces the reveal off the row's update.
- A stream that dies can no longer leave a cursor blinking: the daemon
  pushes the row's `error` arm under the same id.
- Every client renderer switches on `result` the same way for every kind and
  for the page; the daemon's row resolvers emit the terminal arm with
  `ended_at_ms` inside it.
- `FeedApiFailure` stays a row (the vendor may record an API failure with no
  `AgentSaid` at all); whether it should instead be the in-flight
  `FeedAgent.error` when one exists is FLAGGED, not decided.
- Failures in `FailureKind`'s residue are drawn by footer/topbar/gate state
  and named by stage-3 error responses; nothing draws them as rows.

### `feed.proto` kind ⑧: `FeedMerge` — a tabbed phase bubble, the queue as its first tab

**The drawing agreed with the user** (in the kind's comment): a merge bubble
whose head is the branch line + clock + live/settled, and whose body is a TAB
STRIP — `queue ✓ │ rebase ✓ │ conflicts ✓ 3 │ tests ● 8/12` — with the
selected tab's nested rows beneath.

**What changed.** `FeedMerge { FeedMergeHead head; repeated FeedMergePhase
phases }`. `FeedMergeHead { glyph; label; runtime; oneof state { live |
settled { ended_at_ms; oneof outcome { merged{commit} | failed{summary} |
abandoned } } }; fold }`. `FeedMergePhase { FeedMergePhaseLabel label; oneof
state { live | settled { ended_at_ms; oneof outcome { succeeded{summary} |
failed{summary} } } }; oneof kind { queue | rebasing{base} |
resolving{paths, current_path} | building{detail} | testing{suite, progress}
| landing } }` — every working kind carries a `FeedMergePhaseRowCount`; the
queue kind carries `FeedMergeQueue { repeated ahead; current; repeated
behind }` of `FeedMergeQueueEntry { workspace; label; oneof status {
merging { oneof phase — the same FeedMergePhase* messages } | waiting } }`.

**Why, in the user's terms — the conversation that shaped it, in order.**

- The orchestrator first proposed a merge row modeled like `FeedDetached`
  (head with `queued`/`running`/`settled`, nested rows). The user: "we haven't
  actually decided on the merge UI" — waiting in the queue and merging at
  the head are two different representations. The orchestrator then argued
  the WAITING half was live state, not conversation, and belonged in a
  tray; the user DISAGREED: "the waiting half is part of the conversation.
  it's analogous to the other async bubbles (when running async bash you're
  'waiting' for the output, which is populated in the bubble)". One bubble
  for both. RETRACTED and recorded: the tray argument was wrong on the
  bubble analogy — a bubble's early life is exactly "waiting".
- `merging` "should most certainly not carry nothing" — conflict resolution
  is agent emission under the hood, so the bubble "turns into a sort of
  agent bubble". First answer: a phase oneof on the head. The user's
  amendment: the phases are SEPARATE BUBBLES coalesced under TABS.
- A repeat pass (tests break, resolve again): option (b) — a SECOND tab, "no
  special allowance there; visually you want it apparent there's been two
  rounds of something that should normally be one." Consequence accepted:
  phases are APPEND-ONLY and each is MONOTONIC (live once, settled once);
  the tab label carries the round ("conflicts (2)"), resolved by the daemon.
- `FeedMergePhase` is only for phases the run ACTUALLY ENTERED — a tab
  appears because work began; there is no pending tab.
- The user: "a queued merge should be just another phase — another Merge
  bubble, and therefore typically the first tab." This dissolved the
  `ahead`-vs-`phases` body oneof entirely; body = tabs.
- The queue tab's content is a SNAPSHOT replaced whole on every publish (the
  user: "every new update overwrites the content already existing"), like a
  shell spool; the daemon republishes on any queue change AND on any phase
  change at the front, so a waiting user sees the front's progress. The
  user's shape: the same message reaches every workspace in the queue; the
  head workspace's status is `merging` and it is SHOWN NOTHING about the
  queue ("it doesn't need to, it's merging"). The queue tab does not keep
  updating once we reach the front — it settles and the phase tabs take over.
- The user proposed a four-field `ws_merging / ws_ahead / ws_current /
  ws_behind` shape; the orchestrator MISREAD it as double-naming the head
  and objected — the objection was wrong (the user's status oneof made
  `merging` and placement exclusive) and was withdrawn. The landed shape
  keeps the user's structural position (`ahead`/`current`/`behind`, so
  "you are here" is never derived by comparing ids) with one entry type and
  the front's status as an arm on its entry.
- The tab BADGE (asked by the user as a thing to nail down before wrapping
  feed): a tab draws from exactly two fields — its label and its state arm,
  which picks the treatment (`live` / `settled{succeeded|failed}`); the
  badge's detail is the kind arm's resolved evidence. No enum, no client
  mapping.

**Consequences.**

- Nested rows parent to the PHASE, not to the merge, so "which pass produced
  this fix" is structural. The `landing` phase owns the merge's own narration
  (initiating prompt, closing message), so the body has no loose rows.
- The mirrored front-workspace phase in a queue entry imports the real
  `FeedMergePhase*` messages — one vocabulary for tab badge and queue row.
- Deliberately NOT reused: `FeedDetached*`. A merge is not detached work
  (daemon-orchestrated, has a `queued` life the vendor has no arm for), and
  two families = two drawn components.
- Stage 3 owes: the merge-queue push cadence (on every front-phase change)
  and `FeedMergeQueueWorkspace` as a jump target across workspaces.

### `feed.proto` kind ⑦: `FeedContextCut` — the divider, with the token delta modeled

**What changed.** `FeedContextCut { FeedContextCutLabel {text};
FeedContextCutTokens {before_text, after_text}; oneof cut { cleared {} |
compacted { FeedContextCutSummary {markdown}; FeedContextCutFold } } }`.

**Why, in the user's terms.** The first sketch composed the sizes into the
divider's line and had `cleared` empty; the user: "there's tokens before +
after in the conversation equivalents, which should be modeled" — so the
delta is its own element, a sibling of the cut oneof because EVERY cut has
one (a clear reloads system prompt/skills/memory, so after is small, not
zero). And the compacted comment was wrong: "it's not a summary of what was
discarded, it's a compression of the previous conversation" — reworded.

**Consequences.** Both sides are daemon-formatted strings; the client does no
arithmetic and no unit rounding. The summary is markdown the daemon flattens
from the record's `AgentContent` (prose only — a compaction summary has no
tool calls).

### `feed.proto` kind ⑥: `FeedApiFailure`

**What changed.** `FeedApiFailure { FeedApiFailureHeadline {text, tone};
FeedApiFailureMessage {text}; FeedApiFailureWhen {at_ms} }` — the recorded
`conversation.v1.ApiRequestFailed` drawn as a terminal row; the daemon
composes sentence and color from the record's kind. "Okay."

### `feed.proto` kind ⑤: `FeedFailure`; `FeedDetachedRuntime.ended_at_ms` moves into `Settled`

**What changed.** `FeedFailure { FeedFailureHeadline {text, tone};
FeedFailureDetail {text} (unset when none); FeedFailureEvidence
{ FailureKind kind }; oneof lifecycle { open | resolved{at_ms} | terminal } }`.
DELETED: `FailureCardView`, `FailureCardOpen/Resolved/Terminal`. The holding
file's `SessionView.death` now names `frontend.v1.FeedFailure`.
`FeedDetachedRuntime` loses `ended_at_ms`; it lives on `FeedDetachedSettled`
— it means nothing while live (adjacent-exclusivity, caught in the audit the
user asked for).

**Why, in the user's terms.** "Yes" — with two instructions: no serialized-
shape comments at line ends in the protos (checked: none exist in any landed
file — that style was only in the orchestrator's chat sketches, and it stops
there too; every field keeps a description above it); and an audit of the
feed kinds so far for (a) mutual exclusivity modeled as oneofs and (b) real
mapping to UI components — reported in the conversation; the one defect
found is the `ended_at_ms` fix above.

**Consequences.** `FailureKind` is embedded as the drawn evidence — the one
place the shared vocabulary type rightly sits inside a frontend element,
because it IS what is drawn and `agentrepl.v1` names the same arms.

### `feed.proto`: kind suffixes dropped (`FeedUser`, `FeedAgent`, `FeedDetached`, `FeedTool`, …); kind ④ `FeedPermission`

**Naming ruling.** The user asked what a "card" and a "bubble" ARE at the UI
level. Answer: chrome patterns the orchestrator named (line / boxed panel /
collapsible container) — real as CSS, not schema facts, and already
misapplied once (`FeedApiFailureRow` would draw with the failure card's
chrome). Dropped: every top-level kind is the family name (`FeedUser`,
`FeedAgent`, `FeedDetached`, `FeedPermission`, `FeedFailure`,
`FeedApiFailure`, `FeedContextCut`, `FeedMerge`, `FeedPreview`; `FeedToolCard`
→ `FeedTool`), children already family-prefixed. If two kinds ever share a
DRAWN frame element with props, it becomes an element message they embed —
schema when there is a box, never a suffix.

**Why some kinds have headline/arguments and others not** (the user's
question): the differentiator is WHAT the element is about — `FeedTool`,
`FeedPermission` and (reduced) `FeedDetached`'s head draw a TOOL CALL, so
each has its own headline/arguments wrappers filled by the daemon from the
same origin (per-component copies, not a shared type; a `FeedCard` union
was considered and refused — the tool card is nested in the agent row, not
a top-level row, and an intermediate node must be a drawn box).

**What changed.** `FeedPermission { ToolCallId call; FeedPermissionHeadline;
FeedPermissionArguments {lines}; oneof state { open {} | answered { oneof
answer { allowed_once | allowed_for_session | denied{reason} | abandoned };
at_ms } } }`. Buttons are `agentrepl.v1` requests (stage 3).

### `feed.proto` kind ③: `FeedDetachedBubble`; child naming settled as FAMILY prefix

**Naming (the user's ruling, option b).** Children of a row kind take the
FAMILY prefix, not the full parent name: `FeedUserAuthor`, `FeedAgentHead`,
`FeedToolHeadline`, `FeedDetachedHead` — the kind suffix (`Row`/`Card`/
`Bubble`) appears only on the top-level kind message. `FeedToolCard*`
children renamed to `FeedTool*` accordingly. "Bubble" is this codebase's word
for a collapsible container of nested rows (the user's own phrasing; the
webapp's `async-bubble.ts`); Row = a line-ish item, Card = a boxed item
without nested rows.

**What changed.** `FeedDetachedBubble { FeedDetachedHead head; oneof body {
agent {row count} | shell {spool} | workflow {steps} | skill {doc, row
count} | unmodeled {row count} } }`; `FeedDetachedHead { glyph; label;
runtime {started, optional ended}; oneof state { live | settled { outcome
succeeded|failed|cancelled|lost{how}; exit } }; fold }`. Nested rows are
ordinary `FeedRow`s with `parent` = the bubble, paged via the same
page/breadcrumbs. DELETED old bodies: `DetachedWork`, `DetachedWorkLiveness`
/`Live`/`Settled`, `DetachedWorkOutcomeKilled`, and every `DetachedWork*Update`
/`OutputAppend` delta message — a row upsert replaces the whole bubble.

**Why, in the user's terms.** "Your protos look fine" with the naming
ruling. Kind is the arm (mirrors `ToolCallBlock.call`'s detachable arms +
unmodeled); label/glyph resolved from the origin; the shell spool is a
resolved whole with `truncated` for paging inside the bubble.

**Consequences.** The webapp's `async-bubble.ts` reads one bubble message;
its 3-tier identity ladder and append handling go. The daemon caps the spool
it sends and serves the rest as pages inside the bubble.

### `feed.proto` kind ②: `FeedAgentRow` — head, blocks (prose · thinking · tool card), stop notice

**What changed.** `FeedAgentRow { FeedAgentHead {author, usage stamp};
repeated FeedAgentBlock { prose | thinking {shown{text,tokens} | redacted} |
FeedToolCard }; FeedAgentStopNotice {max_tokens|interrupted|refusal|
unsupported} }`. `FeedToolCard { ToolCallId call; FeedToolCardHeadline
{icon,title,subtitle}; FeedToolCardArguments {lines}; oneof outcome {
running | returned {succeeded{blocks} | failed{blocks}} | detached {bubble
MessageId} | denied {reason} } }`. DELETED old bodies: `AgentEmission`,
`AgentResponse`, `ToolCallVerdict`, `ResponseUsageStamp`,
`AgentToolOutcome`.

**Why, in the user's terms.** "Sounds good." A resolved element under the
precedence principle — this is the reopened "carries `AgentSaid` whole"
decision, decided: NOT embedded. The per-tool headline/arguments are the
daemon's projection of `ToolCallBlock.call`, so the "Bash has a `command`"
knowledge lives once, in the resolver. `ToolCallVerdict.spawned_message_id`
became the `detached {bubble}` outcome; the stop notice draws only stops
worth drawing.

**Consequences.** `daemon/internal/frontend/translate.go`'s re-encoding
becomes the row resolver; the webapp's `render.ts` per-tool branches
(`render.ts:1691-1740`) are deleted — it draws headline/arguments verbatim.

### `feed.proto` kind ①: `FeedUserRow`, and the feed's drawn block vocabulary

**What changed.** `FeedUserRow { FeedUserAuthor author {label};
FeedUserBody body { repeated FeedUserBlock } }`; `FeedUserBlock` = oneof
`FeedTextBlock {text}` | `FeedImageBlock {src, alt}` | `FeedUnsupportedBlock
{kind}`. The three block messages are the feed's DRAWN block vocabulary,
shared by every row kind in this file.

**Why, in the user's terms.** "Sounds okay" — after asking what the row is
for: it draws a `UserSaid` record, the user's own prompt in the history
(the webapp renders these today). A resolved element, not an embedded
`UserSaid` (precedence principle): the record's image reference (a host path
or a URL) becomes a `src` the webview can fetch — a resolution only the
daemon can make; `unsupported` draws its kind only, the raw stays in the
record.

**Consequences.** The daemon's feed resolver serves images (or maps paths to
served URLs); the webapp's user-card renderer reads blocks, not the record.

### `frontend.v1/feed.proto`: the container — `FeedRow` (nine kinds) and `FeedPage`; `status_panel.proto` split out; the whole tree compiles again

**The drawing agreed with the user** (in the file header): the feed as a
scrolling list of rows — user, agent (with tool cards), detached bubble,
permission card, synthesized failure card, api-failure row, context-cut
divider, merge bubble, live preview — plus the paging edge. "I don't think
we need to change anything here on the UI organization front."

**What changed.**

- `FeedRow { MessageId id; MessageId parent; TurnId turn; oneof row { user,
  agent, detached, permission, failure, api_failure, context_cut, merge,
  preview } }` — a row is the unit; a live update re-pushes the WHOLE ROW
  (upsert by id). `FeedPage { repeated FeedRow rows; oneof edge { has_more
  {cursor}; at_start {} }; FeedBreadcrumbs breadcrumbs }`;
  `FeedBreadcrumb { MessageId target; string label }`.
- The nine kind messages are declared EMPTY, each filled at its own
  increment; the old bodies (`AgentEmission`, `AgentResponse`,
  `ToolCallVerdict`, `ResponseUsageStamp`, `AgentToolOutcome`,
  `FailureCardView` + lifecycle, the `DetachedWork*` family) are kept
  VERBATIM under a "PENDING" banner and judged kind by kind.
- DELETED: `Message` (→ `FeedRow`), `ConversationDelta`, `DetachedWorkDelta`,
  `TypingCut` (transport arms of the dead stream; the live channel's shape is
  stage 3's), `TaskCatalog`/`TaskEntry`/`TaskStatus*` (superseded by the
  detached bubble and the footer's expanded rows), `CompactionSummaryItem`
  (folds into the context-cut divider), `PageScope*`,
  `ConversationHistoryPage`, `HistoryHasMore`/`HistoryAtStart` (→ `FeedPage`
  + edge; scope survives only as a stage-3 request parameter),
  `DaemonInterceptedCommandItem` — the user: "should not be discernible by
  the UI, it seems like an implementation detail" (answer 3).
- `SessionInitView`/`SessionInitRow` → NEW `status_panel.proto`
  (`StatusPanelView { repeated StatusPanelRow { label; value } }`, no
  workspace/fence): the `/status` panel is its own component opened by the
  command, not a feed row.
- EVERY PACKAGE COMPILES for the first time since the transport reversal.

**Consequences.** Nine kind increments follow, one at a time, each judged
under the precedence principle (a resolved element, not an embedded record).
The `agentrepl.v1` holding file compiles now that `feed.proto` does.

### `workspace` and `fence` leave every `frontend.v1` view (YAGNI); the feed page carries no `scope`

**What changed.** `TopbarWorkspace`/`TopbarFence`,
`TokenBreakdownWorkspace`/`TokenBreakdownFence`,
`DaemonHoldWorkspace`/`DaemonHoldFence`, `FooterWorkspace`/`FooterFence` and
their view fields are DELETED; the views renumber contiguously. (The
sidebar's `RosterRowWorkspace` stays — it is the ROW's identity on a global
stream, not addressing.) All frontend files compile except `feed.proto`.

**Why, in the user's terms — three questions, answered.**

- "Why is FeedWorkspace needed? Is that not implicit in the URL?" — It is:
  every component stream is per-workspace, so the workspace is the stream's.
  "Okay, let's drop it then. YAGNI." Dropped everywhere, not deferred to 3b.
- "What is fence … do we need this then?" — It guarded the old MULTIPLEXED
  stream against a late push from a retired session generation. With one
  ordered stream per component per workspace a push cannot overtake a newer
  one on its own stream, and cross-stream staleness has no comparison target
  (`WorkspaceState.fence` left `frontend.v1`). Liveness is the keepalive,
  ordering is the stream. Dropped; if a cross-stream generation check is
  ever needed it is one convention field at 3b.
- "Is scope implicit in ancestors?" — Yes: the last breadcrumb IS the
  container the page is of, and no crumbs = the top-level feed; `scope` on
  the page was a second spelling. It survives only as a REQUEST parameter at
  stage 3. And `ancestors` as bare ids would have made the client resolve
  labels — a derivation — so it becomes a drawn element,
  `FeedBreadcrumbs { repeated FeedBreadcrumb { MessageId target; string
  label } }`, outermost first, empty for the top-level feed.

**Consequences.** This SETTLES two of the STAGE-3b questions early, by the
user's ruling: per-workspace addressing is the stream's, and there is no
fence. The daemon's fence minting and the webapp's byte-compare gate are
deleted in the wave. `feed.proto`'s container is `FeedPage { rows; edge;
breadcrumbs }`.

### `frontend.v1/failure.proto`: the vendor family shrinks to what nothing recorded; `shim.v1` projected out

**What changed.** `FailureKind` goes from 40 arms to 28: machinery 1–17
unchanged; VENDOR is now five arms — `vendor_max_turns`, `vendor_max_budget`,
`vendor_execution_error`, `vendor_turn_failed`, `vendor_network_down`
(18–22, messages renamed `FailureVendor*`, each still carrying
`VendorFailureContext`); client-local 23–28 unchanged (renumbered). DELETED:
the ten API-class arms and their `FailureApi*` evidence messages
(authentication, billing, rate limit, invalid request, server error,
overloaded, oauth-org, model-not-found, request-failed, unknown), and
`api_refusal`, `api_max_output_tokens`. The four `shim.v1` types
`QueryTerminationFailure` named are PROJECTED into local messages
(`QueryTerminationVendorIdentityUnavailable`, `…UnexpectedEof`,
`…IteratorFailure {cause}`, `…StartupFailure {cause}`); the `shim.v1` import
is gone. `VendorFailureContext`, `FailureCardRef` and every machinery
evidence message unchanged. Compiles.

**Why, in the user's terms.** It stays a shared vocabulary file (the user:
"unless failure needs to be reused in other files, I don't see the point" —
it IS reused: `agentrepl.v1` error arms name its evidence), so the feed's
card stays in `feed.proto` and there are no `*_card.proto` files. The vendor
cut: a vendor API failure the vendor RECORDED is a
`conversation.v1.ApiRequestFailed` record and reaches the feed as its own row
(the daemon resolves it into a frontend-shaped card there, per the
precedence principle); a synthesized failure card carrying the same fact
would draw one failure twice. Only what nothing recorded — the SDK ending
the query on its own terms, or no response at all — is a card kind here.
`api_refusal`/`api_max_output_tokens` are `StopReason` arms on `AgentSaid`
(answer 5). The user: "let's proceed."

**Consequences.**

- `daemon/internal/errclass` classifies API failures into
  `ApiRequestFailed.kind` for the record path, not into ten `FailureKind`
  arms; the ten `errclass` types that mapped to them lose their frontend
  arm.
- The wave must VERIFY that every vendor API failure the daemon sees LIVE
  also lands as a transcript record; if some do not, the feed would miss
  them and this decision reopens.
- `agentrepl.v1` error arms (stage 3) that named the deleted vendor evidence
  now name `FailureVendor*` or the record type.
- The user asked to PAUSE at the end of stage 2 for approval before
  continuing to stage 3.

### PRINCIPLE: figma→idl takes precedence over no-respell at the backend→frontend boundary (no proto landed)

**Decided, the user's words spat back and confirmed:** `frontend.v1` is
composed ONLY of element messages shaped for the drawn element. When an
element's props derive from an internal type — a `conversation.v1` record, a
`shim.v1` wire type, a daemon-internal fact — the DAEMON RESOLVES a
frontend-shaped message for that element: a deliberate re-spelling of the
internal fact into UI vocabulary. "One canonical form / never re-spell /
import the encompassing message" yields to figma→idl at that boundary.

**Why it is safe there and nowhere else.** The daemon is the single resolver,
so the frontend copy is a resolved VALUE re-published on every change and
self-corrects like any duplicated value; the internal type keeps its one
canonical form for records/transport; the frontend never becomes a second
AUTHOR of the fact, only a projection of it. NOT licensed: re-spelling within
`conversation.v1`/`shim.v1`/`store.v1`; `frontend.v1` re-declaring another
surface's type for any reason other than drawing; typed identities
(`MessageId`, `TurnId`, `ToolCallId`) are still imported — join keys, not
props. Being added to the skill by a one-shot subagent
(`proto-figma-idl-precedence`), genericized.

**REOPENED BY NAME.** The superseded record's "`frontend.v1` stops flattening
`AgentSaid`; it carries it whole and stamps alongside" was the no-respell
rule applied across exactly this boundary. Under this principle the feed's
agent card may be a resolved element message rather than an embed — decided
at `feed.proto`'s turn, not kept by default.

### `footer.proto` closes: PLUMBING leaves `frontend.v1`; three state enums and `HeartbeatView` die

**What changed.**

- DELETED outright: `RenderState`, `SessionConnectivity`, `SessionStatus` —
  three state ENUMS (a live violation of the state rule) whose only readers
  were the daemon's resolvers; their projections are now the footer's
  `FooterStatus`/`FooterSubStatus`, the sidebar's `RosterRowStatus` and the
  topbar's `TopbarConnectivity`. And `HeartbeatView` — its own comment said it
  had nothing to tick with.
- MOVED VERBATIM to `agentrepl/v1/host_surface_pending.proto` — a HOLDING
  FILE, explicitly not a design, deleted when stage 3 has placed or deleted
  every message in it: `RuntimeFault`, `WorkspaceState` (with the three enum
  fields `reserved` and marked "STAGE 3"), `SessionView`, `BackfillState`,
  `DaemonView`, `DetachedCancelOutcome`, `DetachedAgentsCancelled`. These are
  the daemon's resolution inputs and the HOST surface Emacs reads (session
  identity, controller generation, catalog entries, backfill, boot id) plus
  one RPC ack payload — none is drawn.
- `footer.proto` now imports only `conversation/v1/message.proto` and
  `conversation/v1/turn.proto`; `frontend.v1` no longer imports `shim.v1` or
  `agentrepl.v1` from the footer. Every `frontend.v1` file except `feed.proto`
  and `failure.proto` (their turns next) compiles.

**Why, in the user's terms.** "Your read sounds good." On heartbeats: "still
useful for animation on status? Or maybe status should just be repeated to
simulate heartbeats" — answered: animation is client-side for as long as the
status arm is active (a state, not a push); LIVENESS is a property of the
connection, so it becomes a STAGE-3b convention (every component stream
carries a keepalive at cadence C; a client that hears nothing for T marks the
component stale) rather than a heartbeat arm on each view or an identical
view re-pushed with a second meaning. The "long tool still working" evidence,
when the daemon has any, is a `StatusActivity` update.

**Consequences.**

- The daemon's SSM keeps its state machine internally; the wire carries only
  the projections. `daemon/internal/ssm/resolve.go`'s rank table stays; the
  `frontendv1.RenderState` Go type it emits does not.
- The `fence` every component view carries is anchored on
  `WorkspaceState.fence`, which is now in the holding file — STAGE-3b decides
  fence vs stream order vs keepalive together.
- Stage 3 owes: a host stream/response set for what Emacs reads out of
  `WorkspaceState`/`SessionView`/`DaemonView`; the `DetachedCancelOutcome` ack
  arms; and the deletion of the holding file.

### `frontend.v1/footer.proto` — StatusActivity settled: six typed arms, no free-text escape

**What changed.** `FooterStatusActivityNote { string text }` and its `note`
arm are DELETED. `FooterStatusActivity` is six typed kinds: merging_commit,
hook, retrying, authenticating, blocked_on_user, rate_limited.

**Why, in the user's terms.** "Intuitively, it seems like it's floating in
the ether? Shouldn't this be specific to the error route?" — Yes: a line with
no kind belongs only where the daemon genuinely CANNOT classify, and that
place already exists — the error route's unclassified funnel on the failure
card. An activity the daemon can name but has no arm for is a modeling gap;
the fix is the arm. Same stance as `UnsupportedBlock`'s "not a fallback",
applied one step further: not even a guarded escape.

**Left as landed, flagged:** `FooterStatusActivityRetrying.status` and
`FooterAllowance.status` carry the vendor's verbatim status words — the
vendor's full vocabularies are not in evidence; typed arms when they are.
`rate_limited` stays an activity (transient, outranks ordinary windows) rather
than a `blocked` sub-status.

**The footer strip is now at submessage resolution end to end**: Status (9
empty arms), SubStatus (5 families), StatusActivity (6 kinds), Clock, Tokens
(5 elements), and the expanded rows. What remains in `footer.proto` is the
PLUMBING block and `DetachedCancelOutcome`, both awaiting the user's ruling.

### `frontend.v1/footer.proto` — the Tokens section's submessages; `ContextCostAlert` and the accounting cell find their home

**The drawing agreed with the user:**

```
│ 18.2k in · 3.1k thought  ⚠  ✓ │
     input     thinking     │  └ accounting verdict badge (hover: phrases)
                            └ expensive-turn alarm (hover: "41k over 20k")
```

**What changed.** `FooterTokens` is five element messages, ALL siblings:
`FooterTokensInput {tokens}`, `FooterTokensThinking {tokens}`,
`FooterTokensFirstToken {latency_ms}`, `FooterTokensExpensiveTurn`,
`FooterTokensAccounting`. `ContextCostAlert` → `FooterTokensExpensiveTurn
{ TurnId turn; uncached_input_tokens; threshold_tokens; at_ms; oneof origin
{ prompt {}; cold_keep_alive {} } }` — the shim's 27-value `PromptOrigin`
PROJECTED to the two cases the alarm renders differently, verified against
`shim/v1/core.proto:97-140`; `frontend.v1` no longer names `PromptOrigin`.
`FooterAccountingCell` + `Accounting*` → `FooterTokensAccounting { summary;
oneof verdict { complete; incomplete{missing}; invalid{problems} } }` and
`FooterTokensAccounting*` arms. The "PENDING INCREMENT 2" block is gone.

**Why, in the user's terms — and a rejected shape, kept visible.**

- "ContextCostAlert needs to be modeled in the token protobuf shipped for the
  corresponding footer section" — done; and the accounting verdict is a fact
  about the turn's tokens, so it sits on the same cell as a badge; the
  topbar's `TopbarAccountingWarning` points at THIS.
- The user asked whether the cell had mutually exclusive siblings. The
  orchestrator first proposed a `live` / `settled` oneof; the user's next
  questions dissolved it: first-token is per MESSAGE (unset until the current
  message's first token, then held) and excludes nothing; the expensive-turn
  alarm renders THE MOMENT it trips (mid-turn), so it is not a "settled" fact;
  the cell always shows ONE turn's cost at increasing completeness, and a new
  turn resets it. So: siblings, no mode oneof; the only exclusivities are
  `verdict` and `origin`.
- The user then asked, non-rhetorically, why not an event shape —
  `oneof { update{ oneof {input|thinking|expensive} }; done{ accounting } }`.
  Answer recorded: it models the TRANSPORT (a change sequence), not the VIEW.
  The client would accumulate updates into the cell (client-side derivation;
  a fence discard or reconnect loses a field with no whole push to recover
  from); the inner oneof claims input and thinking are never drawn together
  (they always are); `done` would erase the figures when the verdict lands;
  and ordering becomes load-bearing inside one component. Every component
  here is pushed WHOLE; if a ticker's rate ever mattered, the fix is
  daemon-side coalescing, not deltas on the wire. The user: "okay proceed".

**Consequences.** `daemon/internal/progress` publishes the whole cell per
change and projects `PromptOrigin` to two arms; the webapp's expensive-turn
and accounting renderers read the tokens cell.

### `frontend.v1/footer.proto` increment 2: the expanded section is rows of in-flight detached work, and nothing else

**The drawing agreed with the user:**

```
├──────────────────────────────────────────────────────────────────┤
│ ⚙ Explore   "find the roster resolver"               0:31   ▸ │  subagent  → click: its feed bubble
│ ⛓ workflow  review-changes · verify 3/5              2:10   ▸ │  workflow  → click: its feed bubble
│ $ bash      go test ./...                            1:04   ▸ │  shell     → click: its feed bubble
```

**What changed.** `FooterExpanded { repeated FooterExpandedRow rows }`;
`FooterExpandedRow { conversation.v1.MessageId target; FooterExpandedRowRuntime
runtime { started_at_ms }; oneof row { FooterExpandedSubagent {agent_type,
description}; FooterExpandedWorkflow {name, current_step};
FooterExpandedShell {command}; FooterExpandedUnmodeled {tool_name} } }`.
Each row is a jump target: `target` is the item's `DetachedWorkStarted`
message, and activating the row navigates to that feed bubble. No heading
(the strip is the header; an invented "in flight (3)" title was dropped
because it maps to no UI element). `footer.proto` imports
`conversation/v1/message.proto` for `MessageId`.

**Why, in the user's terms.** "The expanded section should be fundamentally
restricted to rows. Those rows should have one of some number of types, and
they should be reserved for asynchronous work … clicking the SubAgent takes
you to the subagent's feed-level bubble, clicking the Workflow takes you to
the workflow's feed-level bubble." The row kinds are exactly the detachable
origins of `ToolCallBlock.call` (`agent`, `workflow`, background `bash`), plus
`unmodeled` so detached unmodeled work cannot vanish from the list; a skill is
not async work.

**Homeless as a result — the user rules, one at a time (five answers):**
the four rows the orchestrator first drew all had homes elsewhere and are
NOT footer: failure line → the feed's failure card + the `blocked` status
(answer 5); gate line → the `asleep`/`merging` statuses (5); merge note →
`SubStatus`/`StatusActivity` (5). Two messages remain under the "PENDING
INCREMENT 2" banner awaiting the ruling: `ContextCostAlert` (the
expensive-turn alert — a daemon-synthesized feed card? an activity note?) and
`FooterAccountingCell` + `Accounting*` (the settled turn's reconciliation —
the topbar's `TopbarAccountingWarning` today references "the footer cell's
evidence", so the verdict arms need a home if the cell goes).
`FooterFailureRow` is deleted outright (answer 5).

### `frontend.v1/footer.proto` increment 1: the main strip, reimagined as three typed resolution levels

**The drawing agreed with the user (his reimagining of the strip):**

```
│ merging   │ cherry-picking 3/7  │ 4f2a1c: fold tokens into api │ 0:42 │ 18.2k in · 3.1k thought │
  Status      SubStatus             StatusActivity                 Clock   Tokens
  coarsest ──────────── resolution increases left → right ────────► finest
```

**What changed.** `ProgressView` is replaced by `FooterView { FooterWorkspace;
FooterFence; FooterStrip; FooterExpanded (empty, increment 2) }`.
`FooterStrip { FooterStatus; FooterSubStatus; FooterStatusActivity;
FooterClock; FooterTokens }`:

- `FooterStatus` — nine EMPTY arms: idle, thinking, waiting, interrupted,
  merging, background, blocked, asleep, disconnected. The coarsest state;
  color per arm from the shared vocabulary, resolved by the daemon.
- `FooterSubStatus` — a oneof of per-status FAMILIES, each a oneof of steps
  with the step's small facts: thinking {submitting, thinking, clearing,
  compacting}; merging {enqueuing, queued{position,depth},
  before_action{action}, cherry_picking{commits}, testing{commits}, conflict,
  after_action{action}, failed, merged} — the merge run's own phase
  vocabulary projected; disconnected {starting, degraded, severed, dead,
  start_failed}; idle {ready, done}; blocked {auth, usage_limit,
  vendor_error}. Unset for statuses with no substructure.
- `FooterStatusActivity` — typed KINDS whose payload is mostly composed text:
  merging_commit{sha,subject}, hook{name}, retrying{attempt,status},
  authenticating{line}, blocked_on_user{detail}, rate_limited{session,weekly
  FooterAllowance}, note{text} (the daemon's free line for a thing with no
  kind yet).
- `FooterClock { optional turn_started_at_ms }`; `FooterTokens
  { input_tokens; thinking_tokens; optional ttft_ms }`.

DELETED: `ProgressWindow`, `RateLimitWindow`, `InterruptWindow`,
`FooterPhase`, `FooterMergeChip`, the deprecated `ProgressView.state` copy,
the interrupt chip (an `interrupted` status now), and the counters
(`pending_permissions`, `queue_depth`, `live_task_count` — held → the tray's
heading; permissions → the feed's cards; tasks → the tray/sheet).
KEPT VERBATIM under a "PENDING INCREMENT 2" banner: `ContextCostAlert`,
`FooterFailureRow`, `FooterAccountingCell`, `Accounting*`; under "PENDING":
`DetachedCancelOutcome`/`DetachedAgentsCancelled` (an ack payload, stage 3);
and the whole PLUMBING section (`RenderState`, `SessionConnectivity`,
`SessionStatus` enums, `RuntimeFault`, `WorkspaceState`, `SessionView`,
`BackfillState`, `DaemonView`, `HeartbeatView`) — not drawn anywhere; its
home is decided after the footer.

**Why, in the user's terms.** "The main phase should be on the left … quite
general, so no 'merge testing', just 'merging'. The second section should be
where the finer resolution comes in. The third section [interrupt chip] I
don't see a reason for at all; we should have an 'interrupted' main status
cleared on subsequent status update. The fourth section, activity, should be
the finest resolution … dynamic output as determined by the daemon. Clock and
tokens are good. Counters can go." And: "the first three should all be typed
(just each 'less' typed than the next by having more of its information
implicit in text fields)." Names chosen by the user: Status, SubStatus,
StatusActivity.

**Consequences.**

- The daemon's footer resolver picks ONE activity (today `ProgressView`
  ships all windows and the webapp applies precedence — client derivation,
  gone) and projects the interrupt outcome to a status, so `frontend.v1` no
  longer names `shim.v1.InterruptOutcome`.
- The status/sub-status arm sets are the daemon's `RenderState` re-cut along
  the six colors + merge family; whether `RenderState` itself survives (it is
  a state ENUM, a live violation) is the PLUMBING decision.
- `FooterAllowance.status` stays the vendor's verbatim word: the vendor's full
  rate-limit vocabulary is not in evidence; typed arms when it is.
- Rows and sheet (failure row, expensive-turn row, merge note, gate row,
  accounting cell) are increment 2, drawing first.

### `frontend.v1/daemon_hold.proto` (was `prompt_queue.proto`): the held tray; `conversation.v1/turn.proto` adds `TurnId`

**CORRECTION, KEPT VISIBLE.** The seven-arm `DaemonHold` type and its
`hold.proto` proposed in the previous entry are WITHDRAWN. The user doubted
`hibernated` belonged; the daemon's evidence agreed and went further:
`ErrSessionHibernated` is raised on open/create (`createestablish.go:372`,
`openfailure.go:52`) — the revival GATE, holding nothing;
`ErrPromptRefusedByMergeState` REFUSES a prompt (`mergepromptgate.go:71`),
holding nothing; the uninterruptible context cut is a CLASSIFICATION verdict.
The four real holds (shutdown drain, keep-alive turn, revival pending, build
refresh) are exactly the arms already on disk and are entry-scoped. ROOT
CAUSE of the error: the orchestrator generalized from the WORD "not yet"
across refusals, gates and holds without checking which of them held
anything. There is no shared hold type; the hold oneof stays on the entry.

**What changed.**

- `prompt_queue.proto` → `daemon_hold.proto`. `QueueView` → `DaemonHoldTray {
  DaemonHoldWorkspace; DaemonHoldFence; DaemonHoldHeading; repeated
  DaemonHoldItem }`; `DaemonHoldItem { oneof item { HeldPrompt prompt;
  HeldOffer offer } }`.
- `QueueEntry` → `HeldPrompt { conversation.v1.TurnId turn;
  conversation.v1.UserSaid said; HeldPromptQueuedAt queued_at; oneof
  classification (5 arms, renamed `HeldPrompt*`, semantics verbatim); oneof
  hold (4 arms, renamed, semantics verbatim) }`. `QueueClassificationHold.
  accepted` (bool) is `HeldPromptAccepted { bool }` inside the
  hold_for_turn_end arm. `HeldPromptKeepAliveHold.turn_id` (string) is
  `TurnId turn`.
- NEW `HeldOffer { oneof offer { HeldOfferMergeDequeue merge_dequeue } }` —
  a question the daemon holds for the user's answer; the merge-dequeue card's
  home. `HeldOfferMergeDequeue` is EMPTY ON PURPOSE: its body is decided when
  `agentrepl.v1.MergeDequeueOffer` is walked (stage 3), not guessed here.
- NEW `conversation/v1/turn.proto` — `TurnId { string value }`, reopening
  stage 1 additively. Shared vocabulary in the leaf (the `SessionCommand`
  argument): a submission's response returns one, the tray holds under one,
  the shim's turn bookkeeping names one, feed rows are stamped with one.

**Why, in the user's terms — the questions that shaped it.**

- "Should HeldPrompt be implemented in terms of conversation.v1.UserSaid?" —
  YES: a held prompt IS a `UserSaid` not yet forwarded; `string text` was a
  partial re-spelling of `UserContent` that would drop images. Consequence:
  `SubmitPromptRequest` (stage 3c) carries `UserSaid` too — one canonical form
  client → daemon → tray → shim → record.
- "Should there be a canonical identifier?" — YES, and there were TWO: the
  daemon-minted `QueueEntry.id` (`queue.go:591`) and the client's `request_id`
  carried on the same entry (`queue.go:32`), which becomes the shim's turn id
  (`core.proto:252`). One identity: the turn.
- "Who mints these IDs? I'm concerned the webapp might be minting prompt
  ids." — Today clients do (`fe-80-fdb1`), justified by a race the one-stream
  design had (a push could beat its ack). Under SDUI clients reconcile
  nothing, so THE DAEMON MINTS `TurnId`; `SubmitPromptSuccess` returns it. A
  client-minted idempotency key on the request is a separate stage-3b
  question and is not the turn's identity.
- "How does the webapp's feed resolve a response to a request?" — it
  doesn't: unary responses answer requests; the daemon STAMPS feed rows of a
  turn with `TurnId` (the existing stamps-alongside pattern), so a client that
  wants to highlight its own prompt matches the id it was returned; every
  other effect is a pushed view update. Optimistic rows and the client's
  pending-request map (`command-dispatch.ts` `onAck`) go away.
- Hibernation/revival gate: NOT a tray item — a workspace-level gate;
  placement still open for the footer drawing.

**Consequences.**

- The tray's stream is `WatchDaemonHolds` (stage 3a); the footer counter reads
  "N held".
- `daemon/internal/sessioncontroller/queue.go` drops `newQueueEntryID()`; the
  entry is keyed by the daemon-minted turn id, and duplicate submissions are
  refused by idempotency key rather than by a second id.
- `promptreceipt.go`'s "refuse a turn claim with no request id" becomes
  "refuse a duplicate idempotency key"; the guarantee survives, the owner
  changes.
- Stage 4 (`shim.v1`): `turn_id`/`request_id` strings become `TurnId`.
- Stage 2 `feed.proto`: rows of a turn carry a `TurnId` stamp.

### The prompt queue leaves `footer.proto`: `prompt_queue.proto`, its own component and stream

**What changed.** The "daemon-held prompt queue" section — `QueueView`,
`QueueEntry`, the five `QueueClassification*` arms, the four `QueueEntry*Hold`
arms — moved VERBATIM into `frontend/v1/prompt_queue.proto`. `footer.proto`'s
header no longer claims prompt intake or a composer; its unused
`session_command` import is dropped. Shapes are unchanged by the move and are
judged in the queue file's own increment.

**Why, in the user's terms — three questions, answered in order.**

- "Are we modeling the prompt queue as part of the footer?" — No, and the
  first footer drawing was wrong: the webapp renders queued prompts as
  `queued-card`s (`render.ts:507-570`), the composer is HOST-native (Emacs
  runs the webview with `composer=0`), and the footer strip carries only the
  `N queued` counter.
- "Where do queued prompts come from? The daemon, right?" — Yes,
  exclusively: a prompt submitted while a turn runs is held, classified and
  delivered later by the daemon; the vendor never sees it until then, so it
  is daemon-owned pending intent, correctly absent from `conversation.v1`.
- "Do we want the UI component of the queue to be in the feed? Or should it
  be separate and monitored separately?" — SEPARATE: the feed is history +
  the live turn (scrolls, pages, appends); the queue is the FUTURE
  (whole-list-replaced on every change). Drawn as its own "pending" tray at
  the feed's tail, above the footer; own stream (`WatchPromptQueue`, stage
  3a); the feed knows nothing about it. Agreed: "okay makes sense".

**A vocabulary decision recorded here, to be landed at the queue's shape
increment:** the daemon's "NOT YET" appears in six places with three
spellings (per-entry hold arms; `WorkspaceState.merge_lease_held`;
hibernation/revival gate; scheduled-shutdown drain; the uninterruptible
context cut; `Failure*` refusal arms). One type — `DaemonHold`, a oneof of
reasons (merge lease, hibernated, revival pending, shutdown drain, keep-alive
turn, build refresh, context cut) — declared once in `frontend/v1/hold.proto`
and EMBEDDED wherever a view or response says "not yet": `QueueEntry.hold`,
a footer gate element, the host stream Emacs watches (it owns the composer),
and stage-3 error arms. It is a TYPE with no RPC of its own. What does NOT
generalize: the queue's classification verdict (interject / hold-for-turn-end
/ pending / error) — that is ordering, not deferral.

**Consequences.** `footer.proto` shrinks to the strip, rows and sheet plus
the PLUMBING section, whose fate is the footer increment's; the webapp's
queued-card rendering moves from the feed renderer to a tray component; the
feed's paging never sees a queued entry.

### `frontend.v1/sidebar.proto` follow-up: the message tree is the UI tree

**What changed.** No semantics change; the element-message and
message-tree-is-UI-tree conventions applied. Every bare field became a
message and every drawn box became one message with nesting equal:

- `WorkspaceRoster { view; RosterMergedSection recently_merged;
  RosterCurrentWorkspace current { dir }; RosterNavCursor nav { dir } }`.
- `RosterRepoSection { RosterRepoKey key; RosterSectionHeader header;
  RosterRows rows }`; `RosterTaskSection { RosterTaskKey key;
  RosterTaskSectionHeader header; RosterRows rows }`;
  `RosterMergedSection { RosterSectionHeader header; RosterRows rows }`.
- `RosterSectionHeader { RosterLabel; RosterFold }` shared by repo and merged
  sections; `RosterTaskSectionHeader { RosterLabel; RosterFold;
  RosterTaskDone }` its own, because the done check is drawn IN the task
  header and repos have none — the user's earlier `RosterSection` reuse
  survives as the shared header + `RosterRows`, not as one message
  coalescing header and rows.
- `RosterRow { RosterRowWorkspace; RosterRowName; status (26 arms,
  unchanged); RosterRowCurrent; children; RosterRowWhen; RosterRowDetail;
  RosterRowClosed }`.
- `RosterRowWhen` is a ONEOF — `last_selected { at_ms }` | `merged
  { at_ms }` — chosen by the daemon: precedence (merged wins) is resolved
  server-side, and the client renders whichever arm arrives (the user's
  correction of the orchestrator's two-optional-fields sketch).
- `RosterRowDetail { RosterRowDetailBranch; RosterRowDetailParentBranch;
  RosterRowDetailSummary }` — three lines, each present/absent by message
  presence, not empty string.
- The file header carries the ASCII layout the shapes were checked against.

**Why, in the user's terms.** "Are our messages making this organization
implicit? Or are we coalescing adjacent fields into different components in
the UI hierarchy?" — the answer was that two places coalesced (section header
vs rows; the three detail lines), and both were re-partitioned. Approved:
"apply your RosterSection changes you just suggested, then let's move on."
The constraint itself — message tree = UI tree, ASCII-checked, agreed with
the user BEFORE shapes are sketched — is being added to the skill by a
one-shot subagent (`proto-message-tree-mirrors-ui-tree`).

**Consequences.** Renderers read one message per box; presence replaces the
empty-string and zero-sentinel conventions in the row; the webapp's
when-column precedence code is deleted (the daemon decides).

### `frontend.v1/topbar.proto`: every field an element message; health views out; `ModelOption` moves to `conversation.v1`; the breakdown menu gets its own file

**A NEW CONVENTION, stated by the user during this increment and being added
to the skill by a one-shot subagent (`proto-ui-element-messages`):** in
figma→idl / SDUI, a component's view message contains NO dangling primitives
— EVERY field, including ones like `workspace` that carry addressing, is
wrapped in a dedicated, appropriately-named message, so the UI's
subcomponents are implicit in the schema and no field is conflated with a
neighbor. And when the SAME fact appears in two component views, each
component wraps it in ITS OWN message (`TopbarWorkspace`,
`TokenBreakdownWorkspace`) rather than sharing one — "that's EXACTLY
PERFECT: duplicate information represented with dedicated messages implies
separate UI subcomponents." The orchestrator's first two sketches (scalars
allowed for addressing; a shared wrapper considered) were both corrected by
the user; both corrections are the convention now.

**What changed.**

- `TopbarView` is seven element messages and nothing else: `TopbarWorkspace
  { dir }`, `TopbarFence { token }`, `TopbarTitle { text }`,
  `TopbarSessionLine { text }`, `TopbarModelSelector { ModelOption selected;
  repeated ModelOption options }`, `TopbarConnectivity` (unchanged shape),
  `TopbarWarningStrip { repeated TopbarWarning }`. `model_display` (a string)
  is gone — the selection is the whole option, presence = selected.
- `DaemonHealthView` and `SessionHealthView` DELETED from `frontend.v1`: not
  drawn by anything, they are the answers to two host commands and become
  `agentrepl.v1` responses at stage 3 (`bool healthy + string reason` → a
  healthy/unhealthy oneof there).
- `shim.v1.ModelOption` DELETED; `conversation.v1.ModelOption` ADDED to
  `api.proto` (an API fact: the models the vendor offers), reopening stage 1
  ADDITIVELY — nothing landed in `api.proto` changes. `shim/v1/core.proto`'s
  `ModelCatalog.models` and `frontend/v1/footer.proto:427` repoint to it;
  `frontend.v1` no longer imports `shim.v1` from the topbar (footer still
  does, its turn next).
- `token_breakdown.proto` is a NEW FILE holding `TokenBreakdownView` and its
  tree, moved out of `topbar.proto`; `TokenBreakdownWorkspace`,
  `TokenBreakdownFence`, `TokenBreakdownHeading` are new element wrappers;
  `share_permille` is `optional` (was a -1 sentinel).
- `TopbarConnectivity.tone` stays a string: a color-class NAME from the
  shared `proto/vocab/render-colors.json` vocabulary, a rendering token, not
  a state. The state it derives from (`SessionConnectivity`, footer.proto) is
  an enum and is raised at the footer's turn.
- `workspace` and `fence` are KEPT (wrapped) and marked STAGE-3b: whether a
  per-workspace stream's element still names its workspace, and whether
  cross-stream fencing survives per-component streams, are conventions.

**Why, in the user's terms.** "This message looks good, I approve" — after
the two corrections above.

**Consequences.**

- The daemon's topbar resolver (`daemon/internal/frontend/topbar.go`) and
  the webapp's topbar renderer read element messages; the health-view
  producers move to stage-3 RPC handlers.
- Every consumer of `shim.v1.ModelOption` (shim, daemon, webapp) reads
  `conversation.v1.ModelOption`.
- RETROACTIVE, PENDING THE USER'S CALL: `sidebar.proto`'s `RosterRow` has
  dangling primitives that are elements (`dir`, `name`, `branch`,
  `parent_branch`, `summary`); the same convention applies there as a
  follow-up increment (`RosterRowName`, `RosterRowDetail {…}`, etc.).
- The typed workspace identity question (one `WorkspaceRef` type vs
  per-component wrappers) is ANSWERED by the convention: per-component
  wrappers, because they name subcomponents. A shared identity TYPE may still
  be wanted for `agentrepl.v1` requests (stage 3), where nothing is drawn.

### `frontend.v1/sidebar.proto`: daemon-resolved, global, view-only; `revision`/`boot_id` gone; `RosterSection` shared

**Two settlements above the shapes, both by selection.**

- SCOPE — GLOBAL. The sidebar's stream carries no `workspace`; every webview
  (one per workspace) watches the same roster and the daemon's stream order
  is the only order. Recorded together with the rendering topology it rests
  on: one WKWebView xwidget per workspace, bound for life, in its own pinned
  Emacs buffer (`lisp/frontend.el:847`), each with its own already-
  workspace-scoped socket (`webapp/src/address.ts:71`); consolidation to one
  webview was REFUSED (four objections in the discussion: buffer/xwidget
  binding, per-workspace socket, `SPC .` alignment, no cross-view sharing of
  process memory anyway), and endpoint-per-component ≠ connection-per-
  component — component streams multiplex over the one socket a webview
  already opens.
- FILE CONTENTS — VIEW ONLY. The user chose AGAINST the orchestrator's
  recommendation (view + events per figma→idl). The superseded record's
  "a request type is `agentrepl`, never `frontend`" (drawn/called) STANDS,
  unreopened: sidebar clicks are `agentrepl.v1` requests with plain fields.
  Recorded so the figma→idl "events in the same file" reading is not
  re-proposed for the other components: in this tree, events are called,
  not drawn.

**What changed in the file.**

- `WorkspaceRoster.revision` and `boot_id` DELETED with the whole
  epoch/monotonicity comment — no outside publisher, nothing to order. Fields
  renumber: `view` 1–2, `recently_merged` 3, `current_dir` 4, `nav_dir` 5.
- `RosterRepoSection` and `RosterTaskSection` now WRAP the shared
  `RosterSection { rows, folded, label }` with their own key (`repo_key`;
  `task_id` + `done`); `RosterTaskSection.title` becomes `section.label`.
  The user's amendment: "RosterSection should be reused."
- `RosterRow.last_viewed_at_ms` and `merged_at_ms` are `optional` (presence,
  not zero sentinels).
- Header and `status` comment rewritten for ONE resolver (the daemon
  coarsens `RenderState` onto the dot); the "sidebar.el's wire table is the
  third face" sentence is gone. Arm set unchanged (26 empty arms); `none`
  re-described as "registered, no session ever created".
- Kept, marked for stage 3a: `nav_dir` and `folded` — UI preference the
  daemon holds only if an `agentrepl.v1` verb tells it; else deleted or made
  webview-local there.

**Why, in the user's terms.** "Looks good", with the `RosterSection` reuse.
Emacs has nothing to do with `WorkspaceRoster` under the settled model: "it
can only register a workspace, and select a workspace. It doesn't have any
need to be able to work with the underlying workspace representation message
for the frontend."

**Consequences.**

- `frontend.v1` sidebar messages now flow in exactly ONE direction, daemon →
  webapp; the old inbound `agentrepl.v1` import of `sidebar.proto` for
  `PublishWorkspaceRosterRequest.roster` never returns.
- The webapp's `rosterFromFrame` staleness check on `revision`/`boot_id`, the
  elisp publisher and the daemon roster retainer are deleted in the wave.
- Consumers reading `RosterRepoSection.rows/folded/label` and
  `RosterTaskSection.rows/title` read `.section.*`.
- `RosterRow.dir` stays a bare `string`. A typed workspace identity is a
  real question (same argument as `MessageId`), but the fact originates at
  the daemon; where a daemon-owned identity type lives is a stage-3
  conventions call, flagged.

### Sidebar producer settled: the DAEMON owns the roster; Emacs sends COMMANDS, not state (no proto landed yet)

**Decided (stage 2, `sidebar.proto`, above any shape).** `WorkspaceRoster`
becomes a daemon-RESOLVED `frontend.v1` view. Emacs's contribution collapses
to commands over `agentrepl.v1` — in the user's words, "Emacs only needs to
REGISTER a workspace (subsequently the daemon tracks the information like
parentage, dir, etc.), and to SELECT a workspace (when the user switches tabs
in Emacs) … two separate messages passed along two separate RPCs, like
`RegisterWorkspace` and `SelectWorkspace`." The exact RPC names and set are
stage-3a inventory; the PRINCIPLE is settled here.

**Why.** Today Emacs authors the whole roster (`sidebar.el`), re-deriving each
row's status from what the daemon told it into a second vocabulary
(`RosterRowStatus`), which the webapp maps a third time — three spellings of
one status, and the source of the done-vs-interrupted class of bug. The
daemon already owns `dir` (registry), 24 of 26 status arms (`RenderState`),
branch/merge facts and summaries; the roster UNIVERSE and the selection were
the only genuinely Emacs-held facts, and both are events, not state.

**What it removes, and why each removal is safe.**

- `WorkspaceRoster.revision`, `boot_id` and the epoch/monotonicity rules —
  they guarded against an out-of-order STATE publish; a command has no stale
  roster to resurrect. The daemon's own stream ordering replaces them.
- Emacs's `publishWorkspaceRoster` path and the daemon's roster retainer
  (`daemon/internal/frontend/roster.go`) — the daemon resolves, so there is
  nothing to retain from outside.
- `RosterRow.current` / `last_viewed_at_ms` as Emacs-supplied — the daemon
  stamps both on `SelectWorkspace`.
- `closed` vs gone — marked by the daemon from the open/close/unregister
  traffic it already brokers.
- The presence-snapshot alternative the orchestrator proposed first
  (`WorkspacePresence` per row) — REJECTED by the user in favor of commands;
  recorded so it is not re-proposed.

**Residue, decided at stage 3a:** `nav_dir` (the Emacs keyboard cursor
riding the roster), the repo/task grouping mode and section folds are UI
preference, not workspace fact — either webview-local or one small
`SetRosterView`-style RPC.

**Consequences.**

- Daemon restart: the registry is durable; Emacs re-registers idempotently on
  reconnect (`RegisterWorkspace` must be idempotent by `dir`).
- Every consumer of `revision`/`boot_id` (elisp publisher, daemon retainer,
  webapp `rosterFromFrame` staleness check) is deleted in the wave.
- The `sidebar.el` status table (24 arms) is deleted; the daemon's roster
  resolver coarsens `RenderState` onto the sidebar's dot vocabulary once.
- Scope (global stream, no `workspace`) and file contents (view vs
  view+events) remain the two open sidebar questions above the shapes.

### `conversation.v1/session_command.proto`: no change — STAGE 1 COMPLETE

**What changed.** Nothing. `SessionCommand` stays an enum (a closed set of
command names, not a state; its per-value facts are schema options, not
sibling fields, so the adjacent-exclusivity test passes), `SessionCommandSpec`
stays an enum-value option, and the file stays in `conversation.v1` as the
leaf every surface reads.

**Why, in the user's terms.** Approved. The `context_cut` fold considered
earlier is refused: `ContextCut` is a record, `SessionCommand` a vocabulary no
record carries — different concerns.

**Stage 1 (`conversation.v1`) is complete** at this commit. The package is
nine files — `message`, `tool_call`, `agent`, `user`, `content_blocks`,
`context_cut`, `api`, `detached_work`, `session_command` — and compiles.
`frontend.v1` does not (`feed.proto` names deleted `conversation.v1` types),
which is stage 2's starting condition, by design.

**Carried into stage 2 as requirements from stage 1:**

- A daemon-synthesized MERGE feed item that coalesces the vendor records
  produced while a merge ran (from `detached_work.proto`).
- Typed identities to embed instead of strings: `MessageId`, `ToolCallId`.
- The tool card's per-tool presentation is resolved by the daemon from
  `ToolCallBlock.call`; the client never digs.
- `StopInterrupted` producer question owed to stage 4 / the wave.

### `conversation.v1/tokens.proto` folds into `api.proto`

**What changed.** `tokens.proto` is DELETED; `TokenUsage`, `TokenCacheHits`
and `TokenCacheMisses` move VERBATIM into `api.proto` under a "USAGE
ACCOUNTING" section banner that carries the old file's provenance header.
No shape change. Importers repointed: `conversation/v1/agent.proto`,
`shim/v1/bookkeeping.proto`, `state/v1/durable.proto` now import
`conversation/v1/api.proto`. Every non-frontend package compiles.

**Why, in the user's terms.** Approved as proposed: `TokenUsage` is the API's
own accounting of a request — the same actor as `ApiRequestFailed` — so one
file per concern puts them together: "the vendor API's outcomes: what it
charged, or why it refused".

**Consequences.** `conversation.v1` is now eight files: `message`,
`tool_call`, `agent`, `user`, `content_blocks`, `context_cut`, `api`,
`detached_work`, plus `session_command` (next and last). `state.v1` and
`shim.v1` gained no new dependency, only a renamed import.

### `conversation.v1/detached_work.proto`: the origin call is the whole description; `DetachedWorkKind` and `DetachedMerge` are gone

**What changed.**

- `DetachedWorkStarted { string origin_tool_call_id; string label;
  DetachedWorkKind kind }` becomes `DetachedWorkStarted { ToolCallBlock
  origin }`.
- DELETED: `DetachedWorkKind` and all six arms — `DetachedAgent`,
  `DetachedShell`, `DetachedWorkflow`, `DetachedSkill`,
  `DetachedUnclassified`, `DetachedMerge`.
- `DetachedLost { string inference }` becomes `DetachedLost { oneof how {
  file_vanished, went_silent, swept_up } }` with three empty arm messages.
- `DetachedWorkProgressed`, `WorkflowStepObserved`, `WorkflowStep` and its
  three arms, `SkillBodyResolved`, `DetachedWorkEnded`, `DetachedProcessExit`,
  `DetachedSucceeded`, `DetachedFailed`, `DetachedCancelled` — unchanged.
- The file now imports `tool_call.proto`.

**Why, in the user's terms.**

- The kind IS the origin call's `call` arm now that `ToolCallBlock` is typed:
  `bash` (run_in_background) is a shell, `agent` a subagent, `workflow` a
  workflow, `skill` a skill, `unmodeled` an unmodeled tool that detached.
  `DetachedShell.command`, `DetachedSkill.skill_name/args` and
  `DetachedUnclassified.tool_name` were re-spellings of `ToolCallBash.command`,
  `ToolCallSkill.skill/args`, `ToolCallUnmodeled.tool_name`. Import the
  encompassing message; answer 5 (already represented). `label` was a
  presentation the daemon resolves from the origin (answer 2).
- `DetachedMerge` — THE USER'S RULING, verbatim in substance: "Merge is a
  daemon-synthesized action: workspace merging can be represented specially
  on the frontend, but it's not something the vendor has any knowledge of. It
  can never be 'detached' because the agent isn't orchestrating it, the
  daemon is, exclusively." Dropped from this surface (answer 3, abstraction
  leak). It resolved a contradiction the file carried in its own comments
  ("NO TOOL SPAWNS IT — the daemon opens it" vs `message.proto`'s "the daemon
  is not an author here").
- `DetachedLost.inference` — three enumerated ways in its own comment; a
  closed set that answers "how did we conclude this" is a oneof.

**A STAGE-2 REQUIREMENT this creates, recorded so it is not lost.** The user
wants merging supported in the conversation as a `frontend.v1` feature: a
daemon-synthesized feed item that COALESCES every vendor record produced
while the merge ran (the merge is a Claude skill execution under the hood,
plus the resulting actions/responses) into ONE feed entry, and the daemon —
not any producer — determines which `AgentSaid`/`ToolReturned`/… belong to
that item. `frontend/v1/feed.proto`'s turn must model that row.

**Consequences.**

- `frontend/v1/feed.proto:587` references `conversation.v1.DetachedWorkKind`
  and no longer compiles; `conversation.v1` does. Intended — `frontend.v1` is
  stage 2 and is redesigned there; the break is not patched around.
- The daemon's async classification by tool NAME (`Agent`/`Task`/`Workflow`)
  becomes a switch on `origin.call`; the webapp's `asyncShape`/`classifyAsyncSource`
  likewise. Cards derive their label from `origin` server-side.
- A producer that emits `DetachedWorkStarted` must hold the originating
  `ToolCallBlock` at detachment time; the shim sees the call before the result
  that reveals detachment, so it does. The sidecar reading history has the
  call in the same transcript.
- Open, flagged not decided: whether a *skill* is really "detached" work (its
  records are the main agent's; the nesting is a UI window). Naming only.

### `conversation.v1/api.proto`: `FailureRaised` → `ApiRequestFailed`, with the vendor's error kinds as arms

**What changed.** `FailureRaised { summary, detail, retry_in_ms }` becomes
`ApiRequestFailed { string message; oneof kind { rate_limited, overloaded,
authentication_failed, permission_denied, invalid_request,
request_too_large, not_found, internal, unmodeled { type } } }`.
`retry_after_ms` is `optional` and lives ONLY on `ApiRateLimited` and
`ApiOverloaded`. The `MessagePayload` arm renames `failure_raised` →
`api_request_failed` (tag 4 unchanged).

**Why, in the user's terms.** "Looks good." The concern is the API's own
outcome, and the vendor's error taxonomy is a documented closed set
(400/401/403/404/413/429/500/529 with named types), so it is arms plus
`unmodeled`, not two prose strings. The zero-sentinel `retry_in_ms` becomes
presence, confined by adjacent-exclusivity to the two arms it applies to.
Recoverability stays the daemon's judgement.

**Consequences.**

- Every consumer of `FailureRaised`/`failure_raised` (daemon translate,
  webapp decoder, elisp) renames and switches on `kind`.
- To verify at implementation: whether the producer sees the error TYPE
  structured (SDK error object) or only the CLI's `"API Error: 429 …"` text.
  If only text, the arm is derived from the status code that text carries,
  and the wave records that derivation as the producer's, once.

### `conversation.v1/context_cut.proto`: `ContextTokenDelta` shared by both cuts

**What changed.** New `ContextTokenDelta { int64 tokens_before; int64
tokens_after }`. `ContextCleared` (was empty) gains `ContextTokenDelta tokens
= 1`; `ContextCompacted` loses its two bare `int64`s and gains
`ContextTokenDelta tokens = 2` beside `summary`. `ContextCut` unchanged.

**Why, in the user's terms.** "Clearing context doesn't mean context goes to
zero — there's still system prompt and skills and whatnot that get reloaded
into context." So a clear has a before/after exactly as a compaction does,
and one message carries it for both. This is a duplicated-VALUE-per-arm case
made into a shared TYPE within the namespace — allowed, because it is one
fact (a size change) with one canonical form, not two arms re-spelling each
other.

**Consequences.**

- The producer must observe the post-clear context size. If the vendor does
  not report it on a clear, `tokens_after` cannot be filled honestly; the
  implementation wave verifies what the CLI writes on `/clear` before the
  field is populated, and surfaces a gap rather than writing zero.
- Consumers reading `ContextCompacted.tokens_before/after` read
  `.tokens.tokens_before/after`.
- Naming: "delta" carries endpoints, not a difference; the comment says why.

### `conversation.v1/content_blocks.proto`: `ImageBlock` location as two arms; `UnsupportedBlock` is not a fallback

**What changed.** `ImageBlock.source` (a string that was "a path or URL")
becomes `oneof location { ImageBlockPath path { path }; ImageBlockUrl url
{ url } }`; `media_type` renumbers to 3. `TextBlock` unchanged.
`UnsupportedBlock`'s shape is unchanged; its message comment now says, at the
user's instruction, that it is NOT A FALLBACK: populated only for a block
whose shape is genuinely, realistically unknowable in schema, never because
modeling was inconvenient, the shape varies, or "we'll type it later" — a
recognizable kind found in it is a producer defect.

**Why, in the user's terms.** "Looks good, but update the docstring for
UnsupportedBlock to inform readers/users that this block should only be
populated by messages that are truly not realistically knowable in schema,
and not as a lazy fallback."

**Untyped field, ACCEPTED as a cost.** `UnsupportedBlock.raw` (`Struct`) —
the one qualifying reason: the producer holds no schema for a block kind it
has never seen; nothing renders from it. Accepted by the user explicitly in
the same breath as the docstring instruction.

**Consequences.**

- Consumers reading `ImageBlock.source` switch on the arm; a renderer that
  sniffed `://` to choose between `<img src>` and a file fetch reads the arm.
- The sidecar's/shim's converters must NOT route a knowable block here; the
  implementation wave audits every `UnsupportedBlock` construction site
  against the vendor's block kinds and models what is knowable (the vendor's
  document/PDF block is the likely first candidate).

### `conversation.v1/user.proto`: shapes unchanged, `UserSaid` comment states its two readings

**What changed.** No shape change. `UserSaid`'s comment now states (a) the
nested-prompt reading — under a `DetachedAgent` container it is the spawning
agent's prompt, read from the parent chain, which `MessageAuthor`'s deletion
made implicit; and (b) that a session command is NOT a `UserSaid`, because
the daemon recognizes one before forwarding and it earns no user message
(`session_command.proto:66`), whereas a custom command/skill expands into a
prompt and is one.

**Why, in the user's terms.** Approved. The user asked the UX reason for
`UserContent` being a repeated block list rather than text + images: a person
interleaves words and pictures, the vendor's user message is an ordered block
array, and the feed draws it in composed order — one text + repeated images
would lose placement and force one text run.

**Consequences.** None new. Considered and NOT proposed: a `UserSaid` arm for
a session command — never a record by `session_command.proto`'s own rule;
`session_command.proto` stays a leaf and is judged at its own turn.

### `conversation.v1/agent.proto`: `ThinkingBlock` as two arms, `StopRefusal` added, comments repaired

**What changed.**

- `ThinkingBlock` loses `string text` + `bool redacted`; it gains
  `oneof thinking { ThinkingBlockShown shown { text }; ThinkingBlockRedacted
  redacted {} }`.
- `StopReason` gains `StopRefusal refusal = 5`; `unsupported` renumbers to 6.
  `StopInterrupted` is KEPT, with a note written on the arm that no producer
  is yet verified to observe it as a stop reason (it may only exist as a
  user-role "[Request interrupted]" record).
- `ContentArriving`: shape unchanged; `block_index` is stated as the
  zero-based NODE index into `AgentContent.blocks`; the "tool arguments are a
  typed Struct" sentence now says they arrive typed as `ToolCallBlock.call`;
  the dead `DESIGN-protobuf-surfaces.md` pointer now names the superseded
  file.
- `AgentSaid`, `AgentContent`, `AgentContentBlock` unchanged.

**Why, in the user's terms.** Approved as sketched. Two questions were asked
and closed on the way: (1) `AgentStopped` as its own payload arm instead of
`StopReason` on `AgentSaid` — NO, every settled response has both content and
a stop reason (one turn is several `AgentSaid`, each with its own
`stop_reason`), and turn-level "the agent stopped" is `shim.v1` bookkeeping;
(2) `content` and `stop_reason` as a oneof — NO, they always coexist:
`stop_reason` says how the content ENDED (tool_call with blocks present,
max_tokens with truncated blocks, refusal with empty blocks), and a oneof
would make the ordinary tool-call response unrepresentable.

**Consequences.**

- Every consumer reading `ThinkingBlock.text`/`.redacted` switches on the
  arm; a renderer that showed "reasoning hidden" for `redacted=true` reads
  `redacted` presence instead.
- `StopRefusal` is a new arm every consumer's stop switch must handle
  (compile-surfaced). `stop_sequence` and `pause_turn` deliberately remain
  `unsupported`.
- The `StopInterrupted` question is owed an answer at the shim's turn
  (`shim.v1`, stage 4) or by the implementation wave; if no producer sets it,
  the arm is deleted then, not silently kept.

### `conversation.v1/tool_call.proto`: typed tool arms, `ToolCallId`, outcome and scope arms

**What changed.**

- `ToolCallId { string value }` is new: the typed identity of a tool call,
  the same remedy as `MessageId`. `ToolCallBlock.tool_call_id` and
  `ToolReturned.tool_call_id` embed it. `DetachedWorkStarted.origin_tool_call_id`
  (still a `string`) converts at `detached_work.proto`'s turn.
- `ToolCallBlock` loses `string tool_name` and `google.protobuf.Struct
  arguments`; it gains `oneof call` with fourteen arms — thirteen typed tools
  (`bash`, `read`, `write`, `edit`, `grep`, `glob`, `agent`, `workflow`,
  `skill`, `send_message`, `task_create`, `task_update`, `task_stop`) and
  `unmodeled { string tool_name; Struct arguments }`. THE ARM IS THE TOOL;
  a name beside a typed arm would be a second spelling.
- `ToolCallGrep.output` is a oneof (`GrepOutputContent { line_numbers,
  context_* }`, `GrepOutputFilesWithMatches {}`, `GrepOutputCount {}`), not
  an enum: line numbers and context exist only in content mode.
- `TaskStatus` is a oneof of four empty arms; `ToolCallTaskUpdate` uses it as
  `optional`, with every other field `optional` because an update carries
  only what changed.
- `ToolReturned` loses `bool is_error` and `content`; it gains `oneof outcome`
  with `ToolReturnedSucceeded { content }` and `ToolReturnedFailed { content }`.
- `PermissionAllowed` loses `bool for_session`; it gains `oneof scope` with
  `PermissionAllowedOnce {}` and `PermissionAllowedForSession {}`.
- `PermissionAsked` unchanged; its comment now states WHY it carries the call
  whole (stream-plane timing).

**Why, in the user's terms.** "Toolcalls look good." The typing removes a
client deriving from an untyped blob: the webapp branched on `tool_name` in
eight places (`render.ts:1691-1740`, `async-stream.ts:146-190`,
`permission-preview.ts:33-45`, `stream-member.ts:94`) to dig `command`,
`file_path`, `pattern`, `summary`, `skill`, `status` out of a Struct by string
key. The thirteen typed tools are exactly the tools those sites branch on. The
grep oneof came from the user's heuristic, stated during this increment: if
any value of an enum corresponds to adjacent information exclusive to that
value, that is a strong (sufficient, not necessary) indicator the enum should
be a oneof with the exclusive information confined to the arm.

**Two untyped fields, each ACCEPTED as a cost by its own selection.**

- `ToolCallUnmodeled.arguments` (`Struct`) — the producer holds no schema for
  a tool an MCP server registered at runtime; nothing branches on a key
  inside it.
- `ToolCallWorkflow.args` (`Value`) — arbitrary user-authored JSON only the
  workflow script reads; the producer cannot know its shape and nothing
  renders from it.

**Consequences.**

- The thirteen arms' field sets are the vendor's public tool schemas AS
  KNOWN, not read off the shim: the comment on `ToolCallBlock` says so, and
  the implementation wave VERIFIES each arm against real transcripts before
  the shim converts into it. A vendor field no arm carries is not lost — the
  store's internal half retains the source record when conversion dropped
  structure (superseded record, "A record may be partially convertible").
- `conversation.v1` is no longer vendor-TOOL-neutral: it names Claude Code's
  built-in tools. It remains vendor-CONTENT-neutral (the block model). This
  was weighed against keeping `Struct` and having the daemon resolve a typed
  per-tool card in `frontend.v1`; the user chose typing at the record.
- The shim's converter grows a per-tool switch; adding a tool later is adding
  an arm, and an unhandled arm is a compile-surfaced gap in every consumer.
- Every consumer of `tool_name` — the webapp's eight sites, the daemon's
  async classification (`Agent`/`Task`/`Workflow` by name), permission
  preview — reads the arm instead. `Task` (the older name for `Agent`) maps
  onto the `agent` arm at conversion; the wire does not carry the alias.
- The `StopInterrupted` question stays open for `agent.proto`: whether any
  producer observes `interrupted` as a stop reason.
- Open, NOT modeled: an allow carrying the user's EDITED input
  (`updatedInput`). Whether the transcript preserves it is unverified.

### `conversation.v1` is one file per concern: `message.proto` keeps the record and the arm oneof, the arm bodies move to dedicated files

**What changed.** The regrouped `message.proto` was split along its section
banners, every message VERBATIM, into:

- `message.proto` — `MessageId`, `MessageEntry`, `MessagePayload` (the record
  and WHAT it can say); imports every body file below.
- `tool_call.proto` — `ToolCallBlock`, `ToolResultContent`,
  `ToolResultContentBlock`, `ToolReturned`, `PermissionAsked`,
  `PermissionAnswered`, `PermissionAllowed`, `PermissionDenied`,
  `PermissionAbandoned`.
- `agent.proto` — `AgentSaid`, `AgentContent`, `AgentContentBlock`,
  `ThinkingBlock`, `StopReason` and its five arms, `ContentArriving`.
- `user.proto` — `UserSaid`, `UserContent`, `UserContentBlock`.
- `content_blocks.proto` — `TextBlock`, `ImageBlock`, `UnsupportedBlock`, and
  the "neutral by design" / "narrowed per site" preamble.
- `context_cut.proto` — `ContextCut`, `ContextCleared`, `ContextCompacted`.
- `api.proto` (first `failure.proto`, renamed) — `FailureRaised`.
- `detached_work.proto` — all twenty-one detached-work messages.
- `tokens.proto`, `session_command.proto` — untouched.

`frontend/v1/feed.proto` now imports the four body files it actually
references (`agent`, `detached_work`, `tool_call`, `user`)
instead of `message.proto`, which it did not use. Every package except the
deliberately empty `agentrepl.v1` compiles.

**Why, in the user's terms.** "The message.proto that contains the message +
payload arm, but the arm implementations themselves are in dedicated files —
so all the tool-related stuff is in toolcall.proto, etc." Grouping by concern
at FILE granularity, not just section granularity: a concern is one file, one
review, one import for whoever needs only that concern.

**Consequences.**

- The intra-package import graph is now: `message` → every body file;
  `agent` → `content_blocks`, `tokens`, `tool_call` (an
  `AgentContentBlock` holds a `ToolCallBlock`); `context_cut` →
  `agent` (a compaction summary is `AgentContent`); `tool_call` and
  `user` → `content_blocks`; `detached_work` and `failure` → nothing
  yet (`detached_work` will import `tool_call` once `origin_tool_call_id`
  becomes a `ToolCallId`). Top-down order within stage 1 is therefore
  `message` → `tool_call` → `agent` → `user` →
  `content_blocks` → `context_cut` → `api` → `detached_work` → `tokens` →
  `session_command`, walked one file per increment.
- File name is `tool_call.proto` (snake_case, matching `session_command.proto`),
  not the `toolcall.proto` the user typed; the user may rename. The user DID
  rename the other two: `agent_response.proto` → `agent.proto` and
  `user_message.proto` → `user.proto`, and `failure.proto` → `api.proto` (the names
  above are the renamed ones). `api.proto`'s concern is the vendor API's OWN
  outcomes — the actor is the API, not agent/user/tool — which is why
  `FailureRaised` did not fold into `agent.proto`: the agent said nothing.
  Open for `tokens.proto`'s turn: `TokenUsage` is also an API fact, so
  whether `tokens.proto` folds into `api.proto`.
- A consumer that imported `content.proto` or `payloads.proto` for one type
  now imports the concern file that owns it — narrower, and the compiler says
  which.

### `content.proto` folds into `message.proto` too, and the file is regrouped by concern

**What changed.** `content.proto` is DELETED; its eleven messages moved
VERBATIM into `message.proto`, which now imports `tokens.proto` and
`google/protobuf/struct.proto` directly. `frontend/v1/feed.proto`'s import of
`content.proto` was removed (it already imports `message.proto`). The whole
file was then REORDERED into contiguous sections, each under a banner comment,
with every message's text and leading comment unchanged: THE RECORD
(`MessageId`, `MessageEntry`, `MessagePayload`) → TOOL CALLS (`ToolCallBlock`,
`ToolResultContent`, `ToolResultContentBlock`, `ToolReturned`, the four
`Permission*`) → THE AGENT'S RESPONSE (`AgentSaid`, `AgentContent`,
`AgentContentBlock`, `ThinkingBlock`, `StopReason` and its five arms,
`ContentArriving`) → THE USER'S MESSAGE (`UserSaid`, `UserContent`,
`UserContentBlock`) → BLOCKS COMMON TO EVERY AUTHOR (`TextBlock`,
`ImageBlock`, `UnsupportedBlock`) → CUTS AND FAILURES (`ContextCut` and its
arms, `FailureRaised`) → DETACHED WORK (all twenty-one). The two deleted file
headers ("neutral by design", "narrowed per site") and the floating "a tool
returning is NOT a block" comment were carried into the new file header and
the tool-call section banner respectively; nothing else was dropped.

**Why, in the user's terms.** A holistic view: "I can't know if
`ToolResultContent` is the right message to use because we don't have it
visible in this file and thus you haven't grouped them together for me to
see." Grouping by concern rather than by oneof-arm order is the convention
being added to the skill by the second one-shot workspace
(`proto-group-by-concern`); this is its first application. Sketching now walks
the file SECTION BY SECTION — tool calls, then the agent's response, then the
user, common blocks, cuts and failures, detached work — one concern per
increment.

**Consequences.**

- `conversation.v1` is now three files: `message.proto` (the record model
  whole, 730 lines), `tokens.proto`, `session_command.proto`. Whether
  `tokens.proto` also folds is not decided; it comes up in its own turn.
- The regrouping is a pure reorder — no message changed shape, `protoc`
  compiles the package — but it is a real diff and any binding regeneration
  will churn declaration order in generated code. Harmless; noted.
- The tool-call section sketched before the fold (`ToolCallId`,
  `ToolReturned` outcome arms, `PermissionAllowed` scope arms) is
  re-presented WITH `ToolCallBlock`, `ToolResultContent` and
  `ToolResultContentBlock` in view, which is what the user asked for.

### `conversation.v1/message.proto`: a typed `MessageId`, `MessageAuthor` deleted, and `payloads.proto` folded in

**What changed.**

- `MessageId { string value }` is new — the typed identity of a message.
  `MessageEntry.message_id` and `MessageEntry.parent_message_id` are now
  `MessageId`, and the `optional` on the parent is gone because message-typed
  presence is native and an empty identity is unrepresentable.
- `MessageAuthor`, `AuthorUser`, `AuthorAgent`, `AuthorDetachedAgent` and
  `MessageEntry.author` are DELETED. `MessageEntry` renumbers contiguously to
  `message_id = 1`, `parent_message_id = 2`, `payload = 3`.
- `payloads.proto` is DELETED and its 413 lines of arm bodies (`UserSaid`
  through `ContentArriving`) moved VERBATIM into `message.proto` below
  `MessagePayload`. `message.proto` now imports `content.proto` and
  `tokens.proto` directly. `frontend/v1/feed.proto`'s import of
  `payloads.proto` was repointed at `message.proto` — the one edit outside the
  file, mechanical, so the fold does not leave a dangling import.
- The `MessagePayload` ARM SET (13 arms) is confirmed unchanged at this level:
  names and purposes only; the bodies' shapes are the next increment.

**Why, in the user's terms.**

- `MessageId`: the superseded record already named a bare `string message_id`
  on another surface an identity re-spelling — "can be assigned any string at
  all". A re-spelling can only be remedied by importing the owner's type, and
  the owner had no type. Now it does; every surface embeds it.
- `MessageAuthor`: the field carried the user's own `FIXME: is this actually
  useful?`, and it is not. Author is fully derivable from payload arm plus
  parent chain (an `AgentSaid` under a detached-agent container is the
  subagent; at top level it is the agent; `ToolReturned` always updates the
  agent's response). Two spellings of one fact, exactly what dropping
  `top_level_message_id` removed before. Answer 5 of five: already
  represented. `AuthorDetachedAgent.detached_work_message_id` was the parent
  chain restated.
- The fold: the user asked for `MessagePayload` "in the same file"; on
  clarification, that meant the whole payload model — record, what it can say,
  and what each thing it says looks like — reads as ONE file. The
  `MessagePayload` extraction itself survives (a oneof is not a type;
  extraction is what lets another surface embed the payload set with its own
  stamps).

**Consequences.**

- Every consumer holding a message id as `string` — daemon, webapp, elisp,
  shim, sidecar, store, and every other proto surface (`shim.v1`, `store.v1`,
  `frontend.v1`, `state.v1`) — now has a typed field to embed and a bare
  scalar to retire. Those surfaces are walked later in this sequence; each
  will meet `MessageId` as an existing type. The `tool_call_id` and
  `origin_tool_call_id` scalars are NOT touched here: they are vendor-minted
  correlation values, and whether they get the same treatment is a
  `payloads`-shape question next.
- The old `ToolReturned` arm comment argued from `author` ("author stays the
  AGENT on this record"); with the field gone the comment was rewritten to
  state the same fact without it. That is the one comment edited outside the
  agreed sketch, and it is recorded here because it is.
- `content.proto:123` still says "It is `ToolReturned` in payloads.proto";
  the file no longer exists. Left for `content.proto`'s own turn.
- The store's parent-chain walk at ingest and the daemon's lineage audit read
  a `string`; both now read a `MessageId.value`. Implementation-wave work,
  not contract.

**Verified.** `protoc` compiles `conversation/v1/*.proto` after the fold.
Nothing outside `conversation.v1` referenced `MessageAuthor` or its arms.

### `agentrepl.v1` starts from a clean slate: every RPC and every `endpoint_*.proto` is deleted

**What changed.** All 34 `endpoint_*.proto` files under `src/agentrepl/v1/`
are deleted, and `service AgentRepl` is emptied to `{}`. `service.proto`
survives as the file the RPCs will be re-added to, one at a time, each with
its own `endpoint_<snake_case_method>.proto`. `shared.proto` is NOT deleted:
it is the only `agentrepl.v1` file anything outside the package imports
(`frontend/v1/footer.proto` reads it for `MergeStatus`, `MergeDequeueOffer`,
`HibernationDetail`), and its fate is decided at the `frontend.v1` footer step
and the `agentrepl.v1` conventions step, not by this deletion.

**Why, in the user's terms.** We are going to end up nuking a lot of
`agentrepl.v1` RPCs and their `endpoint_*` files anyway; what is there now is
so far removed from what we want to land that it is better to start from
scratch than to confuse ourselves with preexisting junk.

**Consequences, stated so they are not silently lost.**

- The old `service.proto` header carried normative prose that is NOT
  automatically carried forward: the "no paint attestation on this service"
  invariant, the `request_id`/`workspace`/`client_id` envelope-field
  semantics, and the protojson-on-the-wire note. Each re-enters at the
  `agentrepl.v1` conventions sub-stage as its own question. None is settled
  by having once been written.
- The 34 deleted files were the ONLY spelling of the per-method error arms
  (`Refusal*` messages derived from daemon handlers, per the superseded
  record's "Per-method errors, DERIVED not invented"). That derivation
  evidence — which handler emits which refusal — is in the superseded
  record's prose and in git history (`cfc849d60^`), not on disk. When an
  endpoint is re-added, its error arms are re-derived, and the old file is
  reference material, not a template.
- The build was already broken by the transport reversal; this widens the
  break to every daemon, webapp and elisp site that named a request or
  response type. That is intended, per the land-whether-or-not-it-breaks rule.
- Ten deleted endpoint files carried the comment "see
  DESIGN-protobuf-surfaces.md for why a directory is not available here" (the
  `--go_opt=paths=source_relative` argument for the `endpoint_` prefix). The
  argument still holds and lives in the superseded record; re-added files
  cite the new record.
