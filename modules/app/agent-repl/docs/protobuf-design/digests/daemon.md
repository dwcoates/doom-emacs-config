# DAEMON DIGEST

DERIVED from figma-to-idl-redesign.md at 86fd2b543 — the canonical record WINS on any conflict. Do not edit; regenerate.

Scope: every decision, principle, consequence and gotcha in the record bearing on the daemon
— session orchestration, WSM/state, prompt queue and holds, merge orchestration, view
resolvers and publishers, `agentrepl.v1` serving, `shim.v1` consumption, accounting, failure
classification, push cadence and clock conventions, and the identity rulings it enforces.

## 1. STANDING PRINCIPLES THE DAEMON LIVES UNDER

### The daemon is the ONE component allowed to hold state
The shim, store and sidecar hold NO variable-size state: every observation they make must
cost a constant number of single indexed lookups (one parent-id lookup is static; a lineage
walk is not). The daemon is explicitly NOT bound by this — it is where stacks, queues,
trees, accumulators and set membership legitimately live. Consequence: work the middle
layers refuse (accumulating prose fragments, holding the live background set, coalescing
frames, holding walk positions, tracking task→turn provenance) lands on the daemon.

### `update` frames carry DELTAS; the daemon owns the accumulator
The shim forwards only what CHANGED and accumulates nothing. `AgentResponseUpdate` carries
new markdown, `AgentThinkingUpdate` a text delta; the whole settled text rides the terminal
arms only. How deltas are handled — coalesce or forward as they arrive — is the daemon's
choice. The daemon's feed resolver owns the prose buffer per in-flight unit; the shim's
response accumulator is deleted. No `from_offset` gap detector on prose: a lost fragment is
evident in the response and the terminal frame carries the whole text, so the settled bubble
self-corrects. Bash keeps its offset (its spool has no settled whole to recover from).

### Detachment is read from EDGES; the LEVEL is relayed, never diffed
`task_started` / `task_notification` are edge bookends; `background_tasks_changed` is the
full set with REPLACE semantics and must not be correlated with the edge stream. The shim
takes entered/left from the edges and relays the level verbatim as a session fact; the
daemon MAY hold the set. The no-edge-pairing warning binds the daemon's INDICATOR only.
`KillSession`'s "every live task" is the vendor's own `backgroundTasks()` answer.

### Corpus absence is not deletion evidence
Absence from the personal transcript corpus proves NON-USE, never NON-SUPPORT. Any
deletion or no-producer verdict requires documentation-grade evidence (SDK doc comments,
official docs, release notes, research); corpus absence is supporting color only.

### conversation.v1 carries vendor fields even when NO UI maps them
`conversation.v1` is a fidelity layer: a vendor field with no drawn consumer still lands,
marked EXPECTED UNMAPPED. UI-relevance gates `frontend.v1` only. So the daemon routinely
receives facts nothing draws; it must not treat their presence as an obligation to render.
No relay of vendor identity spaces, and no JSON-in-a-string — unmapped fields stay typed.

### figma→idl takes precedence over no-respell at the backend→frontend boundary
`frontend.v1` is composed only of element messages shaped for the drawn element. Where an
element's props derive from an internal type, THE DAEMON RESOLVES a frontend-shaped
message: a deliberate re-spelling into UI vocabulary. Safe only because the daemon is the
single resolver and the copy is a resolved VALUE re-published on every change. Not licensed
inside `conversation.v1` / `shim.v1` / `store.v1`; typed identities are still imported as
join keys, never respelled.

### The store is NUKED, never migrated
No backfill, hydration, dual-read or schema migration anywhere. A durable-compatibility
argument is never a reason to keep a shape. Where existing contents are in the way, the
store is dropped and recreated.

### The EXEMPT SET
A third category beside modeled and unmodeled: known vendor built-ins the contract
deliberately does not carry. Exempt calls are DROPPED at the shim — never emitted as
`AgentUnmodeled`, never tripping the topbar's unmodeled warning. Members: TaskStop,
TaskOutput, TaskGet, TaskList, ToolSearch, NotebookEdit, the background-shell peek,
the undocumented `mode` disk line, `skip_transcript`-marked ambient tasks, REPL,
ListMcpResources / ReadMcpResource / RefreshMcpTools, SendFeedback, ClaudeDesign,
Projects, ShowOnboardingRolePicker, ProposeSkills. Consequence the user accepted: ambient
work is invisible on our surfaces; the vendor's level set still governs liveness shim-side,
so no indicator wedges.

## 2. IDENTITY RULINGS THE DAEMON ENFORCES

### The four identifier spaces, not interchangeable
- `agent_id` (vendor's) — WHICH AGENT INSTANCE. Its own space; depth beyond 1 cannot be
  resolved from call ids, which is why `parent_agent_id` exists.
- `tool_use_id` (vendor's) — WHICH TOOL CALL. For a Task call it identifies THE SPAWN, not
  the agent spawned. An agent is not its spawning call.
- `activity_id` (ours, `AgentActivityId`) — WHICH UNIT OF WORK. Shim-minted, one per unit
  for the unit's whole life, sourced from `tool_use_id` where one exists and from message id
  + block index for text and reasoning. It names WORK, never an agent.
- `TurnId` (ours) — WHICH TURN. DAEMON-MINTED at submission, unrelated to the other three.

### Vendor uuids never cross the contract
The shim translates where a unit exists; vendor `uuid` / `message.id` / `promptId` /
`requestId` stay shim-side. Permanently uncarried remainder (which messages a compaction
preserved, ancestry across a compaction, vendor request/message ids on failure evidence) is
logged in the deferred metadocument.

### The prompt's id is the DAEMON's
The daemon mints `TurnId` at submission and returns it so the client draws the row at once;
the shim ADOPTS it on delivery and writes the record under it; history returns it. Under the
parity principle the daemon mints one per delivered prompt to ANY agent. Client-minted
prompt ids are gone; a client-minted `idempotency_key` on SubmitPrompt is a separate thing.

### The main agent has an identity, and nothing is nil
`main_agent_id` is SHIM-minted on first fresh start, store-persisted, reported unchanged on
every later start — deliberately DECOUPLED from the vendor session id so a rotation
mid-page cannot split an agent's history. `SessionIdentityRotated` changes only the vendor
handle the shim resumes by. Later superseded at the agent consolidation: `SessionStarted`
LOSES `main_agent_id`; "main agent" survives only inside the shim and as the store's scope,
and no consumer ever sees it.

### `FeedId` — one opaque daemon-minted string
The daemon ENCODES the identity of what a row DRAWS (a unit, a task id, an agent id, an ask
id, or a daemon fact for a synthesized row) and DECODES it on echo. Mint and resolve are
encode/decode — NO id table, stable across pushes and daemon restarts. Typed identity spaces
are FULLY HIDDEN from the frontend; the one typed survivor on the feed is the `TurnId` stamp
for own-prompt matching. UPSERTS ARE UNIVERSAL: every row replaces whole by id under the rule
"one row per drawn subject" (a response row per unit; ONE task bubble fed by many acts; a
subagent bubble keyed by the agent, not its spawn call).

### `WorkspaceRef` / `RepositoryRef` — daemon-minted echo tokens
A path is never an identity. `RegisterWorkspace` PROVIDES the path (any spelling) and the
daemon MINTS the opaque id, returned on success. Both refs carry `{id, dir}` where `dir` is
a normalized directory NOT to be used as an identifier. The daemon owns normalization. They
live in the `workspace.v1` leaf package so `frontend.v1` can import identity without
importing `agentrepl.v1`.

### The typed echo token pattern
The provider mints a wrapper message, embeds it in what it serves, and the request field is
the SAME type echoed back unchanged — a client that invents a value is typed as wrong.
Applied to `AgentModel`, `WorkspaceRef`, `FeedId`, `DetachedWorkId`, `HistoryPointer`,
`FeedWatchToken`, the store's `AgentSessionToken`, question texts and option labels.

### `DetachedWorkId` is the uniform CONNECTION token
Retracting an earlier verdict: it STAYS. It exists so the daemon→shim connection is created
identically regardless of the detached work's kind — just check the id. The producer may map
to it differently per underlying message; the CONSUMER never does. Watch by
`DetachedWorkId` (a stream is one RUN); update a subagent by `AgentId` (a prompt may target
an agent whose run has finished).

## 3. SESSION ORCHESTRATION — SPAWN, ATTACH, END ARE THREE DECOUPLED ACTS

Sessions and turns OUTLIVE the daemon, so the daemon records session/turn identifiers in the
WSM and on restart simply runs the corresponding Watch.

- SPAWN is unary and returns (or adopts) an identity the daemon persists: `StartSession`,
  `StartTurn`. A Watch never creates anything.
- ATTACH is a Watch stream that creates nothing and ENDS NOTHING. A consumer closing it —
  gracefully (which a shutting-down daemon must do) or abruptly — leaves the work running
  and information accumulating. No `CloseXConnection` rpcs exist: cancelling the stream IS
  the graceful close in Connect.
- END is a Kill that refuses while work is live unless forced, and NAMES what it killed:
  `KillSession` (the turn if open and EVERY live task, whichever turn spawned it) and
  `KillTurn` (the main agent and what THIS turn spawned, transitively, nothing else).
  Narrow stops (`AgentInput.stop`, `StopBash`) remain single-target interrupts.

**The bounded-stream rule binds the PRODUCER side only.** A stream the producer ends without
a terminal frame is a transport failure; a consumer closing a Watch stream is a normal act.
Every frame of a bounded stream is a one-level `oneof result { update | success | failure }`
— a conclusion is a MESSAGE, never the stream merely stopping.

**`StartSession`.** Request source is `fresh | resume`; model and mode ride the `fresh` arm;
resume carries the vendor session id and an optional `SessionColdRemediation`. It RESOLVES
WHEN A PROMPT CAN BE ACCEPTED, returning only handshake-fixed facts; anything later is
`WatchSession`'s. A COLD CONTEXT IS REFUSED, NEVER SILENTLY PAID: a bare resume of a lapsed
session fails carrying `SessionCold` (context tokens, last-request instant, lapsed TTL,
requested model) and the daemon reopens naming a remediation (`pay | clear | compact{model,
scope}`). Verified: `query({resume})` makes no API call and emits nothing until the first
prompt, so refusal costs nothing. KEEP-ALIVES BEGIN BEFORE SUCCESS IS RETURNED — a `pay`
resume takes its cold read at open, in the background.

**Resume recovers model and mode.** Every `user` JSONL record carries `permissionMode` and
every `assistant` record carries `model`; the SDK does NOT restore either, so the shim reads
the last of each back and passes them, and the answer reports what was recovered.

**Compaction is OURS**: daemon-directed, shim-implemented. The request names the model that
writes the summary and the SCOPE. The scope is ONE canonical enum `SessionCompactScope
{ ALL | PROMPTS | RESPONSES }` declared at the conversation level and imported everywhere
(the menu's offered radios and the verb) — the four-arm oneof and PROMPTS_AND_RESPONSES are
dead. A compaction is a cold read PLUS output, never the cheap path.

**`SetSessionModel`** resolves AFTER the current turn ends (a turn is answered by one model
throughout; with no turn open it resolves at once) — a deliberate departure from the SDK's
mid-turn `setModel`. It is REFUSED IMMEDIATELY, before any waiting, when context exceeds a
threshold THE REQUEST NAMES, because a switch re-reads everything at full price. The
threshold is DAEMON POLICY stated per call, not a shim constant; the shim only measures.
`SessionCold.reason` is `lapsed{cache_ttl_ms} | model_switch`; a model switch fires only when
the requested model differs from the transcript's last.

**`WatchSession`** is a standing stream of `SessionUpdate`: identity_rotated, query_died,
model_changed, permission_mode_changed, fast_mode, mcp_server, account_usage,
context_budget_warning, and the rest. `query_died` is DUPLICATED on purpose — each open
stream also concludes with its own failure, but a consumer with no stream open still needs
the session-level fact. There is NO third category of fact: everything is about a TURN (its
stream says it) or about the SESSION (this stream says it). The old `BookkeepingEntry` is
dead arm by arm.

**Diagnostics are PULLED, not pushed.** `GetSessionDiagnostics` returns
`{ healthy | unhealthy{faults} ; degraded windows }`, unhealthy being an ANSWER inside
success. A degraded window carries component, reason, `began_at_ms` and an extent oneof
`open | closed{ended_at_ms, dropped_count}` — the closed arm owns the count. Windows are
kept since the shim started, so a daemon asking after the fact still learns what was dropped.
The daemon pulls at its own cadence; a healthy pull retracts the topbar warning on the next
push.

**`GetLiveWork` (store)** — the OPEN-OBLIGATIONS verb. "Live" is a claim about the RECORD
("a start was written and no terminal ever was"), so it cannot go stale. The SHIM (never the
sidecar) calls it once at session start and resolves every item: re-adopt what the revived
vendor process actually has, WRITE the closing terminal for what did not survive. Invariant:
every started thing eventually gets a terminal row, by observation or by reconciliation.

## 4. THE AGENT SURFACE THE DAEMON CONSUMES (shim.v1)

### A subagent is handled EXACTLY as the turn is
Core principle: subagents are internally handled exactly the same as a turn, from the
daemon's perspective and on the daemon↔shim API. Motivating UX: click a subagent and the
webapp renders THAT subagent as first-class — its feed, composer and held prompts — with
prompts routing to it and queued prompts presented exactly as for the main agent.

- One write type (`AgentInput { stop | answer | prompt }`), carried identically; requests
  differ ONLY in the address.
- Prompt QUEUING is DAEMON-SIDE and KIND-AGNOSTIC: a prompt to a busy subagent is held,
  classified and delivered (interrupting if that is what delivery takes) by the same
  machinery as for the turn. "Send a message to a subagent" is the daemon deciding when to
  deliver; the vendor's steer-at-next-tool-round is the shim's mechanism underneath.
- One read type: `AgentFrame` is the frame of the turn's stream and of a subagent's stream.
- Workflow and bash follow in principle and are moot in practice.
- Nothing about bash or workflow becomes agent-shaped.

### The agent consolidation (final shape)
- `StartTurn` is the MAIN AGENT's verb and returns the FIRST PAGE: request { turn; said;
  origin; page_size; optional known_through }, success { `AgentPrompt` prompt; `HistoryPage`
  page }. One call paints and submits; `prompt.agent` is the WatchAgent address.
- `WatchAgent { optional target; page_size; optional known_through }` — a standing stream
  whose FIRST frame is the opening page (full repaint, or only entries newer than the
  caller's own mark) and whose tail is one pointered entry per write.
- `UpdateAgent { optional target; AgentInput }` — stop and answer; `AgentInput.prompt`
  RESTORED for a prompt to an EXISTING agent (a subagent, a workflow's agent), with delivery
  per kind the shim's.
- `ReadHistory` carries `oneof position { first | after(HistoryPointer) }`. A subagent's
  cold paint is: ReadHistory(first) → WatchAgent with the page's newest pointer as
  `known_through`, pinning the tail to exactly after the page.
- DELETED: `WatchTurn`, `UpdateTurn`, `WatchSubagent`, `UpdateSubagent`.
- The one-turn-in-flight rule becomes PER-AGENT, stated at `StartTurn`.
- A subagent's FIRST prompt needs no verb: `AgentSubagentStart.prompt` rides every frame of
  the spawn.

### `AgentFrame` and its routing
`AgentFrame.result` is `{ update | success | failure | detached_work }` and the frame's
oneof IS the datalayer route: `update` → an entry page line; `success`/`failure` → BOTH the
entry table (the stop notice has no other source) and the agent row's terminal columns, one
transaction; `detached_work` → the lifecycle table for its kind, NEVER a page line (the
spawning call is already one). `AgentUpdate` is purely conversation content
`{ activity | question | permission }`, and its arm states the consumer's OBLIGATION:
activity is READ-ONLY; detached work means OPEN A STREAM; a question and a permission mean
the agent is BLOCKED and the user must WRITE BACK.

### Announcement rides the spawning stream; there is no roster stream
The turn's stream announces what the turn spawns as it happens; an item's stream announces
what IT spawns. Provenance is implicit in which stream announced an item. Every stream that
carries a unit OPENS WITH that unit's `start`, and a re-announcement repeats the ORIGINAL
start instant so a drawn clock does not reset when work moves between streams — required
because THE DAEMON MUST SURVIVE ITS OWN RESTART.

### `start` / `update` rule
`start` means "this stream now carries this unit", not "the work began". `update` reports
GROWTH and is EARNED — bash (detach-only; foreground shell output is observable nowhere),
thinking and response. `AgentToolCallProgress { last_progress_at_ms }` is a THIRD arm kind
— a beat, not growth — on read, write, edit, grep, glob, bash, skill_use, send_message and
unmodeled. The shim owns the WEDGE RULING (a timeout settles the unit's failure); the daemon
relays the latest beat into `FeedToolCallRunning.last_progress` by re-pushing the row, and
the client ticks "quiet for N s" locally. The in-between state ("beats stopped, shim has not
ruled") is deliberately unrepresented.

### Detached work
One stream per in-flight detached item, so "zero item streams" IS "no detached work in
flight" — liveness is structural. Cancelling an item is a CALL, never a client-side close.
`DetachForeground` names the turn unit by `AgentActivityId`. A prompt to a finished subagent
starts a NEW run, announced on the prompting stream and followed by a new watch.

### Workflows
`AgentWorkflowUpdate` is `{ repeated AgentWorkflowSubagent all_subagents }` — REPLACE
semantics, the whole list every frame, each entry a spawn plus a `live | ended` oneof. The
old multiplexed `agent_frame` arm is retired: the shim/sidecar/store "should be very stupid"
and merely say an agent EXISTS; THE DAEMON creates the necessary connections as it sees fit,
because a workflow agent is exactly a subagent from the daemon's perspective. The level is
stateless (workflows are small and have no history/pagination). `GetWorkflow` answers with
the start plus `live{token, level} | ended{terminal}`; the token lives INSIDE the live arm.
`StopWorkflow` is the run's only addressable act. FRONTEND SUPPORT FOR WORKFLOWS IS KICKED
DOWN THE ROAD ENTIRELY: no footer support, no `frontend.v1` support, no daemon handling —
a later feature. (An earlier stage-2 direction — the daemon resolving a workflow to all its
top-level agents — is superseded by that deferral.)

### Facts the daemon must know about the shim boundary
- A stream ending without a terminal frame IS a transport failure, recorded by the daemon.
- Keep-alives are ENTIRELY INSIDE THE SHIM: the daemon never submits one, nothing
  keep-alive-shaped is on the wire, and the shim's YIELD obligation (discarding trailing
  keep-alive turns before a real prompt) is the guarantor of "one submitter". Keep-alives are
  invisible on the control plane and visible on the record plane — their cost lands in
  accounting regardless.
- Heartbeats do not exist as messages: transport liveness is HTTP/2's, work liveness is the
  stream being open, and the live-work set is the set of open streams the daemon holds.

## 5. THE PROMPT QUEUE AND HOLDS — THE DAEMON IS THE ONLY QUEUE

The agent binary's own queue is never ours. The daemon holds every turn stream, so it knows
in-flight structurally, and it already holds waiting prompts in the daemon-hold tray. The
daemon submits ONLY when no turn is in flight; an open stream means a RUNNING turn; a second
`StartTurn` while one is open is a HARD FAULT, refused, never queued. Consequences:
`CancelQueuedPrompt` never exists; the vendor's `cancel_async_message` verb is DROPPED; the
interrupt answer's `still_queued`/`cancelled` lists are dropped as exempt (the vendor's queue
functionally never holds anything of ours) and add no `SessionFault` kind; the tray is THE
one authority for waiting prompts.

**The tray.** `DaemonHoldTray { workspace; heading; repeated DaemonHoldItem }` over
`HeldPrompt { TurnId; UserSaid said; queued_at; classification (5 arms); hold (4 arms) }`
and `HeldOffer { merge_dequeue }`. A held prompt IS a `UserSaid` not yet forwarded — one
canonical form client → daemon → tray → shim → record.

**The four real holds** are shutdown drain, keep-alive turn, revival pending (renamed
`session_starting` — "the session is still coming up", covering a cold resume or a chosen
compaction landing, with no classifier, no force, and a loud drop), and build refresh.
Gates and refusals are NOT holds: `ErrSessionHibernated` was a revival GATE holding nothing;
a merge-state prompt refusal holds nothing; an uninterruptible context cut is a
CLASSIFICATION verdict. There is no shared `DaemonHold` type; the hold oneof stays on the
entry.

**`UpdateHeldPrompt { WorkspaceRef; TurnId; release | drop }`** closes the echo-token loop:
minted at submission, served inside the tray entry, handed back unchanged. `release` =
deliver NOW (interrupting the running turn when that is what delivery takes); `drop` =
discard (the composer takes the text back); doing nothing is the normal path — accept is the
default and needs no verb. `AnswerHeldOffer` is addressed by OFFER KIND (a workspace has at
most one offer of a kind standing).

**Held prompts BLOCK a close.** A held prompt is undelivered user intent and a close may
never silently discard it. Closable = no turn in flight, no live async work, no held prompts.
A standing cold gate, tracker contents and a parked (shim-less) session remain NON-blocking.

**Merge-in-flight is an ERROR, never a hold.** A prompt arriving AFTER a merge began is
REFUSED outright — once the workspace merges it closes, so post-merge-start work would be
orphaned. Prompts already held when the merge began stay held (the dequeue offer resolves
their fate). This lands `SubmitPromptError`'s first derived arm (`merging`, empty; the footer
and merge bubble already show which merge) and gives the occupancy-lease projection a
PER-HOLDER REFUSAL POLICY: the merge lease projects to error-on-new-submission;
restart-pending and shutdown-drain project to holds.

**Interrupt acknowledgement.** When an interrupt registers and teardown begins, the footer
flips to `waiting · interrupting` IMMEDIATELY — before the turn's real end — with a composed
activity line, and the interrupting prompt is placed at the QUEUE'S SEMANTIC HEAD so it, not
the pre-interrupt head, is the next delivery.

**Idempotency.** A client-minted `idempotency_key` rides `SubmitPrompt` ONLY — the one verb
where a duplicate is costly and undetectable. The daemon refuses duplicates by key; this
replaces the old request-id refusal.

## 6. WORKSPACE VERBS AND LIFECYCLE

**The verb triad.** Emacs commands are THIN WRAPPERS — send the request, await the daemon's
ack, tear the tab down; every piece of real machinery is the daemon's.
- `CloseWorkspace` — the USER's close, a VIEW act: fast ack, tab gone, the daemon↔shim
  session UNTOUCHED (keepalives continue; the workspace is merely unviewed). REQUIRES QUIET:
  a busy workspace refuses, and the refusal manifests in the FOOTER — status `closing`,
  sub-status `close blocked`, and the daemon's composed plain-English reasons as the activity
  line. The response carries only the `blocked` cause arm; the footer owns the reasons.
- `KillWorkspace` — the big red button: forced session death (connections AND the shim
  itself). Never blocks, never warns, checks nothing. Worktree and branch survive.
- `NukeWorkspace` — DATA DESTRUCTION: kill first if live, then delete worktree and branch.

Killing a turn/session in flight KILLS IT — never an error, because kill is the stated
intent. Close targets the CONTAINER and the collision with live work is incidental, which is
why its refusal deserves a designed surface.

**`CreateWorkspace`.** THE DAEMON DOES THE CREATING and the NAMING: it derives the slug
(from the initial prompt when present) → branch → worktree dir, runs the git itself, and
registers the workspace. NO host materialization round-trip — Emacs sees the workspace on the
roster and opens its buffer. Request: `RepositoryRef` + optional `UserSaid initial_prompt` +
optional `base_ref`.

**`RestartWorkspace`** bounces ONLY the workspace's shim (rebuild if out of date + restart).
`force=false` is GRACEFUL: wait until no turn is in flight and no async/background tasks run,
holding incoming prompts via the daemon hold meanwhile. `force=true` interrupts and bounces
immediately and does NOT resume the agent afterwards. Never the webapp, never the daemon.

**`RegisterWorkspace`** is IDEMPOTENT BY DIR — re-registration after reconnect or daemon
restart is the normal path. `SelectWorkspace` is idempotent too; the daemon stamps `current`
and `last_viewed_at_ms` on it, and the roster stream carries the new `current`.

**HIBERNATION LEAVES THE CONTRACT ENTIRELY.** It is an implementation detail of the daemon
to save memory: NO frontend knowledge, NO daemon API surface. `HibernateWorkspace` and
`ReviveWorkspace` are both DELETED — hibernation is an INACTIVITY policy, and revival is
IMPLICIT (a prompt to a parked workspace revives it under the hood). The machinery
COLLAPSES to an idle-cutoff sweep (shim stopped past cutoff; at most one registry bit) plus
the ORDINARY resume path; the leases, revival modes/holds/gates and hibernation states move
from KEEP to DELETE. Committed consequence: the frontend cannot distinguish a parked
workspace from an idle one — a parked session presents as `live` with `shim_attached=false`,
the footer shows `idle`, the roster shows the ordinary dot. The distinction surfaces ONLY as
the cold gate, when it has a cost.

**The host surface.** Emacs connects, `RegisterWorkspace(dir)` per known worktree, then one
`WatchHostWorkspace(ref)` subscription per OPEN workspace (snapshot first, whole-replace on
change); closes cancel; a daemon restart drops streams and Emacs re-registers and
re-subscribes. THE DAEMON NEVER CALLS EMACS. `ReportHostAction` NEVER EXISTS — the daemon
gives Emacs no orders; Emacs watches streams and REACTS, and the daemon never waits on
Emacs's buffers. A global host stream was rejected: workspace-dependent and
workspace-independent channels must be distinct types.

`HostWorkspace` = `{ session: none | existing ; naming }`; `HostSessionExisting` hoists
`HostSessionId` over `standing { live | terminal{rehydratable} }`. `HostSessionLive` carries
generation, `shim_attached`, vendor info, backfill state, a composer oneof
`{ open | merging | draining | restarting }` and generation-scoped faults — the last two
RELOCATED INTO the live arm (a fault window dies with its generation). COMPOSER GATING IS A
RESOLVED ELEMENT, never raw bools Emacs maps. `WatchHostWorkspaceResponse` is a push oneof:
the whole-state `host` arm plus a `notification` EVENT arm.

## 7. MERGE ORCHESTRATION

A merge is DAEMON-ORCHESTRATED and can never be "detached": the agent is not orchestrating
it, the daemon is, exclusively — so `DetachedMerge` was dropped from `conversation.v1` as an
abstraction leak. The daemon SYNTHESIZES the merge into the feed, coalescing every vendor
record produced while the merge ran into one bubble, and the daemon — not any producer —
determines which records belong to it.

**MERGE IS TURN ACTIVITY.** "From the user's perspective the turn has not concluded while a
Merge is in flight" — which is why the activity container is `FeedTurnActivity`, merge being
the one arm the agent does not author.

**The merge bubble** is a tabbed phase bubble: head (branch line + clock + live/settled)
over a TAB STRIP of phases. Phases are APPEND-ONLY and MONOTONIC (live once, settled once);
a repeat pass is a SECOND TAB and the tab label carries the round ("conflicts (2)"), resolved
by the daemon. A tab exists only for a phase the run ACTUALLY ENTERED — there is no pending
tab. Nested rows parent to the PHASE, not the merge, so "which pass produced this fix" is
structural; the `landing` phase owns the merge's own narration.

**The queue is just another phase** — typically the first tab. Its content is a SNAPSHOT
replaced whole on every publish (like a shell spool); the daemon republishes on any queue
change AND ON ANY PHASE CHANGE AT THE FRONT, so a waiting user sees the front's progress.
The queue tab settles once we reach the front and the phase tabs take over. Structure is
`ahead / current / behind` so "you are here" is never derived by comparing ids; the head
workspace's status is `merging` and it is SHOWN NOTHING about the queue. The mirrored
front-workspace phase imports the real phase messages — one vocabulary for tab badge and
queue row.

**`MergeWorkspace`** success = ENQUEUED; the merge's life from there is the feed's bubble; it
targets the workspace's parent by definition and takes no parameters. **`UpdateMergeQueue`**
is purely inbound: `pause | resume | evict{WorkspaceRef}`. The queue's visible state rides
the merge bubbles' queue tabs and the roster's status arms.

**Merge phase HISTORY stays out of the WSM** — the merge bubble's rows are conversation
content the daemon synthesizes into the feed.

## 8. THE WSM (WORKSPACE STATE MANAGER)

The SSM is RENAMED WSM. Its purpose is WORKSPACE state — "what workspaces are open
currently, what workspaces are merging and what the merge queue status is" — tracked at a
much higher level than the store, which owns agent-response information.

**`state.v1` IS DELETED.** The daemon's durable state needs no wire shape: one producer and
one consumer (the daemon itself), so it is internal DDL under the same columns-vs-blob rule
as the store. `TokenUtilization` and `TurnAccounting` blob rows lose their producer — usage
rides the store's frames and aggregates are derived on read.

**The approved WSM schema (architecture guidance, not proto).** Seven tables: `workspace`
(lifecycle arm + since, current merge phase, last activity), `merge_queue` (repo-ordered
positions), `merge_lease` (one open window per repo), `workspace_merged` (set-once),
`held_prompt` (TurnId PK; `UserSaid` as the one content BLOB; classification and hold reason
as columns), `session_binding` (workspace → mutable vendor_session_id + last known
model/mode for pre-attach display), `shutdown_schedule`.

**What dies with today's state.db, each with its replacement**: turn ledger → open
WatchAgent streams + store entries; prompt receipts → StartTurn; keep-alive windows →
shim-internal; reader positions and compaction gate → client-held pointers and the shim's
store reads; connectivity/fault ledgers → stream lifetimes + pulled diagnostics; token
evidence → store frames; failure cards → frames and resolved views.

**statedb stays an IN-PROCESS library** (one SQLite file, one writer). A service split was
weighed and declined: the store earned its process boundary by having two producer processes;
daemon state has one, and the single file's value is cross-table atomicity.

**The frozen-replay premise is NOT void** — it is an OPERATIONAL prescription, merely not
relevant during development; the constraints return once the schema ships.

**No `seq` on the daemon-facing wire.** The daemon's only position-shaped need is CATCH-UP
AFTER ITS OWN DOWNTIME, which is ordered pagination, not seq. The page identifier is an
OPAQUE token the daemon PERSISTS (deliberately different from `GetFeedPage`, where the daemon
holds each container's walk position because a webview's walk dies with the webview).
Advancing a cursor past a position the store never assigned becomes UNREPRESENTABLE rather
than guarded.

## 9. WHAT THE DAEMON MUST KNOW ABOUT THE STORE

- The store writes `conversation.v1`, resolved at write time with a constant number of
  lookups; rows are conversation.v1 messages and no record-granular vendor vocabulary is
  persisted.
- Four tables, each the ONE canonical home of one fact: `agent` (one row per `AgentId`, main
  included), `workflow` (the subagent LEVEL IS NEVER STORED — it is the join), `entry` (a
  queryable spine around a serialized frame the store never opens), `detached_work`. Columns
  are the queryable and joinable stuff; `entry`'s frame stays serialized.
- Lineage is a DENORMALIZED ROOT: every row is stamped at insert with its top-level ancestor
  (one lookup of the parent's stored root), so "everything spawned under X" is one indexed
  query and no insert ever walks more than one step. `top_level` is the nearest NON-SYNC
  ancestor (main agent or detached-work agent, never a sync subagent) — equivalently, which
  live stream carried the work. OPEN: whether detachable spawn rows also carry a TURN stamp
  so `KillTurn`'s transitive refusal list is one query, or the daemon resolves that from its
  own state (the vendor names a task's owning AGENT but never its owning TURN).
- PAGINATION HAS TWO KEYS: the AGENT (lineage; THE pagination key) and the PARENT ITEM (the
  response an item is a block of, keyed by its anchor unit). A page is the N most recent
  parent-less rows of X, then every row whose parent item is one of those — two indexed
  queries, no walk, reading as "N responses, each with all its blocks". `top_level` is never
  used for paging. Containers (subagent, workflow run) are doors: their contents are rows
  under the CHILD's agent id.
- A PAGE IS "IMMEDIATE CHILDREN OF X". Nothing has a tool call as parent; a unit's later
  frames are upserts, not children. Workflows aren't loaded by page — the SUBAGENTS WITHIN
  are; `ReadHistory` stays addressed by `AgentId`.
- Open/watch bifurcation: the open is a bounded unary answer carrying the first page and a
  store-minted watch token; the watch is a pure tail pinned to begin exactly after the page.
  `known_through` is the caller's own high-water mark: UNSET = repaint, SET = catch-up. The
  store deliberately tracks NOTHING about what it previously served.
- `WriteBatch` success means DURABLE (records + cursor advance, one transaction); failure
  means NOTHING committed, so the producer's spill holds and replays. Replay absorption is by
  `write_id`.
- Terminal arms ARE entries: `AgentSuccess` / `AgentFailure` appear in history and are the
  only record of how a turn ended.

## 10. VIEW RESOLVERS AND PUBLISHERS

### The push-cadence convention (standing rule for EVERY frontend.v1 view)
EVENT-DRIVEN, WHOLE-VIEW, NO TICKS: push whole on any resolved change, push nothing on no
change, client-side ticking from shipped instants, bursts coalescible because the wire
carries STATES, not events. No keepalive frames anywhere — the keepalive convention is
RETRACTED, replaced by a layering: source-of-truth silence is observed by the DAEMON
(degraded windows + footer status); pipe death is the connection failing; a wedged publisher
behind a healthy connection is a DAEMON-INTERNAL fault its own watchdog surfaces.

### The clock convention
Clocks carry only the START INSTANT and the client ticks. An elapsed count on the wire was
rejected twice: it is a second authority for a value derivable from one instant, and it
arrives at the producer's cadence so a drawn clock would jump to network timing. The one
inversion is a COUNTDOWN: for a scheduled wakeup and for a cron job the daemon ships the
DEADLINE instant and the client re-derives remaining time at a one-second tick. For crons the
daemon resolves the next-fire instant from the cron expression (accepted complexity).

### Element/view conventions the daemon's resolvers must honor
- Every element of a view is ALWAYS SET; "nothing to show" is expressed INSIDE the element
  (an empty warnings list is the daemon saying nothing is wrong). An unset element is a
  malformed frame, not a state.
- No dangling primitives in a view message; every field is a dedicated element message, and
  the same fact in two components gets each component's OWN wrapper.
- The message tree IS the UI tree, checked against an agreed ASCII drawing.
- Nilable MESSAGE fields carry the explicit `optional` keyword.
- Daemon-formatted figure strings throughout — no client rounding, no client arithmetic.
- All URLs are clickable; composed omitted lines ("42 more not shown") are daemon prose and
  the client never compares counts.
- Whole-view replace, never deltas or partial-replace oneofs; if a ticker's rate ever
  mattered the fix is daemon-side coalescing.

### The FEED
- `OpenFeed { WorkspaceRef; optional FeedId }` (UNSET = the root feed; SET = a subagent
  bubble's sub-feed) → the newest `FeedPage` + a `FeedWatchToken`. `WatchFeed` takes the
  echoed token alone and is STANDING ACROSS TURNS — a turn's end is the `FeedTurnEnded` ROW,
  never the stream concluding. `GetFeedPage` walks ONE feed older; the daemon holds each
  container's walk position (`first` resets, `next` continues, `next` with no walk standing
  is a refusal). No page-size parameter — the daemon picks. One connection per OPEN feed:
  the client opens the root on view-open and each bubble on expand.
- FEED-WITHIN-FEED: an agent bubble is literally its own feed; the bubble row's own `FeedId`
  IS the sub-feed's address. Two kinds of nesting: real sub-feeds (connection = placement,
  rows carry no parent) vs PRESENTATION nesting (`parent`: merge-phase rows, work under a
  skill heading).
- Row taxonomy: user_prompt / agent_prompt / activity / turn_ended / detached_subagent /
  detached_shell / permission / question / separation (the generalized divider) / cold_gate.
  Sync-vs-detached is PLACEMENT, never a second drawing — the detached wrappers wrap the same
  component their sync forms use.
- TERMINAL-AS-ROW: `FeedTurnEnded { concluded{answer FeedId} | errored | interrupted }` is a
  ROW, streamed and paged like any other. Liveness is structural ON THE DATA: no terminal row
  for the current turn = live. Errored's arms are the API taxonomy respelled figma→idl, plus
  max_tokens, refusal, max_output_tokens and query_died; rate_limited/overloaded carry
  optional `retry_after_ms` so the client ticks a countdown.
- Thinking is DROPPED from the feed (footer only). Unmodeled is DROPPED from the feed → the
  topbar warning dropdown.
- The daemon owns per-tool input PHRASING (including a write's created-vs-updated wording),
  the output FORM selection (text | code spans | diff lines | link rows | plain lines), and
  SYNTAX HIGHLIGHTING — the parser lives in the daemon ("fast Go that can call out to a C
  parser") and the client paints spans it is handed. The daemon needs a grammar set at least
  as wide as highlight.js's twenty, or files silently lose highlighting.
- A middle-slice read is honestly representable (`range`: contents, first_line, line_count,
  total_lines) and the daemon highlights the slice and words the omitted line ("lines
  400-499 of 4,312") — the shipped text IS the text actually read.
- The shell bubble's spool is a whole-replaced TAIL: the daemon caps and replaces it whole
  and the client appends nothing. A NON-ZERO EXIT IS COMPLETED, not failed. Spill path is
  appended to the composed omitted sentence.
- Permission: THE STANDING ECHO TOKEN NEVER REACHES THE CLIENT — the daemon holds it and
  supplies it to the shim when the answer verb picks standing; the card carries presence
  only. A denial is an ANSWER; a policy denial is worded never to read as the user's act.
- Questions and answers ride the same row for cold repaint; answers echo the question TEXT
  and option LABELS (never position, never a minted token), so the shim reconstructs the
  producer call with nothing remembered.
- The COLD-CONTEXT GATE is a ROW (`FeedRow.cold_gate`), which buys cold paint from pages and
  a RESOLVED trace in history. Absence is the "none" — there is no none arm. DATA, NOT PROSE
  is a recorded departure bounded to this component: the daemon serves raw facts (token
  count, last-request instant, model) and the CLIENT owns wording, formatting and ticking.
  `AnswerColdGate` echoes the gate's `FeedId` and the served model/scope.
- Plan mode: enter and exit are separate units with NO wire pairing key, so THE DAEMON
  COALESCES by the episode invariant (at most one open plan episode per agent) into one
  upserting bubble. Daemon prescriptions: the episode closes on exit settle, call failure, or
  the turn's terminal while planning (composed "ended without a plan" line), and MUST close
  before processing the `conversation_reset` that plan acceptance can emit.
- The daemon's feed resolver keys rows by drawn subject and switches per `DetachableWork`
  arm: subagent arm and sync spawn converge on the subagent component; bash → the shell
  component; workflow → nothing (deferred); unmodeled → the topbar.

### The FOOTER
Strip = Status | SubStatus | StatusActivity | Clock | the TOKENS CELL (one figure: the
turn's uncached input, plus alarm and accounting glyphs) | the LIVE-WORK CHIPS
(⚙ agents count, ☑ tasks done/total, $ shells count, 👁 monitors count, ⏱ crons count;
an unset chip is not drawn). Selection is WEBVIEW-LOCAL, so the daemon ships EVERY expanded
panel FULLY RESOLVED ON EVERY PUSH and the client draws whichever the selection picks.

**The status family is ONE TREE, legality by construction.** Each status arm declares
exactly the sub-status steps and activity kinds legal while it stands, so an illegal pairing
is unrepresentable rather than forbidden by comment. Leaf payload messages are SHARED across
the per-status oneofs; the oneof TYPES carry the legality. Three status-independent
activities (notification, rate_limited, context_budget) appear in EVERY status arm, and THE
DAEMON SELECTS ONE standing activity per push by a stated precedence ladder: notification
OUTRANKS everything; status-bound kinds rank next by the daemon's judgment; rate_limited is
second-lowest; context_budget is lowest. Activity optionality is EVIDENCE-GATED — required
where a producer always has a line, optional elsewhere with the WHY stated at the field.
`FooterStatusActivity.at` (when the activity began standing) rides the ENVELOPE, non-optional
— a push without it is a LOUD DAEMON FAULT. Rendering conventions: status and substatus
render lowercase ASCII with spaces, never underscores; activity is the rich colorized cell;
a status arm with no substatus merges that cell into the status cell.

Daemon-side resolution duties in the footer: the wakeup fallback (a pending self-wakeup shows
only when the footer would otherwise read idle/done — any real status wins); the close-blocked
reasons sentence; the expensive-turn alarm sentence (whose phrasing states the
prompt-vs-cold-keep-alive origin); the accounting verdict line; picking ONE activity;
composing the bash interrupt-cause trailing line and the spill sentence; dropping deleted
tracker rows (a deleted task is ROW OMISSION, not an arm). FLAGGED, not solved: cold paint of
the CURRENT task list — acts are history entries, so the daemon must hold or recover the list.

### The TOPBAR
`WatchTopbar` streams the whole view; `token_breakdown` is nested INSIDE it and is ALWAYS
POPULATED, so opening the menu needs no round-trip. The daemon's topbar resolver owns session
accounting composition on every push. The topbar's token figure is the SESSION's; the
footer's is the TURN's — different values, architecturally uncoupled.

`TopbarWarningStrip` is the last warnings newest-first, DAEMON-CAPPED; each warning is a
dropdown line plus a per-kind OVERLAY detail: accounting (composed evidence lines),
unmodeled_tool (tool name + abbreviated legible argument lines — never a dump, never a
failure; one warning per distinct tool name), detached_unmodeled (one per live item),
session_fault (component + verbatim detail), degraded_window (component, reason, began_at_ms,
open|closed extent). The daemon pulls `GetSessionDiagnostics` at its own cadence and a
healthy pull RETRACTS the warning on the next push.

An idea recorded but explicitly NOT decided: classify unmodeled arguments' structure
programmatically and run a small fast model over an unseen structure to produce a
plain-English summary. What IS settled is the requirement — abbreviated and legible, never a
dump.

### The SIDEBAR
THE DAEMON OWNS THE ROSTER; Emacs sends COMMANDS, not state. `WatchWorkspaceRoster` is the
ONE global stream (empty request). The roster carries BOTH groupings fully resolved
(repository and task as siblings) and the client draws the pane its LOCAL preference picks;
grouping mode, section folds and the nav cursor are WEBVIEW-LOCAL and left the wire.
`revision` / `boot_id` and the epoch/monotonicity rules are deleted — a command has no stale
roster to resurrect, and the daemon's own stream ordering replaces them. `RosterRowWhen` is a
oneof the DAEMON chooses (merged wins) — precedence is resolved server-side.
`RosterRow.attention` is an empty presence marker the daemon sets on a push notification and
CLEARS ON THE EXISTING `SelectWorkspace` verb; the canonical blink cadence (two blinks, 500 ms
on/off, then steady) is specified once and implemented identically by the webapp sidebar and
the Emacs tab-bar — divergence is a defect.

### Push notification
The daemon publishes the FACT; each surface applies the policy it alone has knowledge for.
The daemon NEVER asks "is Emacs focused". Emacs owns presentation policy (unfocused → OS
notification that raises the frame and selects the tab; focused+unselected → tab-bar blink;
selected → nothing).

## 11. THE agentrepl.v1 SERVICE THE DAEMON SERVES

Connect, not gRPC, because the clients are an xwidget WebKit view and elisp; all endpoints
multiplex over the ONE HTTP/2 connection, so one-connection is true at the socket level and
endpoint-per-component at the API level. Organized by COMPONENT, seven sections: feed,
sidebar, topbar, footer, daemon-hold tray, host, daemon admin.

Cross-endpoint conventions: every response is `oneof result { <Method>Success |
<Method>Error }`; every rpc returns `<RpcName>Response` (no rpc returns a foreign type
directly, streams included); error arms are DERIVED from the daemon's real refusal sites,
never invented; workspace addressing is a `WorkspaceRef` field on every per-workspace request
and stream-open; no request_id/client_id envelope (Connect's unary response IS the
correlation); no paint attestation — responses never claim anything was rendered; Connect
serves binary or JSON per client and elisp uses JSON.

**Command panels dissolve into SubmitPrompt.** The client sends normal user requests and the
daemon MIGHT answer "this is a programmatically handled command" — the webapp never knows
what is programmatically handled. `RunCommandPanel` never exists; the recognition table lives
only in the daemon. A held prompt is a `turn` success (the tray shows the hold, not an error).
A new panel command is a new arm deployed daemon-side, and old clients fail to match loudly.

**The daemon-handled command set** is /context, /todos, /agents, /mcp, /help, /status.
/cost and /usage LEFT the set (their only structured sources are an EXPERIMENTAL method and
an undeclared-shape one) and fall through to the vendor. Panels are filled by daemon
implementation choice, recorded as guidance and never contract; the TODOS panel is filled
from the DAEMON'S OWN tracker state with no vendor route. Dropped as unproducible headlessly:
/doctor, /hooks, /release-notes, /export, /memory, /permissions. No-panel commands: /clear
and /compact (the context-cut row is the outcome), /model (topbar), and the act/flow commands.

**Daemon admin.** `UpdateShutdownSchedule { schedule{at_ms} | cancel | now }` is DEPLOY
TOOLING's drain-and-exit control, purely inbound, with no UX motivation; its user-visible
consequences ride surfaces already modeled (held prompts in the tray during the drain, footer
status). `DaemonHealth` and `SessionHealth` return `healthy | unhealthy{faults}` inside
success — UNHEALTHY IS AN ANSWER, never an error. `DaemonFault` and `SessionFault` are
DELIBERATELY SEPARATE TYPES with different producers: `{ detail }` now, a `kind` oneof added
with its first DERIVED arms at the implementation wave. `ClientLog` relays webapp diagnostics
(the xwidget's JS console is invisible and unpersisted); its `context` Struct is an ACCEPTED
untyped exception, written verbatim to the daemon's on-disk log; Emacs never calls it.

## 12. ACCOUNTING AND USAGE

- MONEY LEAVES THE API. `RunCost` is deleted and both cost fields are retired; no surface
  draws a currency figure. The rest of run accounting (durations, round trips, per-model
  usage, denials) stands — though most of that wave was later reverted to the deferred doc as
  NEW support rather than completion of a landed surface.
- Usage rides the `AgentActivity` ENVELOPE, with the ONE-UNIT-PER-RESPONSE rule: exactly one
  unit per API response carries it — the unit for the response's FIRST content block — and
  every other unit of that response leaves it unset. Absence means "not the unit carrying its
  response's usage", NEVER "this cost nothing". Reason it is on the envelope: 44% of
  assistant messages are pure tool calls and every one carries usage, so confining usage to
  the thinking/response arms would drop accounting for thousands of responses; and the daemon
  must float usage up to the topbar and footer without looking in fifteen arms.
- The three and only three usage carriers: assistant messages (`message.usage`, one per API
  response), a subagent's completion (`toolUseResult.usage` + `totalTokens` — the subagent's
  OWN consumption, a different fact), and the thinking-tokens estimate channel. TOOL CALLS
  CARRY NO USAGE.
- Usage rides the UPDATE arm too: the vendor states usage when a message OPENS and restates
  it as it grows, so the footer's live token figure has a producer and a correction is simply
  the next frame.
- The thinking figure is an ESTIMATE, not a billed amount, and must never be added to a bill.
- A subagent's async completion carries at most one OPTIONAL `total_tokens` scalar vs the
  sync path's full four-field usage; the split lives at the subagent-totals usage field as a
  two-arm oneof, and absence means UNREPORTED, never zero. The daemon can derive a breakdown
  by summing frames it already holds.
- Only the FIVE-HOUR allowance window is carried; the footer's WEEKLY allowance has no
  producer — flagged.
- `resumed_recipient` on a send-message delivery means A DORMANT AGENT WAS WOKEN and is
  consuming tokens again — a real user-visible consequence with no other producer.

## 13. FAILURE CLASSIFICATION

- `ApiRequestFailed` carries the vendor's documented closed error taxonomy as arms
  (rate_limited, overloaded, authentication_failed, permission_denied, invalid_request,
  request_too_large, not_found, internal, plus billing, oauth_org_not_allowed,
  model_not_found, max_output_tokens, and `unmodeled{type}`), with `retry_after_ms` optional
  and confined to the two arms it applies to. It is an AGENT-LEVEL failure that ends the
  agent's stream, not a property of one prose block. `daemon/internal/errclass` classifies
  API failures into this record's kind, NOT into ten `FailureKind` arms.
- `FailureKind` shrank to the entry-less residue: machinery (session/shim/internal) plus
  client-local. Every entry-correlated arm left the oneof; its EVIDENCE MESSAGES survive and
  are imported by the feed's error arms and by `agentrepl.v1` error responses. Nothing draws
  the residue as rows — footer/topbar/gate state draw them.
- ONE FAILURE, ONE HOME. The response row's error arm carries NO reason (why a response died
  is the turn terminal's fact); turn-level max-tokens/refusal renderings resolve from the LAST
  response's failure reason; a stream that dies leaves no blinking cursor because the daemon
  pushes the row's error arm under the same id.
- Response failure reasons: max_tokens | refused{optional vendor explanation} |
  context_window_exceeded | aborted. Deliberately WITHOUT arms: pause_turn (the vendor
  resumes it itself), compaction (the context cut is that fact's home), stop_sequence.
- Stop hooks: the stop-hook terminal arms stay on `AgentFailure` — the SDK names both as
  first-class loop terminals, so they RELAY the vendor's own stated terminal. The earlier
  "stop hooks never determine the turn terminal" ruling concerned DAEMON-synthesized terminals
  and is not contradicted; no `FeedTurnEnded` hook arm exists.
- A SUCCEEDED hook draws NOTHING; live hook runs fill the footer's hook activity; FAILURES
  draw a feed card with a link to the refused call.
- The daemon must VERIFY at the wave that every vendor API failure it sees LIVE also lands as
  a transcript record; if some do not, the feed would miss them and the decision reopens.

## 14. IMPLEMENTATION INVARIANTS BINDING THE DAEMON

1. **UNSET NON-OPTIONAL FIELDS ARE ILLEGAL, EVERYWHERE, IMMEDIATELY.** A request carrying an
   unset non-optional field is answered with an ERROR to the producer at once — never
   "handled", never defaulted. A non-optional response or stream-push field MUST be set; a
   consumer receiving one unset RAISES A LOUD ERROR itself (on a stream there is no producer
   to answer). Integration tests are expected to catch both.
2. **LOGGING.** Every logical branch carries a DEBUG statement; warnings at WARNING, errors
   at ERROR. Integration/e2e orchestration turns on ≥WARNING logging BEFORE tests run and
   PERUSES the logs EVEN WHEN TESTS PASS; any warning is remediated to zero — fixed or
   deliberately downgraded — never left standing. Remediation runs enable DEBUG to trace.
3. **Proto→code mapping.** Every MESSAGE has one core "base" implementation function per
   language where validation lives ONCE (unset non-optional fields and required-semantics
   empty strings are ERRORS; an unset oneof is an ERROR BY DEFAULT, a documented fallback only
   where the schema comment sanctions absence). Every NON-PRIMITIVE use site gets its own
   dedicated testable function delegating to the child's base; primitives get no wrappers; the
   producer side is symmetric. NO class-per-message mandate — the requirement is dedicated
   testable functions and separated concerns.
4. **Shared subroutines are code-level requirements, not suggestions.** ONE renderer
   subroutine draws EVERY separation-divider arm (a per-arm divider renderer is a defect); ONE
   shared webapp link component and ONE shared Emacs "open path[:line] in a doom popup, right
   side, half width" subroutine serve both the plan bubble's edit button and every findings
   location; the roster attention blink cadence is one spec both surfaces cite.
5. **Implementers NEVER change protobufs.** A needed change is a request to the system
   orchestrator, who triages to the lead; on approval the lead broadcasts PAUSE, lands the
   change, rebuilds bindings, and broadcasts RESUME carrying the NEW FOUNDATION COMMIT SHA.
   Every proto-change request and ruling gets a line in the design record.
6. **Reconciliation test rule.** Any test referencing a DELETED or RESPELLED symbol is
   DELETED, never adapted; pure renames adapt mechanically. Replacement coverage is
   architecture work: INTEGRATION specs go to each subsystem's implementation planning doc and
   E2E specs to the main doc; unit specs are NOT prescribed — they fall out of the mapping
   convention.
7. **Dead code is NAMED WORK** in each subsystem's `docs/implementation/<subsystem>.md`,
   never left for discovery.

**Daemon status at freeze.** The design froze at `2d79f7501`. Five subsystems reconciled
green and merged; THE DAEMON'S reconciliation agent correctly REFUSED — the daemon was never
repointed off `protocol.v1` / `data.v1` / `state.v1` (9,724 dangling reference sites across
452 of 830 files), so "minimum adaptation" would hollow it into an empty shell. The daemon's
re-targeting is FANOUT IMPLEMENTATION work, not reconciliation. `protoc-gen-connect-go` joined
the Makefile's go target because `protoc-gen-go` emits message types only and nothing could
serve the three Connect services.

**Named daemon-side deletions from the hibernation collapse**: the two wire states, the
topbar teal arm, the revival gate and its pushes, the Hibernate/Revive handlers and
user-forced entrypoints, the `SessionHibernated` failure-kind encode (the refusal is retired
outright — prompting a parked workspace just works), six e2e suites, and the
leases/holds/gates machinery, keeping only the idle sweep plus resume. Teal dies from the
palette, which contracts to five colors cross-system.

**Other named daemon deletions/relocations**: the fence minting; the roster retainer and the
`revision`/`boot_id` staleness machinery; `newQueueEntryID()`; the phantom-task reconciler and
`QueryLiveTasks`' steady-state role; the footer's client-side precedence application (the
daemon picks ONE activity); the when-column precedence; the `frontendv1.RenderState` Go type
(the SSM keeps its state machine internally; the wire carries only projections); the
translate-layer re-encoding, which becomes the feed row resolver.

## 15. The graceful-rollout handover (post-freeze increment)

- The rollout controller's wire: `WatchDaemon` (daemon-level host stream,
  `shutdown_announced { address }`), `transferred` + `reload_webapp` push
  arms on `WatchHostWorkspace`, the WEB LINK section (`WatchWebWorkspace`
  with `transferred { address }`, `AdoptWebWorkspace`), and
  `AdoptHostWorkspace` — two adopt verbs so the VERB identifies the
  participant.
- Rendezvous: expected participants = per-workspace stream holders at
  announcement; adoption (kernel-lock claim, shim adoption, held-intake
  drain) completes only when all have called; headless = zero rendezvous
  via WSM facts + lock.
- The new daemon refuses per-workspace rpcs pre-adoption; derived arms
  owed at the wave: `transferring_away { address }` (old) /
  `not_yet_adopted {}` (new).
- Old-daemon-side adoption timeout surfaces as the workspace's error —
  remediate-as-it-comes-up, deliberately NOT a hardened invariant.
- Never-free workspace: wait forever, periodic warning log (~10 min);
  a newer rollout supersedes an unfinished joining daemon.

## 16. The spill removal (post-freeze increment)

- The shim's durable WriteBatch spill is REMOVED: transient store blips
  absorb into a bounded in-memory retry buffer; exhausted retries are a
  LOUD failure (dropped frames logged with what was lost), never a crash.
- Rationale (the user's): persistent store unreachability is a
  lifetime-sequencing defect to fix, not a condition for fallback
  persistence; the graceful shim stand-down waits for all acks before
  exit, so an exit with unacked writes IS the loud failure.

## 17. The merge bubble as a sub-feed; address-driven routing; the parked policy (post-freeze increment)

- FeedMerge is the collapsed HEAD only; the bubble is a SUB-FEED (its
  FeedId is the address) carrying six FeedMergeTab rows — queue | rebase
  | tests | remediation | action | landing — resolved tabs replaced
  whole, agentic tabs as parent containers; per-kind state oneofs;
  rounds are new tabs.
- The feed resolver is MERGE-AGNOSTIC: a lease holder supplies a generic
  OUTPUT ADDRESS {target feed, parent row}; while it stands, everything
  the session produces routes to the merge sub-feed under the active
  tab. Only the merge orchestrator and the footer resolver know "merge".
  Feed + footer pushes dispatch in parallel at every merge-state change.
- PARKED policy: agent exhausts its attempt → lease flips to PARKED →
  prompts deliver through the merge orchestrator as guidance (no
  classifier — the lease state IS the recognition); composer gains
  merge_parked; footer merging family: rebasing, parked{line},
  remediating. Hand-resolution unsupported — no continue verb exists.
- Tests tab ships daemon-parsed ANSI as paint-class spans.
- NON-EMACS-REPO MERGES (ruled): tests + remediation tabs exist IFF the
  merge target is our own repo (self-repo common-dir identity, the
  self-reload check); other repos: queue → actions → rebase → landing.
  Configured before/after prompts run on EVERY merge, every ingress; a
  sessionless workspace with a configured action gets a session started
  under the lease.
- TWO MERGE METHODS (superseding the six-tab and tests-iff-self-repo
  phrasings): Emacs repo = pre-prompt → NO-FF MERGE COMMIT (conflicts
  via parked lease) → tests + fixes → rollout bounce → post-prompt;
  everything else = pre-prompt → post-prompt only. Seven conditional
  tabs (queue | pre_prompt | merge | conflicts | tests | fixes |
  post_prompt); parked only on conflicts/fixes. MULTI_REPO_ROOT is
  account selection ONLY. Self-reload's landed range = the merge
  commit's second-parent history.
- TOPBAR (2026-08-28): the resolver serves TopbarAccount (login email
  from the session config root; logged-out drawn) and TopbarContextChip
  (current context size, session-scoped breakdown). OPEN arch todo: the
  context-size producer (get_context_usage vs separation after-tokens).
- CONTEXT USAGE (2026-08-28): shim.v1 GetSessionContextUsage pulls the
  vendor's own answer (SessionContextUsage: total/max/categories);
  derivation from usage frames is FORBIDDEN as the chip's source; the
  /context panel fills from the same pull. ACCOUNT SWITCHING: the
  daemon determines the config dir (repo-under-root rule) and ports the
  transcript between roots itself — no shim involvement.
- INVARIANT: the account is DETERMINED (repo-under-root), never
  selected — no request field, no override, no inheritance; a
  differently-accounted workspace is structurally unrepresentable.
- SHIM-CONNECTION rulings (2026-08-28): redial forever, give-up by
  evidence only; readiness = GetSessionDiagnostics answering healthy;
  crashed-daemon boot ADOPTS surviving shims. THE RESPONSE HANDLER:
  one single component through which every shim response flows,
  routing per type to the resolvers (feed via output address, footer,
  topbar catalog/context, accounting). CONNECTIVITY: witnessed only by
  the three standing streams' liveness (WatchSession /
  WatchWebWorkspace / WatchHostWorkspace); all live = connected.
- THE SESSION MANAGER (2026-08-28, supersedes "response handler"): one
  per live session, the only consumer of shim output; owns every shim
  watch (eager — open set = live-work set) and routes each frame by
  type (feed via output address, footer, accounting, turn-lifecycle →
  the queue); shim pulls are synchronous member functions answering
  their caller. Detached work: daemon↔shim leg eager, webapp↔daemon
  leg lazy (expand = subscribe only). Client pushes are the
  resolvers'.
- RESOLVERS (2026-08-28): exactly ONE per component — feed, footer,
  topbar, sidebar, hold tray — each producing its finished frontend.v1
  view for verbatim rendering; internals discretionary within the
  landed invariants.
- THE SESSIONWATCHER (final, supersedes session manager): one per
  workspace/shim; three jobs — watch the session's streams (eager),
  sole connectivity truth, route/fan out to resolvers. No pulls (the
  diagnostics + context-usage verbs folded into WatchSession as pushed
  arms 25/26), no writes (prompts = queue only; sync reads may use the
  shim client directly; ALL async streams enter here). Five resolvers
  by name (feed, footer, topbar, sidebar, hold tray), purpose-only;
  resolver in-memory accumulation is fine — ship complete snapshots,
  never partial pushes.
- CLIENT-FACING HALF (2026-08-28): the Connect server delegates to the
  landed components, internals discretionary; INVARIANT: a Watch
  subscriber never misses a published view and never ends on a stale
  one — the latest view first if one exists, else the first ever
  published; empty/partial frames never sent.

