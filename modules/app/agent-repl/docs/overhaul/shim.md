# Shim implementation planning

## Dead code to remove
- `src/uds/framing.ts`'s envelope half (`MessageConn`, `encodeMessage`,
  `decodeEnvelope`, `envelopeType`, `unpackAs`): unexercised since the
  protocol.v1 wire layer died; remove with the shim.v1 Connect service
  implementation (the codec half may survive if the new transport reuses it).
- `runUdsMode`'s loud-throw stub: replaced by the shim.v1 service entrypoint.
- `agent-shim/claude/shim/AGENTS.md`'s three-surface story
  (conversation.v1/protocol.v1/agentshim.v1) — rewrite against the six-surface
  model when the service lands.

## Replacement integration-test specs
(Replacing the 14 deleted suites' subjects under the new contract; unit specs
deliberately absent.)
- shim.v1 service: one integration spec per rpc suite — session lifecycle
  (StartSession cold-gate refusal path included), turn (StartTurn one-in-
  flight refusal, WatchAgent open-with-page), detached work (per-kind watch/
  stop), history (ReadHistory first/after).
- store.v1 client: WriteBatch durable-ack + retry-buffer retirement on
  success; failure = nothing committed, the BOUNDED IN-MEMORY retry
  buffer replays (there is NO durable spill — exhausted retries are a
  loud logged drop; replaces the store-spill/store-replay suites'
  subjects).
- SDK→conversation.v1 conversion: golden transcripts (real captures per
  deferred vetting item 5) driven through the converter, asserting the
  Agent* frames — replaces convert/delta/extras suites.
- Reattach: daemon reconnect via WatchAgent known_through catch-up (replaces
  reattach.test.ts's subject).
- Compaction: shim writes the summarized transcript → the CLI resumes it →
  the conversation serves. WARNING: the CLI's required per-line field union
  is internal and undocumented — the written lines must mimic OBSERVED real
  transcript lines, discovered empirically; this test doubles as that
  experiment.
- Keep-alive rewind: a real prompt submitted after trailing keep-alive turns
  → the served context excludes them (the yield obligation, verified against
  the vendor's actual transcript, not assumed).
- SDK upgrade canary: the SDK version is PINNED (lockfile); one integration
  test asserts every relied-on SDK method and response shape exists (incl.
  the experimental get_usage), failing loudly when an upgrade removes or
  reshapes one.
- CAPTURE HARNESS (deferred vetting item 5 — assigned HERE): the golden
  transcripts above are REAL captures from the actual agent binary, taken in
  a one-time supervised capture run the project lead dispatches (the one
  sanctioned exception to the no-real-calls rule); the mocked vendor's
  scripts are rebuilt FROM these captures so the fake cannot agree with us
  by construction.

## Contract context (for implementers)

Everything below is standing fact about the frozen contract, distilled for shim
implementers. The protos under `proto/src/` are the authority and their comments
are the authoritative documentation — read the files you work on; this section
gives the map and the ideas. Cross-cutting rules (identity spaces, echo tokens,
response/stream conventions, presence rules, the fidelity principle, the exempt
set, implementation/validation/logging conventions) live in
`the teamlead prompt (standing conventions) and the proto comments` and are NOT restated here.
Implementers never change protobufs; a needed change is a request up the
orchestration chain.

### Process-level obligations (rulings 2026-08-29)

- KERNEL LOCKS — THE SHIM HOLDS BOTH, TAKEN INSIDE StartSession (ruled
  2026-09-02, daemon relaunch flow): the shim takes two exclusive kernel
  flocks for its lifetime — one keyed by session id, one keyed by workspace
  dir — both inside StartSession, before the SDK is touched. An INERT shim
  (spawned, serving, no session) holds NEITHER lock, so the rollout's
  prelaunched shim is never blocked behind the live one; a workspace-lock
  conflict answers StartSession `conversation_owned`. The daemon's probe
  semantics are unchanged (a held lock = a live shim owns the conversation). They exist because on a fresh
  daemon boot a surviving shim may not have dialed in yet, so only a kernel
  lock answers "is this conversation already owned" (connection tracking
  says NO when the truth is NOT YET); the workspace key catches two session
  ids over one transcript. The daemon PROBES these locks before spawning,
  and the rollout's per-workspace transfer waits on the shim's workspace
  lock.
- SYSTEM PROMPT AND SETTINGS (load-bearing): every session starts with the
  vendor's `claude_code` preset system prompt PLUS the harness metaprompt
  appended (read from the canonical metaprompt.md file at spawn — the
  file-based mechanism survives), and `settingSources` user+project+local.
  Without the preset the model cannot resolve `~` and invents paths; without
  the setting sources the session loses the user's permission allowlists,
  hooks, and CLAUDE.md — and the vendor-side `denied.by_policy` emissions
  the permission gate RELIES on only exist when settings are loaded.
- THE BINARY: the shim drives the SDK's own bundled agent binary — the
  pinned version the upgrade canary guards. No system-binary override.
- SIGNALS: SIGTERM is the one authorized process-level shutdown and takes
  the same graceful path as `KillSession` (the daemon may be dead, so an
  rpc cannot be the only teardown). SIGINT is REFUSED and logged — an
  attached terminal's Ctrl-C must not end the query.
- PERMISSION-CALLBACK LIVENESS: every teardown path — interrupt, shutdown,
  SDK abort — resolves ALL pending permission callbacks (as denied) before
  proceeding. An unresolved `canUseTool` promise wedges the vendor process.
- TRANSCRIPT BACKUP (shim-owned): the shim captures a copy of the vendor
  transcript at every turn end and at vendor-uuid rotation, into a bounded,
  pruned backup directory beside the work, restorable — the rung below the
  fresh-start refusal, protecting the one artifact nobody can regenerate.
- BUILD IDENTITY: `SessionStarted` reports the shim's build sha; the daemon
  compares it against the current deploy stamp and bounces a stale survivor
  at freeness (the rollout's build-staleness bounce rides this field).
- `--version` stays: a dependency-free bundle smoke that loads every static
  import and exits before touching a socket or the SDK.
- LOG SINK SURVIVAL: the shim logs to an inherited fd (`--log-fd 3`), never
  to a pipe whose far end is the daemon's stderr — a shim must survive its
  daemon's death without dying on its own log line (EPIPE incident,
  2026-08-10); a poisoned sink is surfaced, never silently swallowed.

### What the shim IS

- The vendor adapter: one shim per session, driving the Claude Code binary via
  the Agent SDK, serving the daemon a Connect service (`shim.v1`) and producing
  `conversation.v1` records into the store (`store.v1`).
- Vocabulary: "the agent binary" = the Claude Code engine (queue, permissions,
  tasks, compaction live there); "the SDK" = the npm wrapper that spawns and
  drives it over stdio; "the vendor" = the Claude-vs-other axis.
- Two models, one mapper: `conversation.v1` is the protocol model (units with
  identity, upserted whole); the vendor's transcript/stream is a flat log. The
  shim performs the fold — accumulating nothing beyond constant-size joins —
  and owns the whole record↔protocol mapping in one place.
- What the shim MINTS:
  - `AgentActivityId` — one per unit for the unit's whole life, sourced from
    the vendor's `tool_use_id` where one exists, from message id + block index
    for text and thinking.
  - The main agent's `AgentId` — minted on the first fresh start,
    store-persisted, reported unchanged on every later `StartSession`;
    deliberately decoupled from the vendor session id, which can rotate
    mid-session (`SessionIdentityRotated` changes only the resume handle).
  - History pointers and watch/continuation tokens (opaque, echoed verbatim).
- What the shim TRANSLATES (each a constant number of single lookups):
  - tool return → its call by `tool_use_id`; skill document → its call by
    `sourceToolUseID` (never by name-matching); spawned agent → its spawn;
    re-announced start → its original instant (recovered FROM THE STORE by
    unit id after a bounce, not from memory).
  - the vendor's `DetachedWorkId`↔underlying-identity mapping (the handle is
    the uniform connection token; the mapping is the producer's, one lookup —
    `task_started` carries both).
  - the question tool's answer serialization: the vendor keys answers by
    question TEXT and comma-joins multi-selects; the shim undoes both at the
    boundary using echoed values (it validates echoes against the pending
    callback it already holds — no new state). FREE-TEXT RESIDUE (ruled
    2026-08-29): whatever remains of the joined answer string after every
    validated label is removed IS the typed free text — that residue is
    `free_text`'s producer definition.
- What the shim DROPS: exempt tools (TaskStop, TaskOutput, TaskGet, TaskList,
  ToolSearch, NotebookEdit, REPL, the MCP-resource family, SendFeedback,
  ClaudeDesign, Projects, ShowOnboardingRolePicker, ProposeSkills, the
  background-shell peek) — dropped entirely, never emitted as `AgentUnmodeled`,
  never tripping the unmodeled warning. Also dropped: `skip_transcript`-marked
  ambient tasks (no announcement, no bubble; the vendor's level set still
  governs liveness so no indicator wedges), and vendor identity spaces (uuid,
  message.id stay shim-side).
- `AgentUnmodeled` keeps its meaning: a tool whose schema genuinely cannot be
  known. A recognizable built-in arriving there is a producer defect.
- The one bounded state exception: spawn provenance (`task → spawning call →
  turn`), kept solely so a forced `KillTurn` can name the transitive set it
  stops; never on the wire except inside a kill's outcome.

### shim/v1 — the service the shim serves (17 rpcs, four sections)

Files: `service.proto`, one `endpoint_<rpc>.proto` each, `prompt_origin.proto`
(the one shared file — an enum of send sites; it has no keep-alive value on
purpose).

- SESSION — spawn/attach/condition/end, per the decoupled-acts principle
  (sessions and turns outlive the daemon):
  - `StartSession` (unary): `fresh {model, permission_mode}` or `resume
    {vendor_session_id, optional remediation}`. Resolves when a prompt can be
    accepted. A COLD context is REFUSED with its cost (`SessionCold`), never
    silently paid — verified: a bare SDK resume makes no API call, so refusal
    costs nothing; the cold read lands with the first prompt. The daemon
    reopens naming a `SessionColdRemediation { pay | clear | compact {model,
    scope} }`. Keep-alive cadence BEGINS BEFORE success returns. Resume
    RECOVERS model and permission mode from the transcript (recorded there but
    not restored by the SDK — the shim reads the last of each back and passes
    them; the answer reports what was recovered). `SessionStarted` also
    reports `live_work` (from GetLiveWork reconciliation, below) and the
    vendor session id (the runtime's own answer).
  - `WatchSession` (standing stream): session-scoped facts as they manifest —
    identity_rotated, query_died (duplicated on purpose: stream owners also
    get their own failure, but a consumer with no stream open still needs it),
    model_changed / permission_mode_changed (authoritative even when the
    consumer asked for the change), fast_mode, mcp_server, account_usage,
    context_budget_warning, and the two PUSHED arms `diagnostics` (25) and
    `context_usage` (26) — these superseded the deleted pulled verbs
    GetSessionDiagnostics/GetSessionContextUsage; the shim pushes both at its
    own cadence, context usage also at every turn end, sourced from the
    vendor's get_context_usage (never derived from usage frames). NOTE: the
    rpc comment in `service.proto` ("the shim's own health is pulled")
    predates that fold; the SessionUpdate arm comments are current.
    OPENING FRAME (binding, daemon shim client, 2026-08-29): on EVERY
    WatchSession open the shim pushes a `diagnostics` frame IMMEDIATELY
    (the current health verdict), then at its cadence and on change —
    connect-go surfaces a server-stream refusal only at the first Receive,
    so the daemon consumes every watch's opening frame as the open's answer
    and a silent WatchSession would block bring-up (readiness = the first
    healthy diagnostics push). WatchAgent/WatchBash likewise open with their
    page/start frame, as already contracted.
    A `compacting` arm is owed (contract increment, ruled 2026-08-29):
    vendor-initiated auto-compaction still happens, and its start signal
    (the system status:compacting message — the ContextCut record is the
    end) is forwarded so a surface can draw the in-progress state.
  - `SetSessionModel`: resolves AFTER the current turn ends (one model per
    turn, a deliberate departure from the SDK's mid-turn setModel); refused
    IMMEDIATELY with `SessionCold` when context exceeds the request's
    threshold (daemon policy per call; the shim only measures) and no
    remediation is named. A model switch is a cold cache (cache is per model).
  - `SetSessionPermissionMode`: session state conditioning every gate.
  - `Hibernate` (ruled 2026-08-29, contract increment owed): hibernation is
    a first-class directive — the daemon calls it; the shim COMPACTS the
    session (its ordinary compaction mechanism), then acks; only after the
    ack does the daemon stand the shim down. Revival then never pays a cold
    context.
  - `KillSession {force}`: ends the turn if open and EVERY live task,
    whichever turn spawned it (the vendor's own live-set answer — no shim
    tracking). Refuses while live unless forced; both outcomes NAME the work.
- AGENT — one API whether the agent is the main thread or a subagent:
  - `StartTurn` (unary, the MAIN agent's verb only): `{turn (daemon-minted
    TurnId, adopted by the shim), said (UserSaid), origin, page_size,
    optional known_through}` → `{AgentPrompt prompt; HistoryPage page}` — one
    call submits AND paints; `prompt.agent` is the WatchAgent address. ONE
    TURN IN FLIGHT, structurally: a second StartTurn while one is open is a
    daemon fault, refused, never queued (the daemon is the only queue; the
    vendor's queue is never ours — the interrupt's still_queued lists are
    dropped).
  - `WatchAgent {optional target; page_size; optional known_through}`: ONE
    stream for any agent. Opens with a page (full repaint when known_through
    unset; only newer entries when set — the caller's own high-water mark),
    then tails one pointered entry per write. Attach only: closing it ends
    nothing; replay means a restarted daemon misses nothing.
  - `UpdateAgent {optional target; AgentInput {stop | answer | prompt}}`: the
    ONE write vocabulary for any live agent. `prompt` targets an EXISTING
    agent (subagent, workflow agent) — never the session's own turn (that is
    StartTurn). How a prompt lands (steer-at-next-tool-round vs resume) is
    the shim's business; the consumer never learns which.
  - `KillTurn {turn, force}`: unforced, the synchronous turn ONLY — detached
    work runs on (2026-09-23; the `live` refusal is no longer produced).
    Forced, the main agent and everything THIS turn spawned, transitively (via
    the spawn-provenance map).
- DETACHED WORK — one stream per live item; bespoke verbs because the write
  surface differs per kind:
  - `WatchBash` / `StopBash` (a process's only input is stop).
  - `GetWorkflow` (unary open: the run's description plus either the watch
    token — inside the `live` arm only — or the embedded terminal) /
    `WatchWorkflow` (token-addressed tail: stateless subagent LEVELS with
    replace semantics, then the terminal; the run's agents are then watched
    individually via WatchAgent — the daemon owns the fan-out) /
    `StopWorkflow` (the run's only addressable act). WORKFLOW IS KICKED
    (ruled 2026-08-29): the workflow verbs and vocabulary stay in the
    contract but are NOT implemented in this wave — a future feature.
  - `DetachForeground {AgentActivityId}`: detach of in-flight foreground work
    (no client verb today) — moves in-flight turn work onto its own stream;
    the turn announces the detachment and the consumer opens the matching
    Watch.
- HISTORY:
  - `ReadHistory {optional target; page_size; first | after(HistoryPointer)}`:
    one page of one agent's durable past, newest first. A page is the
    IMMEDIATE CHILDREN of the addressed agent; terminal frames ARE entries
    (the feed's stop notice has no other source); no `start` frames are
    replayed (a settled frame carries the start's facts by the upsert rule);
    keep-alive turns never appear (indexed never-served).

### What the shim's streams owe consumers

- Every bounded stream concludes with a TERMINAL FRAME; a stream ending
  without one is a transport failure, read as one. Standing streams
  (WatchSession, WatchAgent) have no terminal arm.
- Every stream that carries a unit opens with that unit's `start`, and a
  re-announcement repeats the ORIGINAL start instant (drawn clocks must not
  reset when work moves streams; after a shim bounce the instant is read back
  from the store by unit id).
- `update` frames carry DELTAS, never cumulative text (`AgentResponseUpdate.
  new_markdown`, `AgentThinkingTextDelta.new_text`); the accumulator is the
  daemon's. Bash keeps its offset+delta (its spool has no settled whole);
  prose does not (the terminal frame carries the whole text and self-corrects).
- PROGRESS is a third arm kind: the vendor's per-call heartbeat is relayed as
  `AgentToolCallProgress {last_progress_at_ms}` on nine tool kinds — a beat,
  never a shim-invented ping. THE WEDGE RULING: that heartbeat is the only
  evidence of a wedged call, and the SHIM rules — a wedge settles the unit's
  `failure` arm on the shim's timeout. The daemon merely relays beats to the
  feed; no client times cadences.
- Announcements ride the spawning stream, as they happen: `AgentFrame.result`
  is `{update | success | failure | detached_work}` — the `detached_work` arm
  is the consumer's open-a-stream obligation, and `AgentUpdate` is pure
  conversation content `{activity | question | permission}` where question
  and permission mean the agent is BLOCKED awaiting a write-back.
- Frames are FLAT: `AgentFrame.agent_id` is the whole of attribution; a
  subagent's work is never nested in its spawn. `AgentSubagentStart.
  created_agent_id` is the key a consumer draws a container under.
- Liveness is structural: the set of open detached-item streams IS the live
  set; the daemon opens them eagerly on announcement. The shim consumes the
  vendor's LEVEL signal (`background_tasks_changed`, replace semantics) and
  its EDGE bookends without ever diffing the level or pairing edges into a
  retained set.
- Usage rides the `AgentActivity` ENVELOPE, on exactly ONE unit per API
  response (the first-block unit); absence means "not the carrying unit",
  never "free". `effort` follows the same rule.

### Keep-alives (entirely shim-internal)

- The daemon NEVER submits a keep-alive; nothing keep-alive-shaped exists on
  the wire (no PromptOrigin value, no rpc, no control-plane signal). The shim
  determines the keep-alive prompt text; keep-alives make real API calls, so
  their cost lands on the record plane (accounting) without announcement.
- THE YIELD OBLIGATION is the one-submitter invariant's guarantor: a real
  prompt rolls context back to just after the last real prompt, discarding
  trailing keep-alive turns before delivery, so no turn builds on keep-alive
  context.
- Keep-alive turns are first-class in the store as NEVER-SERVED: indexed so
  no page returns them and no activity from them is routed to the daemon
  (discarded turns are excluded from replay, never deleted).

### The permission gate

- ONE vendor gate exists (canUseTool): every tool passes through it, and
  AskUserQuestion is a tool riding the same gate. Mechanism shared, meaning
  not: a QUESTION's "allow" is answer transport, a PERMISSION's allow IS
  consent — two conversation.v1 units (`AgentQuestion`, `AgentPermission`),
  each with its own identity space (a permission's id IS the gated unit's
  `AgentActivityId` on purpose — consent joins to the work it gates; a
  question joins to no unit and has its own id).
- Shim emit points: `AgentPermission.start` from the gate callback (the
  vendor RENDERS the prompt sentence — title/displayName/description — plus
  optional trigger facts and the offered STANDING as a typed echo token);
  `success` from the shim's own resolve; `denied.by_policy` from the vendor's
  system permission-denied message (a denial with no open ask). The standing
  echo token round-trips through the daemon (the client only ever sees
  presence); a standing grant's `set_mode` can change the session's
  permission mode, restated authoritatively on WatchSession.
- An allowed tool then runs as an ordinary unit under the SAME identity; a
  denied tool never starts and has no activity frames.
  RULING (project lead, 2026-09-01, final): a denied tool has NO result;
  its unit settles `failure` with content UNSET (the producer observed no
  error content — the documented meaning) and settled_at stamped, and is
  drawn denied via its permission unit: AgentPermission's id IS the gated
  unit's AgentActivityId, and the permission unit already settles denied, so
  a consumer joins the tool unit's failure terminal to the permission unit by
  that shared id. Tool starts are NOT deferred (the tool_use block is on the
  stream before canUseTool fires; moving every start to the gate answer is
  rejected). No `denied` cause on AgentToolFailure — a landing-7 candidate
  only if the daemon lead reports the join awkward. The gate test asserts
  start → permission denied → failure(no content), no success/output.
- Answer validation is free: the shim already holds the pending callback
  while the agent blocks, so echoes are checked against the ask in hand.

### conversation/v1 — the vocabulary the shim produces (file map)

- `agent.proto` — WHAT A STREAM SAYS: `AgentFrame {agent_id; update | success
  | failure | detached_work}`, `AgentInput`, `AgentAnswer`, the turn-stop
  failure taxonomy (~16 arms incl. the two hook-stop arms — vendor-stated
  terminals, faithfully relayed), and the workflow stream family
  (`AgentWorkflowUpdate` = the whole subagent level, replace semantics).
- `agent_activity.proto` (the big one) — WHAT AN AGENT DOES: `AgentActivity
  {optional usage; optional effort; oneof item}` over ~30 unit kinds, each on
  the standard `start | (update) | (progress) | success | failure` shape with
  the shared `AgentToolFailure` payload: thinking, response, read, write,
  edit (write/edit results can gain a post-terminal `diagnostics` CONSEQUENCE
  arm — the vendor's IDE-diagnostics record carries no tool id; the shim
  joins by adjacency, one remembered last-write/edit unit), grep, glob, bash,
  subagent, skill_use (the unit settles on the DOCUMENT, which arrives after
  the tool's own acknowledgement), send_message (addressed string at start,
  resolved AgentId + queued_to_live|resumed_recipient at success — the
  `resumedAgentId` field is the only structured discriminator in the vendor's
  prose result), task acts, web_fetch, web_search, monitor, schedule_wakeup,
  artifact (the output is TYPED — read fields, never parse prose), plan mode,
  report_findings, worktree, cron, push_notification, hook, context_injected,
  unmodeled. Identities (`AgentId`, `AgentActivityId`) live here.
- `permission.proto`, `question.proto` — the two blocking units (above).
- `detached_work.proto` — the announcement vocabulary: `AgentDetachedWork
  {DetachedWorkId; detached {cause} | created}`, `DetachableWork {subagent |
  bash | workflow | monitor}`.
- `workflow.proto` — the run's DESCRIPTION only (start, script path — always
  persisted, placement local{run_id}|remote{session_url}, notice, summary);
  the stream vocabulary is agent.proto's.
- `session.proto` — SessionStarted/Runtime/Cold/ColdRemediation,
  SessionUpdate and its arm families, SessionDiagnostics (faults + degraded
  windows, kept since shim start), SessionKilled/Live, SessionContextUsage.
- `turn.proto` — `TurnId`, `AgentPrompt {id, agent, said}` (the ONE form of a
  delivered prompt: returned by StartTurn, persisted by the store, replayed
  by history), TurnKilled/TurnLive.
- `history.proto` — HistoryPage/Entry (`AgentPrompt | AgentFrame`)/Pointer.
- `user.proto` (`UserSaid` — ordered blocks), `content_blocks.proto` (text,
  image path|url, `UnsupportedBlock` — not a fallback), `api.proto` (the
  vendor API's own outcomes: `ApiRequestFailed` taxonomy, `AgentModel` /
  ModelOption / capabilities / effort, `TokenUsage`, `ModelMarker`),
  `slash_command.proto` (the SessionCommand enum + the ContextCut family —
  clear and compaction outcomes; compaction is OURS: daemon-directed,
  shim-implemented via a throwaway summarizing session).
- The fidelity principle governs throughout: vendor fields land even when no
  UI draws them, marked EXPECTED UNMAPPED at the field; typed always, never
  JSON-in-a-string.

### store/v1 — the shim as store producer (and reader)

- Callers are the shim and sidecar ONLY; the daemon must never import the
  package (enforced at codegen). The daemon's read path is shim.v1.
- `StoreEntry {plane (stream|file); write_id (dedup); upsert_key
  (producer-minted, opaque to the store — one row per key, a write supersedes
  it whole; the key↔identity mapping is the producer's); StoreAgentUpdate |
  SessionUpdate}`. Pageability is the PRODUCER's arm: a page line NAMES its
  book (`StorePageLine.page_agent_id`); unserveable material
  (`StoreUnservedItem`: keepalive | vendor_specific | unknown | unparsed)
  has no book; run frames (bash, workflow) are lifecycle rows, never page
  lines.
- Routing rule per wire arm: `update` → the entry table (a page line);
  `success`/`failure` → BOTH entry and the agent row's terminal columns, one
  transaction; `detached_work` → the lifecycle table for its kind, never a
  page line (the spawning call already is one). Every foreign key is stamped
  at insert with one lookup.
- `WriteBatch {producer; EntryBatch}`: success means DURABLE — records plus
  cursor advance in one transaction, replay absorbed by write_id; failure
  means NOTHING committed. THE SPILL IS REMOVED: failures hold in a bounded
  IN-MEMORY retry buffer; transient blips absorb silently, exhausted retries
  log what was lost LOUDLY (never a shim crash). Graceful stand-down WAITS
  FOR ALL ACKS before exiting — exiting with unacknowledged writes is the
  loud failure.
- `GetLiveWork` — the open-obligations verb: ids of everything with a start
  and no terminal, per the RECORD (a claim that cannot go stale). The SHIM
  (never the sidecar) calls it once at session start and resolves every
  item: re-adopt what the revived vendor process actually has (reported as
  `SessionStarted.live_work`), and WRITE the closing terminal for what did
  not survive (dual-write closes the record and puts the stop notice in the
  feed). Invariant: every started thing eventually gets a terminal row, by
  observation or by reconciliation.
- Reads the shim serves FROM: `OpenAgentSession` (first page + watch token) /
  `WatchAgentSession` (pure tail) / `ReadAgentPage` (older pages) — the
  open/watch bifurcation shim.v1's WatchAgent parallels; `GetWorkflow` (the
  run row + the DERIVED agent level — the level is stored nowhere, it is the
  join on `spawned_by`); `GetSidecarCursors` (sidecar-only recovery).
- Standing policy: the store is NUKED, never migrated — no backfill,
  hydration, or compatibility code, ever.

### Gotchas (each purchased once; do not re-derive)

- `forwardSubagentText` must be set or a subagent's prose and reasoning never
  reach the shim at all.
- Foreground shell output is observable NOWHERE while running (verified
  empirically; the persisted-output file materializes at exit, final size).
  `AgentBashUpdate` is structurally detach-only; every byte of detached
  shell output comes from the SIDECAR tailing the vendor's `tasks/*.output`
  spool (terminated by its `EXIT=<code>` line), not from any SDK route.
- The sidecar is the second producer: session/subagent transcripts, workflow
  journals, spools. Workflow agents' type/model/worktree come ONLY from the
  per-agent `agent-<id>.meta.json` beside each transcript — ingesting
  transcripts alone is insufficient; a workflow agent's prompt is the first
  user message of its own transcript. Nothing in a journal ever says the run
  finished — the run's terminal comes from the vendor's live-set membership.
- The vendor's `elapsed_time_seconds`/`heartbeat` are consumed, never
  forwarded (the start instant + progress beat replace them). The shim stamps
  start instants at announcement.
- Backgrounding causes for SHELLS are harvested from the BASH TOOL RESULT
  (`timedOutAfterMs`, `backgroundedByUser`), never from the task stream.
  This rule does NOT cover a backgrounded AGENT (detach of a foreground
  subagent — no client verb today): there the candidate producer IS the task stream
  (`task_updated{patch.is_backgrounded}`) — an implementation-wave
  observation confirms or refutes it (no observed sequence exists yet).
- KNOWN-OPEN, BEST-EFFORT ARMS (do not escalate; fill only if the wave finds
  a producer, otherwise leave unset): `AgentPermissionAbandoned` (the SDK
  declares no park deadline, so the arm may be unproducible),
  `AgentPermissionDeniedForWantOfDecider` (no declared discriminator),
  `SessionIdentityRotated.reason`, `ContextCompacted.
  cumulative_dropped_tokens` + `tools_before_cut`, `ContextCleared.tokens`,
  and `AgentEffortLevel`'s missing vendor `max`. Each is a landed
  declaration whose producer the vetting runs could not find; an unset one
  is expected, not a missed obligation.
- Grep/glob omitted figures are shim-subtracted (the vendor reports totals);
  hook duration and spawn depth are shim-derived.
- A subagent is one-run-per-input: it runs its commission to completion,
  persists as transcript + identity, and the next message RESUMES it as a
  new run. A live subagent takes a message at its next tool round; the shim
  absorbs any main-vs-subagent delivery asymmetry so the API stays one.
- Stop hooks: the sixteen turn-stop failure arms include the vendor's own
  hook-stop terminals; relay them faithfully — but the shim never synthesizes
  a turn terminal from hook activity.
- The transcript's vendor-session-id spelling can diverge from the runtime's
  answer (~22% of records); the runtime's answer rides
  `SessionStarted.vendor_session_id`, the divergence stays shim-side.
- Unset non-optional fields are illegal everywhere, immediately: error to
  the producer on requests, loud raise at the consumer on stream pushes;
  debug-log every logical branch (see the standing conventions (teamlead prompt) and proto comments §6).

## Kickoff increments and rulings (2026-08-29, project lead)

- LANDED: shim.v1 failure `kind`/`cause` arms on StartSession,
  SetSessionModel, SetSessionPermissionMode, Hibernate, KillSession,
  StartTurn, UpdateAgent, KillTurn, StopBash, DetachForeground, ReadHistory,
  plus SessionFault.kind; AgentUpdate gains `context_cut` and `api_error`
  page-line arms (corrected 2026-09-02 from the goldens: the shim produces
  `context_cut`; `api_error`'s only observed producer is the transcript's
  system:api_error record, so that page line is the SIDECAR's). Workflow verbs answer Connect
  Code.Unimplemented. A refused WatchBash/WatchAgent open closes at the
  transport.
- UPSERT KEYS (cross-plane, adopted with the store lead): `activity:<
  AgentActivityId>` (tool_use_id; `<message.id>:<block_index>` 0-based for
  text/thinking); `prompt:<TurnId>`; `question:<AgentQuestionId>` (the ask
  tool_use_id); `permission:<AgentPermissionId>`; `terminal:<AgentId>:<
  vendor record uuid>`; `bash:<run AgentActivityId>:<from_offset>` per output delta and `bash:<run AgentActivityId>:terminal` (amended 2026-08-29: one key per run made each delta supersede the last; both planes spell it this way); `session:<arm>:<vendor
  record uuid>`. write_id = sha256("<producer>|<source coordinates>|<
  discriminator>") hex; producer = "claude-shim:<original vendor session
  id>". Shim-synthesized session facts (diagnostics, context_usage) are
  never written to the store.
- KEEP-ALIVE MARKER: every keep-alive prompt begins with the literal
  `<!--agent-repl:keepalive-->`; the sidecar classifies that turn's records
  never-served until the next non-keep-alive prompt.
- R9 default: main AgentId = the conversation's ORIGINAL vendor session id
  (the shim lead settles the final rule against real file behavior). R15:
  the shim's AgentPrompt row is the ONE served prompt; it is durably acked
  before the turn's first activity frame. /agents and /help never reach the
  shim. The mock's spools go under `$AGENT_REPL_FAKE_SPOOL_ROOT` (default
  /tmp/claude-<uid>) and transcripts under `$CLAUDE_CONFIG_DIR/projects/
  <cwd-slug>/`. The one-time real capture run is APPROVED; the project lead
  dispatches it when the harness is ready.

- CROSS-SYSTEM PROCESS CONTRACTS (project lead, kickoff): one state root
  `$AGENT_REPL_STATE_DIR` (default ~/.claude-emacs); the daemon binds ONE
  loopback TCP listener serving Connect (HTTP/1.1 + h2c, binary + JSON) and
  the webapp assets on one origin, writes `127.0.0.1:<port>` to
  `$AGENT_REPL_STATE_DIR/daemon.addr` (atomic replace; removed on orderly
  exit; a joining successor writes it only after it owns every workspace);
  the webview URL is `http://<daemon.addr>/?workspace=<id>&dir=<dir>`
  (`&composer=1` only in dev mode); the shim is spawned as `node
  agent-shim/claude/shim/dist/main.js --listen <uds> --store-socket <uds>
  --log-fd 3 [--fake]` with CLAUDE_CONFIG_DIR, AGENT_REPL_OWNED=1,
  AGENT_REPL_STATE_DIR, SHIM_BUILD_SHA (tests add
  AGENT_REPL_FORBID_VENDOR_CALLS=1), cwd = the workspace; session facts
  travel only in StartSession; readiness = the first healthy `diagnostics`
  push on WatchSession; the store serves on ~/.cache/agent-repl/sock/
  store.sock (tests: env AGENT_REPL_STORE_SOCKET, a flag beats it); kernel
  locks live in ~/.cache/agent-repl/run/ — `workspace-<md5hex(symlink-resolved
  abs dir)[:8]>.lock` (shim-held from StartSession to process death, per the
  2026-09-02 relay ruling below; the daemon probes ONLY this one,
  flock LOCK_EX|LOCK_NB) and `session-<vendor session id>.lock` (taken
  inside StartSession; pre-minted on a fresh start); proto/vocab/
  render-colors.json + paint-classes.json are the daemon's, consumed by
  webapp and Emacs; Go modules pin connectrpc.com/connect v1.17.0 and
  golang.org/x/net v0.43.0 (Go 1.24 on this machine; every module stays
  `go 1.23`).

## The mocked vendor (`--fake`) — file layout and cross-plane rulings (shim lead)

The mock writes vendor-shaped files exactly where the real binary would, so the
REAL sidecar ingests them. IMPLEMENTED (mock agent, 2026-08-29): the layout
below is what `src/fake/vendor-files.ts` writes, and every field named here is
observed in `testdata/corpus` unless marked otherwise.

- `$CLAUDE_CONFIG_DIR/projects/<cwd-slug>/<vendor-session-id>.jsonl` — the
  session transcript. Two KINDS of line, and the distinction is load-bearing:
  - CHAINED records (`user`, `assistant`, `system`, `attachment`) carry
    `parentUuid`, `isSidechain`, `uuid`, `timestamp`, `userType: "external"`,
    `entrypoint: "sdk-cli"`, `cwd`, `sessionId`, `version`, `gitBranch`. A user
    prompt adds `promptId`, `permissionMode`, `promptSource: "sdk"`; a tool
    result adds `toolUseResult` and `sourceToolAssistantUUID` (and
    `toolDenialKind` when a rule refused it); an assistant line adds `requestId`
    and `effort`.
  - UNCHAINED metadata records (`queue-operation`, `mode`, `permission-mode`,
    `ai-title`, `last-prompt`, `pr-link`, `frame-link`, the file-history pair)
    carry NO `uuid` and NO `parentUuid` and never advance the chain. Writing one
    through the chained path would both invent fields and orphan the next
    record.
- `$CLAUDE_CONFIG_DIR/projects/<cwd-slug>/<vendor-session-id>/subagents/agent-<agent-id>.jsonl`
  plus `agent-<agent-id>.meta.json` beside it. The meta sidecar carries EXACTLY
  four camelCase fields — `agentType`, `description`, `toolUseId`, `spawnDepth`
  (corpus: `sidechain/agent-aef975b7bc3422d4b.meta.json`). There is NO `model`
  field: the earlier spelling in this document (`agent_type, description,
  tool_use_id, spawn_depth, model`) was snake_case and named a fifth field the
  corpus does not have. Sidechain records add `agentId` and set
  `isSidechain: true`, and keep their OWN parentUuid chain, independent of the
  session transcript's.
- `<spool-root>/<cwd-slug>/<vendor-session-id>/tasks/<task-id>.output` —
  `b<hex>` shell spools written INCREMENTALLY (one append per chunk, so the
  sidecar's tail observes growth events) and terminated by an `EXIT=<code>`
  line; `a<hex>` AGENT spools, which are the agent's own JSONL transcript and
  carry NO terminator. A shell run that never ends leaves no `EXIT=` line at
  all — the corpus's `spools/bash-midoutput.output` shape. A `stopTask` writes
  `EXIT=143`.
  Task-id shapes are the vendor's: 9-character base36 for a shell
  (`b86pl7ir1`), 17-character hex for an agent (`a0cbd94e5da2d662d`). A detached
  agent's task id IS its agent id, which is what makes the spool, the transcript
  file and the task stream address the same thing.
- `<cwd-slug>` = the absolute cwd with EVERY byte that is not `[A-Za-z0-9]`
  replaced by `-` — underscore included, case preserved, existing dashes
  untouched (verified against the live `~/.claude/projects` tree). Observed:
  `/Users/x/.config/y` → `-Users-x--config-y`;
  `/private/var/folders/_m/x` → `-private-var-folders--m-x`. The same rule names
  the subagent directory beside the transcript.
- ROTATION (`/clear`). The retired identity's file is CLOSED, never truncated or
  moved — a resume of the old id must still load — and gets one closing system
  record before the identity moves. THE CORPUS HAS NO `/clear` RECORD: it
  carries `compact_boundary` for compaction and nothing for a clear, so the mock
  writes a `compact_boundary`-shaped record with `content: "Conversation
  cleared"` and emits the DECLARED `conversation_reset` message plus a fresh
  `system:init` on the stream. Flagged as declared-not-observed.
- RESUME. The writer reads the existing transcript's last CHAINED record and
  adopts its uuid as the chain head, so a resumed session's first record names a
  parent a reader can resolve. Unchained metadata lines are skipped when looking
  for that head, as is a half-written trailing line (what a killed CLI leaves).
- Shapes come from `testdata/corpus` and the pinned `sdk.d.ts`, and are rebuilt
  from the real captures once the capture run lands.

Cross-plane rulings (shim lead, 2026-08-29, relayed to the store/sidecar lead):

- BLOCK INDEX IS PER MESSAGE. `<message.id>:<block_index>` counts blocks
  0-based in the ORDER OF THE ASSISTANT MESSAGE, across every transcript line
  that shares the message id when the vendor splits one message into several
  lines each holding one block. Text and tool_use blocks of one message thus
  get distinct ids on both planes, and the usage carrier is the message's
  FIRST block (index 0) whichever line it lands on. The mock writes split
  lines the way the real binary does (one block per assistant line, same
  `message.id`) — AND splits the SDK side identically, emitting one `assistant`
  message per block sharing the id, each with its own uuid and the SAME usage
  object. That symmetry is what lets the fold pick block 0 on either plane.
- `deferred_tools_delta` and `agent_listing_delta` attachment records are
  VENDOR_SPECIFIC RESIDUE — `StoreUnservedItem.vendor_specific{kind:
  "attachment/<type>"}` (e.g. `attachment/deferred_tools_delta`), the exact
  kind spelling the sidecar uses so both planes' residue agrees — never
  `context_injected`: that unit carries instructions the model reads (memory
  files, skill documents), while a tool-availability delta and an agent-type
  listing are vendor bookkeeping about what the model MAY call. Both planes
  drop them from every page.
### R9, SETTLED (engine agent, evidence below)

The main agent's `AgentId` is the conversation's ORIGINAL vendor session id.
The final rule, one case at a time:

1. FRESH START. The shim pre-mints a uuid, passes it as `Options.sessionId`,
   and adopts it as the AgentId. It is persisted at
   `$AGENT_REPL_STATE_DIR/shim/<workspace-key>/agent-id.json` (atomic replace,
   `{original_vendor_session_id, workspace_key, minted_at_ms}`) BEFORE the
   query is created, so a crash between mint and first record still leaves the
   identity recoverable.

2. RESUME. The resume id IS the original id, and a FILE-ONLY READER CAN DERIVE
   IT. Therefore, when `agent-id.json` is absent on a resume, the shim ADOPTS
   the resume id as the original and persists it; that is a derivation, not a
   guess, and it is logged as one.

3. ROTATION AND FORK. FILES ALONE DO NOT SUFFICE, so the shim writes the link
   they lack: one pointer file per rotated id at
   `$AGENT_REPL_STATE_DIR/shim/<workspace-key>/vendor-id/<vendor-session-id>.json`
   holding `{vendor_session_id, original_vendor_session_id, linked_at_ms}`.
   `engine/identity.ts:resolveOriginal(stateDir, workspaceKey, vendorSessionId)`
   is the file-plane reader: a rotated id answers from its link file, an
   unrotated one answers itself, and neither costs more than one stat. The
   AgentId never moves; `SessionIdentityRotated{previous, new}` is pushed and
   `reason` stays unset (no declared producer).

EVIDENCE, ranked:

- STRONGEST — the real file tree (1,107 transcripts under
  `~/.claude/projects/*/*.jsonl`, scanned whole): every file's `sessionId`
  equals its filename (0 mismatches), and exactly one file carries more than
  one session id (a synthetic fixture with a placeholder uuid). A resume
  therefore appends to the SAME file under the SAME id. The union of all 92
  distinct keys appearing anywhere in those files contains NO lineage field —
  no `forkedFrom`, `parentSessionId`, `resumedFrom`, `originSessionId` or
  equivalent. (`origin` is `{kind: "human"|"task-notification"}`; `source` is
  an api-error retry reason; neither names a session.)
- STRONG — compaction is IN PLACE: a `system`/`compact_boundary` line carries
  the UNCHANGED `sessionId` and links to its predecessor record with
  `logicalParentUuid`. Compaction therefore never rotates an id.
- STRONG — `sdk.d.ts`: `ForkSessionResult` is `{ sessionId }` alone, and
  `SDKSessionInfo` (what `getSessionInfo` returns) has no parent or ancestor
  field. Nothing the SDK can tell a file-only reader names a predecessor.
- THE RUNTIME EXCEPTION — `SDKConversationResetMessage` carries BOTH
  `session_id` and `new_conversation_id`, so the link exists exactly once, in
  the stream, at the moment of rotation. That is why the shim must capture it
  then and write it down: nobody can recover it afterwards.

ROTATION, OBSERVED (real capture identity-rotation-clear, 2026-09-01): at
/clear the stream carries ONE `conversation_reset` whose `session_id` is the
OLD id and whose `new_conversation_id` is an id NOTHING later uses; the id
the session rotates TO is the `session_id` of the SECOND `system:init` that
follows (a third uuid), repeated by every later turn's init. On disk a new
transcript file appears under that init id; the old file simply stops (no
closing record — the mock's "closing system record" was declared-not-observed
and is dropped). Rule: `SessionIdentityRotated.new` = the post-clear init's
session_id; the link file is written for that id; `new_conversation_id` is
never adopted as an identity.

CONCLUSION FOR THE FILE PLANE: files alone suffice for resume and compaction;
they do NOT suffice for rotation or fork, and the shim-written pointer file
above is the smallest link that closes the gap. No store verb is involved.

### Where the sixteen failure arms actually live (mock agent evidence)

`sdk.d.ts` declares FOUR `result` error subtypes — `error_during_execution`,
`error_max_turns`, `error_max_budget_usd`,
`error_max_structured_output_retries`. Every finer stop is a `TerminalReason` on
the result: `blocking_limit`, `rapid_refill_breaker`, `prompt_too_long`,
`image_error`, `model_error`, `api_error`, `malformed_tool_use_exhausted`,
`aborted_streaming`, `aborted_tools`, `stop_hook_prevented`, `hook_stopped`,
`tool_deferred`, `max_turns`, `background_requested`, `completed`,
`budget_exhausted`, `structured_output_retry_exhausted`,
`tool_deferred_unavailable`, `turn_setup_failed`. A converter keyed on `subtype`
alone can reach four of the sixteen conversation.v1 arms; the pairing is the
contract.

`AgentContinuationPrevented` has NO `TerminalReason` of its own. The two
declared prevent-continuation signals are
`SDKInformationalMessage.prevent_continuation` and the transcript's
`system:stop_hook_summary.preventedContinuation`; the mock emits both beside a
`stop_hook_prevented` terminal. UNSETTLED — a real capture may show otherwise.

### Session facts with no message behind them (mock agent evidence)

`sdk.d.ts` declares NO system message for `model_changed`,
`permission_mode_changed`, `fast_mode`, `mcp_server`, `account_usage` or
`context_budget_warning`. Those `SessionUpdate` arms are produced by the shim
from CONTROL ANSWERS and from fields riding other messages:

- model change → the next assistant message's `message.model` (the mock also
  emits the declared `session_state_changed` beat, which carries no model).
- permission mode → `SDKStatusMessage.permissionMode`.
- fast mode → `result.fast_mode_state` / `fast_mode_disabled_reason`, and
  `init.fast_mode_state`.
- mcp servers → `query.mcpServerStatus()`.
- account usage → `query.usage_EXPERIMENTAL…()`, whose four unavailable shapes
  are distinct on the wire: `rate_limits: null` (service), a null WINDOW, a null
  `utilization` inside a present window, and `behaviors: null` (the local scan).
- context budget → UNSETTLED, and ON THE CAPTURE RUN'S CHECKLIST (ruling,
  landing 5). It was read as the vendor's `context_tip` ATTACHMENT record, but
  the one real `context_tip` capture is a GENERIC `/goal` tip — so the shim
  records `context_tip` as itself (residue `attachment/context_tip`) and only a
  record whose own type says `context_budget_warning` becomes the warning. Which
  record the vendor really uses is a capture-run question.
  CAPTURE RUN RESULT (2026-09-01, read from the 67 goldens): NO capture
  carries a `context_tip` or any budget-warning attachment (the scenario ran
  to success without one); the only token-budget carrier observed is a
  `total_tokens_reminder` attachment (`text: "<total_tokens>N tokens
  left</total_tokens>"`, once). The spelling is an OPEN EVIDENCE GAP — the
  `context_budget_warning` producer (sidecar plane) stays marked ungrounded,
  never guessed. Also recorded as negatives from the same run: no failed
  subagent, no compaction summary line, no /clear rotation record (the last
  two because the harness closed the query before the later turns — fixed,
  re-capture owed).

### The e2e mock additions, and what they found (mock agent evidence)

Five families joined the mocked vendor for the e2e suite. Each is a registered
`!name` in the published table; what they found while being written:

- `!context-usage-drift` — `getContextUsage()` answers a GROWING occupancy from
  the turn it runs on: the total, the percentage, the message category and the
  whole `messageBreakdown` move together, and the answer stays a full
  `SDKControlGetContextUsageResponse`. The FIXED costs (system prompt, memory
  files, MCP tool list) deliberately do not move. CADENCE IS THE ENGINE'S:
  `context_usage` is pushed at session start and at every turn end regardless of
  scenario, so a scenario can only change WHAT is sampled, never WHEN.
- `!fault-converter` / `!fault-recover` — the shape the fold demonstrably
  refuses is a `system:hook_started` whose `hook_id` is PRESENT AND EMPTY:
  `hook_id` IS the firing's activity identity, `convert/ids.ts` refuses an
  identity stated as nothing, and the fold's catch turns that into
  `StoreUnparsed{source: "system/hook_started", parse_error: "converter
  defect: …"}` and NO frame. Asserted against the real fold in
  `test/fake/scenarios/failures.test.ts`.
  - GAP FOR THE ENGINE, not the mock: nothing turns a fold defect into a
    `SessionFault.converter_defect` or opens a degraded window. The only
    producer of `converter_defect` today is `store/writer.ts`'s constructor,
    which is never called with that arm, and `store_unreachable` is the only
    fault the writer actually emits. Until the engine subscribes to the fold's
    defects, `!fault-converter` produces the defect and diagnostics stay
    HEALTHY.
- `!model-fallback` — an UNSOLICITED, PERSISTENT swap: the declared
  `model_refusal_fallback` message (`direction: "retry"`, both models, an empty
  `retracted_message_uuids` because nothing had been delivered), the
  `session_state_changed` beat, and then the answer whose `message.model` is the
  fallback. It sticks, so a following turn answers on the fallback too, and no
  `SetSessionModel` was called.
  - GAP FOR THE ENGINE: `model_changed` is an engine-owned arm produced from
    `init.model` and from `SetSessionModel`'s own `applyModel`. Nothing watches
    the next assistant message's `message.model`, so an unsolicited fallback
    currently produces no `model_changed` push.
- `!fast-on` / `!fast-off` / `!fast-cooldown` — the state now STICKS on the
  session, so it is reported in BOTH declared places: every later `result` and
  the `init` a rotation emits. A reason is cleared when fast mode is on, which is
  what `sdk.d.ts` documents (the field is absent when nothing blocks it).
  - GAP FOR THE ENGINE: `fastMode` is in the engine's OWNED_ARMS, so the
    fold-produced `fast_mode` update from `init` is DROPPED, and nothing in the
    engine pushes one. WatchSession's `fast_mode` arm has no producer yet.
- `!usage-full` — the same `available` shape as `!usage-available` under the name
  the e2e roster spells: every window populated (`five_hour`, `seven_day`,
  `seven_day_oauth_apps`, `seven_day_opus`, `seven_day_sonnet`, `model_scoped`,
  `extra_usage`), each with a `utilization` and a `resets_at`, beside
  `subscription_type`. The older name was kept rather than renamed so no caller
  that already spells it breaks.
- `!usage-window-unavailable` nulls the FIVE-HOUR window, and nothing else
  (shim lead ruling): `SessionUsageWindowUnavailable` means "the service
  answered without a five-hour window", so a null `seven_day_opus` is merely an
  ABSENT OPTIONAL window and still produces the AVAILABLE arm. That absent-opus
  shape is worth having and keeps its own row as `!usage-opus-absent`;
  `!usage-utilization-unavailable` nulls `five_hour`'s utilization while the
  window itself is present, so the null WINDOW and the null FIGURE stay
  distinguishable.

## Landing 3 relay (2026-08-29, project lead)

- StartTurnSuccess.page is the opening page (required; empty page ≠ absent).
- UpdateAgent to a subagent with a prompt refuses `not_deliverable`; DetachForeground on a live, detachable unit that the SDK cannot detach refuses `unsupported` (never `not_detachable`).
- AgentId minting rule is binding and in the proto comment: main = original vendor session id; subagent = spawning call's tool_use_id.
- R9 settled as proposed: rotation/fork link file `$AGENT_REPL_STATE_DIR/shim/<workspace-key>/vendor-id/<vendor-id>.json` → {vendor_session_id, original_vendor_session_id, linked_at_ms}; absent file ⇒ the id is its own original.
- ReadAgentPage lines now carry pointers (StoreLineAt); placeholder marks go.
- store.v1 WatchBashRun serves the sidecar-written bash rows; WatchBash serves a run from it when the shim did not write the run itself.
- The detached-work announcement is a page line keyed `detached:<work id>`; the run's rows live in the lifecycle table only.
- Producer string `claude-shim:<original vendor session id>`.

## Landing 4 relay (2026-08-29, project lead)

- SessionUpdate.rate_limit_status maps the SDK's `rate_limit_event` (seconds→ms, fraction→percent, presence never sentinels).
- SessionUpdate tag 24 retired; the budget warning is the sidecar's AgentUpdate page line.
- SessionStarted.live_work always announces `created`-origin; DetachedWorkId.value == the unit's AgentActivityId.

## Landing 6 relay (2026-09-01, project lead)

- No shim.v1 or conversation.v1 change. For awareness: SubmitPromptError.turn_already_open (daemon side) is retired because YOUR UpdateAgentFailure refusal of a busy subagent is the one producer; keep that refusal typed and named.

## Landing 7 relay (2026-09-02, project lead)

Adapt to protos ab7e681f2 / bindings c10714a41 (see PROTO-CHANGES.md):
- StartSessionFresh.model is optional: UNSET = pass no model to the SDK;
  SessionStarted.effective_model reports what the SDK chose.
- WatchSessionResponse is `oneof frame {update; session_started}`: on EVERY
  new watch, right after the opening diagnostics, send the session's
  original SessionStarted recovered from shim state (turn_in_flight and
  live_work reflect NOW, not the original start).
- UpdateAgentFailure.agent_busy: an UpdateAgent{prompt} to a subagent whose
  own turn is running is refused with this arm (replaces the interim
  not_deliverable / detail-only answer).
- OpenAgentSessionFailure.unknown_agent (store): map to Code.NotFound;
  retire the interim shim-side unknown-target rule once the store produces it.

## Landing 8 relay (2026-09-02, project lead)

- No shim.v1 / store.v1 / conversation.v1 change in landing 8 (frontend.v1 only).

## Landing 10 relay (2026-09-04)

- No shim change: every landing-10 shape is a frontend/agentrepl relay of a
  fact the shim already states (AgentSendMessageSuccess arms,
  AgentPermissionDenied.undecidable, SessionQueryDied.cause,
  SetSessionModelFailure.cold).

## Landing 9 relay (2026-09-03)

- No shim.v1 change (the daemon relays the existing StartSessionFailure arm).
