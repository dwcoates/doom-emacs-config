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

## Contract context (for implementers)

Everything below is standing fact about the frozen contract, distilled for shim
implementers. The protos under `proto/src/` are the authority and their comments
are the authoritative documentation — read the files you work on; this section
gives the map and the ideas. Cross-cutting rules (identity spaces, echo tokens,
response/stream conventions, presence rules, the fidelity principle, the exempt
set, implementation/validation/logging conventions) live in
`docs/protobuf-design/digests/conventions.md` and are NOT restated here.
Implementers never change protobufs; a needed change is a request up the
orchestration chain.

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
    callback it already holds — no new state).
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
  turn`), kept solely so `KillTurn` can name its transitive refusal set; never
  on the wire except inside a refusal.

### shim/v1 — the service the shim serves (16 rpcs, four sections)

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
  - `SetSessionModel`: resolves AFTER the current turn ends (one model per
    turn, a deliberate departure from the SDK's mid-turn setModel); refused
    IMMEDIATELY with `SessionCold` when context exceeds the request's
    threshold (daemon policy per call; the shim only measures) and no
    remediation is named. A model switch is a cold cache (cache is per model).
  - `SetSessionPermissionMode`: session state conditioning every gate.
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
  - `KillTurn {turn, force}`: the main agent and everything THIS turn
    spawned, transitively (via the spawn-provenance map); refuses naming live
    work unless forced.
- DETACHED WORK — one stream per live item; bespoke verbs because the write
  surface differs per kind:
  - `WatchBash` / `StopBash` (a process's only input is stop).
  - `GetWorkflow` (unary open: the run's description plus either the watch
    token — inside the `live` arm only — or the embedded terminal) /
    `WatchWorkflow` (token-addressed tail: stateless subagent LEVELS with
    replace semantics, then the terminal; the run's agents are then watched
    individually via WatchAgent — the daemon owns the fan-out) /
    `StopWorkflow` (the run's only addressable act).
  - `DetachForeground {AgentActivityId}`: Ctrl-B — moves in-flight turn work
    onto its own stream; the turn announces the detachment and the consumer
    opens the matching Watch.
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
- Backgrounding causes are harvested from the BASH TOOL RESULT
  (`timedOutAfterMs`, `backgroundedByUser`), never from the task stream.
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
  debug-log every logical branch (see conventions.md §6).
