DERIVED from figma-to-idl-redesign.md at 86fd2b543 — the canonical record WINS on any conflict. Do not edit; regenerate.

# THE SHIM — derived digest

Everything in the canonical record that bears on the shim: the `shim.v1`
service it serves, the `conversation.v1` frames it produces, the SDK/vendor
boundary it adapts, the identities it mints, the start/update/progress rules
it obeys, how it observes detached work, its keep-alive obligations, the
exempt set, the fidelity principle, what it writes to `store.v1`, and the
no-variable-state principle that bounds all of it.

---

## 1. WHAT THE SHIM IS, AND WHAT IT IS NOT

- The shim is the VENDOR ADAPTER and the boundary owner: it drives the SDK, converts the
  vendor's records into `conversation.v1`, serves `shim.v1` to the daemon, and writes
  `store.v1`. It is deliberately "very stupid".
- It is NOT allowed to be smart. The recursion retraction ruled that maintaining subagent-of-subagent relationships would make the shim "no longer a simple vendor-adapter and
  message recorder"; that state belongs in the DOM (frontend) and the daemon, never in the
  middle.
- It is NOT a queue. The daemon is the only queue and the only submitter of real prompts;
  the vendor's own prompt queue is never ours to manage.
- It is NOT an accumulator. `update` frames carry deltas; gluing, coalescing or buffering
  fragments is the daemon's choice, not the shim's.
- The vocabulary is fixed: "the agent binary" = the Claude Code engine (prompt queue,
  permissions, task lifecycle, compaction, plugins, OAuth all live there); "the SDK" = the
  thin npm wrapper `@anthropic-ai/claude-agent-sdk` that SPAWNS the binary over stdio; "the
  vendor" = the Claude-vs-other axis. Bare "CLI" is retired as ambiguous, and sweeping it
  from the shim's AGENTS.md and proto docs is owed work.

---

## 2. THE THREE CORE PRINCIPLES THAT BIND THE SHIM

### 2.1 A subagent is handled EXACTLY as the turn is

- Stated as project-wide FACT: subagents are internally handled exactly the same as the
  turn, from the daemon's perspective AND on the daemon↔shim API.
- The WRITE surface to any live agent is ONE type, `AgentInput`; requests differ only in the
  ADDRESS.
- The READ surface is likewise one type: `AgentFrame` is the frame of the turn's stream and
  of a subagent's stream. One consumer handler serves both.
- Prompt QUEUING is daemon-side and kind-agnostic; the shim's steer-at-next-tool-round
  mechanism sits underneath and is never learned by the consumer.
- THE ONE ASYMMETRY, absorbed inside the shim: a LIVE subagent accepts a message at its next
  tool round without being stopped (observed); whether the main agent does is UNVERIFIED.
  Ruling: keep ONE API and resolve the inconsistency INSIDE THE SHIM — `prompt` to a turn
  and to a subagent may land differently, and the consumer never learns which.
- What a subagent IS, verified: spawned by an agent's `Agent` call with a commission, runs
  it to COMPLETION, then the task ends; it persists only as transcript + identity; the next
  message RESUMES it as a new run. Analogous to a turn — one run per input. The ONLY vendor
  input route to a subagent is another agent's `SendMessage` tool; no SDK route lets a human
  address one directly, so "click a subagent and prompt it" has no vendor path — the shim
  must relay or drive the resume itself.

### 2.2 Sessions and turns OUTLIVE the daemon — spawn / attach / end are decoupled

- SPAWN is unary and returns (or adopts) an identity the daemon persists (`StartSession`,
  `StartTurn`). A Watch never creates anything.
- ATTACH is a Watch stream that creates nothing and ENDS NOTHING. A consumer closing it —
  gracefully (a shutting-down daemon must) or abruptly — leaves the work running and the
  information accumulating; the shim tolerates both identically.
- END is a Kill that refuses while work is live unless forced, and NAMES what it killed. No
  `CloseXConnection` verbs exist: cancelling the Watch IS the graceful close in Connect.
- Consequence for the bounded-stream rule: "a client-side close is a transport failure"
  binds the PRODUCER side only. A stream the PRODUCER ends without a terminal frame is the
  failure.

### 2.3 The shim (and store and sidecar) hold NO VARIABLE-SIZE STATE

- The rule: every observation costs a CONSTANT number of single indexed lookups. A single
  parent-id lookup is static and fine; a VARIABLE number of lookups (walking lineage) is
  not. The DAEMON is exempt — it is the one component allowed to hold state.
- Static and therefore permitted, audited explicitly: tool return → its call by
  `tool_use_id`; skill document → its call by `sourceToolUseID`; a created agent → its
  spawn; a re-announced start's ORIGINAL instant → by unit id (read back from the store, not
  held in memory); the turn's last response; the keep-alive rollback point; a page of an
  agent's children; one pending ask per agent; the detached handle↔identity mapping
  (`task_started` carries both).
- Ruling 1 — lineage is a DENORMALIZED ROOT, never a walk. Every stored row is stamped at
  insert with its top-level ancestor by ONE lookup of the parent's stored root. `KillTurn`'s
  transitive set was recorded as "the one bounded exception"; that exception is WITHDRAWN in
  favor of the stamp. Verified necessary: the vendor names a task's owning AGENT but never
  its owning TURN.
- Ruling 3 — `update` frames carry DELTAS, never cumulative text. The accumulator moves to
  the daemon.
- The ONE bounded state exception that survived in an earlier entry — `task → spawning call
  → turn` spawn provenance kept for kill purposes — is superseded by Ruling 1's denormalized
  stamp; only the frame-level mapping the shim already owns remains.

---

## 3. THE `shim.v1` SERVICE

### 3.1 Stage-4 shape and walk

- `shim.v1` is designed as a Connect RPC SERVICE, exactly like `agentrepl.v1`:
  `service.proto` + one `endpoint_*.proto` per rpc + shared-vocabulary files. The old file-by-file walk (`core` → `entry-delivery` → `message-page` → `external` → `bookkeeping`) was
  RETIRED for this stage; the record files were walked as vocabulary when an endpoint needed
  them.
- FOUR SECTIONS settled: SESSION, TURN (later AGENT), DETACHED WORK, HISTORY.
- Two proposed sections dissolved: PERMISSION is not a section (an ask belongs to whichever
  unit is running a tool, so the ASK is a frame on that unit's stream and the ANSWER a call
  addressed to it); MODEL is not part of a turn (`setModel` changes SUBSEQUENT responses and
  is callable mid-turn, `setPermissionMode` is for the current session — both are SESSION
  state).
- There is NO standing live-conversation stream. Live content rides the agent stream and the
  detached-item streams; what remains of "conversation" is HISTORY, pulled and bounded. A
  real departure from the old standing `Subscribe`.
- Final file model: `service.proto`, eighteen `endpoint_*.proto`, and `prompt_origin.proto`
  as the one shared file. The five old files (`core`, `bookkeeping`, `external`, `entry-delivery`, `message-page`) are DELETED. `ModelMarker` and its literal extension moved
  VERBATIM to `conversation/v1/api.proto` beside `AgentModel`.

### 3.2 The bounded-stream convention

- Every frame of a stream that CONCLUDES is a one-level `oneof result { update | success |
  failure }`. Flattened, not nested, for consistency with the feed-row ruling.
- A conclusion is a MESSAGE the producer sends, never the stream merely stopping. A stream
  ending WITHOUT a terminal frame is a transport failure and is read as one.
- Scope: bounded streams only. STANDING streams (`WatchSession`, `WatchAgent`'s tail) have
  no terminal arm — giving one would invent an ending they do not have.
- NO KEEPALIVE FRAMES ANYWHERE. The convention was retracted whole: source-of-truth silence
  is the DAEMON's to observe, a dead pipe fails at the connection, and a wedged publisher is
  a daemon-internal fault its own watchdog surfaces. Putting the detector in every client
  was the same client-derivation failure the redesign removes elsewhere.
- Every response is `oneof result { <Method>Success | <Method>Error }`; error arms are
  DERIVED from real refusal sites, never invented; unhealthy is an ANSWER inside success,
  never an error (the domain-outcome rule).

### 3.3 The AGENT section (the consolidation)

- `StartTurn` is the MAIN AGENT's verb and returns the FIRST PAGE: request carries turn id,
  said content, origin, page size and an optional `known_through`; success returns the
  delivered `AgentPrompt` plus a `HistoryPage`. One call paints and submits, and
  `prompt.agent` is the `WatchAgent` address.
- `WatchAgent` is ONE standing stream for any agent (`optional target` UNSET = the session's
  prompt thread): its FIRST frame is the opening page (full repaint, or only entries newer
  than the caller's mark), its tail one pointered entry per write.
- `UpdateAgent { optional target; AgentInput }` — stop and answer only. A PROMPT to an
  existing agent (a subagent, a workflow's agent) rides UpdateAgent per the later correction
  that restored `AgentInput.prompt`; delivery per kind is the shim's.
- DELETED by the consolidation: `WatchTurn`, `UpdateTurn`, `WatchSubagent`, `UpdateSubagent`
  and their files. "Main agent" leaves the API entirely — it survives only INSIDE the shim
  and as the store's scope.
- `ReadHistory` went NEXT-ONLY, then REGAINED its `first` arm: a subagent bubble painted
  from scratch had no first-page verb. Cold-paint sequence: `ReadHistory(first)` →
  `WatchAgent` with the page's newest pointer as `known_through`, pinning the tail to
  exactly after the page.
- ONE TURN IN FLIGHT, STRUCTURALLY — now stated per-agent. A second start while a stream is
  open is a DAEMON FAULT: refused, never queued.
- Success is EMPTY on the answer path on purpose: the ask's new state arrives as its next
  frame on the agent's stream; restating it would be a second authority.

### 3.4 The SESSION section

- `StartSession` (was `OpenSession`) — request is `oneof source { fresh | resume }`, with
  model and permission mode INSIDE the `fresh` arm and `resume` carrying the vendor session
  id plus an optional cold remediation. Success wraps `conversation.v1.SessionStarted`.
- THE HIGH-LEVEL CONTRACT: StartSession RESOLVES WHEN A PROMPT CAN BE ACCEPTED. It returns
  only facts fixed at handshake; anything that changes later is `WatchSession`'s.
- A COLD CONTEXT IS REFUSED, NEVER SILENTLY PAID. The shim must not load a lapsed context
  into the model unasked: a bare resume of a cold session FAILS carrying `SessionCold`
  (context tokens, last request instant, the TTL that lapsed, the requested model), and the
  daemon reopens naming a `SessionColdRemediation { pay | clear | compact }`.
- VERIFIED EMPIRICALLY, because the protocol rests on it: `query({resume})` makes NO API
  call and emits NOTHING — a resumed probe sent no prompt for 15s and produced zero
  messages, not even `system/init`; `init` arrived only WITH the first prompt, and the first
  assistant usage was cache_read 0, cache_creation 23,683. So the shim CAN refuse before any
  cost, and "can accept a prompt" is the SHIM's readiness, not a vendor signal. Compaction
  is therefore a cold read PLUS output, never the cheap path.
- `SESSION_SOURCE_COMPACT_CONTINUE` was OURS, not a vendor mode; the SDK has only `resume`
  and `continue` (most-recent-in-cwd, never needed).
- COMPACTION IS OURS: daemon-directed, SHIM-IMPLEMENTED. The request names the MODEL that
  writes the summary and the SCOPE; the shim runs a throwaway session that summarizes, then
  initializes the real session from the compacted transcript. OWED (vetting): whether the
  SDK can initialize a session from a transcript we wrote (`sessionStore` is @alpha;
  `forkSession` and writing the binary's JSONL are the other candidates), and whether our
  bookkeeping survives the vendor's `compact_boundary` format.
- The compaction scope is ONE canonical enum `conversation.v1.SessionCompactScope { ALL |
  PROMPTS | RESPONSES }`; the earlier four-arm oneof reshaped and PROMPTS_AND_RESPONSES
  DIED.
- KEEP-ALIVES BEGIN BEFORE SUCCESS IS RETURNED — a producer obligation stated at the
  message. A warm context stays warm while the user types; a `pay` resume takes its cold
  read at open, in the background, rather than on the first prompt. The CADENCE has started;
  the read need not have finished.
- RESUME RECOVERS MODEL AND MODE. Every `user` record in the JSONL carries `permissionMode`
  (4,722 occurrences) and every `assistant` record carries `model`; the SDK does NOT RESTORE
  either (a resume with no options ran on the default model). So both are recorded, neither
  restored, and the SHIM reads the last of each back and passes them; the answer reports
  what was recovered.
- `SetSessionModel` — request carries the model, a `cold_threshold_tokens` the REQUEST
  names, and an optional remediation. Two rules: it RESOLVES AFTER THE CURRENT TURN ENDS (a
  turn is answered by one model throughout — a deliberate departure from the SDK's mid-turn
  `setModel`), and it is REFUSED IMMEDIATELY, before any waiting, when the context exceeds
  the named threshold, because the switch re-reads everything at full price. The threshold
  rides the request because it is DAEMON POLICY; the shim only measures. A model switch is a
  COLD CACHE (a cache is per model), so `SessionCold` carries `oneof reason { lapsed {
  cache_ttl_ms } | model_switch }`, firing only when the requested model differs from the
  transcript's last.
- `SetSessionPermissionMode` — request is the mode; `SessionUpdate` gains
  `permission_mode_changed` because a standing grant's `set_mode` can change it too and one
  authoritative statement is needed.
- `WatchSession` — empty request, STANDING stream of `SessionUpdate` (identity_rotated,
  query_died, model_changed, fast_mode, mcp_server, account_usage, later
  context_budget_warning). `BookkeepingEntry` is DEAD arm by arm: there is NO third category
  of fact — every fact is about a TURN (its stream says it) or about the SESSION (this
  stream says it). The old arms map to the start verbs, stream terminals, the open-stream
  set, and derived frame instants; `response_usage_corrected` has no successor because the
  next frame IS the correction. `query_died` is DUPLICATED on purpose: a consumer with no
  stream open still needs the session-level fact.
- `GetSessionDiagnostics` — DIAGNOSTICS ARE PULLED, not pushed: the session stream "is for
  notable information as it manifests, and this is a synthetic manifestation" — the shim
  would be intermixing upstream happenings with periodic self-reports at its own cadence.
  Empty request; success wraps health plus degraded windows, each window carrying component,
  reason, began instant and `oneof extent { open | closed { ended_at_ms; dropped_count } }`
  (a `recovered` bool beside a count was the mode-selecting-bool defect). Windows are KEPT
  SINCE THE SHIM STARTED, so a daemon asking after the fact still learns what was dropped.
- `KillSession` (was `CloseSession`) — request `{ bool force }`; refuses while anything is
  live unless forced. A forced kill NAMES what it killed; a refusal NAMES what is live so
  the daemon can stop selectively rather than force blindly. The daemon is the one decider
  of what dies.
- `KillTurn { TurnId; bool force }` — ends the main agent AND everything the turn spawned,
  transitively. `UpdateAgent.stop` (formerly `UpdateTurn.stop`) interrupts one agent alone
  and leaves what it spawned running. Stop ≠ Kill.

### 3.5 The DETACHED WORK section (as originally landed, before the workflow collapse)

- BESPOKE RPCS PER KIND, the ruling: "the update api changes depending on that (you can send
  a prompt to a detached agent, but you can't send one to bash)." A generic update would
  have had arms that half-apply.
- THE ADDRESS SPLIT: WATCH by `DetachedWorkId` (a stream is one RUN and the handle names the
  run); UPDATE a subagent by `AgentId` (a prompt may target an agent whose run has finished,
  when no run handle exists). Bash and workflow never outlive a run, so both their verbs
  take the run handle.
- `DetachedWorkId` STAYS, after a retraction: it is the UNIFORM CONNECTION TOKEN so creating
  the daemon→shim connection is identical regardless of kind. "If the producer maps to it
  differently depending on the underlying message, that's fine, but it shouldn't be the
  CONSUMER doing that." The shim owns the handle↔identity mapping — one lookup, since
  `task_started` carries both.
- Stated at every Watch response, because a late joiner depends on it: EVERY STREAM OPENS
  WITH THE UNIT'S `start`, repeating the ORIGINAL instant. A prompt to a finished subagent
  starts a NEW run, announced on the prompting stream and followed by a new watch.
- WORKFLOW pipe collapsed later into `GetWorkflow` (unary; success carries the start plus
  `oneof standing { live { token; current level } | ended { embedded terminal } }`), a
  token-addressed `WatchWorkflow` with no start arm, and `StopWorkflow`; `UpdateWorkflow`
  DELETED. The run's update became a whole `all_subagents` list with REPLACE semantics — a
  STATELESS LEVEL, because the shim/sidecar/store "should be very stupid": they say an agent
  EXISTS and the daemon creates the connections it sees fit. The watch token lives INSIDE
  the `live` arm, so a concluded run's get is an ANSWER, not a dead token.

### 3.6 The HISTORY section

- `ReadHistory` is addressed by `AgentId`. A page's SCOPE IS THE AGENT and that is the whole
  of placement: a history page can be requested for something that was NOT a turn, and the
  entry already implicitly corresponds to an agent. Entries carry no placement.
- A PAGE IS "IMMEDIATE CHILDREN OF X", newest first. `top_level` is never used for paging —
  only for kill scope and session scope.
- THE MAIN AGENT IS AN AGENT and nil dies: the store's parent column is never nil, every
  frame's `agent_id` is real.
- ROTATION is answered by DECOUPLING: `main_agent_id` is OURS — SHIM-MINTED on the first
  fresh start, store-persisted, reported unchanged on every later start — while
  `SessionIdentityRotated` changes only the vendor handle the shim resumes by. Shim-minted
  rather than daemon-minted so a fresh daemon resuming an old store has ONE authority.
- Terminal arms ARE entries: `AgentSuccess`/`AgentFailure` appear in history; they are the
  only record of how a turn ended and the feed's stop notice has no other source. No `start`
  is replayed — the settled frame carries the start's facts by the upsert rule.
- NO `seq` ON THE DAEMON-FACING WIRE. History is first/next with an OPAQUE continuation
  token (later `HistoryPointer`), minted by the shim and echoed verbatim; the turn stream
  carries no position at all. The decisive argument: a turn frame carrying positions makes
  the turn handler a participant in history, the wrong seam. `seq` stays the STORE's
  addressing.
- The daemon's only position-shaped need is CATCH-UP AFTER ITS OWN DOWNTIME, which is
  ordered pagination, not seq. The daemon persists an OPAQUE TOKEN, so advancing past a
  position the store never assigned is UNREPRESENTABLE.
- Flagged, not modelled: `PromptOrigin` on a history prompt; whether an UNSETTLED unit (the
  turn died mid-unit) appears as its last frame or is dropped.

---

## 4. THE SHIM AS PRODUCER OF `conversation.v1`

### 4.1 The protocol model is NODES, not a log

- Model 2 chosen: identity PER THING, upserted. A response holds ONE identity from its first
  fragment to its last; growth is a re-send of that node; a tool return is an ARM of its
  call, not a separate entry.
- THE SHIM PERFORMS THE FOLD: accumulating fragments into a response, attaching a return to
  its call, applying usage corrections. `ContentArriving` as a payload arm and
  `ToolReturned` as a standalone entry both DIE.
- The daemon stops inventing identities for records it has not seen.

### 4.2 The `start` / `update` / `progress` rules

- `start` announces that THIS STREAM is now carrying this unit — it does NOT mean the work
  began. It is UNIVERSAL: every tool kind has one.
- `update` reports GROWTH, and a kind has one IFF something actually produces growth for it.
  Originally only bash (once detached), thinking and response earned one.
- THE SHIM EMITS A `start` FRAME PER UNIT PER STREAM, not per unit. Detaching work therefore
  produces TWO starts, and the second must repeat the ORIGINAL instant so a drawn clock does
  not reset. Three reasons converge, the decisive one being that the DAEMON MUST SURVIVE ITS
  OWN RESTART — a stream whose first frame presumes a frame delivered before the restart is
  unrecoverable.
- The shim must emit the announcement at `content_block_start` for EVERY tool call, not only
  at the return; it has the call at that point, so no new observation is needed, but the
  emit site is new.
- The shim OWNS THE START INSTANT: it stamps at announcement and does NOT restate an elapsed
  figure, DISCARDING the vendor's `elapsed_time_seconds`. An elapsed count on the wire is a
  second authority for a value derivable from one instant, and it arrives at heartbeat
  cadence, so a drawn clock would jump to network timing.
- The re-announced instant is RECOVERED FROM THE STORE, not held in memory: the store is
  indexable by unit id, so the second announcement reads the first. The path this serves is
  SHIM RESTART, not the ordinary detach.
- `AgentToolCallProgress { last_progress_at_ms }` later landed as a THIRD arm kind on nine
  tool kinds (read, write, edit, grep, glob, bash, skill_use, send_message, unmodeled): the
  vendor's per-call beat RELAYED AS OBSERVED, never a shim-invented ping. The start/update
  rule SURVIVES INTACT — progress is a beat, not growth.
- HEARTBEATS ARE FIRST-CLASS SHIM→DAEMON FEEDBACK (a recorded reopen of the shell landing's
  discard). Division of authority: the SHIM owns the wedge RULING (a timeout still settles
  the unit's `failure`), the DAEMON relays the latest beat into the feed, the CLIENT ticks
  "quiet for N s" locally. The in-between state ("beats stopped, shim has not ruled") is
  deliberately unrepresented — one ruling beats a two-stage alarm.
- The vendor's `heartbeat` flag is the ONLY evidence of a wedged tool call and nothing on
  the wire carries it, so the SHIM must surface a wedge as the item's `failure` arm. That
  obligation is load-bearing and nothing in the schema implies it.
- The SUBAGENT keeps its own richer progress channel (tokens/duration/retry); a beat there
  would be a lesser second spelling. TASK ACTS get no progress arm (an act is
  instantaneous).

### 4.3 Deltas, not cumulative text

- `AgentResponseUpdate { new_markdown }` and `AgentThinkingUpdate` carrying an
  `AgentThinkingTextDelta { new_text }`. The whole text rides the TERMINAL arms only
  (success, and failure's partial prose).
- NO `from_offset` gap detector on prose: a lost fragment "is evident in the response" and
  the terminal frame carries the whole text, so the settled bubble self-corrects. BASH KEEPS
  its offset — its spool has no settled whole to recover from.
- The shim's response accumulator is DELETED.

### 4.4 Usage

- `optional TokenUsage usage` rides the `AgentActivity` ENVELOPE, removed from the thinking
  and response arms.
- THE ONE-UNIT-PER-RESPONSE RULE, which makes the envelope safe: EXACTLY ONE UNIT PER
  RESPONSE carries usage — the unit for the response's FIRST content block, chosen because
  block order is deterministic. Every other unit of that response leaves it UNSET. Absence
  means "not the unit carrying its response's usage", never "this cost nothing".
- The measurement that decided it: across ~13,000 assistant messages, 5,714 (44%) were
  tool_use ONLY, and every one carries usage, because usage is a property of the API
  RESPONSE. Confining usage to thinking/response arms would have DROPPED accounting for
  those 5,714.
- The exhaustive enumeration of usage carriers is three and only three: assistant
  `message.usage` (13,057, one per API response — the envelope field); a subagent
  completion's `toolUseResult.usage`/`totalTokens` (the subagent's OWN consumption, a
  different fact); and `system/thinking_tokens` estimates (zero observed). TOOL CALLS CARRY
  NO USAGE.
- Usage rides the UPDATE arm too: the vendor states usage when the message OPENS (final
  input/cache counters, interim output) and restates it later, so the footer's live figure
  has a producer and the "correction" is simply the next frame.
- MONEY LEAVES THE API: `RunCost` deleted, cost fields retired at every site. Money is
  deliberately not represented anywhere in the contract.

### 4.5 Blocking kinds: permission and question

- `AgentUpdate` is the CONSUMER-OBLIGATION oneof `{ activity | question | permission }`
  (detached_work moved UP to `AgentFrame`). The arm IS the consumer's obligation: activity
  is READ-ONLY; a question and a permission mean the agent is BLOCKED and the user must
  WRITE BACK.
- ARE PERMISSION AND QUESTION THE SAME? At the SDK, yes; in meaning, no. Verified: ONE gate
  exists, every tool passes through it, and `AskUserQuestion` is a tool whose answer is an
  `allow` carrying `updatedInput`. For a question the gate is the answer's TRANSPORT and
  "allow" means nothing as consent; for every other tool the gate IS consent. Two kinds, one
  producer path.
- SHIM OBLIGATIONS: emit `AgentPermission.start` from `canUseTool`, `success` from its own
  resolve, and `denied.policy` from the `system/permission_denied` system message (a policy
  denial has NO open ask).
- The gate is NOT the tool's outcome: an allowed tool then runs as an ordinary activity unit
  under the SAME identity; a denied tool never starts.
- The vendor RENDERS the prompt sentence (title, displayName, description), so no consumer
  composes one. `suggestions` is a classic echo token: the vendor mints what "always allow"
  means and a standing grant returns it VERBATIM. THE STANDING TOKEN NEVER REACHES THE
  CLIENT — the daemon holds it and supplies it to the shim when the answer picks standing.
- `matchedAskRule` marks a rule-forced prompt hosts must not auto-approve. The permission
  trigger became THREE INDEPENDENT OPTIONALS (blocked_path, ask_rule, note) — the vendor can
  report all together.
- QUESTIONS ECHO VALUES, not tokens or order. POSITION was rejected (nothing about the
  producer's data is ordered, so order would be an invention the shim maintained) and a
  MINTED TOKEN was rejected because IT REQUIRES THE SHIM TO TRACK STATE — a stored mapping
  back to the text the producer keys on. The question's TEXT and the option's LABEL are the
  producer's own keys, so echoing them lets the shim reconstruct the producer call from the
  answer alone with NOTHING remembered.
- The shim UNDOES the producer's serialization at the boundary: its answer output is a MAP
  KEYED BY QUESTION TEXT with multiple selections COMMA-JOINED into one string. Joining on
  prose breaks when two questions read alike; comma-joining is unrecoverable for a label
  containing a comma.
- Validation costs nothing: the shim is ALREADY HOLDING the pending permission callback
  while the turn blocks, so it has the original ask by construction. The echo is stateless
  in the sense that matters — no NEW state.
- An ask NOBODY ANSWERED is a SUCCESS carrying `unanswered`, not a failure: the producer has
  an idle timeout and the agent proceeds.
- `AgentQuestionNote` was later DELETED — the wire collapses a selection's typed text into
  one comma-joined answer string, so no producer can fill a distinct note.
  `AgentPermissionAbandoned` was DELETED — the vendor declares permission prompts have no
  park deadline.

### 4.6 Attribution and recursion

- RECURSION IS RETRACTED: subagent activity is FLAT, attributed by `agent_id` on
  `AgentFrame`. The one field the model cannot do without is
  `AgentSubagentStart.created_agent_id` — the agent the spawn produced, as distinct from the
  spawn itself.
- Placement works without ancestry on the wire via three mechanisms: reading history
  (placement comes from the REQUEST), live frames (a consumer draws a container on an
  agent's CREATION and keys later frames by that identity), and the daemon (needs no
  parentage at all).
- The shim MUST attribute every frame to an agent: constant for the main thread, the
  vendor's `agent_id` for a subagent (present on forwarded subagent messages and on the tool
  return).
- OWED: the shim must set `forwardSubagentText`, without which a subagent's prose and
  reasoning never reach us at all.

---

## 5. IDENTITY: THE FOUR SPACES AND WHO MINTS WHAT

- `agent_id` (VENDOR's) — WHICH AGENT INSTANCE. Its own space; proven by `parent_agent_id`
  existing on `SessionMessage`, because depth beyond one cannot be resolved from call ids.
- `tool_use_id` (VENDOR's) — WHICH TOOL CALL. For a Task call this identifies THE SPAWN, not
  the agent it spawned; the two are adjacent fields on one message.
- `activity_id` (OURS) — WHICH UNIT OF WORK. MINTED BY THE SHIM, one per unit for the unit's
  WHOLE LIFE, sourced from `tool_use_id` where the vendor has one and from message id plus
  block index for text and reasoning. IT NAMES WORK, NEVER AN AGENT.
- `TurnId` (OURS) — WHICH TURN. DAEMON-MINTED at submission and returned so the client draws
  the row at once; on delivery THE SHIM ADOPTS IT (`StartTurnRequest.turn`) and writes the
  record under it.
- NOT INTERCHANGEABLE: an agent is not its spawning call; a unit of work is not the agent
  doing it; a turn is neither. The conflation of spawn-call with agent identity is recorded
  as an orchestrator error — provenance is not semantics.
- `AgentId` for the MAIN agent is SHIM-MINTED on the first fresh start and store-persisted;
  a subagent's `AgentId` is the vendor's (it survives its runs).
- THE IDENTITY RULING: vendor uuids NEVER cross the contract; the shim translates where a
  unit exists. The vendor's `uuid`/`promptId`, a monitor's `taskId`, and cron job ids-as-transcript-handles all stay shim-side (cron job ids ARE carried, opaquely, because they
  name jobs rather than records).
- `SessionStarted.vendor_session_id` is THE RUNTIME'S OWN ANSWER; the transcript's divergent
  spelling (observed differing in ~22% of records) stays SHIM-SIDE and never rides the
  field.
- Identity is per BLOCK, not per response: reasoning and prose are separate units, each
  holding one identity across its fragments. Inferring instead ("a new update after a
  success is a new call") BREAKS OUTRIGHT because the agent issues PARALLEL tool calls whose
  updates interleave.

---

## 6. DETACHED-WORK OBSERVATION

- ONE STREAM PER DETACHED ITEM; a stream ends iff its work concludes, so "zero item streams"
  IS "no detached work in flight". Liveness becomes structural.
- THE DISCRIMINATOR is the turn definition, and the vendor makes it observable:
  `system:background_tasks_changed` is "the full set of live background tasks, emitted
  whenever membership changes (start, completion, kill, A FOREGROUND AGENT BEING
  BACKGROUNDED)" — a LEVEL signal with REPLACE semantics, existing expressly "so a missed
  bookend cannot wedge a stale running indicator". Membership in that set IS "not blocking
  the main thread".
- So detachment is a fact with a DEFINED INSTANT (the membership change), not a property of
  how work was launched. `run_in_background: true` is merely the common way to enter the set
  at birth; Ctrl+B MIGRATES an item.
- Ruling 2, CONFIRMED at the type surface and contradicting a name-reading:
  `task_started`/`task_notification` are EDGE bookends and `background_tasks_changed` is the
  full set with REPLACE semantics — "do not correlate it with the edge stream". So the SHIM
  takes "entered" from `task_started` and "left" from `task_notification` — one frame per
  message — and RELAYS THE LEVEL VERBATIM as a session fact for the daemon, which may hold
  the set. The no-edge-pairing rule binds the daemon's INDICATOR only; the vendor's ordering
  warning binds the SHIM.
- ANNOUNCEMENT RIDES THE SPAWNING STREAM; there is NO roster stream (a session-scoped roster
  was REJECTED as a second authority). `task_started` IS emitted DURING the turn, before the
  turn's result — which is why the turn must be a STREAM, being the announcement channel for
  work that outlives it.
- CANCELLING AN ITEM IS A CALL, never closing the stream client-side — a client-side close
  would be misread as a transport failure. The item's stream then concludes with its failure
  arm.
- `KillSession`'s "every live task" is the vendor's own `backgroundTasks()` answer, not a
  shim-held set.
- The live-work lists on the terminal arms are CONNECTION facts: for every async item the
  daemon holds a connection to the shim, so the live set is implicit in the OPEN DETACHED-WORK STREAMS — the lists are filled from the shim's own tracking, never from vendor
  records (whose signals carry counts, not ids).
- FOREGROUND SHELL OUTPUT IS OBSERVABLE NOWHERE. The sidecar tails exactly four kinds of
  file (session transcripts, subagent transcripts, workflow journals, `tasks/*.output`
  spools — PER-TASK, so only background work has one); a foreground command's output exists
  in NO file while it runs. Measured: 603,300 bytes over ~75s with the ~30KB inline cap
  crossed in the first ~4s produced NOTHING in the tool-results directory while running, and
  the file materialized at process exit ALREADY AT FINAL SIZE. So `persistedOutputPath` is a
  POST-COMPLETION artifact and bash's update arm is structurally DETACH-ONLY.
- The OUTPUT PRODUCER IS OURS: the SDK exposes NO route returning a background task's output
  (the whole 28-method control surface was enumerated), and `task_progress` for a shell
  carries only totals. Every byte of detached shell output comes from the SIDECAR tailing
  the spool, terminated by its `EXIT=<code>` line. Accountability shifts: a vendor route
  breaking surfaces at the type surface; a sidecar route breaking is a file format we do not
  own changing underneath us, with no type to fail against.
- The shell item's result is legitimately NEVER RESOLVED when a command backgrounds — it did
  not conclude, it MOVED, and the detached frame naming it is what says so.
- `skip_transcript`-marked (ambient/housekeeping) work joins the EXEMPT SET: THE SHIM DROPS
  IT ENTIRELY — no announcement, no bubble, never `AgentUnmodeled`. The earlier "rides the
  stream as a rendering property" ruling is SUPERSEDED. Ambient work is invisible on our
  surfaces; the vendor's level set still governs liveness shim-side, so no indicator wedges.
- `GetLiveWork` (store) is called by THE SHIM (never the sidecar, a copier whose only
  recovery is cursors) ONCE at session start. It resolves every non-terminal item: re-adopt
  what the revived vendor process actually has (reported as `SessionStarted.live_work`), and
  WRITE THE CLOSING TERMINAL for what did not survive (a dual write that closes the record
  and puts the stop notice in the feed). THE INVARIANT: every started thing eventually gets
  a terminal row, by observation or by reconciliation.
- "Live" there is a claim about the RECORD, not the world — "a start was written and no
  terminal ever was" — so it cannot go stale.
- Monitors ride the detached-work machinery (`DetachableWork` gained a monitor arm); their
  vendor `taskId` stays shim-side.

---

## 7. KEEP-ALIVES

- KEEP-ALIVES LIVE ENTIRELY INSIDE THE SHIM. The user's proposal of a `keep_alive` request
  arm, then a separate rpc, was DECLINED once it was clear 4b had already settled them shim-internal: the daemon never submits one, so nothing keep-alive-shaped is on the wire at all
  — which satisfies the goal more strongly. "It's really its responsibility to know what the
  vendor requires."
- `PROMPT_ORIGIN_CACHE_KEEP_ALIVE` (tag 27) is RETIRED from `PromptOrigin`.
- The shim DETERMINES THE KEEP-ALIVE PROMPT TEXT under the hood.
- ONE SUBMITTER is structural only because the shim discharges the guarantor: a keep-alive
  YIELDS to real work. A real prompt must ROLL BACK context to just after the last real
  prompt, so the next turn does not build on keep-alive context (`SessionRewound` +
  `KeepAliveDiscard`). That yield obligation is the invariant's guarantor rather than an
  optimization; its reliability is a vetting item.
- KEEP-ALIVE TURNS MUST BE FIRST-CLASS IN THE STORE AS NEVER-SERVED: indexed so no page
  returns them and no activity is routed to the daemon.
- Keep-alives are INVISIBLE on the control plane, VISIBLE on the record plane: they make
  real API calls, so their cost lands in accounting whether or not anything announces them;
  discarded turns are marked superseded for readers to exclude — "never deleted, only
  excluded from replay".
- OWED to the history section: a paged read must handle superseded turns.
- The rollback point is a static (constant-lookup) recovery, per the audit.
- A `HeartbeatKeepAlive` arm was proposed and WITHDRAWN: nothing the vendor sends is about
  cache warming, so it would have been a shim invention.
- Keep-alive windows leave the daemon's durable state entirely — they are shim-internal.

---

## 8. THE EXEMPT SET

- The spec: a THIRD category beside modeled and unmodeled — "things that don't make it in
  the protos, but also aren't 'unmodeled', they're known 'not going to model'". A KNOWN
  vendor built-in the contract deliberately does not carry.
- SHIM BEHAVIOR: an exempt tool's calls are DROPPED AT THE SHIM. They must NOT be emitted as
  `AgentUnmodeled` (that arm keeps meaning "genuinely unknowable", its producer-defect
  stance intact) and must NOT trip the topbar's unmodeled warning.
- Scope: the fidelity principle governs FIELDS OF MODELED TOOLS; WHOLE TOOLS can be exempt.
- MEMBERS, as ruled across the walk:
  - TaskStop (the stopped work's own stream already settles cancelled).
  - TaskOutput (reads a background task's spool — already drawn in the work's bubble).
  - TaskGet, TaskList (quiet tracker reads).
  - ToolSearch (deferred-tool schema loading — vendor plumbing).
  - NotebookEdit (declared-only; would need fabricated hunks to ride AgentEdit).
  - The background-shell peek (a Bash call re-addressed at a running shell's id returning a
    snapshot) — the same bytes already reach the bubble via the spool, the TaskOutput
    precedent.
  - `skip_transcript` ambient/housekeeping tasks.
  - REPL (sandboxed code execution — "not useful enough to support now").
  - ListMcpResources, ReadMcpResource, RefreshMcpTools, SendFeedback, ClaudeDesign,
    Projects, ShowOnboardingRolePicker, ProposeSkills.
  - The undocumented `mode` disk line (always "normal", 2,669 observed): exempt AS A RECORD
    — the store's unparsed-residue arm keeps the raw line and nothing else ever sees it.
- TodoWrite stays MODELED: it is a producer route of `AgentTaskAct`.
- A dropped/exempt tool that later gains a real vendor route or a wanted UI re-enters as its
  own increment with a drawing, never silently.

---

## 9. THE FIDELITY PRINCIPLE

- Stated as a CORE PRINCIPLE: `conversation.v1` carries the vendor's fields EVEN WHEN NO UI
  MAPS THEM, marked EXPECTED UNMAPPED at the field. `conversation.v1` is a fidelity layer;
  UI-relevance gates `frontend.v1` only.
- Consequence: "nothing draws it" is no longer a reason to drop from `conversation.v1` (it
  remains one for `frontend.v1`). Recorded drops justified solely by that reason were
  reopened for a sweep.
- What it does NOT license: relaying vendor identity spaces (uuid, message.id stay shim-side), and JSON-in-a-string — unmapped fields are still FULLY TYPED.
- The `AgentUnmodeled` arm is NOT A FALLBACK: it is for a tool whose schema genuinely cannot
  be known, never one whose modelling was inconvenient; a recognizable built-in arriving
  there is a PRODUCER DEFECT. Its ARGUMENTS take the untyped `Struct` escape (the producer
  holds no schema); its RESULT does not. No update arm — this contract holds no knowledge of
  any unmodeled tool's streaming behavior.
- `UnsupportedBlock` carries the same stance: populated only for a block whose shape is
  genuinely unknowable in schema; the shim's and sidecar's converters must NOT route a
  knowable block there.
- EVIDENCE STANDARD, a standing rule: CORPUS ABSENCE IS NOT DELETION EVIDENCE — absence from
  the personal corpus proves NON-USE, never NON-SUPPORT. Deletion/no-producer verdicts
  require DOCUMENTATION-GRADE evidence (SDK doc comments, official docs, release notes,
  research); a re-vet on that standard REFUTED 11 of 13 no-producer claims.

### 9.1 Per-tool producer notes the shim owes

- Skill: the DOCUMENT IS NOT THE TOOL RETURN. The sequence is tool_use → tool_result whose
  content is the bare string "Launching skill: X" → a `user` record with `isMeta: true` and
  `sourceToolUseID` whose text IS the document → an attachment carrying
  `command_permissions.allowedTools`. The unit settles on the DOCUMENT, so the shim HOLDS
  THE UNIT OPEN across the two records; `sourceToolUseID` links the document DIRECTLY to the
  invoking call, making the existing name-map-and-match-what-arrives-next correlation
  unnecessary, positional and fragile. NOTHING DELIMITS A SKILL'S SCOPE — no end record, no
  boundary marker — so nesting under a skill heading is PRESENTATION and the protocol never
  claims an extent it cannot observe.
- Write/edit: land on the VENDOR'S STRUCTURED PATCH, never a reconstruction.
  `SDKAssistantMessage` carries a per-tool structured output object keyed by the matching
  tool_use block's name; `FileEditOutput`/`FileWriteOutput` carry `structuredPatch`.
  Reasoning from the tool's INPUT schema and rebuilding hunks in the shim was the recorded
  error.
- IDE diagnostics arrive on a SEPARATE record after write/edit, and the vendor's record
  carries NO TOOL-CALL ID — the join is by ADJACENCY, one remembered last-write/edit-unit
  value in the shim, constant and recorded. They land as a post-terminal `diagnostics`
  CONSEQUENCE arm; no frame ever says "none are coming".
- SendMessage: ADDRESSED at the call (a plain string — the caller addresses by identity OR
  by a spawn's human-readable name, and nothing has resolved which agent that is), RESOLVED
  at the outcome (an `AgentId` plus `queued_to_live | resumed_recipient`). The presence of
  `resumedAgentId` is the ONLY structured discriminator; the rest lives inside prose written
  for the model and is deliberately not recovered.
- Bash cause arms: the shim harvests the cause from the BASH TOOL RESULT, never the task
  stream, which carries no cause. `timedOutAfterMs` carries the configured limit at auto-backgrounding; `backgroundedByUser` marks Ctrl+B.
- Grep/glob: the modern binary always writes the completeness fields; pre-field CLI versions
  are explicitly UNSUPPORTED. Omitted figures are SHIM-SUBTRACTED. A glob floor is drawn as
  "at least N", never a total.
- Artifact: the vendor's publish result is TYPED (read fields, never parse prose) per the
  corrected producer note.
- Hook duration and spawn depth are SHIM-DERIVED.
- `SessionColdLapsed`'s cache TTL was re-commented as SHIM-AUTHORED, not vendor-stated;
  `SessionIdentityRotated.reason` was DROPPED as unsourced.
- Async subagent completion carries at most one OPTIONAL `total_tokens` scalar versus the
  sync path's full four-field usage; the split lives at `AgentSubagentTotals.usage` as a
  two-arm oneof, and absence means UNREPORTED, never zero.
- `fake-query.ts` — the shim's hand-written stand-in for the SDK's `query()` — can only ever
  confirm our own reading, since we wrote it and it agrees with us wherever we are wrong.
  Two landed decisions rest on it alone, one load-bearing. The fix is to build the fake's
  scripts FROM CAPTURED TRANSCRIPTS.

---

## 10. WHAT THE SHIM WRITES TO `store.v1`

- THE RULING: "the store should be writing `conversation.v1` to the database." Rows are
  `conversation.v1` messages; NO record-granular vendor vocabulary is persisted. The shim
  and sidecar RESOLVE each frame at write time with a CONSTANT number of single lookups, and
  the store holds the resolved frames.
- THE STORE IS A CONNECT SERVICE (`service ShimStore`), callers the shim and the sidecar
  only.
- `WriteBatch { producer; EntryBatch }` — the write GAINS THE ACK the old UDS socket never
  had. Success means DURABLE (records + cursor advance, ONE transaction), with replay
  absorption via `write_id`; failure means NOTHING COMMITTED, so the producer's SPILL HOLDS
  AND REPLAYS. The success arm is exactly what lets the shim's spill retire batches on
  acknowledgment. `StoreEntryWrite` is deleted — under Connect the rpc IS the envelope.
- `StoreEntry { Plane; write_id; upsert_key; oneof entry { StoreAgentUpdate |
  conversation.v1.SessionUpdate } }`, with `StoreAgentUpdate` selecting among a serveable
  page line, an unserveable item, a bash run and a workflow run.
- PAGEABILITY IS DECIDED BY THE PRODUCER: a page line NAMES its book (`page_agent_id`),
  unserveable material has no book, and run frames are not page lines at all. Non-item
  frames must be STRUCTURALLY unable to appear in a page.
- `upsert_key` is ONE OPAQUE PRODUCER-MINTED KEY; the store holds one row per key and a
  write supersedes it whole. "The store should have a single place it looks for a given
  property" — THE MAPPING (TurnId, unit id, run id) IS THE SHIM'S AND THE SIDECAR'S, never
  the store's.
- `top_level` = the nearest NON-SYNC ancestor (main agent or detached-work agent, never a
  sync subagent), copied from the parent's row at insert; documented UNSET when unresolvable
  (an unparsed record may name no agent).
- The unserved oneof states WHY material cannot be served: keepalive (the never-served
  indexing obligation), vendor-specific, unknown, unparsed. The residue rides the SAME
  envelope as everything else — one `upsert_key` space, one write path.
- The frame's ONEOF IS THE DATALAYER ROUTE, and every write lands in exactly ONE table
  decided by its wire arm: `update` → the entry table as a page line; `success`/`failure` →
  BOTH entry (the stop notice has no other source) AND the agent row's terminal columns, one
  transaction; `detached_work` → the lifecycle table for its kind, NEVER a page line (the
  spawning call is already one).
- Four tables, each the ONE canonical home: `agent`, `workflow`, `entry` (a queryable spine
  around a SERIALIZED frame the store never opens), and `detached_work`. THE SUBAGENT LEVEL
  IS NEVER STORED — it is the join. The columns-vs-blob line is "the queryable and joinable
  stuff": the three lifecycle tables are UNPACKED to columns, `entry`'s frame stays
  serialized because unpacking the activity vocabulary would put every `conversation.v1`
  churn into DDL for nothing the store ever queries.
- Prompts and frames share ONE ENTRY TABLE — one position space is what makes "everything
  after the last real prompt" (the keep-alive rollback) a RANGE QUERY.
- `AgentPrompt { TurnId; AgentId; UserSaid }` is the ONE FORM of a delivered prompt:
  returned by the delivering rpc, persisted by the store, replayed by history.
  `HistoryPrompt` deleted. THE SHIM MUST RESOLVE THE RECIPIENT AGENT BEFORE ANSWERING
  `StartTurn`.
- `StoreEntry` carries NO parent column: the agent is read from `AgentFrame.agent_id` or
  `AgentPrompt.agent`; a `SessionUpdate` row is the main agent's.
- REQUIREMENT the store must honor for the shim: the store must be INDEXABLE ON A UNIT'S ID
  (an indexed column), because the re-announced start instant is recovered by that lookup.
  Today's `PRIMARY KEY (session_id, seq)` plus a `top_level_message_id` index would make it
  a scan.
- Read verbs: `OpenAgentSession` (page + a store-minted `AgentSessionToken`, "a hash of the
  actual session identifier, so the client MUST call open to subsequently watch"),
  `WatchAgentSession` (STANDING, one frame per written line, upserts included),
  `ReadAgentPage` (next-only, after a pointer), `GetSidecarCursors`, `GetWorkflow`,
  `GetLiveWork`.
- `known_through` is the CALLER'S OWN high-water mark: UNSET = repaint; SET = catch-up after
  a shim bounce. THE STORE DELIBERATELY TRACKS NOTHING about what it previously served, and
  every streamed line carries its pointer.
- `StoreItemPointer` is opaque and store-minted, stable across upserts BECAUSE ORDER IS BY
  THE UNIT'S FIRST INSERT, never its last write — a unit settling mid-walk cannot teleport
  across a continuation.
- This open/watch split was REOPENED BY NAME onto the shim boundary: the shim gets the same
  split and its watch carries only new lines. In practice `WatchAgent`'s opening-page frame
  plus `known_through` realizes it without a token (an `OpenAgentSession`-style token at the
  shim was weighed and declined twice).
- The CURSOR rides the BATCH, not the entry: one read position yields many entries; it is a
  file bookmark rather than a conversation fact, and STREAM-PLANE WRITES HAVE NO FILE to be
  positioned in (only the sidecar sets `cursor_advance`). Its UX is "after any crash or
  deploy, history has no gaps and no repeated messages".
- STANDING POLICY — THE STORE IS NUKED, NEVER MIGRATED. No backfill, no hydration, no schema
  migration; no field is retained on durable-compatibility grounds; where contents are in
  the way, the store is DROPPED and recreated.
- The DATALAYER/PROTOCOL split: `store.v1` owns persistence, `conversation.v1` owns the
  protocol, and THE SHIM OWNS THE MAPPING — one mapping, one place, one author. Accepted
  cost: a hand-maintained mapping has NO COMPILER to detect divergence, so tests that FAIL
  when a field is added on one side and not mapped are owed to the wave.
  `shim.v1.ExternalEntry` ceases to exist as a shared half; the daemon-facing wire carries
  the protocol model.
- STILL HOMELESS, flagged not landed: the old `source_record` kept-whole field (a faithful
  conversion that was nonetheless LESS than the source).

---

## 11. SHIM-SIDE FACTS ABOUT THE TURN AND THE VENDOR QUEUE

- A TURN is the window during which the main thread cannot ACCEPT a prompt, where accepting
  means the prompt is fed into context and produces output tokens. Typing a prompt and
  having one handled are different facts.
- THE DAEMON IS THE QUEUE; the agent binary's queue is never ours. The daemon submits ONLY
  when no turn is in flight; an open stream means a RUNNING turn. The interrupt answer's
  still_queued/cancelled lists are DROPPED entirely (the vendor's queue functionally never
  holds anything of ours), and the `cancel_async_message` control verb is DROPPED with them.
- THE HEARTBEAT CONCEPT LEFT `shim.v1` ENTIRELY: `ConnectionHeartbeat` → HTTP/2 transport
  liveness; liveness of a work item → THE STREAM BEING OPEN; `AgentHeartbeat.live_work_ids`
  → the set of open item streams the daemon holds; the vendor's per-item `tool_progress`
  detail → an UPDATE (later PROGRESS) frame on that work's own stream. Both heartbeat
  messages DIE.
- FINALITY IS NOT A WIRE FACT: a settled text block and the "final" one are the same SDK
  object; the turn's own conclusion NAMES the answering response
  (`AgentSuccess.completed.answer`). Multiple responses per turn is NORMAL.
- `ApiRequestFailed` is an AGENT-LEVEL failure on `AgentFailure` — the vendor refusing a
  request ends the AGENT's stream; it is not a property of one prose block. To verify at
  implementation: whether the producer sees the error TYPE structured or only the binary's
  `"API Error: 429 …"` text, in which case the arm is derived from the status code that text
  carries, once, by the producer.
- Stop facts land as `AgentResponseFailureReason { max_tokens | refused |
  context_window_exceeded | aborted }`, deliberately WITHOUT arms for pause_turn (the vendor
  resumes it itself), compaction (the context cut is that fact's home) and stop_sequence
  (unused).
- Stop hooks DO end the turn at the SDK level (`TerminalReason` names `stop_hook_prevented`
  and `hook_stopped` as loop terminals), so those arms stay on `AgentFailure` — they RELAY
  the vendor's own stated terminal. The separate ruling that "stop hooks never determine the
  turn terminal" concerned DAEMON-SYNTHESIZED terminals.

---

## 12. IMPLEMENTATION INVARIANTS THE SHIM INHERITS

- UNSET NON-OPTIONAL FIELDS ARE ILLEGAL, EVERYWHERE, IMMEDIATELY. A REQUEST carrying one is
  answered with an ERROR to the producer at once — never "handled", never defaulted. A
  RESPONSE or STREAM PUSH carrying one makes the CONSUMER raise a loud error itself (on a
  stream there is no producer to answer).
- Proto→code mapping: every MESSAGE has one core "base" function per language where
  validation lives ONCE (unset non-optional fields and required-semantics empty strings are
  ERRORS; an unset oneof is an ERROR BY DEFAULT, a fallback only where a schema comment
  sanctions absence). Every non-primitive USE SITE gets its own dedicated testable function
  delegating to the child's base; primitives get no wrappers; the producer side is
  SYMMETRIC.
- The SHIM reconciled green (277 tests) and merged; its dead-code inventory, blockers and
  integration replacement specs are seeded into `docs/implementation/`. Design froze at
  `2d79f7501`.

## 13. STANDING OWED ITEMS AND GOTCHAS FOR THE SHIM

- Verify the SDK can initialize a session from a transcript WE wrote (`sessionStore` @alpha
  vs `forkSession` vs writing the binary's JSONL), and whether our bookkeeping survives the
  vendor's `compact_boundary` format.
- Verify the keep-alive rollback (`SessionRewound` + `KeepAliveDiscard`) is reliable; a
  paged read must handle superseded turns.
- Set `forwardSubagentText`, or subagent prose and reasoning never arrive.
- Verify whether the vendor's background tasks survive the query closing.
- Verify whether a foreground agent backgrounded by Ctrl+B also fires a `task_started` edge
  (the level's doc names it; the edge's does not).
- Verify whether the main agent accepts a mid-turn message the way a live subagent does;
  either way the API stays one and the shim absorbs it.
- Verify whether the producer surfaces API error TYPES structurally or only as text.
- The wave must VERIFY that every vendor API failure the daemon sees LIVE also lands as a
  transcript record.
- Detached timeout has no vendor surface — keep it only as a SHIM-IMPOSED deadline.
- Sweep bare "CLI" from the shim's AGENTS.md and the proto docs.
- The sidecar is a SECOND PRODUCER needing its own verification pass; ingesting per-agent
  workflow transcripts is not sufficient alone — the `agent-<id>.meta.json` file must be
  read alongside each one, being the only source for type, model and worktree.
- Nothing in a workflow journal ever says the RUN finished; the only possible source for a
  run's terminal is the run leaving the vendor's live-background set. 524 `started` records
  against 489 `result` records means agents that started and never produced one, with
  nothing distinguishing "still running" from "died".

## The spill removal (post-freeze increment)

- The durable WriteBatch spill is REMOVED. WriteBatch failure holds the
  batch in a BOUNDED IN-MEMORY retry buffer; exhausted retries log loudly
  what was lost and drop — never a shim crash, never disk persistence.
- The graceful stand-down WAITS FOR ALL ACKS before the shim exits; an
  exit with unacknowledged writes is the loud failure (a sequencing
  defect to fix at the source).
- The sidecar needs no buffer at all: its sources are durable files it
  re-reads from the cursor.
## GetSessionContextUsage (post-freeze increment, 2026-08-28)

- NEW SESSION-section verb: the daemon pulls the session's current
  context usage; the shim answers with the vendor's get_context_usage
  control response mapped to conversation.v1 SessionContextUsage —
  never an estimate, never derived from usage frames. Failure kind arms
  derive at the wave.

