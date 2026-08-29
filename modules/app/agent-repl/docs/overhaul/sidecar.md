# Sidecar implementation planning

## Dead code to remove (with the conversion port)
- convert.UnportedEntry and its "__unported_conversion" discriminator: a
  loud reconciliation stub — every vendor→conversation.v1 conversion now
  writes through it; the port replaces it with the Agent* frame conversion
  and the discriminator enumerates exactly what must be ported.
- The raw length-prefixed Any-over-UDS transport, when the store.v1 Connect
  port lands (messages are already repointed; the transport is not).
- Client.Health's ErrNoHealthProbe stub and main.go's beat-timer teardown
  around it (see blockers).

## Blockers / decisions owed (surfaced at reconciliation)
- AGENT-ID MINTING FROM A TRANSCRIPT: nothing tells a file reader how to
  mint an AgentId from a Claude transcript — the design record's identity
  entries (shim-minted main_agent_id, vendor agent_id for subagents,
  meta.json for workflow agents) are the spec; the sidecar needs the shim's
  minting discipline or a shared rule. LEAD-LEVEL: cross-produces with shim.
- LOAD-BEARING: the sidecar currently CANNOT hold its store link up — the
  beat timer tears down on the always-failing health probe. The port must
  either give it a real probe route or remove probing (streams+transport own
  liveness per the design ruling), FIRST.
- No session attribution on StoreEntry (old ExternalEntry.session_id/
  produced_at_ms have no successor): confirm the new addressing (agent-keyed
  spine) covers every reporting path, or surface a contract gap.
- No bookkeeping arm: sidecar self-diagnostics have no typed home — confirm
  the pulled-diagnostics model covers them or surface a gap.
- No authoritative open-task snapshot (OpenTaskState died): staleness
  tracking / boot LOST sweep / spool-owner seeding re-derive from the store's
  live-work reads (GetLiveWork) per Owed A/H.
- Read WriteBatchResponse (recommended at reconciliation): restores the
  batch-rejection error branch that has been unreachable — take it.
- Workflow per-agent transcript ingestion (Owed E): discovery must glob
  workflows/wf_*/agent-*.jsonl + meta.json.

## Replacement integration-test specs
(Unit specs deliberately absent per the mapping convention.)
- Vendor transcript → Agent* frames: golden real captures driven end to end
  into StoreEntry writes (replaces the ~34 deleted convert tests' subjects
  under the new model; rides item 5's capture harness).
- Detached-work lifecycle from files: spool bytes → AgentBashUpdate deltas;
  EXIT marker → terminal; journal started/result → workflow announcements
  (replaces the handler suites' subjects).
- Restart recovery: cursors from GetSidecarCursors + live-work re-announce
  with original start instants recovered from the store (Owed A).
- WriteBatch ack handling: success retires spill; failure replays.

## Contract context (for implementers)

Meta: the protos live under `proto/src/` (the sidecar produces `store/v1`
envelopes around `conversation/v1` facts); the comments IN the `.proto` files
are the authoritative documentation — read them at the symbol you implement.
Implementers never change protobufs; a needed change is a request up the
orchestration chain. Cross-cutting conventions are in
`the teamlead prompt (standing conventions) and the proto comments` and are not repeated here.

### What the sidecar is

- The FILE-PLANE READER: it tails what the vendor's agent binary writes to
  disk, converts each record into `conversation.v1` vocabulary, and writes it
  to the store as `store.v1.StoreEntry` batches. It is a COPIER — no view of
  liveness, no session semantics, no daemon contact.
- Dual-plane relationship with the shim: `StoreEntry.plane` names the
  producer. The SHIM (stream plane) watches the SDK live — first to know,
  authoritative for session/turn LIFECYCLE and the only source for anything
  not yet on disk. The SIDECAR (file plane) reads what the vendor itself
  recorded — authoritative for conversation CONTENT. Both write through the
  same envelope, one upsert_key space, one write path.
- The no-variable-state principle binds it: every conversion step costs a
  CONSTANT number of single indexed lookups — no stacks, queues, trees, or
  lineage walks. A single parent-id lookup is fine; a variable number is not.

### Constant-cost conversion into conversation.v1

- Each file record resolves with one lookup each, all structural:
  - a tool RETURN finds its call by the vendor's `tool_use_id` (the unit's
    identity where one exists; message-id + block index otherwise);
  - a SKILL document finds its call by `sourceToolUseID` on the isMeta user
    record — direct and structural, NEVER a skill-name map matched against
    "whatever arrives next" (the old converter's fragile positional
    correlation is explicitly retired);
  - IDE diagnostics join their write/edit unit by ADJACENCY — one remembered
    last-write/edit-unit value, constant and sanctioned at the schema;
  - a spawned agent finds its spawn (`AgentSubagentStart.created_agent_id` is
    the container key);
  - `top_level` is copied from the parent's stored row at insert — one
    lookup, inductively correct at any depth.
- A frame is an UPSERT of its whole unit: identity per THING, one id per unit
  for its whole life; a later frame (a return, a diagnostics report) is the
  same unit's row superseded whole, never a second entry.
- Vendor identity never crosses the contract: uuids, message ids, vendor task
  ids stay sidecar-side; the typed identities (AgentId, AgentActivityId,
  TurnId, DetachedWorkId) are what ride the wire.
- Unconvertible material is NEVER dropped: it lands as
  `StoreUnservedItem { vendor_specific | unknown | unparsed }` — durable,
  whole, and investigable (unparsed carries source, offset, parse_error, raw
  bytes). These arms are loud residue, not a fallback: a recognizable modeled
  kind arriving there is a producer defect. The EXEMPT SET is different:
  known built-ins deliberately not carried (TaskStop/TaskOutput/TaskGet/
  TaskList, ToolSearch, NotebookEdit, REPL, the MCP-resource family, …) are
  DROPPED entirely — never AgentUnmodeled, never residue. ONE CARVE-OUT
  (ruled 2026-08-29): the TaskStop CALL stays dropped, but its RESULT
  ({command, taskType, taskId}) is CONSUMED as the owning task's CANCELLED
  terminal before dropping — deliberately-stopped work must resolve
  cancelled, never LOST.
- `AgentUnmodeled` keeps its meaning: a tool whose schema genuinely cannot be
  known. It is not a lazy fallback either.
- CONTEXT-LIFECYCLE CONVERSION (ruled 2026-08-29): /clear is detected by
  UNWRAPPING the vendor's expanded command envelope (the literal "/clear"
  never appears on disk); compaction coalesces the boundary record with the
  FOLLOWING summary line in FILE order, never timestamp order — both
  produce the landed ContextCut records.
- API-ERROR RECORDS (ruled 2026-08-29): a transcript system/api_error line
  converts to the corresponding ApiRequestFailed conversation record —
  mid-turn evidence, not a turn terminal.
- WITHHOLDING (ruled 2026-08-29, conversion-side): records that must never
  become feed rows are CLASSIFIED AT INGEST into non-feed kinds — CLI
  slash-command bookkeeping/machinery, SKILL.md bodies (folded into the
  Skill card), task-notification envelopes, the synthetic "No response
  requested." assistant record, context-cut exclusions — so no resolver
  ever sees them as prose and no history page regrows fake prompt bubbles.

### Discovery scope

- MULTI-ROOT (ruled 2026-08-29): BOTH account config roots (the default and
  the multi-repo root's config dir) are discovery roots, configurable — the
  second account's transcripts are invisible otherwise. Scan/notify latency
  mechanics are the implementer's.
- Exactly four kinds of file, all written by the agent binary:
  1. session transcripts (JSONL);
  2. subagent transcripts;
  3. workflow journals — plus, per Owed E, the workflow PER-AGENT transcripts:
     discovery globs `workflows/wf_*/agent-*.jsonl` AND each agent's
     `agent-<id>.meta.json`, which is the ONLY source for the agent's type,
     spawn depth, model, and worktree — a per-agent transcript is not
     ingestible without its meta file;
  4. `tasks/*.output` spools — PER-TASK files, so only BACKGROUND work has
     one. Spools carry THREE kinds by task-id prefix (ruled 2026-08-29):
     `b*` shell output, `a*` agent transcripts, `w*` workflow journals —
     each routed to its kind's conversion; an unclassifiable prefix is a
     loud total-ingestion violation. Foreground shell output exists in NO file while it runs (the spool
     path materializes at process exit, already final-size): the bash update
     arm is structurally detach-only.
- A workflow journal holds exactly two record shapes — `{started, key,
  agentId}` and `{result, key, agentId, result}` — nothing run-scoped: the
  journal's started record IS a workflow agent's announcement; the agent's
  PROMPT is the first user message of its own transcript; nothing in a
  journal ever says the run finished (run terminals come from the live set,
  i.e. the stream plane).
- The detached-shell spool is a delta stream terminated by its `EXIT=<code>`
  line; spool bytes convert to `AgentBashUpdate` deltas (offset-carrying),
  the EXIT marker to the terminal.

### Owner resolution and the LOST/staleness policy

- PATH NORMALIZATION (ruled 2026-08-29): resolve the macOS /tmp →
  /private/tmp symlink before comparing spool paths — the same file must
  not read as two.

- Every frame must name its agent (`AgentFrame.agent_id`; the store's book is
  read from the frame, never invented). The main agent's id is OURS —
  shim-minted on first fresh start, store-persisted, stable across vendor
  identity rotations; a subagent's is the vendor's agentId space (distinct
  from the spawning call's tool_use_id — an agent is not its spawning call).
  How a file-only reader learns these is the doc's standing lead-level
  blocker (see above): the design record's identity entries are the spec.
- LOST is its own word — "we stopped seeing it", not "known failed":
  `DetachedLost { file_vanished | went_silent | swept_up }`. The arm is HOW
  we concluded it. Staleness judgments (a spool gone quiet, a file removed)
  are the reader's to state loudly, never to silently drop.
- The sidecar holds no authoritative open-task snapshot: boot LOST sweeps,
  staleness tracking, and spool-owner seeding re-derive from the STORE's
  live-work reads — but GetLiveWork's CALLER is the shim; the sidecar's only
  recovery verb is GetSidecarCursors.

### Cursor recovery (the sidecar's whole restart story)

- Per tailed file: `CursorState { file_id ("dev:inode", rename-proof); path;
  offset (next read); carry (bounded partial-line bytes) }`.
- The cursor RIDES THE BATCH (`EntryBatch.cursor_advance`) and commits in the
  SAME store transaction as the records read at that position — that is the
  exactly-once guarantee: no re-read duplicates, no outage holes. `write_id`
  absorption is the recovery for the duplicate case, not the guarantee.
- On startup: `GetSidecarCursors` (all cursors, or one file_id); empty
  success is the fresh-store answer — start every file from zero. After any
  crash or deploy the UX is "no gaps, no repeats".
- STORE-UNREACHABLE INVARIANT (ruled 2026-08-29, connection-free
  restatement of the old link machine): every production cycle BEGINS with
  a successful cursor read from the store; any store error suspends ALL
  production until a full recover-cursors-then-rescan succeeds. Never build
  a tailer from a position the store did not hand us; produce NOTHING while
  the store is unreachable.
- THE HOLD (ruled 2026-08-29): a conversion may HOLD the trailing frame of
  a batch (its meaning depends on the next line) — the cursor then advances
  SHORT of what was read, to the held frame's offset; the hold is bounded
  to one redelivery, and an out-of-batch hold is refused loudly.

### Writing to the store (the sidecar as producer)

- `WriteBatch { producer: "shim-claude-sidecar"; EntryBatch }` — read the
  response: SUCCESS means durable (records + cursor, one transaction), and a
  replayed batch fully absorbed by write_id is the SAME success arm; FAILURE
  means NOTHING committed. On failure the sidecar simply does not advance —
  it needs no retry buffer and no spill, because its sources are durable
  files it re-reads from the last committed cursor.
- Envelope duties per entry: mint `write_id` once per write — and
  DETERMINISTICALLY (ruled 2026-08-29): a digest of the write's source
  coordinates (producer, path, offset, discriminator), so the same bytes
  re-read always mint the same id; randomness is forbidden (replay
  idempotence rests on it); mint
  `upsert_key` from the unit's identity (the mapping is the producer's, the
  store never interprets it); set `plane.file`; resolve `top_level` (UNSET
  only when genuinely unresolvable); pick the `agent_info` arm — pageability
  is the PRODUCER'S decision: `serveable_frame` names its book
  (`page_agent_id`), `bash`/`workflow` wrap run frames with the run identity,
  `unserved_item` for keepalives and residue.
- Transport is Connect rpc — the old length-prefixed Any-over-UDS framing,
  Subscribe/Ack/heartbeat machinery, and any health probe are all gone from
  the contract; there is NO store health verb by design (streams + transport
  own liveness), which is why the beat-timer blocker above must resolve as
  "remove probing", not "find a probe".

### store/v1, the surface it talks to (medium)

- `ShimStore` service: WriteBatch and GetSidecarCursors are the sidecar's two
  verbs. The read side (OpenAgentSession → token → WatchAgentSession;
  ReadAgentPage; GetWorkflow; GetLiveWork) is the SHIM's — know it exists so
  you understand what your rows feed: pages are per-book (`page_agent_id`),
  ordered by FIRST insert (stable pointers across upserts), watched as a pure
  tail pinned after an opened page.
- Standing policy: THE STORE IS NUKED, NEVER MIGRATED — never write backfill
  or migration logic, never preserve a shape for old rows.
- The daemon never touches store.v1 (codegen-enforced isolation).

### conversation/v1, what it converts into (medium)

- The protocol model is NODES, upserted — not a flat log. The families the
  sidecar produces:
  - `AgentPrompt { TurnId; AgentId agent; UserSaid }` — the ONE form of a
    delivered prompt (stored as a page line like any frame).
  - `AgentFrame { agent_id; update | success | failure | detached_work }` —
    the one frame of any agent's stream; `AgentUpdate` is pure content
    { activity | question | permission }; the terminals are the only record
    of how a turn ended.
  - `AgentActivity { AgentActivityId; agent_id; optional usage; item }` —
    ~30 item kinds (read/write/edit/grep/glob/bash/subagent/skill/
    send_message/task acts/hooks/diagnostics/web/artifact/monitor/wakeup/
    plan/findings/worktree/cron/…), each on the start | update (iff growth) |
    success | failure pattern; `start` means "this stream now carries this
    unit", updates carry DELTAS, terminals carry wholes; exactly ONE unit per
    vendor API response carries `usage` (the first content block's unit).
  - `AgentBash` / `AgentWorkflow` — detached-run frames;
    `AgentWorkflowUpdate` is the WHOLE subagent level, replace semantics,
    one frame per observation, emitted as observed, never buffered.
  - `SessionUpdate` — session-scoped facts (a raw StoreEntry arm; the row is
    the main agent's).
- The fidelity principle governs fields of MODELED kinds: vendor fields land
  even when no UI draws them (EXPECTED UNMAPPED at the field); untyped
  Structs appear only where a schema genuinely cannot exist.

### Gotchas

- Keep-alive turns are first-class NEVER-SERVED rows (`unserved_item.
  keepalive`): they must be indexed such that no page returns them and no
  activity routes onward.
- Re-announcement repeats the ORIGINAL start instant, recovered from the
  store by unit id — never re-stamped at emit time (drawn clocks must not
  reset).
- A unit's later frames (a return, a spool delta's settled whole) are
  UPSERTS of the same upsert_key, not children; nothing has a tool call as a
  parent.
- Skill scope has NO delimiter at the source: never invent a skill-ended
  record; post-skill nesting is a presentation choice downstream.
- An assistant message arrives as SEVERAL block units (thinking, prose, each
  tool call its own unit) — never collapse them into one row.
- Empty search results are SUCCESS with an empty answer; a non-zero shell
  exit is COMPLETED (the code is the verdict), not a failure arm.
- The vendor's transcript spelling of the session id diverges from the
  runtime's answer in ~22% of records — stay on the runtime's; transcript
  divergence never rides the wire.

### Additional rulings (final-audit triage, 2026-08-29)

- LOG CORRELATION: store and sidecar work WITH THE PROJECT LEAD to
  consolidate the structured-logging and log-correlation scheme (the old
  keys — claude_session_id, seq counters — are retired with the
  addressing), unified under the /debug-emacs-agent-repl logging contract.
- WORKFLOW IS KICKED: workflow journal/transcript ingestion vocabulary
  stays in the contract but the workflow feature is NOT implemented in
  this wave.

## Kickoff increments and rulings (2026-08-29, project lead)

- LANDED: the sidecar produces `AgentUpdate.context_cut` (upsert key
  `session:context_cut:<record uuid>`, a page line of the main agent's book;
  boundary + summary coalesced into one row) and `AgentUpdate.api_error`
  (`session:api_error:<record uuid>`, never a terminal).
- R15: file-plane user prompts are NEVER page lines — classified as unserved
  vendor_specific{kind "user_prompt"}; the shim's AgentPrompt is the one
  served form (subagent commissions ride AgentSubagentStart.prompt).
- R10: session attribution is the agent-keyed spine; self-diagnostics are
  logs only; no open-task snapshot — re-derive; always read
  WriteBatchResponse. Restart correctness: on boot each tailer resumes from
  the store's cursor REWOUND to the in-progress turn's first record (one
  bounded backward scan per file per boot); the re-emitted records mint
  identical write_ids and absorb as success. The mock vendor's files live
  under `$AGENT_REPL_FAKE_SPOOL_ROOT` and `$CLAUDE_CONFIG_DIR/projects/
  <cwd-slug>/`.

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
  locks live in ~/.cache/agent-repl/run/ — `workspace-<md5hex(clean abs
  dir)[:8]>.lock` (shim-held from startup; the daemon probes ONLY this one,
  flock LOCK_EX|LOCK_NB) and `session-<vendor session id>.lock` (taken
  inside StartSession; pre-minted on a fresh start); proto/vocab/
  render-colors.json + paint-classes.json are the daemon's, consumed by
  webapp and Emacs; Go modules pin connectrpc.com/connect v1.17.0 and
  golang.org/x/net v0.43.0 (Go 1.24 on this machine; every module stays
  `go 1.23`).

## Corpus corrections (2026-08-29, project lead)

- The subagent companion `projects/<slug>/<session>/subagents/agent-<id>.meta.json`
  carries exactly FOUR camelCase fields — `agentType`, `description`,
  `toolUseId`, `spawnDepth` — and NO model (observed in testdata/corpus/
  sidechain/agent-aef975b7bc3422d4b.meta.json; observed beats declared). The
  snake_case five-field spelling above and the claim that meta.json sources a
  subagent's MODEL are superseded: a subagent's model is observable only from
  its own transcript's assistant lines (message.model).
- The vendor's `<cwd-slug>` replaces EVERY byte of the absolute cwd that is
  not [A-Za-z0-9] with `-` (verified on the live projects tree); the same
  slug names the subagent directory and the spool tree. The slug is lossy —
  never decode a path from it.

## Landing 3 relay (2026-08-29, project lead)

- A subagent's AgentId = its spawning call's tool_use_id (meta.json.toolUseId); `agent-<id>` in the filename is never the AgentId.
- `handler.LostTerminal` produces the `lost` cause arms (DetachedLost {file_vanished|went_silent|swept_up}).
- Bash rows the sidecar writes are read back through store.v1 WatchBashRun; the shim's WatchBash is the consumer.
- The sidecar is the ONLY producer of AgentContextInjected (memory files, skills), the write/edit `diagnostics` consequence arm and SessionUpdate.context_budget_warning: the pinned SDK stream carries no attachment records.
