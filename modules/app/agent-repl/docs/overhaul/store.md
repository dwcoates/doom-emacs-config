# Store implementation planning

## Dead code to remove (with the store.v1 service port)
- The UDS + wire-Any framing front end whole: store.v1 is a Connect service
  (WriteBatch/OpenAgentSession/ReadAgentPage/WatchAgentSession/GetLiveWork/
  GetWorkflow/GetSidecarCursors); the old Subscribe/EntryDelivery/
  ConnectionHeartbeat/HealthCheck dial protocol is deleted from the contract.
- The (session_id, seq) + top_level_message_id schema: superseded by the
  settled four-table architecture (agent / workflow / entry spine with
  upsert_key PK / detached_work) under nuke-never-migrate — drop and
  recreate, no migration code.
- Ingest's ErrRecordPersistenceUnreconciled refusal stub: replaced by the
  StoreEntry ingest under the new addressing.

## Blockers / decisions owed (surfaced at reconciliation)
- NO GO SERVICE STUBS EXIST: the Makefile runs protoc-gen-go only, so
  store.v1 (and shim.v1, agentrepl.v1) have message types but no Connect
  handler interfaces in Go — protoc-gen-connect-go must join the codegen
  before any Go service can be implemented. (LEAD-LEVEL, cross-system.)
- StoreItemPointer minting and the row key under StoreEntry's upsert_key
  addressing (the design record's stage-5 architecture entries are the spec).
- AgentSessionToken mint/resolve (OpenAgentSession → WatchAgentSession).
- Idle-producer liveness: ConnectionHeartbeat died deliberately (streams +
  transport own liveness); confirm the sidecar needs no substitute.
- The -health-check JSON/exit-code contract DIES with the port (ruled
  2026-08-29); updating agent-shim-doctor to probe the Connect endpoints
  directly is the store teamlead's task.
- Store health probing: no health verb exists by design; agent-shim-doctor's
  probe re-derives from the Connect endpoints.
- Pre-existing Serve/Close race (trackConn after Accept vs Close snapshot):
  fix in production during the port; tests currently barrier around it.

## Replacement integration-test specs
(Unit specs deliberately absent per the mapping convention.)
- WriteBatch: durable-ack semantics — success = records + cursor advance in
  one transaction; replay absorption by write_id is the same success arm;
  failure = nothing committed (replaces the 23-test ingest suite's subjects
  under the new shapes).
- OpenAgentSession/WatchAgentSession: open answers page + token; watch is a
  pure tail pinned exactly after the page; known_through repaint vs catch-up.
- ReadAgentPage: after-pointer walk, order by first insert, floor/more arms.
- GetLiveWork: ended_at IS NULL scans across agent + detached_work.
- Keep-alive exclusion (Owed G) and logical-session scoping (Owed H): no
  page ever returns a keep-alive row; rotation never splits a page.

## Contract context (for implementers)

Meta: the protos live under `proto/src/` (packages `store/v1`, `conversation/v1`,
`shim/v1`, `agentrepl/v1`, `frontend/v1`, `workspace/v1`); the comments IN the
`.proto` files are the authoritative documentation — read them at the symbol you
implement. Implementers never change protobufs; a needed change is a request up
the orchestration chain. Cross-cutting conventions (response-outcome spelling,
bounded streams, clock convention, validation/logging invariants) are in
`the teamlead prompt (standing conventions) and the proto comments` and are not repeated here.

### What the store is

- The durable half of the conversation: it persists `conversation.v1` facts
  inside a thin storage envelope (`store.v1.StoreEntry`) and serves them back
  to ONE consumer, the shim.
  - Callers are exactly the shim and the sidecar. THE DAEMON NEVER IMPORTS OR
    CALLS store.v1 — the daemon's read path is shim.v1; the isolation is
    enforced at codegen (`check-conversation-isolation.sh`).
- It is deliberately dumb: no variable-size state, no lineage walks, no
  interpretation of the frames it holds. Every observation it supports costs a
  constant number of single indexed lookups (a project-wide principle for
  shim/store/sidecar; only the daemon may hold state).
- store.v1 is a Connect service (`ShimStore`): WriteBatch, OpenAgentSession,
  WatchAgentSession, ReadAgentPage, GetWorkflow, GetLiveWork,
  GetSidecarCursors. There is no subscribe/dial protocol, no heartbeat, and
  deliberately NO health verb — streams and the transport own liveness.

### The datalayer model — two models, one mapping

- The datalayer (`StoreEntry` and the schema) and the protocol
  (`conversation.v1`) are TWO models of the same data. Rows CONTAIN
  conversation.v1 messages; the envelope adds only storage concerns: plane,
  dedup, upsert identity, pageability. The shim/sidecar own the mapping; the
  store never invents or re-derives conversation content.
- `StoreEntry`:
  - `plane` — who observed it: `stream` (the shim, live from the SDK;
    authoritative for session/turn LIFECYCLE) or `file` (the sidecar, reading
    the vendor's own disk; authoritative for conversation CONTENT). Two arms
    only; the daemon has no write path.
  - `write_id` — replay dedup, unique; minted once per write and
    DETERMINISTICALLY (ruled 2026-08-29: a digest of the write's source
    coordinates — the same bytes re-read mint the same id; randomness
    breaks replay absorption), never
    regenerated.
  - `upsert_key` — the row's identity, OPAQUE to the store. One row per key; a
    write supersedes it whole. The mapping (a prompt's TurnId, a unit's
    activity id, a run's id) is the producer's; the store has exactly one
    place it looks, never a per-kind rule.
  - `entry` oneof — `StoreAgentUpdate` or a raw `conversation.v1.SessionUpdate`
    (a session-update row belongs to the main agent).

### Pageability is decided by the PRODUCER

- `StoreAgentUpdate.agent_info` is the whole rule:
  - `serveable_frame` (`StorePageLine`) — a line in some agent's book; it NAMES
    its book (`page_agent_id`). Only these can ever appear in a page.
  - `unserved_item` — held but never served; THE ARM IS WHY: `keepalive` (a
    well-formed fact with no book — keep-alive turns are first-class as
    never-served: no page returns them, no read includes them),
    `vendor_specific`, `unknown`, `unparsed` (the loud residue arms — nothing
    unconvertible is dropped; it lands durably, whole, and investigable).
  - `bash` / `workflow` — detached-run frames, wrapped with the run identity
    (`AgentActivityId` / run `AgentId`) for the join; structurally not page
    lines.
- A page line's content is `StoreAgentItem { AgentPrompt | AgentFrame }` —
  a delivered prompt or an agent's frame. Those are the ONLY pageable kinds.

### The lineage keys

- (Supersession note, 2026-08-29: the vetting register's Owed H clause
  "every record's parent is an AgentId, never nil" is SUPERSEDED by this
  document's nullable design — this doc wins.)
- `StoreAgentUpdate.top_level` — the nearest NON-SYNC ancestor (the turn's
  main agent or a detached-work agent, never a sync subagent): which live
  stream carried the work. Copied from the PARENT'S row at insert — one
  lookup, inductively correct at any depth. Optional: UNSET only when
  unresolvable (an unparsed record may name no agent). Used for kill scope and
  session scope, NEVER for paging.
- `StorePageLine.page_agent_id` — THE pagination key: the book. The agent is
  read from the frame itself (`AgentFrame.agent_id`, `AgentPrompt.agent`),
  never restated on the envelope. A sync subagent is one item (its spawn) in
  its parent's page while its own constituents form its own book.
- Pagination is two-keyed in practice: filter by book, walk by pointer. Order
  is by the unit's FIRST insert, never its last write — so a
  `StoreItemPointer` stays stable across upserts and a unit settling mid-walk
  cannot teleport across a continuation.

### The four tables and canonical homes (schema architecture, not wire)

- `agent` — one row per AgentId, main agent included. THE home of agent
  metadata: `spawned_by_agent` XOR `spawned_by_workflow` (main: neither), the
  unpacked AgentSubagentStart fields, `started_at`, `ended_at` (NULL = live).
- `workflow` — one row per run, keyed by the announced handle: spawner +
  origin unit, unpacked AgentWorkflowStart, terminal once ended. THE SUBAGENT
  LEVEL IS NEVER STORED — it is the join (agents whose spawned_by is the run,
  with their liveness), so agent liveness has exactly one home.
- `entry` — the page lines: a queryable SPINE (`upsert_key` PK,
  `book_agent_id` indexed and NULL for unserveable, `write_id` unique, plane,
  first-insert position) around a SERIALIZED frame the store never opens.
- `detached_work` — one row per detached non-agent run (bash today): handle,
  kind, origin unit, owner agent, unpacked latest state, `ended_at`.
- The columns-vs-blob line: agent/workflow/detached_work are UNPACKED to
  columns (the store filters and joins on them; mapping tests guard the
  unpacking); `entry`'s frame stays a serialized blob (activity vocabulary is
  content — unpacking it would drag every conversation.v1 change into DDL).
- Routing by wire arm, exactly one table per write, plus one dual-write:
  `update` → entry; `success`/`failure` → BOTH entry (the stop notice has no
  other source) AND the agent row's terminal columns, in ONE transaction;
  `detached_work` announcements → the lifecycle table for their kind, never a
  page line (the spawning call already is one).
- Prompts and frames share ONE entry table / one position space (that is what
  makes "everything after the last real prompt" — the keep-alive rollback — a
  range query).

### Open/watch bifurcation and reads

- `OpenAgentSession { agent; page_size; optional known_through }` → the first
  page + `AgentSessionToken watch`. The token is store-minted at THIS open, a
  hash of the identity, never the identity — a caller MUST open to watch. It
  pins the tail to begin exactly after the page's newest item: nothing missed
  or doubled between page and stream.
- `known_through` is the caller's own high-water mark: UNSET = full repaint;
  SET = catch-up, only newer items; a gap wider than page_size is walked older
  via ReadAgentPage until the caller meets its own mark. The store tracks
  NOTHING about what it previously served.
- BACKPRESSURE (ruled 2026-08-29): per-subscriber buffering is the
  lead's call; if bounded, the bound must be SUBSTANTIALLY higher than
  ~1k frames (daemon bounces produce bursts); a dropped watcher recovers
  by re-opening with known_through catch-up.
- `WatchAgentSession { token }` → a STANDING stream of `StoreLineAt` (one
  frame per written line, upserts included; every line carries its pointer so
  the caller always holds a current mark). Pure tail; creates nothing, ends
  nothing.
- `ReadAgentPage` is next-only: `after` (a served pointer) is required; the
  first page is always the open's answer. Response boundary is
  `more { last_item } | floor` — echo `last_item` to continue; page_size rides
  each request and may vary across one walk.
- `GetWorkflow { DetachedWorkId }` → the workflow row + `live { derived level }
  | ended { embedded terminal }`; the level is computed at serve time from the
  agent table, stored nowhere. The shim's own GetWorkflow serves from this.
- `GetLiveWork {}` → ids only from the `ended_at IS NULL` scans (live agents,
  live workflows, live detached). "Live" is a claim about the RECORD ("a start
  was written and no terminal ever was") — timeless, cannot go stale. The SHIM
  calls it once at session start and resolves every item (re-adopt or write
  the closing terminal); the sidecar never calls it.

### WriteBatch semantics

- `WriteBatchRequest { producer; EntryBatch { entries; optional
  cursor_advance } }`. The rpc is the envelope; no separate carrier exists.
- Success means DURABLE: records + cursor advance committed as ONE
  transaction. A replayed batch whose write_ids all landed before is THE SAME
  success arm — absorption is success, and the producer retires the batch
  from its retry buffer either way.
- Failure means NOTHING committed — the transaction fails whole. The producer
  holds the batch in a BOUNDED IN-MEMORY retry buffer (there is deliberately
  no durable producer-side spill; exhausted retries are a loud failure). The
  sidecar needs no buffer at all: its sources are durable files it re-reads
  from the cursor.
- `cursor_advance` is UNSET for stream-plane writers (no file to be positioned
  in) and set by the sidecar — the cursor rides the batch on purpose: the
  exactly-once contract is the position advancing in the same transaction as
  the records read at it.

### Cursor semantics

- `CursorState { file_id; path; offset; carry }` per tailed file:
  - `file_id` is the file's stable identity ("dev:inode"), surviving the
    vendor's renames/rotation — a path-keyed cursor would restart a renamed
    file from zero.
  - `offset` is where the NEXT read starts; `carry` is the bounded bytes of an
    incomplete final line so a split line parses once and whole.
- `GetSidecarCursors { optional file_id }` is the sidecar's startup recovery;
  empty success is the fresh-store answer (start every file from zero).

### Standing policies and gotchas

- THE STORE IS NUKED, NEVER MIGRATED: no backfill, no hydration, no schema
  migration, ever. Where existing contents are in the way, drop and recreate.
  Writing migration code, or preserving a shape on durable-compatibility
  grounds, is forbidden.
- Logical-session scoping: the store scopes by the logical session / OUR
  main-agent id (shim-minted once, persisted, reported unchanged on every
  later start); the vendor session id is a mutable attribute — an identity
  rotation must never split an agent's book or a page.
- No keep-alive row is ever returned by any page (a covered integration
  subject); rotation never splits a page.
- No `seq` exists anywhere on any wire; positions are opaque store-minted
  pointers and tokens, echoed verbatim, never parsed or constructed by a
  caller.
- The re-announced `start` instant is recovered FROM the store by unit id
  (a shim-restart path): rows must be reachable by their upsert identity.
- "Main agent" exists only inside the shim and as the store's internal scope;
  no consumer of the shim ever sees the term.

### The abstract surface, by package

- `store/v1` (own it): `store.proto` (StoreEntry, the envelope arms, Plane,
  the residue messages, EntryBatch, CursorState, the shared page vocabulary —
  StoreItemPointer, StoreLineAt, AgentSessionToken, AgentSessionPage,
  More/Floor), `service.proto`, and one `endpoint_*.proto` per rpc. Every
  response is `oneof result { success | failure }`; failure `kind` arms are
  DERIVED at implementation from real refusal sites, never invented —
  `detail` strings are for humans and logs, never switched on.
- `conversation/v1` (know its shape): the rows ARE its messages. `AgentPrompt
  { TurnId; AgentId; UserSaid }` is the one form of a delivered prompt;
  `AgentFrame { agent_id; update | success | failure | detached_work }` is
  the one frame of any agent's stream; a frame is an UPSERT of its whole unit
  (identity per THING, not per arrival). `AgentBash` / `AgentWorkflow` are
  the detached-run frame families; `SessionUpdate` is the session-scoped
  fact stream. The store never opens any of these beyond routing by arm.

### Additional rulings (final-audit triage, 2026-08-29)

- PPROF: an opt-in, LOCAL-ONLY profiling surface (unix socket or
  loopback; wildcard refused), opened BEFORE the database so a wedged
  migration/boot is still diagnosable.
- LOG CORRELATION: consolidated with the project lead under the
  /debug-emacs-agent-repl logging contract (retired addressing keys die
  with the schema).
- KEEP-ALIVE OWNERSHIP: the keep-alive stamp on turns is written by the
  SHIM at submission; the store's never-served index rides that stamp —
  the daemon holds no keep-alive knowledge.

## Kickoff increments and rulings (2026-08-29, project lead)

- LANDED: AgentUpdate `context_cut` and `api_error` are page lines (an
  `update` arm never touches the agent row's terminal columns). No failure
  arm on WatchAgentSession: a refused watch (unknown/consumed token, store
  restarted) closes at the transport with Connect CodeNotFound and the
  shim re-opens — the contract's refused-open convention.
- R11: the doctor probes the Connect endpoints; the Serve/Close race is fixed
  in production; the private test socket is env AGENT_REPL_STORE_SOCKET (a
  flag beats it). The upsert-key rule and keep-alive marker in shim.md are
  the cross-plane contract. Go modules pin connect v1.17.0 + x/net v0.43.0.
  agent-shim/wire is dropped as a dependency here and deleted by the daemon
  rewrite. Workflow: the table exists, nothing routes into it; GetWorkflow
  answers the typed not-implemented failure.

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

## Landing 3 relay (2026-08-29, project lead)

- Every refusal site sets its `kind` arm (keeping `detail`), asserted per refusal.
- ReadAgentPageSuccess.lines is `repeated StoreLineAt`: continuation pages carry real pointers.
- New rpc WatchBashRun {run: AgentActivityId} → stream {row: StoreAgentBash}: replay every stored row of the run in write order, then follow, end after the terminal; unknown run = refused open at the transport (no failure frame).
- AgentFrame.detached_work is a PAGE LINE (upsert key `detached:<work id>`), the source of GetLiveWork.live_detached; lifecycle tables never hold the announcement.
- R9 (shim-settled): rotation/fork lineage lives in the shim's link files, not in the store; no store verb.
- DetachedLost `lost` arms replace the sidecar's LostTerminal placeholder.

## Landing 6 relay (2026-09-01, project lead)

- No store.v1 or conversation.v1 change in landing 6.

## Landing 7 relay (2026-09-02, project lead)

Adapt to protos ab7e681f2 / bindings c10714a41 (see PROTO-CHANGES.md):
- OpenAgentSessionFailure.unknown_agent (tag 5): a well-formed agent id that
  names no book of this store is REFUSED with this arm instead of served
  empty. Distinguish it from a real agent with no lines yet (that one still
  serves an empty page). Log at the refusal site with `refusal_site`.
- No sidecar change.

## Landing 8 relay (2026-09-02, project lead)

- No shim.v1 / store.v1 / conversation.v1 change in landing 8 (frontend.v1 only).
