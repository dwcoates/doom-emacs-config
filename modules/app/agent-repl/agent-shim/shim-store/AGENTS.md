# agent-shim/shim-store/

The record store (Go, singleton, launchd-managed): the sole owner of the record
database (SQLite/WAL) and the only server of `store.v1.ShimStore`. Its callers
are exactly the shim and the sidecar.

**THE DAEMON NEVER IMPORTS OR CALLS `store.v1`.** The daemon's read path is
`shim.v1`; `check-conversation-isolation.sh` refuses the import at codegen.

The protos under `proto/src/store/v1/` are the authoritative documentation —
read them at the symbol you are implementing. Nothing here ever changes a
`.proto`; a needed change goes up the orchestration chain.

## Transport: Connect over a unix domain socket

- `net.Listen("unix", path)` plus `http.Server{Handler: h2c.NewHandler(mux,
  &http2.Server{})}`, so HTTP/1.1 and prior-knowledge HTTP/2 both reach the
  same handler on one cleartext socket, and both Connect codecs (binary and
  JSON) come for free. `storev1connect.NewShimStoreHandler` does the routing.
- **THERE IS NO HEALTH VERB, BY DESIGN.** Streams and the transport own
  liveness. `agent-shim-doctor` probes the Connect endpoints directly. The old
  `Subscribe`/`EntryDelivery`/`ConnectionHeartbeat`/`HealthCheck` dial
  protocol, the `agentrepl/wire` framing, `internal/healthcheck` and the
  `-health-check`/`-health-request-id`/`-health-timeout` trio are all DELETED.
  Do not reintroduce a health rpc, a heartbeat, or a write ack.
- **The Serve/Close race is gone.** Nothing tracks connections by hand
  (`trackConn` after `Accept` used to race the `Close` snapshot);
  `http.Server` owns connection lifetime and `Server.Shutdown(ctx)` is the only
  stop. Shutdown closes a `done` channel FIRST so standing watches return, then
  drains — a pure tail would otherwise make `http.Server.Shutdown` wait
  forever. Tests must not barrier around any of this.
- **`WatchBashRun` is the one stream with a NATURAL END.** A book never ends —
  an agent can always say more — but a shell run concludes, and its conclusion
  is a row the store recognizes, so the stream closes after it. The caller needs
  no cancellation protocol and no timeout to know it has the whole run.
- A watch over the **Connect protocol is half-duplex**: the client's
  `WatchAgentSession` call does not return until the server writes response
  headers, which it does on its first frame. So **acceptance is silent** — a
  standing watch on a quiet agent simply has not answered yet — and **refusal
  is `CodeNotFound` on the first `Receive`**. That is acceptable because the
  consumer opened with a page first. A test that must observe a subscribed but
  undelivered watch runs the call off its own goroutine.

## Flags

| flag | default |
| --- | --- |
| `--socket` | `$AGENT_REPL_STORE_SOCKET`, else `~/.cache/agent-repl/sock/store.sock` (`XDG_CACHE_HOME` honored) |
| `--db` | `…/store/events.db` |
| `--log` | `…/log/shim-store.log` (size-capped, N generations; NOT mirrored to stderr) |
| `--pprof` | `$AGENT_REPL_STORE_PPROF_ADDR`, else OFF |
| `--watch-buffer` | `8192` frames per subscriber |

`--log` is opened through `agentrepl/logging`.`OpenRotating`: it appends to
what it finds and ROLLS AT A BYTE CAP into a fixed number of generations
(`<path>.1` newest through `<path>.N` oldest), so the store's disk footprint is
`(N+1) x cap` however long the process runs. It does NOT roll on open, because
a launchd service is bounced by every deploy and every crash.

THE TERMINAL IS NOT A SECOND LOG. Production builds the logger with
`logging.NewDurableOnly`, so ordinary records reach the durable sink ALONE.
Under launchd stderr is an append-only file the process neither owns nor can
roll, and mirroring every record there is an unbounded second copy of an
already-rotated log. The terminal keeps the BOOTSTRAP errors written before a
logger exists and the SINK-FAILURE record, which is the one thing the durable
sink cannot report about itself.

`AGENT_REPL_STORE_SOCKET` is only the flag's DEFAULT, so an explicit `--socket`
always beats it. That is how a test harness points every participant at a
private store without editing a command line.

**THE SOCKET IS THE STORE'S SINGLETON TOKEN, AND THE KERNEL ARBITRATES IT.**
Before unlinking a socket already at the listen path, the store DIALS it. A path
that accepts is owned by a live store: the boot is refused with one
`store.listen.occupied` error record and a non-zero exit, because unlinking it
would leave the incumbent serving a socket no client can reach while this process
silently took its callers. A path that refuses is debris, reclaimed with the
existing `store.listen.reclaim` warning. A non-socket is never touched at all.

**The signal handler is installed before anything is bound or opened.** The
kernel queues connections from `listen(2)` onward, so a supervisor that waits for
the socket sees a ready store the moment it binds; installed any later, `SIGTERM`
in that window still had its default disposition and killed the process outright,
leaving the socket file behind for the successor to reclaim. "The socket accepts"
now implies "signals are answered", and a slow boot is interruptible rather than
unkillable.

**Boot order is load-bearing: the pprof surface is opened BEFORE the
database**, so a store wedged recreating its schema or on a cold first read is
still profilable. `--pprof` is opt-in and local-only (a unix socket path, or an
explicitly loopback `host:port`); a wildcard or routable bind is refused rather
than served and hoped about, and a configured surface that cannot bind is a
hard error. Both outcomes are recorded (`store.pprof.disabled` /
`store.pprof.enabled`).

## The four tables

- `agent` — one row per `AgentId`, main agent included. THE home of agent
  metadata: `spawned_by_agent` XOR `spawned_by_workflow` (main: neither), the
  unpacked `AgentSubagentStart` fields, `started_at_ms`, `ended_at_ms`
  (NULL = live), `terminal`.
- `entry` — the spine: `position` (FIRST-insert order — the page order and the
  pointer; an upsert keeps it), `upsert_key` UNIQUE, `write_id` UNIQUE,
  `write_seq` (store-internal global write ordinal, bumped on every insert AND
  every upsert), `plane`, `kind`, `book_agent_id` (NULL for every never-served
  row — the keep-alive/residue index), `run_id` (a bash row's run; NULL
  otherwise — the `WatchBashRun` index, and the bash equivalent of
  `book_agent_id`), `top_level`, and `frame`, the serialized `StoreEntry` the
  store never opens beyond routing.
- `write_ledger` — one row per write ever APPLIED (`write_id` PK, `upsert_key`,
  `write_seq`, `applied_at_ms`), written in the same transaction as the row it
  applied. **ABSORPTION ASKS THIS TABLE, NEVER `entry.write_id`.** `entry` holds
  only the LATEST write applied to a row, so probing it answered "is this the
  write that currently owns the row?" — and a producer replaying w1 after w2
  settled the same `upsert_key` read as never-seen, overwrote the newer content
  and bumped `write_seq`, re-delivering the regression to every live watcher.
- `detached_work` — **THE JOIN AND NOTHING ELSE**: `work_id`, `kind`,
  `origin_unit`, `owner_agent`, `announced_at_ms`, `ended_at_ms`, `terminal`.
  The announcement itself is a page line and is the one durable copy of what was
  announced (the spool, its readability, the detach cause, the timeout);
  unpacking any of that here too would give one fact two homes that can disagree.
- `workflow` — the table exists and **NOTHING routes into it this wave**.

`agent`/`workflow`/`detached_work` are UNPACKED to columns because the store
filters and joins on them; `entry`'s frame stays a BLOB because activity
vocabulary is content, and unpacking it would drag every `conversation.v1`
change into DDL.

### The store is NUKED, never migrated

`db.Open` checks `schema_meta.version`; any mismatch, or any pre-existing table
set that differs, REMOVES THE DATABASE FILE and creates a fresh one. There is no
`ALTER`, ever, and no migration code. Writing migration code, or preserving a
stored shape on durable-compatibility grounds, is forbidden.

**THE NUKE IS AN UNLINK, NEVER A `DROP TABLE`.** Emptying a foreign schema in
place walks every page of what it discards: on 2026-09-09 the store met an
11.5 GB `events.db` at a superseded version, ran the DROP for minutes with no
socket listening, and `deploy-all.sh` gave up waiting for `store.sock` and left
the sidecar and the runtime un-bounced. Unlinking costs the same whatever the
file weighs, so `ensureSchema` never drops: it returns a `schemaMismatchError`
naming what it found, and `Open` — the layer that owns the file — closes the
handle, removes the file with its siblings, and reopens onto an empty one. The
mismatch is recorded ONCE, as a warn from `Open`, naming the found version and
table set and saying the file was removed.

**A `--db` FILE THAT IS NOT A DATABASE IS IN THE WAY, SO IT GOES** — the same
answer, because the store holds a cache of what the vendor and the shim already
know how to produce again, and refusing to boot would wedge the service on bytes
nobody can read. It is removed with its `-wal`/`-shm` siblings (a stale WAL
beside a fresh database is how a "recreated" store comes up carrying fragments of
the one it replaced) and recreated, with a warning naming the cause. Two guards:
the nuke happens ONCE (a second failure is a real problem — an unwritable
directory, a full disk — and is returned), and only for a REGULAR FILE. A
directory at `--db` is reported, never replaced: unlinking whatever sits at an
operator-supplied path is how a service deletes somebody's data.

### Any transaction that writes must BEGIN IMMEDIATE

The DSN carries `_txlock=immediate` (plus WAL, `busy_timeout`,
`synchronous(NORMAL)`). A write batch reads before it inserts, so a DEFERRED
transaction takes a WAL read snapshot and then tries to upgrade — and SQLite
will not run the busy handler for an upgrade: it returns `SQLITE_BUSY_SNAPSHOT`
(517) or `SQLITE_BUSY` (5) immediately, so `busy_timeout` never applies. One
store process serves every live producer on its own pooled connection, which
makes those collisions routine. Keep the DSN, and never add a read-then-write
transaction that begins DEFERRED.

## Routing, orderings, pointers, tokens

- **Pageability is the PRODUCER's decision**, read from exactly one place:
  `StoreAgentUpdate.agent_info`. `serveable_frame` names its book
  (`page_agent_id`) and is the ONLY thing a page can ever return;
  `unserved_item` (keepalive / vendor_specific / unknown / unparsed) is durable
  and never served; `bash` and `workflow` are structurally not page lines.
- One transaction per batch, and **failure commits nothing**. Per entry, in
  producer order: absorb by `write_id` against the **write ledger** (a hit is
  success), else check that the upsert does not change the row's IDENTITY, then
  upsert by `upsert_key` and route by arm. `success`/`failure` frames are a DUAL
  WRITE — the page line AND the agent row's terminal columns, in the one
  transaction. `cursor_advance` upserts in the same transaction, which is the
  whole exactly-once guarantee; **a cursor-only batch is LEGAL**, because a
  sidecar that read bytes yielding no entries must still advance or it re-reads
  them forever.
- **AN UPSERT SUPERSEDES CONTENT, NEVER IDENTITY.** A write whose `book_agent_id`
  or `kind` differs from the existing row's is refused
  (`upsert_changes_identity`). `upsert_key` names one thing, and the page model
  rests on it: moving a row between books would leave every pointer already
  served for it naming a line of a book the caller never asked about, and
  changing its kind would turn a served page line into unservable residue under
  a pointer that still exists.
- **A DETACHED-WORK ANNOUNCEMENT IS A PAGE LINE** of the announcing agent's book
  (`AgentFrame.detached_work`, keyed by the producer as `detached:<work id>`),
  and it is the SOURCE of `GetLiveWork.live_detached`. "Work left this stream" is
  the handoff a reader must see; a feed that drew the spawning call without it
  keeps claiming work that is no longer in the turn.
- **A BASH FRAME IS ITS OWN ENTRY ROW** (`kind = bash`, book NULL,
  `run_id = StoreAgentBash.run.value`) as well as a `detached_work` update. A
  detached run's output arrives as deltas, so the run's history lives in the
  spine under its own indexed key the way a book's does. Producers key the rows
  (`bash:<run>:start`, `bash:<run>:<from_offset>`, `bash:<run>:terminal`); **the
  store never parses a key.** It is still not a page line: a run has no book, and
  its reader is `WatchBashRun`.
- **ONE `detached_work` ROW PER RUN, LOCATED BY ORIGIN UNIT FIRST.** The
  announcement addresses a run by its `DetachedWorkId`; the run's own frames
  address it by its `AgentActivityId`. Both writers resolve the row the same way
  — if any row already carries this origin unit, that row IS the run; otherwise
  the writer's own identity keys it and the later writer finds it through the
  same lookup. Symmetric, because the file plane can observe a spool before the
  stream plane announces it. (Landing 4 mints the two to the same bytes; the
  lookup still converges on one row when they coincide.)
- `agent_update.workflow` lands durably as `kind=workflow` with a WARNING that
  workflow ingestion is not implemented this wave; a workflow-kind ANNOUNCEMENT
  is a page line like any other and raises the same warning. Nothing touches the
  workflow table. `GetWorkflow` answers the typed not-implemented failure.
- **RESIDUE MUST CARRY ITS VERBATIM RECORD.** `vendor_specific` and `unknown`
  with no `raw`, and `unparsed` with empty `raw`, are refused
  (`residue_raw_unset`): a row saying only "there was something here" IS the drop
  the residue arms exist to prevent, and the follow-up work — a converter, a
  model, a parser fix — is impossible without the bytes. A keep-alive is exempt;
  it is a well-formed fact with no book, not material that failed to convert.
- **THE ENVELOPE AND THE FRAME MUST AGREE.** A page line whose `page_agent_id`
  differs from the frame's own agent (`AgentFrame.agent_id`, `AgentPrompt.agent`)
  is refused (`page_book_mismatch`). They are two statements about one line and
  the store cannot pick a winner; accepting either files an agent's words under
  another agent's name.
- **Order is by FIRST insert, never last write**, so a `StoreItemPointer` is
  stable across upserts and a unit settling mid-walk cannot teleport across a
  continuation. Pointers are opaque encodings of `position`, echoed verbatim; a
  pointer that names no row IN THAT BOOK is a stale-pointer refusal. No `seq`
  exists on any wire.
- `OpenAgentSession` answers the page plus a store-minted `AgentSessionToken`
  (128 random bits from `crypto/rand`, hex) — **UNLESS THE REQUEST SAID
  `page_only`**, which is the caller stating that no watch follows: nothing is
  minted and `watch` comes back UNSET. The caller has to state it because the
  store cannot work it out — the rpc is unary and the service has no close, so
  a token minted for a page that is then abandoned can never be reclaimed and
  lives for the whole process lifetime (measured at two per turn, 2026-09-10;
  every one-shot read in the shim — the turn's opening page, the teardown's
  book head, the reconciliation read, the live-work re-announcement — now says
  `page_only`). A watch attempted from such an open has no token to present and
  meets the ordinary unknown/empty-token refusal; there is no arm of its own.
  The token is **SINGLE-USE**:
  `WatchAgentSession` consumes it, and an unknown, consumed, or
  previous-process token is refused. Tokens live in memory and do not survive a
  restart — "you already used this" and "the store restarted" are the same
  recovery for the caller: re-open. **An agent the store HAS HEARD OF with no
  rows is a legal EMPTY book** (empty page, floor, valid token) — a freshly
  spawned subagent has an `agent` row from its spawn frame before it says a
  word, so it is openable and watchable immediately.
- **AN AGENT ID THE STORE HOLDS NO `agent` ROW FOR NAMES NO BOOK AND IS
  REFUSED** with `OpenAgentSessionFailure.unknown_agent` (landing 7), never
  served an empty page; the shim maps it to `CodeNotFound`. Serving it told a
  caller with a stale or mistyped target exactly what it told a caller watching
  a live agent that had not spoken yet, so the two were indistinguishable and
  the mistake looked like patience. THE `agent` TABLE IS THE REGISTER that
  separates them — every page-line write ensures a row there, so "no row" is
  "never heard of" — and it is asked BEFORE the pointer, because a
  `known_through` against a book that does not exist is stale only as a
  consequence, and `stale_pointer` would send the caller off to repaint a book
  nobody ever kept. It is its own refusal class (`db.ErrUnknownAgent`) with its
  own site and arm, both spelled `unknown_agent`: the request is well formed and
  respelling it cannot help, so it is neither `invalid_request` nor a race to
  retry. `ReadAgentPage` is unchanged — it always carries a pointer, which is
  already stale for a book that does not exist.
- The watch pin is the global `write_seq` at the moment the page was read,
  taken INSIDE that read transaction. `WatchAgentSession` then **subscribes to
  the fan-out BEFORE running the replay query** and dedupes by `write_seq`, so
  the replay-to-live handoff is gapless and duplicate-free. Upserts of old rows
  stream at their ORIGINAL pointer. No keep-alive or residue row is ever
  streamed, and there is no terminal frame: the stream ends only when the
  client cancels or the store shuts down.
- `WatchBashRun` replays every stored row of a run in **FIRST-INSERT order**
  (`position`), then follows live rows, and ENDS after a `success`/`failure` row
  — including when the terminal was already stored, in which case the replay IS
  the whole answer. **THE REPLAY SERVES EVERY STORED ROW AND THE END FIRES AFTER
  THE LAST OF THEM**, not at the terminal: a delta the sidecar reached only once
  the spool was already closed is first inserted AFTER the terminal's row, and
  stopping there handed the consumer a run that produced less output than it did.
  A terminal re-upsert against an ended stream is absorbed silently — nothing is
  re-sent to watchers that already ended, and a fresh replay still serves the
  terminal exactly once. The order is `position` and not `write_seq` because a run's
  rows are a spool being filled in: a redelivered delta upserts its row and must
  appear where it always was. (`WatchAgentSession` replays by `write_seq` for the
  opposite reason: a book's upsert is NEW INFORMATION about a line the caller has
  already read past.) A run the store holds no row for is a refused open at the
  transport (`CodeNotFound`) — the absence of a row IS the unknown-run signal, so
  no sentinel crosses the db/server contract; an empty run identity is
  `CodeInvalidArgument`, because the caller must fix the request rather than
  conclude the run does not exist. There is no token: a run is addressed by the
  identity the spawning stream already announced.
- Backpressure: a bounded per-subscriber channel of `--watch-buffer` frames.
  Publishing is a non-blocking send under the fan-out lock, so one slow reader
  can never stall a writer's acknowledgement. On overflow the store logs a
  WARNING and **ENDS that stream with `CodeResourceExhausted`**; recovery is a
  re-open with `known_through`. Never silently thin a subscriber.
- **`AgentFrame.detached_work` IS A PAGE LINE FOR EVERY `DetachableWork` KIND,
  WORKFLOW INCLUDED.** "Work left this stream" is the handoff the announcing
  agent's book has to show, and it is the one durable copy of what was announced;
  drawing the spawning call without it leaves the feed claiming work that is
  still in the turn. The workflow kick affects only what is SERVED BACK:
  `live_detached` excludes workflow and `GetWorkflow` answers `not_implemented`,
  while the announcement itself is stored and paged like any other.
- `GetLiveWork` is the `ended_at IS NULL` scans. "Live" is a claim about the
  RECORD — a start was written and no terminal ever was — so it is timeless and
  cannot go stale. Main agents are never listed; `live_workflows` is empty this
  wave.

## Refusal sites

Every response is `oneof result { success | failure }`. `detail` strings are
for humans and logs and are **never switched on**. The sites below are the real
places the store says no; they are the vocabulary the proto's failure `kind`
arms are derived from, and each one is logged once with `refusal_site`.

`producer_empty`, `batch_missing`, `batch_empty`, `entry_plane_unset`,
`entry_write_id_empty`, `entry_upsert_key_empty`, `entry_arm_unset`,
`cursor_file_id_empty`, `agent_id_empty`, `page_size_zero`, `pointer_empty`,
`token_empty`, `unknown_watch_token`, `file_id_empty`, `run_empty`,
`unknown_bash_run`, `store_refused_request`, `upsert_changes_identity`,
`page_book_mismatch`, `residue_raw_unset`, `stale_pointer`, `unknown_agent`,
`database_failure`, `workflow_not_implemented`, `watch_buffer_overflow`,
`listen_occupied`.

- **THE SITE IS NOT THE ARM.** A site says which of the store's many checks said
  no — the vocabulary an operator counts by — while the failure's `kind` arm says
  which typed answer the caller receives, and several sites map to one arm.
  Keeping them apart is what lets a new site be added without inventing a wire
  arm. The sites `internal/db` decides live in `internal/db`, and
  `internal/server` ALIASES the constants rather than restating them.
- **EVERY REFUSAL SETS ITS `kind` ARM AND NAMES THE FIELD IT BLAMES.** `detail`
  is prose; a caller switches on the arm and reads `invalid_request.field`, which
  is the store's own name for what was wrong, with the entry index where one
  applies (`entries[1].upsert_key`).
- **`invalid_request.field` IS A FULL ENVELOPE PATH.** The vocabulary is owned by
  the store — `internal/db` for anything inside an entry, `internal/server` for
  the request around it — and every path is walkable from the message the
  producer sent, so a caller never has to guess the top of it. The forms are:
  request-level (`producer`, `batch`, `agent`, `book`, `page_size`,
  `known_through`, `after`, `watch`, `run`, `file_id`, `work`); batch-level
  (`cursor_advance.file_id`); and entry-level, always rooted at
  `entries[i]` — `entries[i].write_id`, `entries[i].upsert_key`,
  `entries[i].plane`, `entries[i].entry`, `entries[i].agent_update…`,
  `entries[i].session_update`. Anything inside a page line's frame carries the
  whole prefix it is reached through:
  `entries[i].agent_update.serveable_frame.agent_item.agent_frame.agent_id`, not
  `agent_frame.agent_id`. A path that starts halfway down names a field that
  appears nowhere in the request. `internal/server` never opens a frame, so
  the site and the field for anything INSIDE one come up from `internal/db`
  through the refusal it returns. Per verb: `WriteBatch`
  invalid_request|storage_failure (a stale pointer is unreachable — the verb
  names no position); `OpenAgentSession`
  invalid_request|stale_pointer|storage_failure|unknown_agent; `ReadAgentPage`
  all three; `GetLiveWork`
  storage_failure only, because it takes no request fields; `GetSidecarCursors`
  invalid_request|storage_failure; `GetWorkflow` not_implemented, the one honest
  arm while nothing routes into the workflow table.

- Validation runs **before the store is touched**: a refused request never
  opens a transaction. An unset non-optional field is illegal, an unset oneof
  is an error, and absence is spelled with optional presence — never `""`, `0`
  or `-1`. A batch refusal's detail names the offending entry index and its
  `write_id`.
- **A refusal is an HTTP 200 with the typed failure arm.** `connect.NewError`
  is reserved for transport and malformed-protobuf cases — plus the two rpcs
  that have no failure arm: `WatchAgentSession` (`CodeNotFound` for a refused
  open, `CodeResourceExhausted` for overflow, `CodeInternal` for a replay
  failure).
- Never swallow, default away, or fall back. Fallback persistence, silent
  continue and limp-along modes are forbidden.

## Logging

- `internal/logging` is the store's ONE canonical JSON logging API. Direct
  `fmt`, `log`, `slog` or ad hoc diagnostics are forbidden except the
  documented pre-logger bootstrap failure and the logger's own sink-emergency
  path.
- Every logical branch logs its selection: verbose for the ordinary path,
  `warn` for degraded-but-handled, `error` for failures. **Every error is
  logged exactly once, by its owning layer — and WHO OWNS IT DEPENDS ON WHOSE
  FAULT IT IS.**
  - A REFUSED REQUEST (`ErrInvalid`, `ErrStalePointer`) belongs to the CALL.
    `internal/db` traces it at VERBOSE — its statement and table are context, and
    it can name neither the rpc nor the request id nor the producer — and
    `internal/server` writes the single normal-level record, at `warn`. Emitting
    both put two normal-level records on one refusal and made the rule false
    wherever anyone counted.
  - **A STALE POINTER IS NOT AN ERROR.** It is an ordinary race — the caller
    walked a book that moved — and its recovery is a repaint. It is a `warn` and
    never an `error`, because a healthy store writing error records during normal
    operation is how an error log stops being read.
  - A STORAGE FAILURE is `internal/db`'s own, with statement and table context
    nothing above can supply: its `error` record stays there and the server adds
    only a VERBOSE trace tying the rpc to it.
- **The caller's request id reaches the storage layer through the CONTEXT**
  (`logging.ContextWithRequestID`), not through a logger: the db's logger is built
  once at boot and belongs to the process, while a request id belongs to one call
  in flight. Every statement family the store runs emits a verbose
  `store.db.statement` record carrying it — which is what lets a test assert that
  a REFUSED request never opened a transaction. No `Store` method signature
  changed to carry it.
- Correlation keys, each in its own `context` field and never left to the
  message text: `producer`, `agent_id`, `vendor_session_id`, `book_agent_id`,
  `write_id`, `upsert_key`, `position`, `write_seq`, `watch_token_hash`, `rpc`,
  `refusal_site`, `refusal_kind`, `file_id`, `path`, `offset`, `task_id`,
  `activity_id`, `turn_id`. Top-level `request_id` comes from the `X-Agent-Repl-Request-Id`
  header when a caller sends one. `rpc` is the Connect procedure spelled
  exactly as Connect spells it, leading slash included
  (`/store.v1.ShimStore/WriteBatch`).
- **EVERY REFUSAL RECORD CARRIES BOTH `refusal_site` AND `refusal_kind`.** The
  site is which of the store's checks said no; the kind is the wire arm the
  caller received (`invalid_request`, `stale_pointer`, `storage_failure`,
  `not_implemented`, `unknown_agent`), derived from the refusal's class in `logRefusal` and never
  restated by hand. Several sites map to one arm, so a record naming only the
  site leaves a reader unable to tell whether the caller could ever have retried,
  and one naming only the arm leaves it unable to find the check that fired.
- **The retired addressing keys are DEAD**: `claude_session_id`, `seq`,
  `from_seq`, `replay_*_seq` and anything else naming the old `(session_id,
  seq)` addressing. Do not add one back.
- A **watch token is never logged** — only `watch_token_hash`, a sha256 prefix.
  A token is a capability.
- `offset` is a `*int64` on purpose: zero is a meaningful file offset and must
  be reported, not omitted as unset.
- Hot per-record and per-batch success diagnostics go through the VERBOSE
  helper (gated by `AGENT_REPL_LOG_VERBOSE` at startup). Lifecycle, invariant
  violations, refusals and failures are normal verbosity. Slow queries are the
  one deliberate exception: a statement past
  `AGENT_REPL_STORE_SLOW_QUERY_MS` (default 250ms) emits a normal-verbosity
  `warn` at `store.db.slow-query` with `statement`, `duration_ms`, `rows` and
  `threshold_ms`, because by the time an operator knows to look the stall is
  over. `statement` is a FAMILY NAME, never rendered SQL and never bound
  values — the payloads are opaque to the store, and quoting a parameterized
  statement would put session content into the global log. A malformed
  threshold aborts `db.Open`.

Read store records and harvest run windows through `../../bin/logs.sh`; the
full path, rotation, attribution, and level-switch table is in
`../../AGENTS.md`.

## Suites

```bash
cd modules/app/agent-repl/agent-shim/shim-store
go build ./... && go vet ./... && go test -race ./...
go test -race ./internal/server/      # the Connect service, with a fake Store
go test -race ./internal/db/          # the schema, routing and reads
go test ./integration/                # the whole module end to end over a socket
make coverage                         # ../../bin/report-nonlisp-coverage.sh store
```

- Go tests are table-driven, AAA, one edge case per test function, one
  `_test.go` per source file.
- **Never `time.Sleep` for synchronization.** Use channels, `sync.WaitGroup`,
  contexts, or a real signal — a Connect stream delivering a frame is a
  legitimate primitive. `internal/server`'s fake `Store` gates `LinesSince`,
  which is entered strictly after the handler subscribes; that gate is how the
  replay-to-live handoff, buffer overflow and shutdown paths are driven
  deterministically.
- **Unix socket paths must be short.** macOS caps `sun_path` at ~104 bytes, so
  never build one under `t.TempDir()`; use
  `filepath.Join(os.TempDir(), "ar-"+<8 random hex>+".sock")` and remove it in
  cleanup.
- Every test process exports `AGENT_REPL_FORBID_VENDOR_CALLS=1` from `TestMain`.
  Nothing here ever calls a vendor.
- **`integration/` asserts on record KIND AND FIELD SET, never on detail prose.**
  A refusal subject scopes to the operation that owns it (`recordsAtOperation`),
  counts it (`assertExactlyOneNormalRecord` — every error is logged exactly once
  by its owning layer), and asserts the correlation keys the refusal is looked up
  by (`refusal_site` AND `refusal_kind` together, `watch_token_hash`, `rpc`,
  `write_id`, `task_id`) — `assertRefusalKeys` is the helper for the first pair. A level
  filter alone is not an assertion: "some warn was logged" passes for a reclaimed
  socket or a slow query as readily as for the thing under test.
- **`assertNoDatabaseTouch` is scoped by `request_id` and needs `verbose: true`.**
  It proves one refused request never opened a transaction, by finding no
  statement trace carrying that request's id. It carries no per-window positive
  control on purpose — a refusal is often the only call in its window — so the
  control is the global one,
  `TestAnAcceptedRequestDoesLeaveAStatementRecordCarryingItsId`, which fails the
  moment the store stops leaving the mark the scan looks for.
- Store-side fixture keys reproduce the producers' real spellings exactly:
  `bash:<run>:start` / `bash:<run>:<from_offset>` / `bash:<run>:terminal`,
  `detached:<work id>` for an announcement,
  `session:context_budget_warning:<uuid>`, and
  `residue:<vendor record uuid>` with the `residue:file:<path>:<offset>`
  fallback. A subject that keys an announcement by a unit key is testing a
  spelling no producer uses.
- **The fan-out is generic over its item**, keyed by book for page lines and by
  run for bash rows. Two hand-copied registries would be two places for the
  non-blocking-publish and overflow rules to drift apart. They are still two
  REGISTRIES, not one keyed two ways: a book watcher must never be handed a bash
  row.
- `internal/server` must stay testable with NO database: it declares the
  storage contract as `type Store interface`, whose result types and sentinels
  are ALIASES of `internal/db`'s — so `*db.DB` satisfies it directly (asserted
  at compile time in `internal/server/store.go`) and there is no adapter to
  drift. Add a method to `db` and the interface in one commit, never a
  conversion layer between them.
- **A LEVEL FILTER IS NOT AN ASSERTION.** `len(warnings) != 0` passes for a
  reclaimed socket or a slow query as readily as for the thing under test; narrow
  to the `operation` and assert the context keys. Likewise
  `assertNoDatabaseTouch` is scoped by `request_id` against the verbose statement
  traces (start the store with `verbose: true` and send the header), and a
  POSITIVE CONTROL subject fails if an accepted write ever stops leaving that
  mark — without it the negative assertions rot back into vacuity, which is
  exactly what they had done.
- Fixtures must build CONTRACT-VALID messages, or they test a refusal by
  accident: residue carries its `raw` record, a page line's envelope agrees with
  its frame, and a detached run's handle and unit id are DIFFERENT strings unless
  the subject is specifically about their converging.
- The harness's `restart()` does NOT remove the socket, and a SIGKILL variant
  leaves one behind on purpose: removing it masked the reclaim path entirely, so
  no subject ever reached it.
- Collect every failure in a run before fixing any of them; never fail fast.
- Maintain at least 90% statement coverage. Until the measured store baseline
  reaches that target, never reduce it, report the gap explicitly, and add
  focused tests for every critical branch and every error path changed.
- `modules/app/agent-repl/bin/test-all.sh` (from the repository root) runs
  every tracked suite across the module;
  `bin/report-logging-density.sh store` is a rough review aid, not semantic
  coverage — audit critical branches and errors directly even when its ratio
  rises.

### Test wait bounds

Every harness timeout is a small multiple (~3x) of the healthy max observed on
a clean `go test -race -count=1 -v ./...` run, never a round number picked by
feel. Re-derive them the same way after a change materially alters a suite's
real timing (a new real-process boot, a new outage ladder) rather than nudging
a number that started failing.

| Bound | Where | Old | New | Basis |
| --- | --- | --- | --- | --- |
| `readyTimeout` | `integration/helpers_test.go` | 30s | 2s | `integration` package healthy max 0.62s (the store binary is built once in `TestMain`; every boot lands under 100ms) |
| `shutdownTimeout` | `integration/helpers_test.go` | 20s | 2s | same basis as `readyTimeout` |
| `callTimeout` | `integration/helpers_test.go` | 30s | 2s | same basis as `readyTimeout` |
| `streamTimeout` | `integration/helpers_test.go` | 30s | 2s | same basis as `readyTimeout` |
| `openBound` | `internal/server/flush_test.go` | 10s | 1s | `internal/server` package healthy max 0.01s (in-process `httptest.Server` over a fake `Store`, no real I/O); floored above a literal 3x because the package's timings round to 0.00–0.01s and a sub-30ms bound would be fragile under `-race` scheduler/GC jitter |
| inline shutdown bounds (x3) | `internal/server/server_test.go` | 5s, 10s, 5s | 1s each | same basis and floor as `openBound` |

Per-site exception, deliberately NOT tightened by this pass:

- `pprofBootFailureGrace` (`main.go`, 5s) is production behavior, not a test
  harness default: `TestRunWithLoggerOpensThePprofSurfaceBeforeTheDatabase`
  observes the real hold duration directly, so its ~5s cost is the thing under
  test, not a bound guarding the test. Tightening it would change the store's
  runtime diagnosability contract, which this pass does not touch.
- `shutdownGrace` (`main.go`, 5s), `occupancyDialTimeout`
  (`internal/server/listen.go`, 2s), the `http.Server` `ReadHeaderTimeout`
  values (10s), and `DefaultSlowQuery` (`internal/db/slowquery.go`, 250ms) are
  all production defaults, not test-only harness bounds, and are out of scope.

## Dependencies

`agentrepl/proto` (generated Go for `store.v1` and `conversation.v1`, plus the
`storev1connect` handlers and clients), `agentrepl/logging`,
`connectrpc.com/connect` **v1.17.0**, `golang.org/x/net` **v0.43.0**,
`modernc.org/sqlite`. Both pins are project rulings: keep them, keep the module
at `go 1.23.0` with no `toolchain` line so the host toolchain builds it without
a switch, and never edit `proto/gen/go/go.mod`.
