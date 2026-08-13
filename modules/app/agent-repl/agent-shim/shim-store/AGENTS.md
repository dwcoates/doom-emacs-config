# agent-shim/shim-store/

The record store (Go, singleton, launchd-managed). Responsibility: sole owner
of the record database (SQLite/WAL). It ingests `StoreEntryWrite` batches from
producers (shims, the sidecar), assigns each session's gapless `seq`, absorbs a
producer's replayed write by `write_id`, commits records plus producer cursors
in one transaction, and serves `Subscribe{session_id, from_seq}`
replay-then-live-tail subscriptions.

## Both halves in, one half out

What it PERSISTS is `agentshim.v1.Entry` — the whole record, internal half
included. What it SERVES is `protocol.v1.EntryDelivery`, carrying only the
external half plus the position the store assigned. The store is one of the
three runtimes entitled to the internal half, and nothing it hands back carries
it; `check-conversation-isolation.sh` keeps the daemon and the webapp out of
that package entirely.

A record with NO external half — something the producer could not convert —
goes to a separate `unconverted` table. It is durable, as
`InternalEntry.unconverted` requires, and unreachable from every read the store
serves, because replay, subscribe and a message page all select from `entry`.

The store is deliberately tiny and frozen: schema, seq, replay identity,
fan-out — nothing else. Payloads are opaque to it (no vendor knowledge, no
parsing, no interpretation); only envelope columns are extracted for indexing.

## The write direction has NO ACK

`StoreWriteAck` was retired with the `Event` layer and `StoreEntryWrite` has no
reply message on any surface. So a producer cannot learn how many records
landed, which were absorbed as replays, what seq the batch reached, or that the
batch was refused at all. Do not invent a substitute message; it is a recorded
gap awaiting a contract decision.

The one consequence in code: **a rejected batch drops the producer connection.**
Silence and success are indistinguishable on a write-only stream, so a producer
whose batch was refused would otherwise keep writing into a store discarding its
work. The refusal is loud-logged with its cause first. Do not soften this into
a silent continue.

## `session_id` is the VENDOR session id, never a daemon/shim id

Every `session_id` in this store — the seq scope, the `(session_id, write_id)`
replay index, the fan-out routing key, and `Subscribe{session_id, from_seq}` —
is the **vendor** session id: Claude's uuid, which is also its transcript
filename. It has to be, because two producers write the same conversation and
must agree on its name: the shim (stream plane) reads `session_id` off the SDK
message, and the shim-sidecar (file plane) derives it from `<uuid>.jsonl` — the
sidecar never talks to the daemon and cannot know a daemon `s_…` id.

Fan-out is an exact map lookup on the delivered record's `session_id`, so
subscribing under any other id registers a subscriber on a channel nothing
publishes to: writes still succeed, and replay plus live-tail silently return
nothing. That is precisely the 2026-07-25 bug — the shim subscribed under its
`--session-id`, so nothing durable ever reached the daemon, and prompts never
rendered while responses arrived structureless.

## Any transaction that writes must BEGIN IMMEDIATE

`Ingest` reads (`SELECT MAX(seq)`) before it inserts, so a DEFERRED
transaction — Go's `database/sql` default — takes a WAL read snapshot and then
tries to upgrade to a writer. SQLite will not run the busy handler for an
upgrade: it returns `SQLITE_BUSY_SNAPSHOT` (517) when another connection
committed since the snapshot, or `SQLITE_BUSY` (5) when a writer holds the
lock, both immediately, so `busy_timeout` never applies. One store process
serves every live shim on its own goroutine and pooled connection, which makes
those collisions routine, and a rejected batch is PERMANENT loss (the shim's
store-client drops it — no spill, no retry, and now no ack to notice it by).
The DSN therefore carries `_txlock=immediate`; keep it, and never add a
read-then-write transaction that begins DEFERRED.

Dependencies: `proto/agentshim/v1` and `proto/protocol/v1` (generated Go),
SQLite.

## Telemetry families that no longer exist

`events_by_task` and `open_tasks` are gone with the statements they timed. Both
selected on a `task_id` column extracted from the retired `TaskStarted` /
`TaskProgress` / `TaskEnded` payloads; detached work is modelled as messages
now and there is no task-scoped envelope column to index. `CursorList.open_tasks`
is consequently always empty with `open_tasks_authoritative=false` — a recorded
gap, not an oversight.

## Logging

- The store owns one canonical JSON logging API in `internal/logging`, divided
  between normal and verbose emission functions. New or changed store code
  uses that API only.
- Store records are genuinely global and persist in
  `~/.cache/agent-repl/log/shim-store.log`. The store must not narrate
  successful session activity that is already owned by a producer or consumer.
  It logs its own lifecycle, storage, transaction, protocol, and error outcomes
  with the relevant identifiers as structured context.
- Every new or materially changed nontrivial function logs its entry. Every
  meaningful branch that selects a different nontrivial block, call, state
  transition, or outcome logs its selection.
- The normal helper persists and emits to the terminal. The verbose helper
  emits to neither sink unless `AGENT_REPL_LOG_VERBOSE` enabled verbose mode at
  process startup. This gate is load-bearing for the singleton global log:
  successful per-batch, heartbeat, replay-query, and connection diagnostics
  are high-volume and must not consume durable space in normal operation.
- Each error is logged exactly once by its owning layer with database path,
  table, session, producer or subscriber, transaction, operation, branch
  outcome, and cause. Error-path tests assert the canonical record and context.
- Frequent or hot diagnostics use the verbose helper. Do not bypass logging.
  Direct diagnostic output through `fmt`, `log`, `slog`, or an ad hoc logger is
  forbidden except a documented pre-logger bootstrap failure or logger-sink
  emergency path.

## Telemetry

- Every statement family in `internal/db` is timed: `replay`, `max_seq`,
  `events_by_task`, `open_tasks`, `list_cursors`, `cursor`, and the whole
  `BEGIN IMMEDIATE` `ingest` transaction. One that exceeds
  `AGENT_REPL_STORE_SLOW_QUERY_MS` (default 250ms) emits a `warn`,
  normal-verbosity record at `store.db.slow-query` with `statement`,
  `duration_ms`, `rows` and `threshold_ms`.
- Normal verbosity is deliberate and is the one exception to the rule above
  about hot per-operation diagnostics: a query that blew the threshold must be
  visible without verbose mode, because by the time an operator knows to look
  the replay that stalled is over. Successful query timing stays verbose.
- `statement` is a FAMILY NAME, never rendered SQL and never bound values. The
  payloads are opaque to the store, and quoting a parameterized statement would
  put session content into the singleton global log.
- A malformed threshold aborts `db.Open` rather than running the shipped
  default underneath an operator who believes they changed it.
- `-pprof` (default `AGENT_REPL_STORE_PPROF_ADDR` — store-specific, so
  profiling one service does not open a listener in the other) is the OPT-IN Go
  profiling surface. Empty is OFF and is the default. Same local-only rules as
  the daemon's: a unix socket path, or an explicitly loopback `host:port`;
  anything else is refused. Recorded at `store.pprof.disabled` /
  `store.pprof.enabled`.

## Verification

- `make coverage` exercises every package with `-coverpkg=./...` and prints
  `go tool cover -func` statement output.
- `modules/app/agent-repl/bin/test-all.sh` (from the repository root) runs
  every tracked suite across the module.
- Maintain at least 90% statement coverage. Until the measured store baseline
  reaches that target, never reduce it, report the gap explicitly, and add
  focused tests for every critical branch and every error path changed.
- Run `modules/app/agent-repl/bin/report-logging-density.sh store` and report
  its source-line and canonical-call counts as a rough review aid. It is not
  semantic logging coverage, so directly audit all critical branches and
  errors even when the ratio rises.
- After a commit lands on `master`, run
  `modules/app/agent-repl/bin/test-all.sh --record`, inspect
  `modules/app/agent-repl/test_time.csv`, and surface every reported timing
  regression.
