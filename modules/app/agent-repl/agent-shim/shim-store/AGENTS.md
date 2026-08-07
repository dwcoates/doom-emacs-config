# agent-shim/shim-store/

The event store (Go, singleton, launchd-managed). Responsibility: sole owner
of the event database (SQLite/WAL). It ingests protocol events from producers
(shims, the sidecar), assigns each session's gapless `seq`, dedups
stream-plane/file-plane overlap on `dedup_key`, commits events plus producer
cursors in one transaction, and serves `Subscribe{session_id, from_seq}`
replay-then-live-tail subscriptions. EPHEMERAL-class events are fanned out to
live subscribers but never persisted.

The store is deliberately tiny and frozen: schema, seq, dedup, fan-out —
nothing else. Payloads are opaque to it (no vendor knowledge, no parsing, no
interpretation); only envelope columns are extracted for indexing.

## `session_id` is the VENDOR session id, never a daemon/shim id

Every `session_id` in this store — the seq scope, the `(session_id, dedup_key)`
dedup index, the fan-out routing key, and `Subscribe{session_id, from_seq}` —
is the **vendor** session id: Claude's uuid, which is also its transcript
filename. It has to be, because two producers write the same conversation and
must agree on its name: the shim (stream plane) reads `session_id` off the SDK
message, and the shim-sidecar (file plane) derives it from `<uuid>.jsonl` — the
sidecar never talks to the daemon and cannot know a daemon `s_…` id. Disagree
and the dedup that merges the two planes cannot fire.

Fan-out is an exact map lookup (`f.subs[ev.GetSessionId()]`), so subscribing
under any other id registers a subscriber on a channel nothing publishes to:
writes still succeed, and replay plus live-tail silently return nothing. That
is precisely the 2026-07-25 bug — the shim subscribed under its `--session-id`,
so only EPHEMERAL events (which bypass the store) ever reached the daemon, and
prompts never rendered while responses arrived structureless.

## Any transaction that writes must BEGIN IMMEDIATE

`Ingest` reads (`SELECT MAX(seq)`) before it inserts, so a DEFERRED
transaction — Go's `database/sql` default — takes a WAL read snapshot and then
tries to upgrade to a writer. SQLite will not run the busy handler for an
upgrade: it returns `SQLITE_BUSY_SNAPSHOT` (517) when another connection
committed since the snapshot, or `SQLITE_BUSY` (5) when a writer holds the
lock, both immediately, so `busy_timeout` never applies. One store process
serves every live shim on its own goroutine and pooled connection, which makes
those collisions routine, and a rejected batch is PERMANENT loss (the shim's
store-client drops it — no spill, no retry). The DSN therefore carries
`_txlock=immediate`; keep it, and never add a read-then-write transaction that
begins DEFERRED.

Dependencies: `proto/agentshim/` (generated Go), SQLite.

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

- Run `make coverage` after every store Go change. It exercises every package
  with `-coverpkg=./...` and prints `go tool cover -func` statement output.
  The command must pass.
- Before handoff, run `modules/app/agent-repl/bin/test-all.sh` from the
  repository root. Every tracked suite must pass.
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
