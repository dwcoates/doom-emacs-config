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
  always persists and gates terminal output through the store verbose setting.
- Each error is logged exactly once by its owning layer with database path,
  table, session, producer or subscriber, transaction, operation, branch
  outcome, and cause. Error-path tests assert the canonical record and context.
- Frequent or hot diagnostics use the verbose helper. Do not bypass logging.
  Direct diagnostic output through `fmt`, `log`, `slog`, or an ad hoc logger is
  forbidden except a documented pre-logger bootstrap failure or logger-sink
  emergency path.

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
