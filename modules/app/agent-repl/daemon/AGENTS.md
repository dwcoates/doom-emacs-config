# daemon/

## Session-controller vocabulary

A **session controller** is the daemon's in-memory control object for one
workspace's current agent-repl session. `sessioncontroller.Manager` owns the
fleet, keyed by absolute workspace path in `Manager.byWS`; each
`sessionController` value binds that workspace to exactly one agent-repl
session ID and owns its shim client, event consumer, cancellation boundary,
bring-up state, and other connection-local bookkeeping. A durable registry
record says a session exists. A session controller says this daemon instance
currently owns the live route used to operate the session.

A session controller is **live** precisely when the current
`sessioncontroller.Manager` has a `Manager.byWS[workspace]` entry. This fact is
daemon-local and deliberately does not survive a daemon restart. It does not
mean the shim has completed its handshake, the route is `wired`, or an agent
turn is active: a live session controller may still be bringing up or
reconnecting. `Manager.Live` reports this map-membership fact; send paths
additionally wait for the session controller's readiness gate.

A **matching live session controller** is stronger: the workspace entry must
exist and its `sessionController.sessionID` must equal the session ID announced
by the shim. A `turn handshake has no matching live session controller` error
therefore means the shim handshook while the current daemon either had no
session controller for that workspace or had already assigned the workspace to
a different session. Never interpret that error as merely "the session is not
thinking" or infer session-controller liveness from persisted session fields.

## Logging

- The daemon owns one canonical JSON logging API in `internal/dlog`, divided
  between normal and verbose emission functions. New or changed daemon code
  uses that API only.
- Workspace-bound records persist through the canonical
  `<workspace>/.claude/emacs/daemon.log` symlink. The daemon's global service
  log is only for events that are conceptually unrelated to every workspace
  and agent session. Difficulty resolving a known workspace is an invariant
  violation, never a reason to write its record globally.
- Every new or materially changed nontrivial function logs its entry. Every
  meaningful branch that selects a different nontrivial block, call, state
  transition, or outcome logs its selection.
- The daemon's normal helper persists and emits to the terminal. The verbose
  helper always persists and gates terminal output through the daemon's
  established verbose setting.
- THE DURABLE SINK IS AUTHORITATIVE AND SYNCHRONOUS; THE TERMINAL IS A MIRROR
  AND IS NOT. Every logger's terminal is the one shared `dlog.TerminalSink` the
  daemon builds at boot, which queues the line and returns. In production the
  terminal is a pty Emacs drains, so a synchronous terminal write blocks for as
  long as Emacs is busy — and it used to block holding the durable sink's
  mutex, which put every other emitter, and therefore every frontend command's
  ack, behind Emacs's own startup. Never wire a logger's terminal straight to
  `os.Stderr` again; the only exceptions are the documented bootstrap-fatal and
  sink-emergency paths. The mirror drops nothing, reorders nothing, and reports
  a write failure to the next emitter rather than swallowing it.
- Each error is logged exactly once by its owning layer with session, workspace,
  operation, resolved inputs, branch outcome, and cause. Error-path tests assert
  the canonical record and its context.
- Frequent or hot diagnostics use the verbose helper. Do not bypass logging.
  Direct diagnostic output through `fmt`, `log`, `slog`, or an ad hoc logger is
  forbidden except a documented pre-logger bootstrap failure or logger-sink
  emergency path.

## Telemetry

- Every completed frontend command emits one record at
  `daemon.frontend.command-latency`. Its context carries `command` (the
  `FrontendCommand` oneof field name), `client_kind`, `workspace`,
  `queue_depth` (commands in flight daemon-wide at receipt, including this
  one), `duration_ms` (receipt through ack enqueue — what the client waits
  out), `processing_ms` (the dispatch's share), `threshold_ms`,
  `ack_deadline_ms`, and `ok`. `request_id` is in its own top-level field.
- A fast command is `debug`/`verbose`. An ack at or past
  `AGENT_REPL_FRONTEND_ACK_WARN_MS` (default 2s, a fifth of the client's 10s
  ack deadline) is `warn`/`normal`, so a slow ack is visible without verbose
  mode and before the client's own deadline expires. A malformed value aborts
  boot.
- A command that names a workspace is workspace-owned; only the genuinely
  workspace-less commands reach the global service log.
- `-pprof` (default `AGENT_REPL_PPROF_ADDR`) opens the OPT-IN Go profiling
  surface: a unix socket path, or an explicitly loopback `host:port`. Empty is
  OFF and is the default — there is no always-on listener, and a wildcard or
  routable bind is refused at construction. The decision is recorded either way
  (`daemon.pprof.disabled` / `daemon.pprof.enabled`); the enabled record is
  `warn` and names the resolved `network`, `address` and `url`, which is the
  only place a port-0 bind's chosen port appears.

## Verification

- Non-interactive agents run under a permission classifier that blocks
  compound shell chains such as `cd modules/app/agent-repl/daemon && go
  test ./...`. Every command documented below is a single, standalone
  invocation runnable from the repository root, with no `cd` and no `&&`:
  - `go -C modules/app/agent-repl/daemon test ./...` — canonical unit test
    invocation.
  - `go -C modules/app/agent-repl/daemon vet ./...` — vet.
  - `make -C modules/app/agent-repl/daemon coverage` — the module-rooted
    form of `make coverage` below.
  - `go -C modules/app/agent-repl/daemon test ./internal/sessioncontroller/
    -run XXX -bench BenchmarkReplayDrain` — the shim-bring-up drain: one
    realistic replayed transcript through the consumer sinks over a real SSM,
    state database and durable log sink. Reports `events/s` and `ms/event`.
  - `go -C modules/app/agent-repl/daemon test ./internal/registry/ -run XXX
    -bench BenchmarkCursorWrite` — the per-event durable `last_seq` advance
    against a steady-state registry. Reports `writes/s` and `ms/write`.
  - Both are the standing guard on bring-up latency. A change to the consumer
    sinks, the state-database writes, or the registry's write path runs them
    and reports the before/after.
  - `go -C modules/app/agent-repl/daemon test ./internal/frontend/ -run XXX
    -bench BenchmarkRosterPublish` — one editor-global roster publication at
    fleet scale (16 workspaces, 4 connected clients) through validation,
    retention, per-client fan-out and its canonical log record. It is the
    standing guard on publication latency: a startup issues one publication per
    restored workspace and blocks on each ack. A change to roster validation,
    frontend delivery, or the daemon's logging sinks runs it and reports the
    before/after.
- `make coverage` runs all `cmd`, `e2e`, and `internal` packages with
  `-coverpkg=./...` and reports `go tool cover -func` output.
- `modules/app/agent-repl/bin/test-all.sh` (from the repository root) runs
  every tracked suite across the module.
- Maintain at least 90% statement coverage. Until the measured daemon baseline
  reaches that target, never reduce it, report the gap explicitly, and add
  focused tests for every critical branch and every error path changed.
- `modules/app/agent-repl/bin/report-logging-density.sh daemon` reports
  source-line and canonical-call counts as a rough review aid. It is not
  semantic logging coverage, so directly audit all critical branches and
  errors even when the ratio rises.
- `modules/app/agent-repl/bin/test-all.sh --record` records suite timings to
  `modules/app/agent-repl/test_time.csv` for spotting timing regressions.
