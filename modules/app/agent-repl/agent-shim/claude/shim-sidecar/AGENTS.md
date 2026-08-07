# agent-shim/claude/shim-sidecar/

The Claude file-plane reader (Go, singleton, launchd-managed). Responsibility:
observe the Claude harness's on-disk artifacts (session transcripts, agent
sidechain transcripts, workflow journals, `/tmp` task spools), parse them with
cursored, truncation-aware tailing, convert records into agent-shim protocol
events (same loud-validation contract as the shims), infer terminal `LOST`
transitions per the staleness policy, and write everything to the shim-store
with atomic cursor advancement.

The sidecar is 100% specific to Claude's file formats BY DESIGN; its entire job
is converting that vendor reality into the (treated-as-)vendor-agnostic
protocol. It interprets no resolved state and owns no database.

## Total-ingestion mandate

The sidecar handles ALL JSONL objects in the files it reads. If a JSON object
exists in a file on disk, it MUST end up written to the shim-store as a
protobuf shape — somewhere in the SQLite database, ultimately. No exceptions.
Ever.

- No sampling, no skipping, no "not visually interesting" filtering — curation
  is a downstream (daemon/frontend) concern, never an ingestion concern.
- The mandate binds INGESTION only. Downstream consumers (daemon, frontends)
  are free to never read a stored record, and to ignore records they do
  read — irrelevance to the user is a legitimate consumption-side judgment.
  It is never a legitimate reason to skip parsing a record or to leave it
  out of the database.
- A shape the schema cannot express is a SCHEMA GAP to be surfaced loudly and
  fixed (via the extras-enforcement contract that fails the build on
  undocumented extras), never a record to silently drop.
- The zero-`UnparsedEvent` golden-corpus contract is the executable form of
  this mandate; weakening it violates this document.
- DEFERRING a record is not skipping it. A record whose meaning depends on the
  line after it (today: the compaction boundary and its summary) may be left
  unconverted at the end of a batch — but only with the reader's cursor parked
  BEFORE it, so the next scan, and a restart, both read it again. A deferral
  that could lose the record is a violation of this mandate; see
  `tail.Context` ("deferred frames") and `internal/handler/clearcompact.go`.

## Ingestion is connection-scoped, never boot-scoped

The sidecar reads a watched file ONLY while a store connection is established,
and the FIRST act of every established connection — boot and reconnect alike —
is recovering that store's cursors. There is no boot path: boot is simply the
first time the link is not up yet. See `link.go`.

- While no connection exists the sidecar produces NOTHING, loudly. A store that
  has not started yet is a down dependency, never a reason to read anyway.
- A tailer's read position may ONLY come from a cursor the store handed us on
  the live connection. `rescan` is the only thing that builds a tailer and it
  fails hard if that is not true.
- "Cold" means exactly one thing: a CONNECTED store that genuinely holds no
  cursor for a file. That is the backfill path and it reads from offset 0
  honestly.
- Recovery that fails is never softened into a cold start. The predecessor of
  this design recovered cursors once at boot and, on failure, re-read every
  watched file from offset 0 — a fallback masking a down dependency, which
  re-ingested whole conversations and drove an SSM task-count clamp storm.

## Vendor carry-over (viral)

Any future vendor-equivalent sidecar (e.g. a codex sidecar) MUST inherit this
AGENTS.md's mandates into its own AGENTS.md — including the total-ingestion
mandate above AND this carry-over clause itself, so the directive propagates
to every subsequent vendor equivalent in turn.

Dependencies: `proto/agentshim/` (generated Go), the shim-store UDS socket,
the Claude harness file formats it parses.

## Logging

- The sidecar owns one canonical JSON logging API in `internal/logging`,
  divided between normal and verbose emission functions. New or changed
  sidecar code uses that API only.
- Sidecar lifecycle and ingestion-service records are genuinely global and
  persist in `~/.cache/agent-repl/log/shim-claude-sidecar.log`. A diagnostic
  conceptually attached to a session is persisted through the store, shim, and
  daemon into `<workspace>/.claude/emacs/sidecar.log`. Do not duplicate that
  session narrative in the global sidecar log.
- Every new or materially changed nontrivial function logs its entry. Every
  meaningful branch that selects a different nontrivial block, call, state
  transition, or outcome logs its selection.
- The normal helper persists and emits to the terminal. The verbose helper
  reaches neither the durable sink nor the terminal unless
  `AGENT_REPL_LOG_VERBOSE` is enabled.
- Each error is logged exactly once by its owning layer with store socket,
  transcript path, cursor, session, operation, branch outcome, and cause.
  Error-path tests assert the canonical record and its context.
- Frequent or hot diagnostics use the verbose helper. Do not bypass logging.
  Direct diagnostic output through `fmt`, `log`, `slog`, or an ad hoc logger is
  forbidden except a documented pre-logger bootstrap failure or logger-sink
  emergency path.

## Verification

- `make coverage` runs the full suite with `-coverpkg=./...` and reports
  per-function and aggregate statement coverage.
- `modules/app/agent-repl/bin/test-all.sh` (from the repository root) runs
  every tracked suite across the module.
- Maintain at least 90% statement coverage. Until the measured sidecar baseline
  reaches that target, never reduce it, report the gap explicitly, and add
  focused tests for every critical branch and every error path changed.
- Run `modules/app/agent-repl/bin/report-logging-density.sh sidecar` and report
  its source-line and canonical-call counts as a rough review aid. It is not
  semantic logging coverage, so directly audit all critical branches and
  errors even when the ratio rises.
- After a commit lands on `master`, run
  `modules/app/agent-repl/bin/test-all.sh --record`, inspect
  `modules/app/agent-repl/test_time.csv`, and surface every reported timing
  regression.
