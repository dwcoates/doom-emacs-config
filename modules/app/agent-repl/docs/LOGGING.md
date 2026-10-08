# Logging — the contract and the work

The owner's ruling, 2026-09-10: every agent-repl system logs through one
function per codebase; every record guarantees time and debugging context;
debug/info/warn/error coverage is thorough; a workspace's whole log is
retrievable efficiently; and all of it is codified in AGENTS.md.

## The contract is `logging-contract.md`

`modules/app/agent-repl/logging-contract.md` already states the shared
record schema, the per-workspace persistence layout
(`<workspace>/.claude/emacs/{emacs,daemon,shim,webapp,sidecar}.log`, each a
canonical symlink to a runtime-owned target), runtime ownership, the
64 MiB cap, and the emission rules. It stays the contract. This document
records the owner's requirements on top of it and the gaps the 2026-09-10
audit found, which are the work.

## Requirements restated

1. ONE logging function per codebase (or one object with one method per
   level), called by every log site; nothing else writes a record. A lint or
   test per codebase fails on a bypass.
2. Every record guarantees: `timestamp` (RFC 3339, offset, sub-second),
   `runtime`, `level`, `verbosity`, `operation`, `message`, `context`, `pid`,
   and the workspace/session/request identifiers whenever the event has them.
   A record about a workspace without `workspace_id` is a defect; the logger
   takes the workspace from its scope so a site cannot forget it. That
   `workspace_id` is the DAEMON-MINTED 16-hex `ids.WorkspaceID` in every
   runtime, and every log sink target a runtime mints is named after it; a
   runtime that cannot resolve it omits the field rather than substitute a
   path-derived id (Emacs before its first roster push), and the workspace
   directory hash the kernel lock file is named after travels separately as
   `context.workspace_dir_hash`.
3. Coverage: `error` never swallowed and always carrying the error text;
   `warn` only for a defect or a named decision; `info` for every lifecycle
   edge; `debug` for request boundaries, state transitions and decisions.
   One switch, `AGENT_REPL_LOG_LEVEL` (`debug|info|warn|error`, default
   `info`), honored by every system without a rebuild; a level other than
   `info` is a window of at most five minutes named by
   `AGENT_REPL_LOG_LEVEL_UNTIL` (`../logging-contract.md`).
4. Retrieval: ONE reader, `bin/logs.sh`, that answers a workspace (by id, dir
   or name), the central records, or everything, merged by timestamp,
  filtered by level and runtime, and `--harvest <from> <to>` for every
  warn/error and unavailable-sink finding in a window as an attributed table
  (the realtest remediation bar). An unavailable sink does not block readable
  peers; other modes summarize those findings on stderr. The reader exits
  nonzero when none of the selected sinks can be read.
  `scripts/agent-repl-log-discovery.sh` is what it grows from. Reading logs is
  expensive: `--tally`, `--sample`, `--fields`, and `--timeline` are compact
  query modes for the questions that recur (what happened and how often, one
  representative record per operation, a window's timeline, the full message
  and context for one operation) instead of dumping and post-processing raw
  records — see AGENTS.md "Logs" → "Querying compactly" for the canonical
  recipes and `bin/logs.sh --help` for the flags. `--json` stays the escape
  hatch for raw JSONL, not the default. The two non-JSONL sources a run can
  carry — a service's `.err.log` stderr and a captured Emacs `*Messages*`
  snapshot passed with `--messages` — are queryable the same compact way; the
  reader synthesizes their `operation`/`runtime`/`level` fields rather than
  requiring a separate look.
5. Rotation everywhere: size cap with N generations (`agentrepl/logging`'s
   `OpenRotating`); never truncate-and-lose, never poison.
6. Codified: the module's AGENTS.md carries the layout, every log path, the
   reader and a harvest recipe; each system's AGENTS.md carries its function,
   its level switch, where its records land, and the lint that fails a bypass.

## Gaps (audit 2026-09-10) → the work

Daemon — landed
- `AGENT_REPL_LOG_LEVEL` governs persisted and mirrored levels.
- `ClientLogRecord.runtime` routes forwarded records to `webapp.log` or
  `sidecar.log`; an unset arm preserves the historical webapp route.
- forwarded records retain their client timestamp and verbosity class.
- the run log and daemon-owned workspace sinks rotate on size with retained
  generations, and a new daemon instance APPENDS to the target the workspace's
  canonical link already names instead of opening a generation of its own, so
  `--workspace` reads every instance rather than the current one alone. `shim.log` rotates only with a shim process roll because the
  writer owns inherited descriptor `3`; a 110% hard ceiling emits one error
  and forces the freeness-aware roll.
- the bypass lint rejects raw stderr/`fmt.Print`/`log.Print` outside the
  sanctioned bootstrap sites.

Shim (TypeScript)
- no explicit `debug` level at any site; 163 `warn` / 102 `error` / 10
  `info` is an inverted pyramid. Give the API one method per level, reclassify
  every site, add debug records at request boundaries and state transitions.
- `AGENT_REPL_LOG_VERBOSE` gates only the stderr mirror; honor
  `AGENT_REPL_LOG_LEVEL`.

Store and sidecar (Go)
- DONE: sidecar config-root files resolve the transcript's authoritative `cwd`
  without decoding the lossy project slug, stamp
  `workspace_dir`/`workspace_id`/`claude_session_id`, and carry that scope over
  spawn observations to claimed spools. Store request loggers stamp the
  `agent_id`/book they concern (sorted plural keys for a mixed batch).
- DONE: `agentrepl.v1.ClientLogRecord.runtime.sidecar` lets every file-scoped
  sidecar diagnostic retain its originating timestamp, verbosity, level,
  operation, message, context, PID, Claude session, and workspace ref while the
  daemon persists it into workspace `sidecar.log`. An ordered forwarding worker
  keeps ClientLog latency and failures off the tailer's path, and shutdown
  drains records already accepted by the logger. Global lifecycle records remain
  in the sidecar's own rotating sink.
- DONE: both processes honor `AGENT_REPL_LOG_LEVEL`; verbose request/state
  records are `debug`, lifecycle remains `info`, decisions/refusals are `warn`,
  and owned failures are `error`.
- DONE: one AST-backed bypass lint per Go process names the sanctioned
  bootstrap and canonical-sink writers.

Emacs (lisp) — closed
- `agent-repl--emit-log-record` is the sole record builder and writer caller;
  every logging rung is a thin wrapper, and `test-core.el` scans production
  sources for bypasses.
- nil-workspace calls resolve through the request edge, buffer owner, or
  current workspace. Genuinely process-wide format prefixes carry an explicit
  reason in `agent-repl--central-log-format-prefixes`, which the source audit
  enforces. An unroutable workspace writes a correlated central error, warns
  visibly, and aborts without writing the original record globally.
- workspace records read `agent_repl_session_id` from `host.el`; verb and
  SubmitPrompt boundaries propagate request identity through their callbacks,
  and each inbound roster push receives one correlation id.
- Emacs targets rotate before crossing 64 MiB, retaining five completed
  generations (`.1` newest through `.5` oldest) while the canonical
  `<workspace>/.claude/emacs/emacs.log` symlink continues to name the active
  target path. Oversized single records remain whole.
- `agent-repl--log-verbose` emits `level=debug`, `verbosity=verbose` and is
  durably governed by `AGENT_REPL_LOG_LEVEL` like every other Emacs record.
  The load-time default is `info`; `agent-repl-log-file-level` remains the
  live Elisp knob and is reset from the environment on module reload.
- `test-core.el` parses every hand-written production `lisp/*.el` form. Each
  direct `message` call is named in a file/function/template allowlist with a
  user-facing reason; an unlisted diagnostic echo fails the suite.

Webapp (closed 2026-09-10)
- forwarded records carry the client's RFC 3339 instant and verbosity class;
  protobuf context contains call-site evidence and bound identities rather
  than a nested copy of the whole record.
- one `log` object exposes one method per level. The retired client-only
  verbose-console switch is gone, and the existing webview URL boot seam
  delivers `AGENT_REPL_LOG_LEVEL` as `log_level` without a rebuild.

Reader and docs
- Landed in this work: `bin/logs.sh` as specified; the module AGENTS.md "Logs"
  section (path, writer, format, window selection, attribution field, level switch, recipe);
  per-system AGENTS.md sections; `logging-contract.md` amended for rotation,
  the level switch, and the reader. The sidecar seam remains owned by the
  sidecar/daemon implementation work above.
