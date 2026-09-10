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
   takes the workspace from its scope so a site cannot forget it.
3. Coverage: `error` never swallowed and always carrying the error text;
   `warn` only for a defect or a named decision; `info` for every lifecycle
   edge; `debug` for request boundaries, state transitions and decisions.
   One switch, `AGENT_REPL_LOG_LEVEL` (`debug|info|warn|error`, default
   `info`), honored by every system without a rebuild.
4. Retrieval: ONE reader, `bin/logs.sh`, that answers a workspace (by id, dir
   or name), the central records, or everything, merged by timestamp,
   filtered by level and runtime, and `--harvest <from> <to>` for every
   warn/error in a window as an attributed table (the realtest remediation
   bar). `scripts/agent-repl-log-discovery.sh` is what it grows from.
5. Rotation everywhere: size cap with N generations (`agentrepl/logging`'s
   `OpenRotating`); never truncate-and-lose, never poison.
6. Codified: the module's AGENTS.md carries the layout, every log path, the
   reader and a harvest recipe; each system's AGENTS.md carries its function,
   its level switch, where its records land, and the lint that fails a bypass.

## Gaps (audit 2026-09-10) → the work

Daemon
- verbosity is hardcoded off (`cmd/claude-repld/run.go` passes `false` to
  `OpenSurfaces`); wire `AGENT_REPL_LOG_LEVEL`.
- `sidecar.log` is dead: `ClientLog` persistence hardcodes the webapp runtime
  (`internal/server/admin.go`, "SEAM GAP") and the sidecar never forwards;
  honor the record's runtime and persist the sidecar's forwarded diagnostics.
- the forwarded record's own `timestamp` and `verbose` (proto, landed) are
  ignored; persist them instead of the arrival clock and a recomputed class.
- per-workspace sinks truncate in place and poison at the cap
  (`internal/dlog/sink.go`); rotate with generations. The run log rotates on
  open, so a bounce loop evicts history; rotate on size.
- a bypass lint (raw stderr/`fmt.Print`/`log.Print` outside the sanctioned
  bootstrap sites).

Shim (TypeScript)
- no explicit `debug` level at any site; 163 `warn` / 102 `error` / 10
  `info` is an inverted pyramid. Give the API one method per level, reclassify
  every site, add debug records at request boundaries and state transitions.
- `AGENT_REPL_LOG_VERBOSE` gates only the stderr mirror; honor
  `AGENT_REPL_LOG_LEVEL`.

Store and sidecar (Go)
- records carry no workspace attribution. The sidecar knows the transcript's
  project dir; stamp `workspace_dir`/`workspace_id`/`claude_session_id` on
  every file-scoped record and forward file-scoped diagnostics to
  `sidecar.log` through `ClientLog` (the dead seam). The store stamps the
  `agent_id`/book on every request-scoped record so the reader can join it.
- no meaningful `debug` population; `AGENT_REPL_LOG_LEVEL`.
- bypass lint.

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

Webapp
- send the client instant and verbosity class on every forwarded record
  (proto landed); remove the dead localStorage verbose toggle.

Reader and docs
- `bin/logs.sh` as specified; the module AGENTS.md "Logs" section (path,
  writer, format, window selection, attribution field, level switch, recipe);
  per-system AGENTS.md sections; `logging-contract.md` amended for rotation,
  the level switch, the reader, and the sidecar seam once implemented.
