# Agent REPL Logging Contract

This document defines the persistence and attribution contract shared by every
agent-repl runtime. Runtime-owned logging APIs may expose language-appropriate
types, but they must produce the same JSONL record shape and obey the same
routing invariants.

## Persistence layout

Workspace-owned records are persisted through canonical symlinks inside the
workspace:

- `<workspace>/.claude/emacs/emacs.log`
- `<workspace>/.claude/emacs/daemon.log`
- `<workspace>/.claude/emacs/shim.log`
- `<workspace>/.claude/emacs/webapp.log`
- `<workspace>/.claude/emacs/sidecar.log`

Each link points to an external temporary file created and opened by the
runtime that owns the sink. The runtime must never follow a workspace-provided
regular file or symlink as its durable sink. Link replacement is atomic. An
owned target is reused from the runtime's in-memory workspace map during that
runtime lifetime. After a runtime restart, the runtime creates a new unique
target under the operating system's temporary directory and atomically
replaces the canonical link rather than trusting its old destination. An
active target is truncated in place so readers holding the target open
continue to observe the same inode.

The daemon opens its workspace targets with append semantics and manages a
64 MiB cap for `daemon.log`, `shim.log`, `webapp.log`, and `sidecar.log`.
Daemon-owned writes check the cap synchronously. A periodic daemon scan also
checks direct shim writes made through inherited file descriptor `3`.
Truncation first proves the canonical symlink still names the manager-owned
inode, then clears that inode in place. A cap-maintenance failure is a
workspace-attributed JSON error and poisons the affected sink.

Global service records use the runtime's canonical global log only when the
record genuinely has no conceptual workspace or agent association. Failure to
mechanically resolve a workspace for a workspace-owned record is a routing
invariant violation, not permission to write the record globally.

Operators resolve and query these paths through
`scripts/agent-repl-log-discovery.sh`. The script can select a workspace or
genuine global scope, narrow to one runtime, and filter strict JSONL by
`agent_repl_session_id`, `claude_session_id`, or `pid`.

## JSONL schema

Every persisted line is exactly one JSON object. Human-formatted persisted
records are forbidden.

Required fields:

- `timestamp`: RFC 3339 timestamp in the shared representation below
- `runtime`: `emacs`, `daemon`, `shim`, `webapp`, `sidecar`, or `store`
- `level`: `debug`, `info`, `warn`, or `error`
- `verbosity`: `normal` or `verbose`
- `operation`: stable machine-readable operation name
- `message`: concise human-readable description
- `context`: JSON object containing operation-specific structured evidence

Process runtimes include `pid`. The browser webapp includes `connection_id`
instead because it cannot reliably identify the server process.

Workspace records also include:

- `workspace_dir`
- `workspace_id`

Identity fields are included whenever the owning runtime knows them:

- `agent_repl_session_id`
- `claude_session_id`
- `request_id`

Identifiers belong in their dedicated fields, never only inside `message`.
Dynamic values and error causes belong in `context`, never in an incompatible
per-call text convention.

## Timestamp representation

Every runtime renders `timestamp` identically, so records from different
runtimes interleave and compare without per-runtime normalization:

```
2026-07-28T12:34:56.789000-04:00
```

- RFC 3339 date and time on a 24-hour clock.
- The machine's local zone, never UTC and never a `Z` suffix.
- Exactly six fractional digits. Fixed width is required so records sort
  lexically; a runtime that resolves instants only to milliseconds pads the
  remaining digits with zeros rather than emitting a shorter field.
- An explicit numeric offset in `±HH:MM` form.

The layout is expressed as `TimestampLayout` in each Go runtime
(`dlog`, `shim-store/internal/logging`, `shim-sidecar/internal/logging`),
`logTimestamp` in the shim and webapp TypeScript loggers, and
`agent-repl--log-timestamp-format` in Emacs.

Timestamps arriving from another runtime are parsed as ordinary RFC 3339, so a
forwarded record carrying a UTC instant is still readable; the daemon converts
it to the local zone before persisting.

## Runtime ownership

- Emacs owns `emacs.log`.
- The daemon owns `daemon.log`.
- The daemon persists forwarded browser records into `webapp.log`.
- The daemon persists forwarded sidecar diagnostics into `sidecar.log` after
  resolving the Claude session identifier through its registry.
- The daemon creates or reuses the external target for `shim.log`, opens it,
  and passes only the already-open descriptor as inherited file descriptor
  `3` when spawning the shim. The shim never receives, resolves, or reopens
  the target path.
- The shim writes directly to that target so daemon disconnects do not
  interrupt persistence.
- The sidecar writes only genuinely global service records directly. A
  file-specific diagnostic is forwarded with the Claude session identifier,
  source path, sidecar PID, operation, and structured error context.
- The store writes only genuinely global lifecycle, database, protocol, and
  sink failures. Successful replay, heartbeat, subscription, and ingestion are
  not store-owned narrative records. Session-specific failures are returned to
  the requester and logged once by the workspace-aware requester.

The shim `--cwd` argument is the shim's authoritative workspace directory. It
remains valid across daemon reconnects and is not duplicated onto
`DaemonHello`.

## Emission behavior

Each runtime exposes one canonical API with normal and verbose public emission
functions. Normal records use the runtime's canonical durable JSONL sink.
Workspace-owned runtimes persist verbose records to that same sink while their
verbose setting controls only terminal or console visibility.

The global `shim-store` and `shim-claude-sidecar` services are different:
`AGENT_REPL_LOG_VERBOSE` is a process-startup gate for both persistence and
terminal emission of verbose records. Their global sinks have no workspace
cap, so persisting hot successful per-event, per-batch, per-heartbeat, or
per-file diagnostics while verbose mode is disabled is forbidden. Normal
records continue to persist lifecycle transitions, invariant violations,
bounded summaries, and owned failures.

Every OS-process record includes the emitting process's `pid`. Multiple shim
processes may share one workspace `shim.log`; `pid` and session identifiers
disambiguate their records. The daemon must not duplicate persisted shim
records when it mirrors shim terminal output.

Every error is recorded exactly once by its owning layer. Sink failure is the
only permitted emergency-output exception because the canonical sink cannot
record its own failure. A missing expected workspace association fails loudly
and does not persist the original record to a global sink.
