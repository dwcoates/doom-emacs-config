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

Each link points to an external target created and opened by the runtime that
owns the sink. Emacs targets live under the operating system's temporary
directory; daemon-owned targets live beside `daemon.run.log` under the daemon
state root's `logs/` directory. The runtime must never follow a workspace-provided
regular file or symlink as its durable sink. Link replacement is atomic. An
owned target is reused from the runtime's in-memory workspace map during that
runtime lifetime. After a runtime restart, the runtime APPENDS to the target
the canonical link already names, so one file spans instances and rotation
happens only at the byte cap: a bounce loop must not evict history merely by
restarting, and a reader resolving the canonical path must not see the current
instance alone. The standing target is joined only when it is a symlink naming
a regular file directly inside the runtime's own logs directory and that file
is under the cap; anything else -- a regular file or foreign symlink the
workspace put there, a swept target, a target at the cap -- is displaced by a
new unique target and an atomic link replacement, exactly as before.

A daemon-owned target the daemon MINTS is named
`<state>/logs/agent-repl-<workspace_id>-<runtime>-<unique>.log`, with the
minted 16-hex workspace id. A target an earlier instance minted under the
8-character directory hash is APPENDED TO wherever the canonical link still
names it, under the standing-target rule above: nothing is renamed, no history
is orphaned, and only a new target carries the current scheme. The runtime
records the scheme it used at INFO when it opens a sink.

The daemon opens its workspace targets with append semantics and manages a
64 MiB cap for `daemon.log`, `shim.log`, `webapp.log`, and `sidecar.log`.
For daemon-owned writes (`daemon.log`, `webapp.log`, and `sidecar.log`), the
daemon uses `agentrepl/logging.OpenRotating`: it rotates synchronously before a
record would cross the cap and retains `logging.DefaultBackups` generations as
`<target>.1` through `<target>.N`. After a roll it atomically refreshes the
canonical symlink onto the fresh current target. A reader that already holds
the retired target open keeps reading that inode as generation `.1`.

`shim.log` is different because the shim writes through inherited descriptor
`3` and never receives a path. A periodic daemon scan marks the target when it
reaches 64 MiB. At the next shim process roll—an ordinary bounce or restart,
or the stale-bundle turn-boundary roll—the daemon moves the old target into
the retained generations, opens a fresh current target, atomically refreshes
the canonical symlink, and gives only the fresh descriptor to the replacement
shim. Until that roll, the hard ceiling is 110% of the cap: daemon-controlled
writes are refused beyond it, the daemon records exactly one
`daemon.dlog.shim_hard_ceiling` error, and it asks the ordinary rollout engine
to force a roll at the next free turn boundary. The old shim is never given a
path and its open descriptor is never renamed and reopened underneath it.

A cap-maintenance failure is a workspace-attributed JSON error and poisons the
affected sink. Reaching a cap is ordinary rotation state, not poison.

Every global durable sink follows the same 64 MiB, five-generation retention
rule. Long-lived services append on open and rotate only at the byte cap, so a
bounce loop cannot evict history merely by restarting. The daemon run log may
start a fresh current generation at a run boundary and also rotates on size.
The Emacs sink rotates by the same generation rule (`.1` newest, `.5` oldest,
rotation before the append that would cross the cap, a record larger than the
cap kept whole in the active generation); no runtime truncates and loses the
only copy of earlier records.

Global service records use the runtime's canonical global log only when the
record genuinely has no conceptual workspace or agent association. Failure to
mechanically resolve a workspace for a workspace-owned record is a routing
invariant violation, not permission to write the record globally.

Operators resolve and query these paths through `bin/logs.sh`. It resolves a
workspace by daemon ID, canonical directory, or daemon display name; selects
central records or every daemon-known workspace; includes all retained
generations; merges records by timestamp; filters by time, minimum level, and
runtime; follows active current generations; and emits either compact local
time lines or original JSONL. A malformed selected line is reported with its
file and line number and aborts the read. An absent or unreadable sink and a
workspace sink that is not a symlink are findings rather than read failures:
the reader reports each one, continues through every other sink, and exits
nonzero only when no selected sink can be read. `--harvest <from> <to>` reads
every workspace and central sink and groups every warning, error, and sink
finding by workspace ID and directory, level, runtime, operation, and message.
Other modes summarize sink findings on stderr.

`scripts/agent-repl-log-discovery.sh` remains the focused identity and latency
diagnostic for callers that need its session, process, span, or gap queries;
it is not the canonical whole-system reader.

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

`workspace_id` is the DAEMON-MINTED workspace identity in every runtime: 16
hex characters (`daemon/internal/wsm.IDLength`), opaque, never derived from a
path. A runtime that is not the daemon receives it rather than computing it --
the shim reads it off the `--listen` socket the daemon named after the
workspace -- and a runtime that cannot resolve it refuses the record rather
than substituting a path-derived stand-in, so a reader grouping by
`workspace_id` sees one group per workspace across every runtime.

Emacs receives the id from the daemon's roster push, which writes each row's
ref (carrying its `:id`) onto the workspace. A record Emacs writes BEFORE that
push has reached the workspace omits `workspace_id` entirely and is attributed
by `workspace_dir` alone; it never carries a path-derived stand-in, which would
split one workspace across two groups in a harvest.

The workspace directory hash md5hex(clean absolute dir)[:8] -- the
shim-held kernel lock file's derivation -- is separate evidence and travels in
`context` as `workspace_dir_hash`, on every workspace record of every runtime.
It is never a `workspace_id`.

Identity fields are included whenever the owning runtime knows them:

- `agent_repl_session_id`
- `claude_session_id`
- `request_id`

The browser webapp learns `agent_repl_session_id` and `claude_session_id`
from `session_identity` on its own WatchWebWorkspace stream (landing 15) and
binds them into its log context, rebinding on every push. Both are absent
until the daemon states them, and absence attributes the record to the
workspace alone.

The store reads `request_id` from the inbound `X-Agent-Repl-Request-Id`
header. No production client currently sends that header, so the field is
absent from store records until one does; the reader stands because the header
is the contract's declared way to carry a caller's request identity into the
store.

Identifiers belong in their dedicated fields, never only inside `message`.
Dynamic values and error causes belong in `context`, never in an incompatible
per-call text convention.

## Operation queries

Operators query stable `operation` values rather than matching human-readable
`message` text. In particular, the shim entrypoint reserves
`shim.main.fatal` for unrecoverable process termination records only. Each
such record has `level: "error"` plus a classified cause and an explicit exit
outcome in `context`. Startup, model normalization, query selection, session
lock acquisition, signal handling, and authorized shutdown use
`shim.main.lifecycle` at their individually documented severity.

An operation-only fatal query therefore needs no message filter:

```sh
modules/app/agent-repl/scripts/agent-repl-log-discovery.sh \
  --workspace /absolute/workspace \
  --runtime shim \
  --tail 2000 | jq -c 'select(.operation == "shim.main.fatal")'
```

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

The layout has one owner per language, not one per runtime:

- Go: `agentrepl/logging` at `agent-shim/logging/go`, imported by the daemon,
  the store and the sidecar.
- TypeScript: `agent-shim/logging/ts/timestamp.ts`, compiled by the shim and
  the webapp.
- Emacs: `agent-repl--log-timestamp-format` in `core.el`.

Three languages cannot compile one source, so `proto/vocab/log-timestamp.json`
is the seam holding the three to the same answer, and each language asserts
against it. See `agent-shim/logging/AGENTS.md`.

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

Each runtime exposes one canonical API with methods for `debug`, `info`,
`warn`, and `error`. `AGENT_REPL_LOG_LEVEL` is the one process-startup level
switch for every runtime. It accepts exactly `debug`, `info`, `warn`, or
`error`, defaults to `info`, requires no rebuild, and persists records at or
above the selected minimum to the runtime's canonical durable JSONL sink. An
unrecognized value is a startup refusal, never an ignored setting or a
substitute value. The record's `verbosity` field remains its diagnostic class;
it does not create a second persistence switch.

Hot successful per-event, per-batch, per-heartbeat, or per-file diagnostics
are `debug`. Lifecycle transitions, invariant violations, named decisions,
and owned failures retain their contract levels and remain governed by the
same minimum-level switch.

Every OS-process record includes the emitting process's `pid`. Multiple shim
processes may share one workspace `shim.log`; `pid` and session identifiers
disambiguate their records. The daemon must not duplicate persisted shim
records when it mirrors shim terminal output.

Every error is recorded exactly once by its owning layer. Sink failure is the
only permitted emergency-output exception because the canonical sink cannot
record its own failure. A missing expected workspace association fails loudly
and does not persist the original record to a global sink.
