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
owns the sink. Emacs-owned and daemon-owned targets alike live beside
`daemon.run.log` under the state root's `logs/` directory: an Emacs target a
person is asked to read must not sit where the operating system may sweep it
or where a per-launcher `TMPDIR` moves it. Emacs targets minted into the
operating system's temporary directory before that rule are appended to
wherever a canonical link still names one, under the standing-target rule
below; nothing is renamed, and a bounded idle sweep removes only the
unreferenced ones left behind there. The runtime must never follow a workspace-provided
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

The Emacs central sink is `<state>/logs/emacs.central.log`
(`~/.claude-emacs/logs/emacs.central.log`), beside `daemon.run.log`, in the
same JSONL record shape as a workspace `emacs.log` minus the workspace
fields. It carries every record Emacs writes under the `:agent-repl-central`
scope -- workspace creation and fork, kill, teardown, daemon administration --
so those records survive the session rather than living only in *Messages*.
It is written through Emacs's one logging function, never a second writer, and
it rotates by the global generation rule below. Its two earlier defaults
(`<state>/doom-agent-repl.log`, then `$TMPDIR/doom-agent-repl-<uid>/doom-agent-repl.log`)
are redirected to it on reload and hold only historical records.
`bin/logs.sh --central` and `scripts/agent-repl-log-discovery.sh --global`
resolve it by default; `AGENT_REPL_EMACS_GLOBAL_LOG` names a customized path.

Global service records use the runtime's canonical global log only when the
record genuinely has no conceptual workspace or agent association. A
workspace-owned record whose call site named NO workspace is a routing
invariant violation, not permission to write the record globally.

RESOLVING A NAMED WORKSPACE'S SINK IS A TOTAL FUNCTION. A workspace that
exists always resolves to some durable sink: its own when its directory can
host one, and otherwise the central sink, with the workspace preserved on the
record under `unroutable_workspace` so the line still says which workspace it
is about. A directory that is a scratch or temporary path, has been deleted,
or does not exist yet is an ORDINARY outcome — recorded once per workspace at
`debug` (`elisp.core.log-central-fallback`, `daemon.dlog.central_fallback`),
never as an error beside every record. Nothing that merely renders, sweeps or
bounds a workspace may fail or repeat itself because that workspace's
directory is unavailable.

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

## Daemon address advertisement

The daemon advertises its one loopback listener in `<state>/daemon.addr`,
written atomically (a temporary sibling, then a rename) once the listener is
bound and removed on orderly exit. Its payload is two lines:

```
127.0.0.1:<port>
pid=<n>
```

The first line is the bare `host:port` a client dials; the second names the
advertising daemon's process id. The format is forward-compatible: a reader
that needs only the address takes the first line, which is exactly what a
legacy daemon wrote, and a file that carries no `pid=<n>` line reads as
`pid-unknown`. `daemonaddr.ReadAdvertisement` (Go) and
`agent-repl-connect-read-daemon-addr` / `agent-repl-connect-read-daemon-addr-pid`
(Emacs) are the readers.

The pid makes a dead advertiser distinguishable from a live-but-unreachable
one WITHOUT a dial. When Emacs reads a `daemon.addr` whose pid names no live
process — a predecessor that died without withdrawing (crash, SIGKILL, or a
restart that skipped withdraw) — it treats the advertisement as ABSENT: it
retires the file at INFO (`elisp.daemon.addr-retired-dead-advertiser`, with the
address and pid) and proceeds straight to a spawn, with no dial and so none of
the transport records (`elisp.connect.dial-failed`, `elisp.connect.unary-failure`,
`elisp.rpc.transport-failure`, `elisp.daemon.stale-addr`) a live-but-unreachable
daemon would rightly earn. A pid that names a live process, and a legacy file
that names no pid, are dialed as before; a live-but-unreachable daemon remains a
real WARN/ERROR.

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

A LEVEL OTHER THAN `info` IS A WINDOW, NEVER A STANDING SETTING. It is
honored only together with `AGENT_REPL_LOG_LEVEL_UNTIL`, the Unix second
(decimal integer) the window ends at, and only when that end is in the future
and no more than five minutes away when the runtime reads it. Without one --
no UNTIL, an ended window, a window further away than five minutes -- the
runtime starts at `info` and records, once at `info`, the level it ignored
(`outcome` `no_expiry`, `expired`, or `beyond_window`). A runtime whose window
ends while it runs reverts to `info` by itself and records the revert at
`info` (`outcome` `window_ended`). An UNTIL that is not a decimal integer is a
startup refusal. So a debug level left in a launchd environment never outlives
its five minutes: every runtime that starts later comes up at `info`. To turn
debug on for every runtime, set both, then start or bounce what should pick it
up:

```sh
launchctl setenv AGENT_REPL_LOG_LEVEL debug
launchctl setenv AGENT_REPL_LOG_LEVEL_UNTIL "$(( $(date +%s) + 300 ))"
```

Emacs's own level is also set at runtime (`agent-repl-set-log-file-level`,
`agent-repl-toggle-verbose-to-disk`), and a level other than `info` set that
way is the same five-minute window. The Emacs webview host carries the window
into the page as `log_level` plus `log_level_until`, so a page reloaded from
an address whose window ended boots at `info`. Every level window record uses
the runtime's `level-window` operation (`elisp.core.log-level-window`,
`daemon.dlog.level_window`, `shim.logging.level-window`,
`webapp.log.level-window`, `sidecar.logging.level-window`,
`store.logging.level-window`). `proto/vocab/log-level-window.json` is the
cross-language contract the Go (`agent-shim/logging/go/window.go`),
TypeScript (`agent-shim/logging/ts/level-window.ts`) and elisp (`core.el`)
selections are each asserted against.

A RECORD THAT WILL BE NEITHER PERSISTED NOR SHOWN COSTS NEARLY NOTHING.
Emacs returns before routing or building a dropped record whose scope is
certainly central, and routes (with every routing error intact) but never
serializes a dropped workspace record; the wire codec writes one debug record
per top-level decode or encode, never one per message.

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
