# launchd/

launchd user-agent plists for the two OS-managed shim-ecosystem services,
`com.agentrepl.shim-store` and `com.agentrepl.shim-claude-sidecar`, and for
the metrics store, `com.agentrepl.victoriametrics` (all `RunAtLoad` +
`KeepAlive`, so they are always available at login and recreated on
failure). The metrics store's program is Homebrew's `victoria-metrics`, not
a binary built here; its flags and why are in the plist's header and
`metrics/AGENTS.md`. Installed idempotently by `.claude/install.sh`, which runs
`bin/requirements.sh install`: each service is a `launchd_service` entry in its
system's `requirements.json` (`agent-shim/shim-store/`, `agent-shim/claude/shim-sidecar/`).

Both files are TEMPLATES: they use `~/`-relative paths so they read portably,
and the installer (`bin/requirements.sh`, `rendered_plist`) rewrites every leading
`~/` to the invoking user's absolute `$HOME` when copying them into
`~/Library/LaunchAgents/`. launchd does not expand `~` itself, so neither
template is ever loaded directly.

## Priority: Standard, never Background

Both services declare `ProcessType` `Standard`. The live editor waits on
them, so they must never rank below a test suite: launchd's `Background` is
darwinbg (efficiency cores, throttled disk I/O), and suites run at `nice 19`
with normal disk I/O, so a `Background` store lost every I/O contention to
them and logged thousands of slow-query warnings while suites ran.
`bin/test-requirements.sh` holds every `launchd_service` to `Standard`.

## The flag contracts

Each plist's `ProgramArguments` is a contract with that service's `main.go`,
and its header comment restates it. Nothing beyond what the service needs is
passed; every other flag keeps its default.

- `com.agentrepl.shim-store` — `--socket` (the Connect-over-UDS server path,
  `~/.cache/agent-repl/sock/store.sock`), `--db`
  (`~/.cache/agent-repl/store/events.db`) and `--metrics-root`
  (`~/.cache/agent-repl/metrics`). `--log`, `--pprof` and
  `--watch-buffer` stay at their defaults.
- `com.agentrepl.shim-claude-sidecar` — `--store-socket` (the same store
  socket), `--config-roots` (the Claude config roots whose transcripts and
  spools are observed), `--spool-root`, and `--state-dir` (`~/.claude-emacs`,
  the state root holding the shim's identity records — how a rotated vendor
  session id resolves to the conversation's original one), and
  `--metrics-root` (`~/.cache/agent-repl/metrics`). `--log`,
  `--poll-interval` (1s) and `--rescan-interval` (30s) stay at their defaults.

`AGENT_REPL_STATE_DIR` is the sidecar's `--state-dir` default, and the same
variable the daemon resolves and exports into every shim it spawns; the two
processes must agree on one root or the identity records the shim writes are
not the ones the sidecar reads.

`AGENT_REPL_STORE_SOCKET` is the TEST-ONLY override of the socket default (the
store's `--socket`, the sidecar's `--store-socket`); an explicit flag always
beats it. It is deliberately NOT set in either plist: the OS-managed services
are pinned to the real socket path by their explicit flags, so no environment
can move a production service off it.

`--metrics-root` is the ALWAYS-ON metrics surface's root, the same one the
daemon's launcher passes (`agent-repl-daemon-metrics-root`): each service
listens on an ephemeral loopback port and advertises it in
`targets/<program>.json` under it, which VictoriaMetrics reads
(`docs/PLAN-runtime-metrics.md`). Profiling (`--pprof`) stays opt-in and
separate.

The store serves `store.v1` with Connect over that socket and has no health
verb by design; `scripts/agent-shim-doctor.sh` probes the Connect endpoints to
judge store health.

Dependencies: the built `agent-shim/shim-store/` and
`agent-shim/claude/shim-sidecar/` binaries. The binding designs are
`docs/overhaul/store.md` and `docs/overhaul/sidecar.md`.

## Performance lessons (living document)

THIS SECTION IS A LIVING DOCUMENT. Whenever an important performance lesson
about the launchd jobs is found -- at runtime or in its tests -- add or update an entry
here: what was slow or heavy, the evidence with numbers and the date, the fix
and its commit (or the open item). Test-performance lessons also go into the
`/write-tests` skill; runtime profiling and the perf-sensitive list live in
`/profile-agent-repl`.

- A job's process stuck in uninterruptible I/O cannot take SIGTERM, so launchd waits out ExitTimeOut (20s for the sidecar) before killing it; the sidecar's 29s restart came from a spool glob that stat'ed a dead RAM-disk mount (741051189). Never let a service's hot path stat paths outside what it owns.
