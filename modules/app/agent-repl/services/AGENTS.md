# services/

The services the platform's service manager keeps running: the shim-ecosystem
services `com.agentrepl.shim-store` and `com.agentrepl.shim-claude-sidecar`,
the knowledge-graph service `com.agentrepl.zettel`, and the metrics store
`com.agentrepl.victoriametrics`. launchd runs them on macOS and the user's own
systemd manager (`systemctl --user`) on Linux; on both they start at login and
are restarted when they die.

## One entry, rendered for either service manager

Each service is ONE `service` entry in its system's `requirements.json`
(`agent-shim/shim-store/`, `agent-shim/claude/shim-sidecar/`, `zettel/`,
`metrics/`): its label, its program (built here, or an installed command), its
arguments, environment and log files. `bin/requirements.sh` (`rendered_service`)
renders that entry into a launchd plist under `~/Library/LaunchAgents/` or a
systemd unit under `~/.config/systemd/user/`, and `install` loads it. No plist
or unit is checked in, so the two can never disagree about a service.

- Every `~/` in an entry becomes the user's absolute `$HOME`: no service
  manager expands `~`.
- What every service shares is the renderer's, not an entry's: started at
  login, kept alive, restarted no faster than every 10s when it crash-loops,
  and killed when a stop it was asked for is not done within 20s. A service's
  own shutdown is bounded well inside that (`logging.DefaultShutdownDrain`,
  5s), so reaching the 20s means the process is stuck (one teardown once held
  past three minutes while launchd waited).
- The daemon restarts these services through the same service manager
  (`daemon/internal/deploy`, `ServiceManager`: `Launchctl`, `SystemdUser`),
  finding each installed definition by its label.
- A systemd unit logs by appending to the same files launchd writes
  (`StandardOutput=append:`), because the daemon reads the store's error log
  to tell a slow boot from a wedged one.

## Priority: Standard, never Background (macOS)

Every rendered plist declares `ProcessType` `Standard`. The live editor waits
on these services, so they must never rank below a test suite: launchd's
`Background` is darwinbg (efficiency cores, throttled disk I/O), and suites run
at `nice 19` with normal disk I/O, so a `Background` store lost every I/O
contention to them. Measured 2026-10-12 on a copy of the store's database
under ordinary suite load: a 200-row write took a median 72-79ms at Standard
and 273-363ms under darwinbg, over the store's 250ms budget, and the live
store logged thousands of slow-query warnings (checkpoints up to 99s) while
suites ran. `bin/test-requirements.sh` holds the rendering to `Standard`.

## Launched through /usr/bin/env

Every service's program is run by `/usr/bin/env`, which execs it in its
place, on both platforms. macOS's background task manager names each login
item by its program's code signature and posts an "App Background Activity"
notice whenever it sees a new one. The binaries built here (`shim-store`,
`shim-claude-sidecar`, `zettel`) carry only the Go linker's ad-hoc signature,
so every rebuild read as a new login item and posted a notice on each deploy
(2026-11-02). Apple-signed `/usr/bin/env` never changes, so each service keeps
one identity across rebuilds. A `command` service (the metrics store's
`victoria-metrics`) needs `env` on either platform to find its program on a
`PATH` the renderer states: Homebrew's prefixes on macOS, the user's
`~/.local/bin` and the system's on Linux, since a service manager's own `PATH`
holds neither. `bin/test-requirements.sh` holds the rendering to
`/usr/bin/env`.

## The flag contracts

Each entry's `args` are a contract with that service's `main.go`. Nothing beyond what the service needs is
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

- `com.agentrepl.zettel` — `--socket` (`~/.cache/agent-repl/sock/zettel.sock`),
  `--db` (`~/zettel/zettel.db`, beside the threads' directories, because the
  graph is the user's data and never cache) and `--log`
  (`~/.cache/agent-repl/log/zettel.log`). `AGENT_REPL_ZETTEL_SOCKET` is its
  test-only socket override, deliberately not set. The daemon keeps it loaded
  and on the installed build at every boot and deploy
  (`daemon/internal/deploy/services.go`); nothing else depends on its socket.

`AGENT_REPL_STATE_DIR` is the sidecar's `--state-dir` default, and the same
variable the daemon resolves and exports into every shim it spawns; the two
processes must agree on one root or the identity records the shim writes are
not the ones the sidecar reads.

`AGENT_REPL_STORE_SOCKET` is the TEST-ONLY override of the socket default (the
store's `--socket`, the sidecar's `--store-socket`); an explicit flag always
beats it. It is deliberately NOT set in any entry's `env`: the OS-managed services
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
about these services is found -- at runtime or in its tests -- add or update an entry
here: what was slow or heavy, the evidence with numbers and the date, the fix
and its commit (or the open item). Test-performance lessons also go into the
`/agent-repl-tests` skill; runtime profiling and the perf-sensitive list live in
`/agent-repl-profile`.

- A job's process stuck in uninterruptible I/O cannot take SIGTERM, so launchd waits out ExitTimeOut (20s for the sidecar) before killing it; the sidecar's 29s restart came from a spool glob that stat'ed a dead RAM-disk mount (741051189). Never let a service's hot path stat paths outside what it owns.
