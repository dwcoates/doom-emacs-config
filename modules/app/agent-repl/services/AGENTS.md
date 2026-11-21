# services/

The services the platform's service manager keeps running: the shim-ecosystem
services `com.agentrepl.shim-store` and `com.agentrepl.shim-claude-sidecar`,
the knowledge-graph service `com.agentrepl.zettel`, the test service
`com.agentrepl.tests`, the metrics store `com.agentrepl.victoriametrics`, and,
on macOS alone, the launcher `app.zettel.launcher` (Zettel.app; `launcher/AGENTS.md`)
with its socket holder `app.zettel.sockethold.launcher`. launchd runs them on macOS and the user's own systemd
manager (`systemctl --user`) on Linux; on both they start at login and are
restarted when they die. The launcher is the one service with no Linux unit:
it is a macOS app, and Linux's banners are `notify-send`'s.

## One entry, rendered for either service manager

Each service is ONE `service` entry in its system's `requirements.json`
(`agent-shim/shim-store/`, `agent-shim/claude/shim-sidecar/`, `zettel/`,
`testservice/`, `metrics/`, `launcher/`): its label, its program (built here,
or an installed command), its arguments, environment and log files. A built
program runs from the cache's `bin/` unless its build installs it elsewhere,
which the entry's `program` names: the launcher runs from
`~/Applications/Zettel.app`, the bundle its build registers with
LaunchServices, so macOS attributes its banners to Zettel.app. The launcher also has a
`socket-holder-launcher` entry: a socket holder that owns `launcher.sock`
for good, the same pattern as the daemon's and the store's holders.
`bin/requirements.sh` (`rendered_service`)
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
  finding each installed definition by its label. A service the manager does
  not hold and whose definition is missing (one added since the machine's
  install) has its definition written by `bin/requirements.sh define-service
  <label>`, the same renderer, before the deploy starts it.
- A systemd unit logs by appending to the same files launchd writes
  (`StandardOutput=append:`), because the daemon reads the store's error log
  to tell a slow boot from a wedged one.

## Priority: Interactive (macOS)

Every rendered plist declares `ProcessType` `Interactive`. The live editor
waits on these services, and the test service's runs keep normal work's CPU
priority (owner ruling 2026-11-13), so no service and no run it starts may be
clamped. Measured 2026-11-17 on battery with a probe agent and a `nice 10`
child of it (`ps -o pri`): `Standard` and no `ProcessType` both run the agent
and its children at priority 20, utility's clamp; `Adaptive` at 4;
`Interactive` at 31, the priority a shell's child of any niceness gets. Under
`Standard` the test service's whole run tree sat at 20, and an integration
test waited minutes for a CPU. `Background` is worse still: darwinbg
(efficiency cores, throttled disk I/O), under which a 200-row store write took
273-363ms against 72-79ms unclamped (2026-10-12). `bin/test-requirements.sh`
holds the rendering to `Interactive`.

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
- `com.agentrepl.tests` — `--socket` (`~/.cache/agent-repl/sock/tests.sock`),
  `--runs` (`~/.cache/agent-repl/tests/runs`, the run registry) and `--log`
  (`~/.cache/agent-repl/log/tests.log`). Every merge queue gate runs on it,
  never in the daemon: the gate records its run's id in the merge's progress,
  and a merge resumed on the next daemon watches that run again from its first
  line, so a daemon deploy never cuts a gate. `AGENT_REPL_TESTS_SOCKET` is its
  test-only socket override, deliberately not set, which the daemon's
  `--tests-socket` beats; `--listen-fd` serves an already-listening socket a
  test harness binds and hands over (`daemon/integration/harness/testservice.go`),
  so a client's connect queues until the service accepts. The daemon keeps it
  loaded and current exactly as it keeps zettel.
- `app.zettel.launcher` — `--resident` (started by launchd, so the app
  stays in the background rather than opening Emacs as an open by hand does),
  `--log` (`~/.cache/agent-repl/log/launcher.log`) and `--holder` (the
  holder's control socket, `~/.cache/agent-repl/sock/launcher-holder.sock`,
  from which it takes the listening socket `launcher.sock`); the contract is
  `launcher/Sources/ZettelLauncher/Options.swift`. The state root
  (`daemon.addr`) keeps the daemon's own default. A macOS deploy installs and
  restarts it (`daemon/internal/deploy`, `Restarter.RestartLauncher`).

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
