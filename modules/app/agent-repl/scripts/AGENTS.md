# scripts/

Operational/diagnostic scripts for the agent-repl system. Notably
`agent-shim-doctor.sh`: a connectivity and liveness check across every UDS
socket (store, sidecar-facing, per-session shims, daemon frontend) plus log
pointers — the first stop when diagnosing a degraded state. It is strictly
read-only and never mutates a service, socket, file, or launchd state.

## The store probe

`store.v1` is served with Connect over a UNIX domain socket and deliberately
defines NO health verb — streams and the transport own liveness. The doctor
therefore probes the store's real endpoints directly with `curl
--unix-socket`, POSTing the empty request message to
`store.v1.ShimStore/GetLiveWork` and `store.v1.ShimStore/GetSidecarCursors`.
Both are pure reads, which is what keeps the probe read-only. The store serves
on `~/.cache/agent-repl/sock/store.sock`; `AGENT_REPL_STORE_SOCKET` is the
test-only override of that default and an explicit `--socket` beats it, so the
doctor resolves the path from its state root rather than from the environment.

A healthy store answers HTTP 200 with a JSON body whose single top-level key is
`success` (an empty success arm is `{"success":{}}`). Everything else is a
separate failure class with its own hint, never collapsed into one "unhealthy":
`missing_socket`, `connection_refused`, `timeout`, `transport_failure`,
`http_status` (a served endpoint answers 200 even when it REFUSES),
`malformed_response`, `failure_arm` (the store is up and refused the read; its
`detail` is retained), and `unexpected_body`. When `curl` or `python3` is
absent the probe SKIPs — an honest environment report, never a pass and never a
silently different classification. Each probe sends an
`X-Agent-Repl-Request-Id` header and reports that id and its latency in the
result metadata. `AGENT_REPL_DOCTOR_STORE_PROBE_TIMEOUT` (whole seconds) sets
the per-probe deadline.

The sidecar has no socket of its own; its health is read from launchd state and
its log's freshness, exactly as before.

`test-agent-shim-doctor.sh` covers every one of those classes against
`fake-store-fixture.py` — a scripted Connect server on a temporary UNIX socket.
It builds and runs no Go binary, so it stays deterministic and independent of
the store's own build. The fixture's readiness latch is a FIFO it opens only
after bind+listen, so the harness never sleep-polls for startup.

`agent-repl-log-discovery.sh` is the read-only resolver for structured logs.
It lists canonical workspace links at `<workspace>/.claude/emacs/*.log`, the
small set of genuine global logs, and can filter JSONL by a Claude/agent-repl
session identifier or process id. It also extracts latency evidence from those
records with `--spans`, `--latency-by` and `--gaps`, which compose with every
selector and emit headerless TSV; `--help` documents the exact columns.
Keep its focused test beside it in
`test-agent-repl-log-discovery.sh`; the test must create all state under a
temporary directory.

`test-agent-repl-log-discovery.sh` exercises the resolver, and
`modules/app/agent-repl/bin/test-all.sh` runs every tracked suite across the
module. Dependencies include
the running services' sockets under `~/.cache/agent-repl/sock/`, global logs
under `~/.cache/agent-repl/log/`, `~/.claude-emacs/`, and Emacs's
UID-qualified OS-temporary log directory, plus workspace symlinks under
`<workspace>/.claude/emacs/`.

For merged, level- and time-filtered records across rotation generations, or
for a realtest warn/error harvest, use the canonical reader documented in
`../AGENTS.md` and run `../bin/logs.sh`; this directory's discovery script
remains the focused session/PID/span/gap diagnostic.

## Bouncing every backend by force

`bounce-agent-repl-forcefully.sh` rebuilds every component in place in this
checkout (protobufs, shim, webapp, daemon, store, sidecar, lock; the service
binaries are installed into `~/.cache/agent-repl/bin`), then stands the daemon
and its shims down so every one of them comes back on the fresh build:

1. the daemon is asked to stand down now (`claude-repld call
   UpdateShutdownSchedule {"now":...}`), which announces its ending to every
   client and stands each of its shims down itself before it exits -- so the
   daemon reads every shim exit as one it ordered, and every shim concludes
   its session against a store that is still up;
2. whatever it left (a refused or unanswered request, a daemon past the
   grace, a straggling shim or lock helper) gets SIGTERM, then SIGKILL after
   the grace -- only the processes named BEFORE the request, never the fresh
   daemon Emacs relaunches meanwhile or the shims it starts.

THE SERVICES ARE NEVER TOUCHED HERE. The daemon Emacs starts next finds the
store and the sidecar running an older build than the installed one and
restarts them, in the safe order, before it lets any shim start
(daemon/AGENTS.md, "The boot makes the services current before any shim
starts"). Stopped from the script instead, they went down under whatever the
freshly relaunched daemon had already started.

A failed build stops nothing. Emacs starts the fresh daemon when it next
links, and the daemon its shims. A process is the bounce's by this
checkout's own paths OR by the state root it serves -- the daemon
`daemon.addr` names, any shim listening under `<state root>/sock/` -- because
a deploy-started daemon and its shims run from the checkout the daemon was
deployed from (2026-10-03: an old-build shim survived a bounce and was
adopted). A daemon or shim of another checkout serving another state root is
left alone.
`test-bounce-agent-repl-forcefully.sh` covers
it hermetically: a temporary checkout, stand-in processes, and a `launchctl` on PATH that
only records it was called (any call fails the run).
