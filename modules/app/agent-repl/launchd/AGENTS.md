# launchd/

launchd user-agent plists for the two OS-managed shim-ecosystem services:
`com.agentrepl.shim-store` and `com.agentrepl.shim-claude-sidecar` (both
`RunAtLoad` + `KeepAlive`, so they are always available at login and
recreated on failure). Installed idempotently by `.claude/install.sh`.

Both files are TEMPLATES: they use `~/`-relative paths so they read portably,
and `.claude/install.sh` (`install_agent_shim_services`) rewrites every leading
`~/` to the invoking user's absolute `$HOME` when copying them into
`~/Library/LaunchAgents/`. launchd does not expand `~` itself, so neither
template is ever loaded directly.

## The flag contracts

Each plist's `ProgramArguments` is a contract with that service's `main.go`,
and its header comment restates it. Nothing beyond what the service needs is
passed; every other flag keeps its default.

- `com.agentrepl.shim-store` — `--socket` (the Connect-over-UDS server path,
  `~/.cache/agent-repl/sock/store.sock`) and `--db`
  (`~/.cache/agent-repl/store/events.db`). `--log`, `--pprof` and
  `--watch-buffer` stay at their defaults.
- `com.agentrepl.shim-claude-sidecar` — `--store-socket` (the same store
  socket), `--config-roots` (the Claude config roots whose transcripts and
  spools are observed), `--spool-root`, and `--state-dir` (`~/.claude-emacs`,
  the state root holding the shim's identity records — how a rotated vendor
  session id resolves to the conversation's original one). `--log`,
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

The store serves `store.v1` with Connect over that socket and has no health
verb by design; `scripts/agent-shim-doctor.sh` probes the Connect endpoints to
judge store health.

Dependencies: the built `agent-shim/shim-store/` and
`agent-shim/claude/shim-sidecar/` binaries. The binding designs are
`docs/overhaul/store.md` and `docs/overhaul/sidecar.md`.
