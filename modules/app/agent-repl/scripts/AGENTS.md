# scripts/

Operational/diagnostic scripts for the agent-repl system. Notably
`agent-shim-doctor.sh`: a connectivity and liveness check across every UDS
socket (store, sidecar-facing, per-session shims, daemon frontend) plus log
pointers — the first stop when diagnosing a degraded state.

`agent-repl-log-discovery.sh` is the read-only resolver for structured logs.
It lists canonical workspace links at `<workspace>/.claude/emacs/*.log`, the
small set of genuine global logs, and can filter JSONL by a Claude/agent-repl
session identifier or process id. Keep its focused test beside it in
`test-agent-repl-log-discovery.sh`; the test must create all state under a
temporary directory.

Run `test-agent-repl-log-discovery.sh` after changing the resolver, then run
`modules/app/agent-repl/bin/test-all.sh` before handoff. Dependencies include
the running services' sockets under `~/.cache/agent-repl/sock/`, global logs
under `~/.cache/agent-repl/log/` and `~/.claude-emacs/`, and workspace
symlinks under `<workspace>/.claude/emacs/`.
