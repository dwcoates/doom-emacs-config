# scripts/

Operational/diagnostic scripts for the agent-repl system. Notably
`agent-shim-doctor.sh`: a connectivity and liveness check across every UDS
socket (store, sidecar-facing, per-session shims, daemon frontend) plus log
pointers — the first stop when diagnosing a degraded state.

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
