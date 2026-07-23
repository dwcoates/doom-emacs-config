# scripts/

Operational/diagnostic scripts for the agent-repl system. Notably
`agent-shim-doctor.sh`: a connectivity and liveness check across every UDS
socket (store, sidecar-facing, per-session shims, daemon frontend) plus log
pointers — the first stop when diagnosing a degraded state.

Dependencies: the running services' sockets under `~/.cache/agent-repl/sock/`
and logs under `~/.cache/agent-repl/log/`.
