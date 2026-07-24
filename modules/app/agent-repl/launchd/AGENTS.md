# launchd/

launchd user-agent plists for the two OS-managed shim-ecosystem services:
`com.agentrepl.shim-store` and `com.agentrepl.shim-claude-sidecar` (both
`RunAtLoad` + `KeepAlive`, so they are always available at login and
recreated on failure). Installed idempotently by `.claude/install.sh`.

Dependencies: the built `agent-shim/shim-store/` and
`agent-shim/claude/shim-sidecar/` binaries.
