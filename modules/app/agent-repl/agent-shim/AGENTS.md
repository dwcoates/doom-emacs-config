# agent-shim/

The shim ecosystem. Its responsibility is EXCLUSIVELY facilitating agent-backend
interaction: driving a vendor's agent SDK/harness and surfacing everything it
produces as agent-shim protocol messages (`proto/agentshim/`). Frontend serving,
merge/workspace state, and render-state derivation never live here.

Layout: one directory per vendor shim (`claude-shim/`, a future `codex-shim/`),
plus the two vendor-facing services `shim-claude-sidecar/` (file-plane reader)
and `shim-store/` (event store).

Dependencies: `proto/agentshim/` (the protocol definitions).
