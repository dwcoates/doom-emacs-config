# agent-shim/

The shim ecosystem. Its responsibility is EXCLUSIVELY facilitating agent-backend
interaction: driving a vendor's agent SDK/harness and surfacing everything it
produces as agent-shim protocol messages (`proto/agentshim/`). Frontend serving,
merge/workspace state, and render-state derivation never live here.

Layout: one directory per VENDOR (`claude/`, a future `codex/`), each holding
that vendor's shim and its vendor-facing services — `claude/shim/` (the
per-session SDK subprocess) and `claude/shim-sidecar/` (the file-plane reader)
— plus the vendor-neutral `shim-store/` (event store) and `wire/` (shared Go
framing) at this level.

Dependencies: `proto/agentshim/` (the protocol definitions).
