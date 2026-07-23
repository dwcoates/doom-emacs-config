# daemon/internal/shimclient/

The daemon's client side of the agent-shim protocol: one UDS connection per
session to that session's shim (`session-<id>.sock`). Responsibility:
handshake, `Subscribe{from_seq}` replay with `last_seen_seq` tracking,
reconnect-to-live-shim (reattach), control-plane sends (`SubmitPrompt`,
`Interrupt`, `PermissionResponse`) with `request_id` correlation, and demuxing
the inbound event stream to the daemon's internal consumers.

Dependencies: `proto/agentshim/` (generated Go), the per-session shim UDS
sockets.
