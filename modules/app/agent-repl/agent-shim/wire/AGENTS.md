# agent-shim/wire/

The shared Go framing layer for every agent-shim UDS hop: a 4-byte big-endian
length prefix followed by that many bytes of a serialized protobuf message.
One owner so shim-store, shim-claude-sidecar, and the daemon cannot drift on
framing. Oversized or truncated frames are protocol violations surfaced as
loud errors, never absorbed.

Dependencies: none (stdlib only). The TypeScript twin lives in
`agent-shim/claude/shim/src/uds/framing.ts` (necessarily separate language).
