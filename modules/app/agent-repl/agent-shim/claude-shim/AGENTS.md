# agent-shim/claude-shim/

The per-session Claude shim (TypeScript/Node, one process per session).
Responsibility: drive the Claude Agent SDK (`query()`), convert the SDK stream
into agent-shim protocol events (loud validation: hard-error on missing
expected fields, capture-and-log unknown new fields), write PERSISTENT events
to the shim-store, forward the store-merged session stream plus EPHEMERAL
deltas to its daemon connection, and execute control messages (prompts,
interrupts, `canUseTool` permission round-trips).

It holds no cross-turn state, serves no frontend, and derives no render-state.
A daemon disconnect does not end the in-flight turn (reattach support).

Dependencies: `@anthropic-ai/claude-agent-sdk`, `proto/agentshim/` (generated
TS), the shim-store UDS socket.
