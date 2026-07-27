# agent-shim/claude/shim/

The per-session Claude shim (TypeScript/Node, one process per session).
Responsibility: drive the Claude Agent SDK (`query()`), convert the SDK stream
into agent-shim protocol events (loud validation: hard-error on missing
expected fields of a KNOWN family, capture an UNKNOWN discriminator onto the
`unknown` passthrough arm, capture-and-log unknown new fields), write PERSISTENT events
to the shim-store, forward the store-merged session stream plus EPHEMERAL
deltas to its daemon connection, and execute control messages (prompts,
interrupts, `canUseTool` permission round-trips).

It holds no cross-turn state, serves no frontend, and derives no render-state.
A daemon disconnect does not end the in-flight turn (reattach support).

Dependencies: `@anthropic-ai/claude-agent-sdk`, `proto/agentshim/` (generated
TS), the shim-store UDS socket.

## No real SDK calls from tests

`src/vendor-guard.ts` is the ONLY place that may dynamically import
`@anthropic-ai/claude-agent-sdk`; every call site goes through `importRealSDK`.
When `AGENT_REPL_FORBID_VENDOR_CALLS` is set to any non-empty value the guard
throws and the shim exits nonzero — never a silent no-op, never a fake
fallback. `test/setup.ts` sets it for the whole vitest suite, so a test that
needs offline behavior must pass `--fake`. Production must never set it.
