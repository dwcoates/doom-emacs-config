/**
 * engine/pushes.ts — the WatchSession fan-out.
 *
 * OWNER: the session-engine agent.
 *
 * RESPONSIBILITY. One standing stream per consumer, fed by every session-level
 * fact: the vendor's own (`identity_rotated`, `model_changed`, `fast_mode`,
 * `mcp_server`, `account_usage`, `rate_limit`) and the shim's own about itself
 * (`diagnostics`, `context_usage`).
 *
 * THE FIRST PUSH IS READINESS. After `StartSession`, the first thing every
 * WatchSession receives is `diagnostics{healthy}` — that push IS the daemon's
 * readiness signal, and there is no other. `context_usage` is pushed at start,
 * at every turn end, and on a slow cadence.
 *
 * CADENCE: EVENT-DRIVEN, WHOLE-VIEW, NO TICKS. Push the whole message on any
 * resolved CHANGE and nothing at all on no change; the client ticks locally
 * from the instants already shipped. A periodic push of an unchanged view is
 * indistinguishable from a change at the consumer and defeats every "on change"
 * optimization above it.
 *
 * SYNTHESIZED FACTS ARE NEVER WRITTEN. `diagnostics` and `context_usage` are
 * the shim's report about ITSELF, not vendor conversation, so they are pushed
 * and never landed in the store.
 */
export {};
