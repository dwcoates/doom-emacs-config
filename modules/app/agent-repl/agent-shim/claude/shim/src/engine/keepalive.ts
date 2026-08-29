/**
 * engine/keepalive.ts — the keep-alive cadence, and the yield it owes a real
 * prompt.
 *
 * OWNER: the session-engine agent.
 *
 * RESPONSIBILITY. The vendor's prompt cache lapses on a timer, and a lapsed
 * cache makes the next real turn cost full price. The shim submits its own
 * minimal turns to keep it warm — entirely shim-internal work that no consumer
 * asked for and none should see.
 *
 * THE MARKER IS THE CONTRACT (ruled 2026-08-29). Every keep-alive prompt BEGINS
 * with the literal `<!--agent-repl:keepalive-->`, mirroring the existing
 * `<!--agent-repl:meta-->` marker. The store and sidecar treat a turn opened by
 * such a prompt as keep-alive — its prompt and every frame land as
 * `unserved_item.keepalive` — until the next non-keep-alive prompt.
 *
 * THE YIELD OBLIGATION. A real prompt must never wait behind a keep-alive: the
 * cadence rewinds its own in-flight turn before a user's prompt is submitted.
 * A user who typed and waited for the harness's own housekeeping is the failure
 * this exists to prevent.
 */
export const KEEPALIVE_PROMPT_MARKER = "<!--agent-repl:keepalive-->";
