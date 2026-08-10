/**
 * async-teal — WHAT MAKES A CARD TEAL.
 *
 * Teal used to be a LIST OF TOOL NAMES (`ASYNC_TEAL_TOOLS`: Skill, Agent,
 * Task). That list was a client-side wash painted over a set the contract did
 * not model uniformly. It promised async-bubble semantics — a whole
 * conversation of its own, nested inside the card, its panels fixed rather
 * than folding — to every name on the list, while only the kinds the daemon
 * actually minted a bubble for could deliver them. A kind could join the list
 * and LOOK like detached work without being any, which is exactly how Skill
 * came to be painted teal while its content still rendered flat.
 *
 * So teal is DERIVED now, from the one fact that makes those semantics true:
 * the daemon classified this call as detaching and stamped the id of the
 * bubble it detached onto the call. `AgentToolCall.spawned_bubble_id` is that
 * verdict, and tool-call.proto is unambiguous about how to read it —
 * "non-empty exactly when this call detached work that has its own
 * AsyncBubble", and empty is "this call detached nothing" and nothing else. A
 * frontend MATCHES the id; it never derives one.
 *
 * THE INVARIANT THIS BUYS: a teal card IS an async bubble. Not by a list two
 * files keep in step, but because the class and the bubble come from the same
 * string, so a kind cannot get the wash without the semantics. Adding a kind
 * to the wash is now a DAEMON change — classify it, mint it a bubble — and
 * there is no frontend edit that could shortcut that.
 *
 * READ OFF THE CALL, NOT THE REGISTRY. The verdict rides the call itself, so a
 * card is teal from the moment the daemon announces it rather than flickering
 * grey until the async push carrying its bubble arrives. The daemon opens a
 * window's bubble in the very delta that announces its call, so the two are
 * not racing — but the class must not depend on which of them a renderer has
 * seen, or the wash would blink on a resync.
 *
 * This module lives BELOW `render.ts` so `async-render.ts` may use it without
 * the cycle importing `render.ts` would create, exactly as `fold.ts` does.
 */

/**
 * The class a tool card wears when its call detached work with an AsyncBubble.
 *
 * The stylesheet's teal rule is keyed on this ONE class rather than on a list
 * of `.tool-card.tool-<name>` selectors, and `styles.test.ts` pins the two
 * together — so the drift guard now guards the DERIVATION rather than a list.
 */
export const ASYNC_BUBBLE_CLASS = "async-bubble-card";

/**
 * Whether this call detached work that has its own AsyncBubble — the whole of
 * the teal decision, and the whole of the fixed-panel decision with it.
 */
export function hasAsyncBubble(item: { spawnedBubbleId?: string }): boolean {
  return typeof item.spawnedBubbleId === "string" && item.spawnedBubbleId !== "";
}

/**
 * The card's teal class, or the empty string. A helper rather than an inline
 * ternary at each call site so the class name reaches the markup through one
 * path that `styles.test.ts` can pin.
 */
export function asyncBubbleClass(item: { spawnedBubbleId?: string }): string {
  return hasAsyncBubble(item) ? ` ${ASYNC_BUBBLE_CLASS}` : "";
}
