/**
 * Async projection — which live-or-settled detached background work a
 * response bubble owns, so BOTH its border and its in-bubble catalog derive
 * from ONE per-bubble map (the async-quiescence invariant: a bubble is amber
 * IF AND ONLY IF its live async is enumerated and selectable inside it).
 *
 * A turn can arm long-running background work (a backgrounded Bash poll, a
 * detached agent) whose progress outlives the turn that armed it. The bubble
 * that talks about that work is inert prose; this projection links the bubble
 * to the spawning tool items so the renderer can border it, catalog it, and
 * fold each member's live tail into it.
 *
 * # ONE TIER. THE DAEMON'S.
 *
 * This module used to walk a THREE-TIER IDENTITY LADDER: a classification
 * first, then a landed notification's task id, then an announcement parsed out
 * of paired result prose. It is gone, and it is not kept as a fallback.
 *
 * A ladder is a staged-probabilistic identity. Each rung is individually
 * plausible and the whole is usually right, but "usually right" is the property
 * the architecture forbids here: the lower rungs read evidence — completion
 * notifications, free-text prose — that two frontends parse differently and
 * that the same frontend parses differently as prose changes. When a ladder
 * picks wrong it does so silently, and the wrong answer is an IDENTITY, so
 * every later update addressed to it lands on the wrong work.
 *
 * `AgentToolCall.spawned_bubble_id` replaced all of it. The daemon owns the
 * classification, mints the bubble's id, and stamps the same string on the
 * call and on its structured outcome, so a frontend MATCHES an id and never
 * derives one. Empty means "this call detached nothing" — the ONLY reading of
 * empty, and specifically not an invitation to go looking elsewhere.
 */
import { ConversationItem, ToolItem } from "./store.js";

/**
 * The bubble ITEM's call detached, or null when it detached nothing.
 *
 * This is the membership verdict every async surface keys off, and it has
 * exactly one input: the daemon's verdict as the store recorded it. There is
 * deliberately no second source to consult when the first says nothing.
 */
export function watcherRef(item: ConversationItem): string | null {
  if (item.kind !== "tool") return null;
  const verdict = item.spawnedBubbleId;
  return verdict === undefined || verdict === "" ? null : verdict;
}

/** A tool call is an async member exactly when the daemon says it detached work. */
export function isWatcher(item: ConversationItem): item is ToolItem {
  return watcherRef(item) !== null;
}

/**
 * Map each bubble that owns async work to the member tool items its turn
 * armed, in spawn order — every member, live and settled alike (the border
 * decides amber-vs-green by re-checking each member's liveness, so the
 * projection stays pure of the render-time `taskTail`/child state that
 * liveness reads).
 *
 * A turn is the span between a `user-turn` and the `result` that closes it;
 * a member spawned anywhere in that span belongs to the turn. Ownership
 * extends PAST success turns, so no live async is ever orphaned: the host is
 * the turn's last main-chain text bubble, or — when the turn produced no
 * final text (a tools-only turn) — its prompt bubble (the `user-turn`'s
 * request id). An interrupted (`aborted`) or errored turn hosts its
 * survivors just the same, since its background work outlives the severed
 * turn exactly as a completed turn's does.
 *
 * A turn still streaming at the tail keys NOTHING: its frontier is main-chain
 * ACTIVE, not quiescent, so the bubble must not go amber until the turn ends
 * (the same gate the global `monitoring…` row reads off `turnInFlight`).
 */
export function asyncByBubble(
  items: readonly ConversationItem[],
): Map<string, ToolItem[]> {
  const byBubble = new Map<string, ToolItem[]>();
  let members: ToolItem[] = [];
  let lastText: string | null = null;
  let promptId: string | null = null;

  const flush = (): void => {
    // The prompt bubble is the fallback host, so a tools-only turn's live
    // async lands on its own prompt rather than vanishing.
    const host = lastText ?? promptId;
    if (host !== null && members.length > 0) {
      const list = byBubble.get(host) ?? [];
      list.push(...members);
      byBubble.set(host, list);
    }
    members = [];
    lastText = null;
  };

  for (const item of items) {
    if (item.kind === "user-turn") {
      // A prompt landing on an unclosed turn (an interrupt race) still hosts
      // that turn's survivors on its own bubble before the new turn opens.
      flush();
      promptId = item.requestId;
    } else if (isWatcher(item)) {
      members.push(item);
    } else if (item.kind === "text" && item.parentToolUseId === undefined) {
      lastText = item.blockId;
    } else if (item.kind === "result") {
      flush();
      // What follows is a fresh turn — a notification-woken one has no prompt
      // bubble to fall back to, so the prompt host does not carry across.
      promptId = null;
    }
  }
  return byBubble;
}
