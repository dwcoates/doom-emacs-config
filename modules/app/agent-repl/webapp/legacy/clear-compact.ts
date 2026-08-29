/**
 * The two ways a conversation's history stops informing the agent — a CLEAR
 * and a COMPACTION — and where each one leaves the feed.
 *
 * A clear discards the context outright; a compaction replaces it with a
 * summary. Both are first-class events on the wire
 * (`core.v1.ContextCleared` / `core.v1.ContextCompacted`), so this module
 * only has to recognize an item's kind — there is no text to match, no
 * plane to special-case, and no vendor spelling to keep up with. That is
 * the whole reason the pair became typed: the file plane records a clear as
 * an expanded command envelope, which no amount of string matching on the
 * live stream's prompt text was ever going to find.
 *
 * Shared rather than render-local: the boundary the feed draws is the same
 * boundary the topbar's subagent roster counts from, so both read it here
 * instead of each deciding for itself what a clear or a compaction looks
 * like.
 */
import type { ContextClearedItem, ContextCompactedItem, ConversationItem } from "./store.js";

/** A clear or a compaction, as one type. */
export type ClearOrCompactItem = ContextClearedItem | ContextCompactedItem;

/**
 * Whether ITEM is a clear or a compaction. The two are treated IDENTICALLY
 * here: both end the history above them, and every consumer cares only that
 * one happened and where.
 */
export function isClearOrCompact(item: ConversationItem): item is ClearOrCompactItem {
  return item.kind === "context-cleared" || item.kind === "context-compacted";
}

/**
 * Index of the last clear or compaction in ITEMS, or -1 when the session
 * neither cleared nor compacted.
 */
function lastIndex(items: readonly ConversationItem[]): number {
  for (let i = items.length - 1; i >= 0; i--) {
    if (isClearOrCompact(items[i])) return i;
  }
  return -1;
}

/**
 * The items the session's CURRENT context carries: everything after the last
 * clear or compaction, or all of them when neither ever happened.
 *
 * Any projection that speaks for the CONTEXT (what the agent still knows)
 * starts here; the clear or compaction itself is chrome, not context, so it
 * falls outside.
 */
export function itemsSinceClearOrCompact(
  items: readonly ConversationItem[],
): readonly ConversationItem[] {
  const i = lastIndex(items);
  return i === -1 ? items : items.slice(i + 1);
}

/**
 * The items the feed still DRAWS: the last clear or compaction and everything
 * after it, or all items when neither ever happened.
 *
 * Both clear the SCREEN as well as the context — the discarded turns leave
 * the feed entirely rather than lingering behind the divider — so the feed
 * opens on the event's own rule (and, for a compaction, the summary bubble
 * that is the only surviving account of what came before) and the
 * conversation below it.
 *
 * One item wider than `itemsSinceClearOrCompact`: the event stays visible as
 * the record of itself, even though the context it names no longer carries
 * it.
 *
 * The daemon floors every REPLAY at the newest of the two, so on a fresh
 * subscription this is already the identity. It still runs because either can
 * arrive LIVE, over items this end has already drawn.
 */
export function itemsFromClearOrCompact(
  items: readonly ConversationItem[],
): readonly ConversationItem[] {
  const i = lastIndex(items);
  return i === -1 ? items : items.slice(i);
}

/**
 * Identity of the feed's boundary: the uuid of the clear or compaction the
 * feed opens on, or null for a feed that carries neither. ITEMS is the sliced
 * list (`itemsFromClearOrCompact`), whose head is one of the two exactly when
 * one happened.
 *
 * A change between renders means one just landed, which is the renderer's cue
 * to rebuild the feed rather than reconcile it: the truncation shifts every
 * index-based key, so reconciling would reuse stale elements from above the
 * boundary, out of position.
 */
export function clearOrCompactKey(items: readonly ConversationItem[]): string | null {
  const first = items[0];
  return first !== undefined && isClearOrCompact(first) ? `${first.kind}:${first.uuid}` : null;
}
