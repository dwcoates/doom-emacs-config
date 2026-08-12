/**
 * The load-more affordance at the top of the conversation.
 *
 * # Why the feed has a top at all now
 *
 * It used not to. A cold open replayed the WHOLE conversation, so the first
 * item the feed held was the first item the session ever produced and there
 * was nothing above it. The cold open is now a bounded tail page, so the
 * feed's top is a place where more history may exist — and a user who cannot
 * reach it has simply lost their history.
 *
 * # IT IS DRIVEN BY THE CONTINUATION ONEOF AND BY NOTHING ELSE
 *
 * The symptom this replaces: every message replayed on startup, followed
 * seconds later by a "load earlier messages" affordance for messages ALREADY
 * ON SCREEN. The affordance was decorative because the full replay had already
 * delivered everything, and it was decorative precisely because it was derived
 * from something other than the daemon's own answer — a count, a scroll
 * position, a page that merely LOOKED short.
 *
 * So there is exactly one input to availability: `ConversationHistoryPage`'s
 * `continuation` oneof.
 *
 *   - `HistoryHasMore` — older history remains. The affordance is available,
 *     and pressing it issues `NextPageCmd`. The arm is EMPTY on purpose:
 *     "there is more" is a FACT the client acts on, not a handle it stores.
 *     The cursor that used to live here is precisely the position the client
 *     no longer holds.
 *   - `HistoryAtStart` — this page reaches the conversation's beginning. The
 *     affordance is RETIRED. A fact the daemon established by reading to the
 *     floor, never an inference from a short page.
 *
 * A page with NEITHER arm set is malformed, and {@link historyContinuation}
 * throws rather than picking one: defaulting to "has more" gives an affordance
 * that can never succeed, and defaulting to "at start" silently hides the rest
 * of a conversation.
 *
 * # It renders a DECISION
 *
 * Four states, and the control is a pure function of them, because three
 * separate conditions spread across a renderer is how a button ends up
 * pressable while a request is already out:
 *
 *   - RETIRED: `HistoryAtStart`, or no page has been adopted yet. The control
 *     hides entirely; an exhausted history leaves no chrome behind.
 *   - LOADING: a page request is in flight. Shown and disabled, so a second
 *     click cannot mint a request the pager would only drop.
 *   - STOPPED: the pager hit its failure ceiling. Shown with the retry
 *     wording, because a load-more that silently stops working is worse than
 *     one that says it did.
 *   - READY: `HistoryHasMore` and nothing in the way.
 *
 * `loading` and `givenUp` decide PRESSABILITY, never availability. Whether
 * there is anything older is the continuation's answer alone.
 */

/**
 * The `continuation` oneof, as the affordance reads it.
 *
 * Two arms and no third, because "there is more" and "we reached the
 * beginning" are the only two answers and a shape that could carry both or
 * neither invites a client to invent a third.
 */
export type HistoryContinuation = { case: "more" } | { case: "start" };

/**
 * The raw arms as they arrive on a `ConversationHistoryPage`, before either is
 * resolved to a decision.
 */
export interface ContinuationArms {
  /** `HistoryHasMore` — set and EMPTY when older history remains. */
  more?: unknown;
  /** `HistoryAtStart` — set and empty when this page reaches the beginning. */
  start?: unknown;
}

/**
 * Resolve the continuation oneof, LOUDLY.
 *
 * Neither arm set, and both arms set, are both refused here rather than
 * defaulted anywhere downstream. This is the one place the affordance learns
 * whether history remains, so it is the one place a malformed answer can be
 * caught before it becomes a button that lies.
 */
export function historyContinuation(arms: ContinuationArms): HistoryContinuation {
  const hasMore = arms.more !== undefined && arms.more !== null;
  const hasStart = arms.start !== undefined && arms.start !== null;
  if (hasMore && hasStart) {
    throw new Error(
      "load-more: ConversationHistoryPage set both `more` and `start`, which are one oneof",
    );
  }
  if (hasMore) return { case: "more" };
  if (hasStart) return { case: "start" };
  throw new Error(
    "load-more: ConversationHistoryPage set NEITHER `more` nor `start`; defaulting to `more` " +
      "would offer an affordance that can never succeed and defaulting to `start` would " +
      "silently hide the rest of the conversation, so neither is chosen",
  );
}

/**
 * The anchor a next-page request carries in place of the cursor it no longer
 * holds.
 *
 * SEAM, sibling-owned. `NextPageCmd` carries NO position — that absence is the
 * design — but the pager this webapp currently holds still takes a non-empty
 * token to distinguish "the page older than the last one served" from the tail
 * read that an empty one means. This constant is that distinction and nothing
 * more: it is never sent as a position, never parsed, and never derived from
 * anything the client observed. The sibling that lands `NextPageCmd` deletes
 * it along with the pager's cursor parameter.
 */
export const NEXT_PAGE_ANCHOR = "next-page";

/** Everything the control needs to decide what it is. */
export interface LoadMoreState {
  /**
   * The continuation the LAST adopted page carried, or null when no page has
   * been adopted yet.
   *
   * Null is not "there is more". A client that has adopted no page has been
   * told nothing about whether history remains, and offering the affordance on
   * that silence is the decorative button this design removes.
   */
  continuation: HistoryContinuation | null;
  /** A page request is in flight. */
  loading: boolean;
  /** The pager stopped asking after repeated failures. */
  givenUp: boolean;
}

/** The four decisions {@link loadMoreView} resolves to. */
export type LoadMoreMode = "hidden" | "ready" | "loading" | "stopped";

export interface LoadMoreView {
  mode: LoadMoreMode;
  label: string;
  /** Whether a click may dispatch a request. */
  enabled: boolean;
}

/**
 * THE decision. Ordered so the strongest fact wins: a retired history is
 * retired whatever else is true, so neither a request in flight nor a spent
 * failure ceiling can keep chrome on screen above a conversation that has no
 * more of itself to give.
 */
export function loadMoreView(state: LoadMoreState): LoadMoreView {
  if (state.continuation === null || state.continuation.case === "start") {
    return { mode: "hidden", label: "", enabled: false };
  }
  if (state.loading) {
    return { mode: "loading", label: "Loading earlier messages…", enabled: false };
  }
  if (state.givenUp) {
    return {
      mode: "stopped",
      label: "Could not load earlier messages — retry",
      enabled: true,
    };
  }
  return { mode: "ready", label: "Load earlier messages", enabled: true };
}

/**
 * Paint one {@link LoadMoreView} onto its host element.
 *
 * The host is hidden rather than emptied in the retired case, so the element
 * takes no layout at all once history is exhausted — an empty but present bar
 * would leave a gap above the first bubble for the rest of the session.
 */
export function paintLoadMore(
  host: HTMLElement,
  view: LoadMoreView,
  onClick: () => void,
): void {
  if (view.mode === "hidden") {
    host.hidden = true;
    host.replaceChildren();
    return;
  }
  host.hidden = false;
  const button = document.createElement("button");
  button.type = "button";
  button.className = `load-more load-more--${view.mode}`;
  button.textContent = view.label;
  button.disabled = !view.enabled;
  // The mode rides the DOM so a test — and a person reading the inspector —
  // can see WHICH of the four states produced this button, rather than having
  // to infer it from the copy.
  button.dataset.mode = view.mode;
  if (view.enabled) button.addEventListener("click", onClick);
  host.replaceChildren(button);
}
