/**
 * The load-more affordance at the top of the conversation.
 *
 * # Why the feed has a top at all now
 *
 * It used not to. A cold open replayed the WHOLE conversation, so the first
 * item the feed held was the first item the session ever produced and there
 * was nothing above it. The cold open is now a bounded tail page
 * (conversation-page.proto), so the feed's top is a place where more history
 * may exist — and a user who cannot reach it has simply lost their history.
 *
 * # It renders a DECISION, not a cursor
 *
 * Four states, and the control is a pure function of them, because three
 * separate conditions spread across a renderer is how a button ends up
 * pressable while a request is already out:
 *
 *   - RETIRED: the daemon reported the conversation's beginning. The control
 *     hides entirely; an exhausted history leaves no chrome behind. This is a
 *     FACT the daemon established by reading to the floor, never an inference
 *     from an empty page.
 *   - LOADING: a page request is in flight. Shown and disabled, so a second
 *     click cannot mint a request the pager would only drop.
 *   - STOPPED: the pager hit its failure ceiling. Shown with the retry
 *     wording, because a load-more that silently stops working is worse than
 *     one that says it did.
 *   - READY: there is a cursor and nothing in the way.
 */

/** Everything the control needs to decide what it is. */
export interface LoadMoreState {
  /** The daemon's opaque continuation token, or null when there is none. */
  cursor: string | null;
  /** The daemon reported the conversation's beginning. */
  reachedStart: boolean;
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
 * retired whatever else is true, and a stopped pager is stopped even though it
 * still holds a cursor.
 */
export function loadMoreView(state: LoadMoreState): LoadMoreView {
  if (state.reachedStart || state.cursor === null) {
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
