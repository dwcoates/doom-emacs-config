/**
 * root-feed-shell — the root feed's scroll box as the page draws it, for the
 * suites that read its geometry (the page census, `feed-geometry`). The rows
 * stand in the feed's row list (`.feed-body > .feed-rows`), built from the
 * classes the feed's own body mount uses, so a suite cannot drift onto a
 * nesting the page never draws.
 */
import { FEED_BODY_CLASS, FEED_ROWS_CLASS } from "../src/feed/row-id.js";

/** The root feed's elements, drawn into the document's body. */
export interface RootFeedShell {
  /** `#feed-scroll`, the scroll box. */
  box: HTMLElement;
  /** `#feed`, the root feed's host. */
  feed: HTMLElement;
  /** The row list the root rows are drawn in. */
  rows: HTMLElement;
}

/** Replace the body with the root feed's scroll box, `#feed` and its empty row list, then TRAILING after `#feed`. */
export function drawRootFeedShell(trailing = ""): RootFeedShell {
  document.body.innerHTML =
    `<div id="feed-scroll"><main id="feed"><div class="${FEED_BODY_CLASS}">` +
    `<div class="${FEED_ROWS_CLASS}"></div></div></main>${trailing}</div>`;
  return {
    box: document.getElementById("feed-scroll")!,
    feed: document.getElementById("feed")!,
    rows: document.querySelector<HTMLElement>(`.${FEED_ROWS_CLASS}`)!,
  };
}
