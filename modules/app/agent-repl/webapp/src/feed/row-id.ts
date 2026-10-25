/**
 * THE FEED ROW'S IDENTITY ON THE PAGE: the one attribute every feed row's
 * element carries its FeedId on (`frontend.v1.FeedRow.id`), at whatever depth
 * of sub-feed it is drawn, and the selector that finds those elements. Every
 * module that finds, names or marks a feed row by its element reads it through
 * here, so the attribute is spelled once.
 */

/** The attribute a feed row's element carries its FeedId on. */
export const FEED_ROW_ATTRIBUTE = "data-feed-row";

/** Every feed row's element, at whatever depth of sub-feed it is drawn. */
export const FEED_ROW_SELECTOR = `[${FEED_ROW_ATTRIBUTE}]`;

/** ROW's FeedId, or null when the element carries none. */
export function feedRowId(row: Element): string | null {
  return row.getAttribute(FEED_ROW_ATTRIBUTE);
}

/** The feed row EL is drawn in, EL itself when it is one, or null. */
export function feedRowOf<E extends Element = Element>(el: Element): E | null {
  return el.closest<E>(FEED_ROW_SELECTOR);
}
