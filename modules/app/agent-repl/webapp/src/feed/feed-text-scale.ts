/**
 * feed-text-scale — apply the daemon-owned feed text zoom to the DOM.
 *
 * The daemon owns and persists a single global feed text scale and pushes it on
 * every feed watch (frontend.v1.FeedTextScale). This module is the ONE place
 * that lands that value in the DOM: it writes the `--feed-text-scale` custom
 * property on the document element, which every feed-scroll font-size multiplies
 * into as `calc(<base> * var(--feed-text-scale))` (see styles.css). The scale is
 * therefore applied in exactly one spot and reaches all feed text at once, while
 * nothing outside the feed references the property.
 */
import { log } from "../log.js";

/** The custom property every feed font-size multiplies into. */
export const FEED_TEXT_SCALE_PROPERTY = "--feed-text-scale";

/**
 * Write `scale` onto the document element's `--feed-text-scale`, re-scaling all
 * feed text at once. A non-finite or non-positive value is a defect in the
 * pushed frame (the daemon clamps to a positive range), so it is refused and
 * logged rather than silently written — the current zoom stays in force.
 */
export function applyFeedTextScale(scale: number): void {
  if (!Number.isFinite(scale) || scale <= 0) {
    log.warn("ignored a non-finite or non-positive feed text scale", {
      operation: "feed.text-scale.invalid",
      context: { scale: String(scale) },
    });
    return;
  }
  document.documentElement.style.setProperty(FEED_TEXT_SCALE_PROPERTY, String(scale));
}
