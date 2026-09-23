/**
 * bubble-scroll — THE ONE SCROLL BOX every bubble kind hangs its body in.
 *
 * OWNER RULING, 2026-09-14: "ensure the scrollbar of the response bubble (and
 * all bubbles, in fact) abuts (or close to) the right hand side of the bubble
 * (currently there's quite a bit of gap), and starts UNDER the token count /
 * metadata in the top right corner of the bubble."
 *
 * Two facts about the DOM are what deliver that, and neither of them is
 * expressible in the stylesheet alone:
 *
 *   - THE SCROLL BOX IS A SIBLING BELOW THE METADATA STRIP. A prompt's author
 *     line and a notice's heading are appended BEFORE this element, so the strip
 *     spans the bubble's full width and the bar underneath it starts where the
 *     strip ends. (The response's cost corner is the exception — owner ruling,
 *     2026-09-15: it lives INSIDE this box, floated top-right before the body,
 *     so the prose's first line wraps around it; see `drawFeedResponse`.) While
 *     the stamp held its own flex column beside the body, the bar necessarily
 *     ran the whole height of the bubble alongside it.
 *   - THE SCROLL BOX IS NOT THE CONTENT WRAPPER. The body (`.bubble-body`)
 *     keeps every rule written against it — the markdown resets, the flush
 *     first/last child margins, the prose repaints that rewrite it whole — and
 *     carries the horizontal inset the bubble's own right padding gave up. This
 *     element carries none, so its right edge is the bubble's inner edge and
 *     the scrollbar lands on it.
 *
 * The N-line cap and the scrollbar styling ride this class in `styles.css`; the
 * only thing built here is the nesting the two rules assume.
 */
import { installHasMore } from "./bubble-more.js";

/** The class the stylesheet caps, scrolls and paints a scrollbar on. */
export const BUBBLE_SCROLL_CLASS = "bubble-scroll";

/** Hang one bubble body in its scroll box, and give the box back. */
export function bubbleScroll(body: HTMLElement): HTMLElement {
  const scroll = document.createElement("div");
  scroll.className = BUBBLE_SCROLL_CLASS;
  scroll.append(body);
  // FIX2 (owner ruling, 2026-09-15): keep the "more below" fade in
  // step with the box's overflow for its whole life. The gate to response/prompt
  // bubbles ONLY lives in refreshHasMore (bubble-more.ts) — this factory is
  // shared, so the observer is armed on every bubble and self-restricts.
  installHasMore(scroll);
  return scroll;
}
