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
import { bubbleBodyOf, installHasMore, refreshHasMore } from "./bubble-more.js";

/** The class the stylesheet caps, scrolls and paints a scrollbar on. */
export const BUBBLE_SCROLL_CLASS = "bubble-scroll";

/**
 * The class EVERY bubble's box wears, whatever its cap: the structural half of
 * the box (it reaches the bubble's inner edge, never side-scrolls, and is the
 * containing block of anything positioned in it), which the stylesheet keys on
 * separately from the cap, the scroll and the fold `BUBBLE_SCROLL_CLASS` carries.
 */
export const BUBBLE_BOX_CLASS = "bubble-box";

/**
 * Hang one bubble body in its box, and give the box back. A CAPPED box is also
 * the scroll box (`BUBBLE_SCROLL_CLASS`): the cap, the clip, the gutter, the
 * fold and the has-more fade all key on that class. An UNCAPPED box
 * (`BUBBLE_UNCAPPED`, src/bubble/draw.ts) never wears it, so none of them can
 * reach it, and it arms no has-more measurer, having nothing to hide. Which a
 * box is, is fixed when it is built: a redraw that changes it builds a new one.
 */
export function bubbleBox(body: HTMLElement, capped: boolean): HTMLElement {
  const box = document.createElement("div");
  box.className = capped ? `${BUBBLE_SCROLL_CLASS} ${BUBBLE_BOX_CLASS}` : BUBBLE_BOX_CLASS;
  box.append(body);
  // FIX2 (owner ruling, 2026-09-15): keep the "more below" fade in
  // step with the box's overflow for its whole life. The gate to response/prompt
  // bubbles ONLY lives in refreshHasMore (bubble-more.ts) — this factory is
  // shared, so the observer is armed on every capped bubble and self-restricts.
  if (capped) installHasMore(box, refreshHasMore, bubbleBodyOf);
  return box;
}

/** Whether BOX, a bubble's box, was built capped (see `bubbleBox`). */
export function isCappedBox(box: Element): boolean {
  return box.classList.contains(BUBBLE_SCROLL_CLASS);
}
