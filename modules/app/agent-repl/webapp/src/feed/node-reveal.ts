/**
 * node-reveal — A RESPONSE TREE'S NODE BROUGHT INTO VIEW INSIDE ITS BUBBLE.
 *
 * A node drawn in a capped bubble can sit below that bubble's own scroll box
 * (`BUBBLE_SCROLL_CLASS`): the feed's scroll box counts it in view while the
 * bubble's box clips it. So every reveal of a node first brings the node's
 * lines into view inside each bubble box around them, innermost first, only as
 * far as they need (`nearestEdgeOffset`) for the composer's reveal and
 * centered (`centerOffset`) for a jump that names the node, and then measures
 * what those boxes leave visible. The feed's own reveal moves against that span, never against
 * lines a bubble hides.
 *
 * Its callers: the composer's reveal (composer-entry.ts) and the revealed-node
 * selection's centering (feed-view.ts).
 */
import { log } from "../log.js";
import { bubbleNodeRevealed, centerOffset, elementsSpan, nearestEdgeOffset, type VerticalSpan } from "../scroll.js";
import { BUBBLE_SCROLL_CLASS } from "./bubble-scroll.js";

/** The bubble scroll boxes around LINE, innermost first. */
function bubbleBoxesAround(line: HTMLElement): HTMLElement[] {
  const boxes: HTMLElement[] = [];
  let box = line.parentElement?.closest<HTMLElement>(`.${BUBBLE_SCROLL_CLASS}`) ?? null;
  while (box !== null) {
    boxes.push(box);
    box = box.parentElement?.closest<HTMLElement>(`.${BUBBLE_SCROLL_CLASS}`) ?? null;
  }
  return boxes;
}

/**
 * How a reveal places a node inside a bubble's box: only as far as it needs
 * (`nearestEdgeOffset`, the composer's reveal), or centered
 * (`centerOffset`, a jump that names the node).
 */
export type BubblePlacement = "nearest" | "center";

/**
 * Bring LINES, one node's laid-out lines in one tree, into view inside every
 * bubble scroll box around them, innermost first, each placed as PLACEMENT
 * says, and answer the span of them those boxes leave visible; null when no
 * line is laid out. Lines in no bubble box are answered as laid out, and
 * nothing moves. WHAT names the reveal in its records.
 */
export function revealInBubbles(lines: readonly HTMLElement[], what: string, placement: BubblePlacement): VerticalSpan | null {
  if (elementsSpan(lines) === null) return null;
  const boxes = bubbleBoxesAround(lines[0]);
  for (const box of boxes) {
    const span = elementsSpan(lines) as VerticalSpan;
    const view = box.getBoundingClientRect();
    const visible = { top: view.top, bottom: view.bottom };
    const offset = placement === "center" ? centerOffset(visible, span) : nearestEdgeOffset(visible, span);
    if (Math.abs(offset) < 1) continue;
    const before = box.scrollTop;
    bubbleNodeRevealed(box, offset);
    log.debug(`brought ${what} into view inside its bubble`, {
      operation: "feed.node-reveal.bubble-scrolled",
      context: { what, placement, from: before, to: box.scrollTop },
    });
  }
  let visible = elementsSpan(lines) as VerticalSpan;
  for (const box of boxes) {
    const view = box.getBoundingClientRect();
    visible = { top: Math.max(visible.top, view.top), bottom: Math.min(visible.bottom, view.bottom) };
  }
  return visible;
}
