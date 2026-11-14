/**
 * node-reveal — A RESPONSE TREE'S NODE BROUGHT INTO VIEW INSIDE ITS BUBBLE.
 *
 * A node drawn in a capped bubble can sit below that bubble's own scroll box
 * (`BUBBLE_SCROLL_CLASS`): the feed's scroll box counts it in view while the
 * bubble's box clips it. So every reveal of a node first brings the node's
 * lines into view inside each bubble box around them, innermost first, only as
 * far as they need (`nearestEdgeOffset`), and then measures what those boxes
 * leave visible. The feed's own reveal moves against that span, never against
 * lines a bubble hides.
 *
 * Its callers: the composer's reveal (composer-entry.ts) and the revealed-node
 * selection's centering (feed-view.ts).
 */
import { log } from "../log.js";
import { nearestEdgeOffset, type VerticalSpan } from "../scroll.js";
import { BUBBLE_SCROLL_CLASS } from "./bubble-scroll.js";

/**
 * The span LINES are laid out over, from the first's top to the last's
 * bottom, in viewport coordinates; null when no line is laid out (its row not
 * drawn, or drawn folded away). A line of no height is not laid out.
 */
export function linesSpan(lines: readonly HTMLElement[]): VerticalSpan | null {
  const rects = lines.map((line) => line.getBoundingClientRect()).filter((rect) => rect.height > 0);
  if (rects.length === 0) return null;
  return { top: Math.min(...rects.map((r) => r.top)), bottom: Math.max(...rects.map((r) => r.bottom)) };
}

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
 * Bring LINES, one node's laid-out lines in one tree, into view inside every
 * bubble scroll box around them, innermost first and each only as far as the
 * lines need, and answer the span of them those boxes leave visible; null when
 * no line is laid out. Lines in no bubble box are answered as laid out, and
 * nothing moves. WHAT names the reveal in its records.
 */
export function revealInBubbles(lines: readonly HTMLElement[], what: string): VerticalSpan | null {
  if (linesSpan(lines) === null) return null;
  const boxes = bubbleBoxesAround(lines[0]);
  for (const box of boxes) {
    const span = linesSpan(lines) as VerticalSpan;
    const view = box.getBoundingClientRect();
    const offset = nearestEdgeOffset({ top: view.top, bottom: view.bottom }, span);
    if (offset === 0) continue;
    const before = box.scrollTop;
    box.scrollTop = before + offset;
    log.debug(`brought ${what} into view inside its bubble`, {
      operation: "feed.node-reveal.bubble-scrolled",
      context: { what, from: before, to: box.scrollTop },
    });
  }
  let visible = linesSpan(lines) as VerticalSpan;
  for (const box of boxes) {
    const view = box.getBoundingClientRect();
    visible = { top: Math.max(visible.top, view.top), bottom: Math.min(visible.bottom, view.bottom) };
  }
  return visible;
}
