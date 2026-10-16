/**
 * collapsed-width — A COLLAPSED BUBBLE IS AS WIDE AS THE TEXT IT SHOWS.
 *
 * OWNER REQUEST, 2026-10-16: "the width of a collapsed bubble should be
 * determined by the max width of the text visible in the collapsed bubble."
 * A bubble is `width: fit-content` (styles.css), and the engine fits it to
 * ALL of its content, the lines its collapsed cap hides included. So a
 * collapsed prompt showing only "hello world" was as wide as the long line
 * only its expanded view shows. Expanded, it still fits everything, so it
 * widens as it grows taller.
 *
 * CSS cannot size a box by its visible lines: which lines are visible depends
 * on the wrap, and the wrap depends on the width. So the collapsed box is
 * MEASURED. At its natural width, its visible lines are read
 * (`domCollapsedGeometry`), and the narrowest box that still lays every one of
 * them out on the same line is `collapsedWidth`. That width is handed to the
 * stylesheet as `COLLAPSED_WIDTH_PROPERTY` on the box, which then wears
 * `COLLAPSED_FIT_CLASS`. The width only counts toward the BUBBLE's fit-content
 * size, so the header strip and the footer still widen the bubble when they
 * need more, and the box still stretches to it (styles.css "THE COLLAPSED
 * FIT"). Narrowing never re-wraps a visible line, since each one already fits.
 * Only the hidden lines below re-wrap, and they are hidden.
 *
 * The measurer is bubble-more.ts's `refreshHasMore`, which already runs at
 * every moment a bubble's collapsed face can change: the draw, a re-wrap, the
 * expand toggle. `followContainingWidth` adds the one moment it cannot see: a
 * column that widens around a fitted bubble moves its cap without resizing it.
 * A bubble that hides nothing shows everything, and its natural width is
 * already the fit, so it wears no width.
 */
import { observeContainingBlock, type BubbleBody } from "../bubble/body.js";
import { computedPx } from "../dom.js";
import { log } from "../log.js";
import { domClampGeometry } from "./ellipsis-trim.js";
import { onDiscard } from "./ticking.js";

/** The custom property the stylesheet sizes a fitted collapsed box by. */
export const COLLAPSED_WIDTH_PROPERTY = "--collapsed-width";

/** The class a collapsed box whose width is fitted wears. */
export const COLLAPSED_FIT_CLASS = "collapsed-fit";

/** The operation a collapsed width that cannot be measured is recorded under. */
export const COLLAPSED_WIDTH_UNMEASURABLE = "feed.collapsed-width.unmeasurable";

/** The operation a collapsed width that changed is recorded under. */
export const COLLAPSED_WIDTH_FITTED = "feed.collapsed-width.fitted";

/** One laid-out fragment of a visible line: its right edge and its vertical span, px. */
export interface LineRect {
  readonly right: number;
  readonly top: number;
  readonly bottom: number;
}

/** A float beside the body's lines: its left margin edge and its vertical span, px. */
export interface FloatRect {
  readonly left: number;
  readonly top: number;
  readonly bottom: number;
}

/** What the fit reads off a laid-out collapsed box; the DOM's own unless a test says otherwise. */
export interface CollapsedGeometry {
  /** The box's border-box width, px. */
  boxWidth(box: HTMLElement): number;
  /** The right edge of BODY's content box, px: where its lines may reach. */
  contentRight(body: HTMLElement): number;
  /** Every fragment of BODY's lines that BOX shows. */
  visibleLines(box: HTMLElement, body: HTMLElement): LineRect[];
  /** Every float in BOX beside BODY (the usage corner). */
  floats(box: HTMLElement, body: HTMLElement): FloatRect[];
  /** The width of the `…` the clamp writes in BODY, px. */
  ellipsisWidth(body: HTMLElement): number;
}

/**
 * The width, px, a collapsed box of BOXWIDTH needs to show LINES (at least
 * one) exactly as they lie now, or null when it needs all of it.
 *
 * A line needs its own right edge, plus the room every float beside it takes
 * out of the body's content box (a float outside that box takes none). The
 * last visible line of an ELLIPSIS bubble also needs the `…` the clamp writes
 * after it (ELLIPSISWIDTH, 0 for a fade bubble). The box gives up the slack
 * between the body's content edge and the widest need, rounded so it never
 * gives up a fraction of a pixel a line needs.
 */
export function collapsedWidth(
  boxWidth: number,
  contentRight: number,
  lines: readonly LineRect[],
  floats: readonly FloatRect[],
  ellipsisWidth: number,
): number | null {
  const lastTop = Math.max(...lines.map((l) => l.top));
  let need = -Infinity;
  for (const line of lines) {
    let right = line.right;
    for (const f of floats) {
      if (f.top < line.bottom && line.top < f.bottom) right += Math.max(0, contentRight - f.left);
    }
    if ((line.top + line.bottom) / 2 > lastTop) right += ellipsisWidth;
    need = Math.max(need, right);
  }
  const slack = contentRight - need;
  if (slack <= 0) return null;
  const width = Math.ceil(boxWidth - slack);
  return width < boxWidth ? width : null;
}

/** Record that BOX's collapsed width cannot be measured, and fail: a drawn bubble always can be. */
function unmeasurable(box: HTMLElement, cause: string, context: Record<string, unknown> = {}): never {
  log.error("a collapsed bubble's width cannot be fitted: its box cannot be measured", {
    operation: COLLAPSED_WIDTH_UNMEASURABLE,
    context: { box: box.className, cause, ...context },
  });
  throw new Error(`collapsed width unmeasurable: ${cause}`);
}

/** Drop BOX's fitted width, so it lays out at its natural width. */
export function clearCollapsedWidth(box: HTMLElement): void {
  box.classList.remove(COLLAPSED_FIT_CLASS);
  box.style.removeProperty(COLLAPSED_WIDTH_PROPERTY);
}

/**
 * Fit BOX, a collapsed bubble's box at its natural width
 * (`clearCollapsedWidth`) that hides content, to the lines its BODY shows.
 * ELLIPSIS says whether its clamp writes a `…` after them. PREVIOUS is the
 * width it wore before it was cleared, so only a change is recorded.
 */
export function applyCollapsedWidth(
  box: HTMLElement,
  body: HTMLElement,
  ellipsis: boolean,
  previous: string,
  geometry: CollapsedGeometry = domCollapsedGeometry,
): void {
  const boxWidth = geometry.boxWidth(box);
  const lines = geometry.visibleLines(box, body);
  // A box that shows no line (a zero-line cap: a peer message's face is its
  // label alone) needs no width, and the bubble fits its header strip.
  const width =
    lines.length === 0
      ? 0
      : collapsedWidth(
          boxWidth,
          geometry.contentRight(body),
          lines,
          geometry.floats(box, body),
          ellipsis ? geometry.ellipsisWidth(body) : 0,
        );
  const next = width === null ? "" : `${String(width)}px`;
  if (next !== previous) {
    log.debug("a collapsed bubble's width follows the text it shows", {
      operation: COLLAPSED_WIDTH_FITTED,
      context: { natural_px: boxWidth, fitted: next === "" ? "natural" : next, previous: previous === "" ? "natural" : previous },
    });
  }
  if (width === null) return;
  box.style.setProperty(COLLAPSED_WIDTH_PROPERTY, next);
  box.classList.add(COLLAPSED_FIT_CLASS);
}

/** The width BOX wears now, as `applyCollapsedWidth`'s PREVIOUS: "" when natural. */
export function collapsedWidthOf(box: HTMLElement): string {
  return box.style.getPropertyValue(COLLAPSED_WIDTH_PROPERTY);
}

/** The bottom of the region EL shows of its content, px: its clip edge. */
function clipBottom(el: HTMLElement): number {
  return el.getBoundingClientRect().top + el.clientTop + el.clientHeight;
}

/** The geometry of a collapsed box the engine has laid out. */
export const domCollapsedGeometry: CollapsedGeometry = {
  boxWidth(box) {
    return box.getBoundingClientRect().width;
  },
  contentRight(body) {
    return domClampGeometry.contentRight(body);
  },
  visibleLines(box, body) {
    const doc = body.ownerDocument;
    const view = doc.defaultView;
    if (view === null) unmeasurable(box, "the page has no view");
    // The box clips at its cap, and an ellipsis body clips at its own clamp.
    const bottom = Math.min(clipBottom(box), clipBottom(body));
    const range = doc.createRange();
    const lines: LineRect[] = [];
    const take = (rects: DOMRectList): void => {
      for (const r of Array.from(rects)) {
        if (r.width > 0 && r.top < bottom) lines.push({ right: r.right, top: r.top, bottom: r.bottom });
      }
    };
    const walker = doc.createTreeWalker(body, NodeFilter.SHOW_TEXT | NodeFilter.SHOW_ELEMENT);
    for (let node = walker.nextNode(); node !== null; node = walker.nextNode()) {
      if (node.nodeType === Node.TEXT_NODE) {
        range.selectNodeContents(node);
        take(range.getClientRects());
      } else if (view.getComputedStyle(node as Element).display === "inline") {
        // An inline element's padding and border (inline code's) sit past
        // its text's edge, and a line ending in one needs them too.
        take((node as Element).getClientRects());
      }
    }
    return lines;
  },
  floats(box, body) {
    const view = box.ownerDocument.defaultView;
    if (view === null) unmeasurable(box, "the page has no view");
    const floats: FloatRect[] = [];
    for (const child of Array.from(box.children)) {
      if (child === body) continue;
      const style = view.getComputedStyle(child);
      if (style.float === "none" || style.display === "none") continue;
      const margin = computedPx(style.marginLeft);
      if (margin === null) unmeasurable(box, "a float's left margin is not a px length", { margin: style.marginLeft });
      const rect = child.getBoundingClientRect();
      floats.push({ left: rect.left - margin, top: rect.top, bottom: rect.bottom });
    }
    return floats;
  },
  ellipsisWidth(body) {
    return domClampGeometry.ellipsisWidth(body);
  },
};

/**
 * Refresh the box BODY hangs in whenever the bubble's CONTAINING BLOCK changes
 * width. A fitted box keeps its width when the column grows around it, so no
 * observer on the box or the body sees the cap move, and a visible line the old
 * cap wrapped would stay wrapped. The column is followed through the one
 * observer of a bubble's column (`observeContainingBlock`), armed afresh every
 * time the body joins the document (a tool-group re-arrange moves the row), and
 * the box is the one holding the body when the width moves (a keep-scroll morph
 * hands a body to a box it was not drawn in). A height change alone (the row
 * growing) moves no cap and refreshes nothing.
 */
export function followContainingWidth(body: BubbleBody, refresh: (box: HTMLElement) => void): void {
  let observer: ResizeObserver | null = null;
  body.onConnect(() => {
    observer?.disconnect();
    let width: number | null = null;
    observer = observeContainingBlock(body, (containing) => {
      const next = containing.getBoundingClientRect().width;
      if (next === width) return;
      width = next;
      // A body moved out since stays unrefreshed: its next connect re-follows.
      if (!body.isConnected) return;
      refresh(body.parentElement as HTMLElement);
    });
  });
  onDiscard(body, () => observer?.disconnect());
}
