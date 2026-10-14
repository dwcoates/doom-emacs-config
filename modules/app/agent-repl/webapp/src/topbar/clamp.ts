/**
 * clampReveal — where a topbar reveal actually lands.
 *
 * THE REVEAL CONVENTION IS ABSOLUTE (topbar.proto, and the visual directives):
 * every reveal renders BELOW the strip and CLAMPS within the viewport. Never
 * upward — a dropdown that flips above its anchor is the single most common way
 * a topbar menu ends up covering the thing it belongs to — and never off any
 * edge.
 *
 * SO THE TOP IS NOT NEGOTIABLE and only the LEFT and the HEIGHT are. A reveal
 * anchored under the warning chip at the far right would run off the right edge
 * at its own width, so it slides LEFT until it fits, keeping its top edge under
 * the strip; a reveal too tall for what is left of the window keeps its
 * position and takes a scrollable height instead of growing past the bottom.
 * Sliding is right because the reader's eye is already at the anchor and the
 * reveal stays adjacent to it; flipping would move it somewhere the reader is
 * not looking.
 *
 * PURE ARITHMETIC over plain rectangles, so the rules are testable without a
 * layout engine — jsdom reports every rect as zero, which is exactly the shape
 * a "clamps correctly" test cannot be written against.
 */

/** The part of a DOMRect this cares about. */
export interface Rect {
  left: number;
  top: number;
  right: number;
  bottom: number;
  width: number;
  height: number;
}

/** The window the reveal must stay inside. */
export interface Viewport {
  width: number;
  height: number;
}

/** Where a reveal is placed, in viewport coordinates. */
export interface RevealPlacement {
  left: number;
  top: number;
  /** The height it may take before it must scroll inside itself. */
  maxHeight: number;
}

/**
 * The gap kept between a reveal and the viewport edge.
 *
 * Non-zero so a clamped reveal reads as "held back from the edge" rather than
 * as one that has been cut off by it.
 */
export const REVEAL_MARGIN_PX = 8;

/**
 * The left edge nearest PREFERRED that keeps a WIDTH-wide box inside VIEWPORT.
 *
 * It slides left only as far as the right edge demands. A box WIDER than the
 * viewport would make the rightmost fitting edge smaller than the margin, so
 * the lower bound is applied second and wins: overflowing right is better than
 * overflowing left, where the reader's line of text begins.
 */
export function clampLeft(preferred: number, width: number, viewport: Viewport): number {
  const rightmost = viewport.width - REVEAL_MARGIN_PX - width;
  return Math.max(REVEAL_MARGIN_PX, Math.min(preferred, rightmost));
}

/**
 * The height a box whose top edge is TOP may take inside VIEWPORT before it
 * must scroll inside itself. Floored at zero so a box starting below the
 * window yields no negative height for a caller to apply.
 */
export function heightBelow(top: number, viewport: Viewport): number {
  return Math.max(0, viewport.height - top - REVEAL_MARGIN_PX);
}

/**
 * Place REVEAL under ANCHOR inside VIEWPORT.
 *
 * The left edge prefers the anchor's own left, so a reveal reads as belonging
 * to the control that opened it; it slides only as far as it must.
 */
export function clampReveal(anchor: Rect, reveal: Rect, viewport: Viewport): RevealPlacement {
  const top = anchor.bottom;
  return {
    left: clampLeft(anchor.left, reveal.width, viewport),
    top,
    maxHeight: heightBelow(top, viewport),
  };
}
