/**
 * placement — the DOM side of the placement arithmetic (topbar/clamp.ts):
 * reading the window a fixed box is placed inside, and applying a placement to
 * the box. Every fixed-position popup (a row's panel, its details popup, the
 * version's memory and notices, the host's copy of a row's panel) reads and
 * applies through these, so none can drift from the others.
 */
import { clampBeside, type Rect, type RevealPlacement, type Viewport } from "./topbar/clamp.js";

/** WIN's viewport, as the placements read it. */
export function viewportOf(win: Window = window): Viewport {
  return { width: win.innerWidth, height: win.innerHeight };
}

/**
 * Stand fixed-position EL where AT says: viewport coordinates, applied
 * verbatim, since a fixed box is positioned against the window.
 */
export function applyPlacement(el: HTMLElement, at: RevealPlacement): void {
  el.style.left = `${at.left}px`;
  el.style.top = `${at.top}px`;
  el.style.maxHeight = `${at.maxHeight}px`;
}

/**
 * Stand fixed-position EL beside ANCHOR inside VIEWPORT (`clampBeside`):
 * its left on the anchor's right edge, its top on the anchor's, sliding only
 * as far as the window's edges demand. EL is measured with MEASURE at its
 * natural height, since a cap an earlier placement applied would otherwise
 * hold a box that moved up the window at its old, shorter size. Every popup
 * that stands beside its anchor (a row's panel and menu, a panel's details
 * popup, the host's copy of either) is placed here. Answers the placement.
 */
export function placeBeside(
  el: HTMLElement,
  anchor: Rect,
  viewport: Viewport,
  measure: (el: Element) => Rect = (box) => box.getBoundingClientRect(),
): RevealPlacement {
  el.style.maxHeight = "";
  const at = clampBeside(anchor, measure(el), viewport);
  applyPlacement(el, at);
  return at;
}
