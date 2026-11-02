/**
 * placement — the DOM side of the placement arithmetic (topbar/clamp.ts):
 * reading the window a fixed box is placed inside, and applying a placement to
 * the box. Every fixed-position popup (a row's panel, its details popup, the
 * version's memory and notices, the host's copy of a row's panel) reads and
 * applies through these, so none can drift from the others.
 */
import type { RevealPlacement, Viewport } from "./topbar/clamp.js";

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
