/**
 * dom-rect — the one scripted `DOMRect` every suite hands a stubbed
 * `getBoundingClientRect`. jsdom lays nothing out, so a suite states the box
 * the engine would give; every edge is derived here from the box's LEFT, TOP,
 * WIDTH and HEIGHT (each 0 unless stated), so no suite's rect can disagree
 * with itself about where its right or bottom edge is.
 */

/** The box a scripted rect is stated by. */
export interface BoxInit {
  left?: number;
  top?: number;
  width?: number;
  height?: number;
}

/** The `DOMRect` of the box INIT states. */
export function boxRect({ left = 0, top = 0, width = 0, height = 0 }: BoxInit = {}): DOMRect {
  return { left, top, width, height, right: left + width, bottom: top + height, x: left, y: top, toJSON: () => ({}) };
}
