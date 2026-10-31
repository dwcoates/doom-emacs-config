/**
 * The ONE drawing of a topbar glyph on its disc: the persistent-wifi chip's
 * wifi and the remote-control chip's phone (owner ruling, 2026-10-31: the two
 * chips share one look and feel and differ only in their symbol).
 *
 * Each glyph is a set of stroked paths on a 24-unit grid centered on (12, 12),
 * symmetric about x = 12 so its horizontal center IS the grid's. The stylesheet
 * strokes the paths and fills the disc; this file fixes only the geometry, so
 * two glyphs drawn through it share their size, their disc and their center.
 */
import { SVG_NS } from "../dom.js";

/**
 * THE DISC IS DRAWN IN THE GLYPH'S OWN SVG, centered on the same (12, 12).
 * It used to be the chip's CSS background with a smaller svg centered inside
 * it by flexbox; the two then landed on the pixel grid separately, and WebKit
 * snaps an inline svg to whole pixels while it paints a background where
 * layout put it, so the glyph sat up to a pixel right of the disc's center
 * (measured in test/webkit/topbar.webkit.test.ts). One svg has one placement,
 * so the glyph and the disc cannot drift apart. The radius keeps every glyph
 * just inscribed: its farthest stroked corner is under 15 units from center.
 */
export const DISC_RADIUS = 15;

/** The viewBox: the disc's square, so the svg's box is the disc's. */
export const DISC_VIEW_BOX = `${12 - DISC_RADIUS} ${12 - DISC_RADIUS} ${2 * DISC_RADIUS} ${2 * DISC_RADIUS}`;

/** What one chip's glyph is: its class names and its stroked paths. */
export interface DiscGlyph {
  /** The svg's class, the hook its stroke rule is keyed on. */
  readonly glyphClass: string;
  /** The disc circle's class, the hook its fill rule is keyed on. */
  readonly discClass: string;
  /** The glyph's path data, drawn in order over the disc. */
  readonly paths: readonly string[];
}

/** The glyph element: the disc, then the glyph's paths over it. */
export function drawDiscGlyph(glyph: DiscGlyph): SVGSVGElement {
  const svg = document.createElementNS(SVG_NS, "svg");
  svg.setAttribute("viewBox", DISC_VIEW_BOX);
  svg.setAttribute("aria-hidden", "true");
  svg.setAttribute("class", glyph.glyphClass);
  const disc = document.createElementNS(SVG_NS, "circle");
  disc.setAttribute("class", glyph.discClass);
  disc.setAttribute("cx", "12");
  disc.setAttribute("cy", "12");
  disc.setAttribute("r", String(DISC_RADIUS));
  svg.append(disc);
  for (const d of glyph.paths) {
    const path = document.createElementNS(SVG_NS, "path");
    path.setAttribute("d", d);
    svg.append(path);
  }
  return svg;
}
