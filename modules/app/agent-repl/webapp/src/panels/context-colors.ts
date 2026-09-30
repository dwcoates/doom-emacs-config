/**
 * context-colors — the /context panel's two OWN color scales.
 *
 * The panel does NOT reuse the vendor's category color scheme. It owns its
 * presentation, so the colors are minted here:
 *
 *  - the HEADER PERCENT is colored on a CONTINUOUS gradient by proximity to
 *    100 — flat green while there is room, warming through yellow and orange to
 *    red as the window fills, so a glance at the one number reads as pressure.
 *  - each TOP-LEVEL SECTION takes a distinct color from a fixed palette BY
 *    ORDER, so the eye tracks a label to its figure across the horizontal gap
 *    and tells one section from the next.
 *
 * Both are HSL, matching the rest of the webapp's color helpers, and pick a
 * lightness that reads on either theme.
 */

import { HUE_GREEN, HUE_ORANGE, HUE_RED, HUE_YELLOW, percentGradientColor, type PercentStop } from "../percent-gradient.js";

/**
 * Where the header percent's colors sit. At or below 50 the hue is flat green —
 * there is room, and a moving color would cry wolf. From 50 it runs
 * green→yellow (to 70)→orange (to 90)→red (to 100).
 */
const CONTEXT_PERCENT_STOPS: readonly PercentStop[] = [
  { at: 50, hue: HUE_GREEN },
  { at: 70, hue: HUE_YELLOW },
  { at: 90, hue: HUE_ORANGE },
  { at: 100, hue: HUE_RED },
];

/**
 * The color for a context-fill percent, on a continuous gradient by proximity
 * to 100, so every intermediate percent has its own shade and the top of the
 * range is unmistakably red.
 */
export function contextPercentColor(percent: number): string {
  return percentGradientColor(percent, CONTEXT_PERCENT_STOPS);
}

/**
 * A fixed palette of visually distinct hues for the top-level section rows.
 * Deliberately NOT the green/yellow/red of the fill gradient, so a section's
 * eye-tracking color is never mistaken for a pressure reading.
 */
const SECTION_PALETTE: readonly string[] = [
  "hsl(210 70% 55%)", // blue
  "hsl(280 55% 62%)", // purple
  "hsl(170 60% 42%)", // teal
  "hsl(330 65% 60%)", // pink
  "hsl(45 75% 48%)", // gold
  "hsl(255 60% 66%)", // indigo
  "hsl(190 68% 45%)", // cyan
  "hsl(15 70% 58%)", // coral
];

/**
 * The color a top-level section takes, assigned by its ORDER. The palette
 * cycles, so a panel with more sections than colors reuses hues from the top
 * rather than running out.
 */
export function contextSectionColor(index: number): string {
  return SECTION_PALETTE[index % SECTION_PALETTE.length];
}
