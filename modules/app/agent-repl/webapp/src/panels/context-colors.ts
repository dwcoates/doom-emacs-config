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

/** Linear interpolation between two numbers. */
function lerp(from: number, to: number, t: number): number {
  return from + (to - from) * t;
}

// Hue anchors for the percent gradient. Green is the resting hue; the window
// warms to red as it fills.
const HUE_GREEN = 140;
const HUE_YELLOW = 60;
const HUE_ORANGE = 30;
const HUE_RED = 0;

/**
 * The color for a context-fill percent, on a continuous gradient by proximity
 * to 100.
 *
 * At or below 50 the hue is flat green — there is room, and a moving color
 * would cry wolf. From 50 it interpolates green→yellow (to 70)→orange (to
 * 90)→red (to 100), so every intermediate percent has its own shade and the
 * top of the range is unmistakably red. The input is clamped to `[0, 100]`.
 */
export function contextPercentColor(percent: number): string {
  const p = Math.max(0, Math.min(100, percent));
  let hue: number;
  if (p <= 50) {
    hue = HUE_GREEN;
  } else if (p <= 70) {
    hue = lerp(HUE_GREEN, HUE_YELLOW, (p - 50) / 20);
  } else if (p <= 90) {
    hue = lerp(HUE_YELLOW, HUE_ORANGE, (p - 70) / 20);
  } else {
    hue = lerp(HUE_ORANGE, HUE_RED, (p - 90) / 10);
  }
  return `hsl(${Math.round(hue)} 80% 45%)`;
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
