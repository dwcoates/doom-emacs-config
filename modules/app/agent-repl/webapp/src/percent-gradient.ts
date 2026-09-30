/**
 * percent-gradient — the ONE way a percentage is colored by how full it is.
 *
 * A pressure figure (a context window's fill, an allowance's use) warms from
 * green through yellow and orange to red as it rises. Each surface picks WHERE
 * those colors sit (its stops); this module owns HOW a percent between two
 * stops is colored, so two surfaces never interpolate differently.
 *
 * HSL, matching the rest of the webapp's color helpers, at a saturation and
 * lightness that read on either theme.
 */

/** One stop on a gradient: at PERCENT, the color is HUE. */
export interface PercentStop {
  readonly at: number;
  readonly hue: number;
}

/** The hues every pressure gradient runs through. */
export const HUE_GREEN = 140;
export const HUE_YELLOW = 60;
export const HUE_ORANGE = 30;
export const HUE_RED = 0;

/**
 * The color for PERCENT on the gradient STOPS.
 *
 * Below the first stop the color is the first stop's; at or past the last it is
 * the last stop's. Between two stops the hue is interpolated linearly, so every
 * intermediate percent has its own shade. Two stops at the same percent are a
 * HARD STEP: the percent itself takes the later stop's color.
 *
 * STOPS out of order, or empty, are a programming error in the caller's own
 * table and throw.
 */
export function percentGradientColor(percent: number, stops: readonly PercentStop[]): string {
  const first = stops[0];
  const last = stops[stops.length - 1];
  if (first === undefined || last === undefined) {
    throw new Error("percentGradientColor: a gradient needs at least one stop");
  }
  for (let i = 1; i < stops.length; i++) {
    if ((stops[i]?.at ?? Number.NaN) < (stops[i - 1]?.at ?? Number.NaN)) {
      throw new Error(`percentGradientColor: stop ${String(i)} sits below the stop before it`);
    }
  }
  return `hsl(${String(Math.round(hueAt(percent, stops, first, last)))} 80% 45%)`;
}

function hueAt(percent: number, stops: readonly PercentStop[], first: PercentStop, last: PercentStop): number {
  if (percent < first.at) return first.hue;
  for (let i = 0; i + 1 < stops.length; i++) {
    const from = stops[i];
    const to = stops[i + 1];
    if (from === undefined || to === undefined) break;
    if (from.at <= percent && percent < to.at) {
      return from.hue + ((to.hue - from.hue) * (percent - from.at)) / (to.at - from.at);
    }
  }
  return last.hue;
}
