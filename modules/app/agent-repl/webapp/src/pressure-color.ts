/**
 * pressure-color — the ONE colour a "how full is it" percentage takes, shared
 * by the footer's allowance percentages and the topbar's context figure.
 *
 * THE SAME CODE ON BOTH SURFACES (owner, 2026-10-01): the topbar's context
 * figure is coloured by exactly the rule the footer's percentages use, keyed
 * on the fraction of the context window in use, so a chip at 40% of its window
 * wears the colour a 40% allowance wears. Neither surface may grow its own
 * stops; both call `pressurePercentColor`.
 */
import { HUE_GREEN, HUE_ORANGE, HUE_RED, HUE_YELLOW, percentGradientColor, type PercentStop } from "./percent-gradient.js";

/**
 * Where a pressure percentage's colors sit (owner, 2026-09-30): green below
 * 40, yellow by 70, orange by 90, red at 90 and above. Between 40 and 90 the
 * hue runs continuously, so 55 sits halfway between green and yellow and 80
 * halfway between yellow and orange; 90 is a hard step to red.
 */
const PRESSURE_PERCENT_STOPS: readonly PercentStop[] = [
  { at: 40, hue: HUE_GREEN },
  { at: 70, hue: HUE_YELLOW },
  { at: 90, hue: HUE_ORANGE },
  { at: 90, hue: HUE_RED },
];

/**
 * The colour a pressure percentage takes: an allowance's use or the context
 * window's fill. PERCENT is the figure as drawn, 0..100.
 */
export function pressurePercentColor(percent: number): string {
  return percentGradientColor(percent, PRESSURE_PERCENT_STOPS);
}
