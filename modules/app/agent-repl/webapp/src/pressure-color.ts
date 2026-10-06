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
import { MalformedView } from "./rpc/malformed.js";

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

/**
 * A window fill (0..1 on the wire) as the whole percent the footer would draw
 * it. Every surface that colors a token figure by its window fill reads the
 * fill through here, so they all blend from the same whole percent. A fill
 * outside [0, 1] breaks the contract (the daemon clamps) and is refused at
 * PATH, never painted.
 */
export function windowFillPercent(windowFill: number, path: string): number {
  if (!(windowFill >= 0 && windowFill <= 1)) {
    throw new MalformedView(path, `${String(windowFill)} is outside [0, 1]`);
  }
  return Math.round(windowFill * 100);
}

/**
 * Where the cold gate's token figure's colors sit (owner, 2026-10-06): green
 * below 20, yellow at 35, orange at 50, red at 70 and above, blended between
 * stops exactly as the pressure gradient blends. They are NOT the pressure
 * stops because the figure means something else: how costly paying to pass
 * the gate is, which is already dear at a fraction of the window.
 */
const COLD_GATE_PERCENT_STOPS: readonly PercentStop[] = [
  { at: 20, hue: HUE_GREEN },
  { at: 35, hue: HUE_YELLOW },
  { at: 50, hue: HUE_ORANGE },
  { at: 70, hue: HUE_RED },
];

/** The colour a cold gate's window-fill percentage takes. PERCENT is 0..100. */
export function coldGatePercentColor(percent: number): string {
  return percentGradientColor(percent, COLD_GATE_PERCENT_STOPS);
}

/**
 * The cold gate's token figure's colour from its window fill. THE SAME CODE
 * ON BOTH SURFACES: the footer's cold-gate line and the docked gate's figure
 * both call this, refusing a malformed fill at PATH.
 */
export function coldGateFigureColor(windowFill: number, path: string): string {
  return coldGatePercentColor(windowFillPercent(windowFill, path));
}
