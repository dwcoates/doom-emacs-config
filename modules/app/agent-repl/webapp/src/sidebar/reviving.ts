/**
 * reviving — the shimmer a roster row's NAME wears while the daemon brings its
 * parked (hibernated) session back up.
 *
 * THE WIRE DECIDES WHEN. `frontend.v1.RosterRowReviving` is present from the
 * instant the daemon decides to revive the workspace until that revival ends,
 * success or failure; this end draws the shimmer exactly while the marker is on
 * the row and remembers nothing. It is not a status arm: the dot, the viewed
 * mode and everything else on the row are untouched by it.
 *
 * SUBTLE ON PURPOSE (owner ruling, 2026-09-19): a soft band of reduced ink
 * ripples across the name's text and rests off the text between passes. The
 * `ws-revive-shimmer` keyframes in styles.css paint it; reduced motion stops
 * it outright, leaving the name drawn plainly.
 *
 * PHASE CONTINUITY. The sidebar redraws every row on every roster push, and a
 * fresh element restarts its animation at 0%, which mid-pass reads as the band
 * jumping back. So the same {@link AnimationEpoch} the footer and the prompt
 * bubble use: one start time, and every draw emits a negative
 * `animation-delay` seeking the new name to where the band already was.
 */
import { AnimationEpoch } from "../breathing.js";

/**
 * One full pass of the shimmer. Must match the `ws-revive-shimmer` duration in
 * styles.css, or a redrawn name seeks to the wrong phase.
 */
export const REVIVE_SHIMMER_PERIOD_MS = 2400;

/** The class the name wears while reviving; the shimmer rule keys on it. */
export const REVIVING_CLASS = "reviving";

/** The row-level hook attribute, and the one value it takes. */
export const REVIVING_ATTRIBUTE = "data-reviving";

/** The page-global shimmer phase, so every reviving row ripples in unison. */
export class ReviveShimmer {
  private epoch = new AnimationEpoch();

  /** How far into the current pass, in `[0, REVIVE_SHIMMER_PERIOD_MS)`. */
  delayMs(nowMs: number): number {
    return this.epoch.elapsedMs(nowMs) % REVIVE_SHIMMER_PERIOD_MS;
  }
}

/** The one shimmer every sidebar row draws against. */
export const reviveShimmer = new ReviveShimmer();

/**
 * Mark ROW's NAME as reviving: the class the shimmer keys on, the negative
 * delay that continues the page's pass, and the row's hook attribute.
 */
export function markReviving(
  row: HTMLElement,
  name: HTMLElement,
  shimmer: ReviveShimmer = reviveShimmer,
  nowMs: number = Date.now(),
): void {
  name.classList.add(REVIVING_CLASS);
  name.style.animationDelay = `-${Math.round(shimmer.delayMs(nowMs))}ms`;
  row.setAttribute(REVIVING_ATTRIBUTE, "true");
}
