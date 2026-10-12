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
 * jumping back. The rail's wall-clock seek (`wall-phase.ts`) carries every
 * looping rail animation across a redraw, this one included, so nothing here
 * keeps a clock of its own.
 */

/** The class the name wears while reviving; the shimmer rule keys on it. */
export const REVIVING_CLASS = "reviving";

/** The row-level hook attribute, and the one value it takes. */
export const REVIVING_ATTRIBUTE = "data-reviving";

/**
 * Mark ROW's NAME as reviving: the class the shimmer keys on, and the row's
 * hook attribute.
 */
export function markReviving(row: HTMLElement, name: HTMLElement): void {
  name.classList.add(REVIVING_CLASS);
  row.setAttribute(REVIVING_ATTRIBUTE, "true");
}
