/**
 * Duration formatting, shared by the two places a session reports time:
 * the end-of-turn result chip (how long the turn took) and the topbar's
 * live task timer (how long the running turn has taken so far).
 */

/** Duration units, coarsest-first. Each scale is a whole multiple of the next. */
const DURATION_UNITS: ReadonlyArray<{ suffix: string; scale: number }> = [
  { suffix: "h", scale: 3_600_000 },
  { suffix: "m", scale: 60_000 },
  { suffix: "s", scale: 1000 },
  { suffix: "ms", scale: 1 },
];

/**
 * A duration in whole units: the coarsest unit whose magnitude reaches 1,
 * with the leftover carried by the next unit down rather than by a
 * fraction of the coarse one (330000ms → `5m 30s`, 12300ms → `12s 300ms`,
 * 5400000ms → `1h 30m`).
 *
 * The leftover is dropped when it is zero (120000ms → `2m`), and rounding
 * it up past its own unit promotes the coarse magnitude instead
 * (3599999ms → `1h`, never `59m 60s`).
 */
export function formatDuration(ms: number): string {
  const total = Math.round(ms);
  const found = DURATION_UNITS.findIndex((unit) => total >= unit.scale);
  // A sub-millisecond duration reaches no unit's magnitude, so milliseconds
  // (the finest unit) carry it.
  const index = found === -1 ? DURATION_UNITS.length - 1 : found;
  const major = DURATION_UNITS[index];
  const minor = DURATION_UNITS[index + 1];
  if (!minor) return `${total}${major.suffix}`;

  // Snap to a whole minor unit first, so a leftover that rounds up to a
  // full major unit re-picks the coarser unit instead of overflowing.
  const snapped = Math.round(total / minor.scale) * minor.scale;
  if (snapped !== total) return formatDuration(snapped);

  const majorValue = Math.floor(total / major.scale);
  const minorValue = (total - majorValue * major.scale) / minor.scale;
  return minorValue === 0
    ? `${majorValue}${major.suffix}`
    : `${majorValue}${major.suffix} ${minorValue}${minor.suffix}`;
}

/**
 * Wall time elapsed so far, at second resolution: `0s`, `45s`, `5m 30s`,
 * `1h 5m`. A live timer ticks once a second, so milliseconds would be a
 * digit that churns without ever being read.
 *
 * Truncated rather than rounded, since a timer that reads `1s` before a
 * whole second has passed is reporting time that has not happened yet.
 *
 * A start stamped fractionally ahead of the reader's own clock (the daemon
 * stamps the turn, the browser reads it) lands here as a negative elapsed,
 * which floors to `0s` rather than counting backwards.
 */
export function formatElapsed(ms: number): string {
  const seconds = Math.max(0, Math.floor(ms / 1000));
  // formatDuration's finest unit is milliseconds, so a zero would come back
  // as `0ms` — the one value the second-resolution scale has to say itself.
  return seconds === 0 ? "0s" : formatDuration(seconds * 1000);
}
