/**
 * format — THE ONE client-side token formatter.
 *
 * WHY IT IS SHARED. The daemon composes most token figures itself (a footer
 * stamp, a response's usage line) with `format.go`'s rules, and the client
 * formats the few counts that reach it raw (a cold gate's context size). Two
 * implementations of the same scale is a defect the reader sees directly: the
 * same number rendered "12k" in one place and "12.3k" in another reads as two
 * different facts. So the daemon's rules are written here once, and every
 * client-side site that scales a token count calls this.
 *
 * THE RULES (ruled 2026-08-31, mirroring the daemon's `format.go`):
 *   - below 1000 the count is drawn unscaled;
 *   - at or above it the count is scaled to `k` or `M` with EXACTLY one
 *     fractional digit, rounded to nearest, and a trailing ".0" trimmed;
 *   - THE UNIT IS CHOSEN BY THE RENDERED VALUE, not the raw one: 999950 rounds
 *     to 1000.0k, and "1000k" is a figure no reader should ever be shown, so it
 *     becomes "1M".
 */

/** A token count as the figure a reader weighs: `999`, `1.2k`, `182k`, `1.2M`. */
export function formatTokens(n: number): string {
  if (n < 1000) return String(n);
  const thousands = scale(n / 1000);
  // The rounding is what can push a figure over its unit, so the promotion is
  // decided AFTER it: `999949` is still 999.9k, `999950` is already 1M.
  if (thousands < 1000) return `${trim(thousands)}k`;
  return `${trim(scale(n / 1_000_000))}M`;
}

/** One fractional digit, rounded to nearest. */
function scale(value: number): number {
  return Math.round(value * 10) / 10;
}

/** The digit, with a bare ".0" dropped. */
function trim(value: number): string {
  return value.toFixed(1).replace(/\.0$/, "");
}
