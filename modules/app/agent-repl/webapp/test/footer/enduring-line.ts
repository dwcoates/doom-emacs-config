/**
 * The enduring line as a fixture: a subscription's usage allowances, a
 * per-seat account's spend, or unobserved, spelled from the figures a test
 * has on hand.
 */

/** What a fixture knows about the enduring figures. */
export interface EnduringFigures {
  usage?: unknown;
  /** A per-seat account's spend (FooterActivityEnduringSeatSpend init). */
  seatSpend?: unknown;
}

/** The enduring line carrying USAGE, a seat's spend, or the unobserved arm when the fixture names neither. */
export function enduringLine(figures: EnduringFigures = {}): { line: { case: string; value: unknown } } {
  if (figures.usage !== undefined) return { line: { case: "usage", value: figures.usage } };
  if (figures.seatSpend !== undefined) return { line: { case: "seatSpend", value: figures.seatSpend } };
  return { line: { case: "unobserved", value: {} } };
}
