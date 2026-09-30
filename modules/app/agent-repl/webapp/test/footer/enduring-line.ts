/**
 * The enduring line as a fixture: the usage allowances, or unobserved,
 * spelled from the figures a test has on hand.
 */

/** What a fixture knows about the enduring figures. */
export interface EnduringFigures {
  usage?: unknown;
}

/** The enduring line carrying USAGE, or the unobserved arm when the fixture names none. */
export function enduringLine(figures: EnduringFigures = {}): { line: { case: string; value: unknown } } {
  if (figures.usage !== undefined) return { line: { case: "usage", value: figures.usage } };
  return { line: { case: "unobserved", value: {} } };
}
