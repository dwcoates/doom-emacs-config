/**
 * The enduring line as a fixture: the usage allowances, the account's having
 * no allowance window, or unobserved, spelled from the figures a test has on
 * hand.
 */

/** What a fixture knows about the enduring figures. */
export interface EnduringFigures {
  usage?: unknown;
  /** The account's usage service reported no allowance window. */
  noAllowance?: true;
}

/** The enduring line carrying USAGE, the no-allowance arm, or the unobserved arm when the fixture names neither. */
export function enduringLine(figures: EnduringFigures = {}): { line: { case: string; value: unknown } } {
  if (figures.usage !== undefined) return { line: { case: "usage", value: figures.usage } };
  if (figures.noAllowance === true) return { line: { case: "noAllowance", value: {} } };
  return { line: { case: "unobserved", value: {} } };
}
