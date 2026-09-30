/**
 * The enduring line as a fixture: the one line the daemon chose — the usage,
 * the context window, or neither observed yet — spelled from the figures a
 * test has on hand.
 */

/** What a fixture knows about the enduring figures. */
export interface EnduringFigures {
  usage?: unknown;
  contextWindow?: unknown;
}

/**
 * The enduring line carrying USAGE, or else CONTEXTWINDOW, or the unobserved
 * arm when the fixture names neither. A fixture naming both is a test defect:
 * the line is one line.
 */
export function enduringLine(figures: EnduringFigures = {}): { line: { case: string; value: unknown } } {
  if (figures.usage !== undefined && figures.contextWindow !== undefined) {
    throw new Error("an enduring line carries one figure, not both");
  }
  if (figures.usage !== undefined) return { line: { case: "usage", value: figures.usage } };
  if (figures.contextWindow !== undefined) {
    return { line: { case: "contextWindow", value: figures.contextWindow } };
  }
  return { line: { case: "unobserved", value: {} } };
}
