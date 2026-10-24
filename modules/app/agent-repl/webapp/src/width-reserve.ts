/**
 * width-reserve — the fixed width a ticking figure is drawn in.
 *
 * A clock that repaints every second changes its text's width ("9s" to "10s",
 * "59s" to "1m"), and whatever sits after it slides, or wraps onto a new line
 * and changes the feed's height. A RESERVE is the widest label of every shape
 * the figure's formatter writes, with each digit zeroed: in tabular figures
 * every digit is as wide as a zero, so the reserve is exactly as wide as the
 * widest reading the figure can tick to. The reader draws the reserve as an
 * invisible ghost under the figure (a one-cell grid, or a `white-space: pre`
 * pseudo-element reading `reserveLabelsCss`), so the figure's box never
 * changes width while it ticks.
 *
 * The labels are derived from the formatter itself, so a formatter that
 * changes its shapes changes every reserve with it.
 */

/** LABEL with every figure zeroed: in tabular figures, the same width. */
export function zeroFigures(label: string): string {
  return label.replace(/\d/g, "0");
}

/** The reserve for LABEL's readings: the zeroed label of each of the WIDEST instants. */
export function reserveLabels(widestMs: readonly number[], label: (ms: number) => string): readonly string[] {
  return widestMs.map((ms) => zeroFigures(label(ms)));
}

/**
 * Whether LABEL fits RESERVE: some reserved label has its shape (the same
 * units in the same order) and at least as many figures. In tabular figures
 * that is "no wider".
 */
export function fitsReserve(label: string, reserve: readonly string[]): boolean {
  const zeroed = zeroFigures(label);
  const shape = zeroed.replace(/0+/g, "0");
  return reserve.some((reserved) => reserved.replace(/0+/g, "0") === shape && zeroed.length <= reserved.length);
}

/**
 * RESERVE as one CSS string, one label per line (`\A`), for a ghost's
 * `content`: drawn `white-space: pre`, its box is as wide as the widest label.
 */
export function reserveLabelsCss(reserve: readonly string[]): string {
  return `"${reserve.map(cssStringBody).join("\\A ")}"`;
}

/**
 * TEXT as one CSS string, for a ghost's `content` or an attribute selector's
 * value: quoted, with its backslashes and quotes escaped and a line break
 * written `\A `, so any text reads back as exactly itself.
 */
export function cssString(text: string): string {
  return `"${cssStringBody(text)}"`;
}

/** TEXT escaped for the inside of a CSS string. */
function cssStringBody(text: string): string {
  return text.replace(/[\\"]/g, "\\$&").replace(/\n/g, "\\A ");
}
