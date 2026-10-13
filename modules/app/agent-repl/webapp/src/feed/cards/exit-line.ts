/**
 * exit-line — a tool card's exit code, as CONTENT.
 *
 * A shell call's and a failing hook's exit code are bespoke to those cards, so
 * they never sit in a tool card's head (owner ruling, 2026-10-13: the head is
 * the title, the badge and the clock alone). Both cards lead their content
 * with the chip on a line of its own, built here so the two cannot drift.
 */

/** The class the exit line wears. A folded tool card hides it with its output. */
export const TOOL_EXIT_LINE_CLASS = "tool-exit-line";

/** CHIP on a line of its own, leading a card's content. */
export function drawToolExitLine(chip: HTMLElement): HTMLElement {
  const line = document.createElement("div");
  line.className = TOOL_EXIT_LINE_CLASS;
  line.append(chip);
  return line;
}
