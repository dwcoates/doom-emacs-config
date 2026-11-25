/**
 * The parts of a drawn roster row (src/sidebar/row.ts) the row's tests reach
 * for, shared by its behavior tests (test/sidebar/row.test.ts) and its rules
 * (rules/sidebar/row.rules.test.ts).
 */

/** The row line: what the pointer and the keyboard focus are read from. */
export const lineOf = (drawn: HTMLElement): HTMLElement =>
  drawn.querySelector(":scope > .row") as HTMLElement;

/** The row's "⋯" control, which opens the panel at once too (owner request, 2026-11-02). */
export const moreOf = (drawn: HTMLElement): HTMLElement => lineOf(drawn).querySelector(":scope > .sb-more") as HTMLElement;
