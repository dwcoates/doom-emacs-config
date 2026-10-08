/**
 * merge-bubble — the one mark a MERGE bubble wears, and the feed's reading of it.
 *
 * The feed holds a merge bubble to rules of its own (owner rulings,
 * 2026-10-08): the return to the tail never closes it, a page replace keeps
 * it open when the reader had it open, and its own updates never move the
 * feed's scroll. Each of those reads this one mark rather than the row's arm,
 * so the bubble element is the single place the kind is stated.
 */

/** The attribute a merge bubble's element wears (BubbleOptions.merge). */
export const MERGE_BUBBLE_ATTRIBUTE = "data-merge-bubble";

/** Whether EL is a merge bubble's element. */
export function isMergeBubble(el: Element): boolean {
  return el.hasAttribute(MERGE_BUBBLE_ATTRIBUTE);
}
