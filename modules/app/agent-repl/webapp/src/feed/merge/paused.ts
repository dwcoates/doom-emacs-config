/**
 * paused — the one mark a paused merge queue draws in the merge bubble: a
 * pause glyph and a word, in the paused amber (owner request, 2026-10-23).
 *
 * The head's "paused" pill (merge.ts) and the queue tab's "queue paused"
 * banner (queue.ts) are both this mark, told apart by their class, so the
 * glyph and its accessibility cannot drift between the two.
 */

/** The class every paused mark carries, whichever place draws it. */
export const PAUSED_MARK_CLASS = "merge-paused-mark";

/** A paused mark reading WORD, with CLASSNAME naming the place it is drawn. */
export function drawPausedMark(className: string, word: string): HTMLElement {
  const el = document.createElement("span");
  el.className = `${PAUSED_MARK_CLASS} ${className}`;
  const glyph = document.createElement("span");
  glyph.className = "merge-paused-glyph";
  glyph.setAttribute("aria-hidden", "true");
  const text = document.createElement("span");
  text.className = "merge-paused-word";
  text.textContent = word;
  el.append(glyph, text);
  return el;
}
