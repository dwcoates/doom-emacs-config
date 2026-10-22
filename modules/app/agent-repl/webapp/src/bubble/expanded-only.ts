/**
 * expanded-only — A BLOCK A COLLAPSED BUBBLE DOES NOT DRAW.
 *
 * A collapsed prompt bubble (a feed prompt row, a held prompt card) shows the
 * words the person typed and nothing else: a reply's quote (src/bubble/quote.ts)
 * appears, in its place among the blocks, only once the bubble is opened. (An
 * image is drawn small instead, src/bubble/collapsed-thumbnail.ts.) Each such
 * block wears `BUBBLE_EXPANDED_ONLY_CLASS`; the stylesheet hides it in a body whose scroll
 * box is not `.expanded`, so the one toggle (expand.ts) that opens the text
 * opens them too. The has-more measurer (feed/bubble-more.ts) counts none of
 * them: the fade is the person's words' alone (owner ruling, 2026-10-14).
 */

/** The class every block a collapsed bubble hides wears, whichever bubble draws it. */
export const BUBBLE_EXPANDED_ONLY_CLASS = "bubble-expanded-only";

/** Mark EL as a block only an expanded bubble draws, and answer it. */
export function expandedOnly<T extends HTMLElement>(el: T): T {
  el.classList.add(BUBBLE_EXPANDED_ONLY_CLASS);
  return el;
}
