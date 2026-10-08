/**
 * quote — A REPLY'S QUOTE in a prompt bubble: the earlier bubble a prompt
 * replied to, which the daemon carries as its own block (a feed row's
 * `FeedQuoteBlock`, a held prompt's `conversation.v1.UserQuoteBlock`).
 *
 * EXPANDED ONLY (owner request, 2026-10-08). A collapsed prompt bubble shows
 * the words the person typed and nothing else; the quote appears, in its
 * place among the blocks, once the bubble is opened. The stylesheet hides
 * every element wearing `BUBBLE_QUOTE_CLASS` in a body whose scroll box is not
 * `.expanded`, so the one toggle (expand.ts) that opens the text opens the
 * quote too, and the has-more measurer (feed/bubble-more.ts) counts a hidden
 * quote as content the collapsed bubble hides.
 *
 * The text is drawn VERBATIM as markdown: the daemon composed it (preamble,
 * fenced quote, closing marker), and this end never parses it.
 */
import { markdownSlot } from "./body.js";

/** The class every quote block wears, whichever bubble draws it. */
export const BUBBLE_QUOTE_CLASS = "bubble-quote";

/**
 * A quote block: a markdown slot the bubble's body paints, wearing CLASSNAME
 * (the drawing kind's own hook) and `BUBBLE_QUOTE_CLASS`.
 */
export function quoteSlot(className: string, text: string): HTMLElement {
  return markdownSlot(`${className} ${BUBBLE_QUOTE_CLASS}`, text);
}
