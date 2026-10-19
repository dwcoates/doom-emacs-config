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
 * quote too. The has-more measurer (feed/bubble-more.ts) never counts a
 * hidden quote: the fade is the person's words' alone (owner ruling,
 * 2026-10-14).
 *
 * A HELD prompt's quote is drawn VERBATIM as markdown: the record carries the
 * text the daemon composed (preamble, fenced quote, closing marker), and this
 * end never parses it. A FEED prompt's quote arrives taken apart by the daemon
 * (`frontend.v1.FeedQuoteBlock`), and its HEADING shows folded too (owner
 * request, 2026-10-19): see `QUOTE_PARTS_CLASS`.
 */
import { markdownSlot } from "./body.js";

/** The class every quote block wears, whichever bubble draws it. */
export const BUBBLE_QUOTE_CLASS = "bubble-quote";

/**
 * A feed prompt's quote, drawn from its parts (`frontend.v1.FeedQuoteBlock`):
 * the block wears `QUOTE_PARTS_CLASS`, its heading line and its closing line
 * wear `QUOTE_MARKER_CLASS` (smaller and greyer than the prompt's words), and
 * every part but the heading's words — the heading's colon, the quoted text
 * and the closing line — wears `BUBBLE_QUOTE_CLASS`, so the collapsed bubble
 * shows the heading alone (owner request, 2026-10-19).
 */
export const QUOTE_PARTS_CLASS = "bubble-quote-parts";
export const QUOTE_MARKER_CLASS = "prompt-quote-marker";

/**
 * A quote block: a markdown slot the bubble's body paints, wearing CLASSNAME
 * (the drawing kind's own hook) and `BUBBLE_QUOTE_CLASS`.
 */
export function quoteSlot(className: string, text: string): HTMLElement {
  return markdownSlot(`${className} ${BUBBLE_QUOTE_CLASS}`, text);
}
