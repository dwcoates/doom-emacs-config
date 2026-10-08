/**
 * markdown-output — a tool card's TEXT output drawn as markdown (owner
 * request, 2026-10-08), through the feed's one markdown renderer
 * (`renderMarkdown`, src/markdown.ts) and its safety: raw HTML is never passed
 * through (`html: false`), and links are restricted to web and file targets.
 * This module adds no HTML of its own.
 *
 * WHEN A TEXT OUTPUT IS MARKDOWN (`isMarkdownOutput`) is decided
 * CONSERVATIVELY, so ordinary command output is never turned into markdown by
 * accident:
 *
 *   - A SHELL's output (the card's input line is a `command`) is NEVER
 *     markdown: what a command prints is drawn verbatim. That is the feed's
 *     structured knowledge of the output, and it is asked first (the caller's
 *     `shell`).
 *   - A FAILED call's output is its error text, drawn verbatim and red.
 *   - Otherwise the text is markdown only when it carries a BLOCK construct
 *     prose never carries by chance: an ATX heading ("# Title"), a closed
 *     fenced code block, or a GFM table (a pipe row over a `---` delimiter
 *     row); or at least two list items together with inline markup (`code`,
 *     **bold** or a [link](target)). A bare "-" line, a lone asterisk, a
 *     path with underscores, none of these is enough.
 */
import { log } from "../../log.js";
import { renderMarkdown } from "../../markdown.js";

/** An ATX heading line. */
const HEADING = /^#{1,6} \S/m;
/** A fence opening at the start of a line. */
const FENCE = /^ {0,3}(```|~~~)/gm;
/** A GFM table: a row with pipes directly over a delimiter row. */
const TABLE = /^\s*\|?.*\|.*\n\s*\|?\s*:?-{3,}:?\s*(\|\s*:?-{3,}:?\s*)*\|?\s*$/m;
/** One list item line. */
const LIST_ITEM = /^\s*(?:[-*+]|\d+\.) \S/gm;
/** Inline markup that accompanies a markdown list. */
const INLINE = /`[^`\n]+`|\*\*[^*\n]+\*\*|\[[^\]\n]+\]\([^)\s]+\)/;

/** Whether TEXT, the output of a call that is not a shell's, reads as markdown. */
export function looksLikeMarkdown(text: string): boolean {
  if (HEADING.test(text)) return true;
  if ((text.match(FENCE)?.length ?? 0) >= 2) return true;
  if (TABLE.test(text)) return true;
  return (text.match(LIST_ITEM)?.length ?? 0) >= 2 && INLINE.test(text);
}

/**
 * Whether a TEXT output is drawn as markdown: never a shell's, never a failed
 * call's, and otherwise only when it `looksLikeMarkdown`.
 */
export function isMarkdownOutput(text: string, shell: boolean, failed: boolean): boolean {
  if (shell || failed) return false;
  return looksLikeMarkdown(text);
}

/** Draw TEXT as the card's markdown output section. */
export function drawMarkdownOutput(text: string, path: string): HTMLElement {
  log.debug("drawing a text output as markdown", {
    operation: "feed.cards.markdown-output.draw",
    context: { path, length: text.length },
  });
  const el = document.createElement("div");
  el.className = "tool-output tool-output-md";
  el.innerHTML = renderMarkdown(text);
  return el;
}
