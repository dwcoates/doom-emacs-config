/**
 * MARKING WORDS IN A RENDERED ANSWER.
 *
 * The daemon lists, per answer, the words a child question quoted and the
 * words held highlighted (frontend.v1.ChatAnswer). They are words of the
 * answer's TEXT, and the answer is drawn from markdown, so the words can
 * cross element boundaries (`the **blue** sky`). Each is therefore found in
 * the rendered element's text, and every text node it covers is wrapped in a
 * mark of its own, so a mark never breaks the markup it crosses.
 *
 * Each list entry is marked at its FIRST occurrence, as the contract says.
 * Words that do not occur in the rendered text (markdown syntax the person
 * selected around, say) are reported back, so the caller can log them.
 */

/** Where one character of the element's text lives. */
interface TextSpan {
  node: Text;
  start: number;
}

/** The element's text nodes in document order, and their concatenated text. */
function textOf(root: HTMLElement): { text: string; spans: TextSpan[] } {
  const spans: TextSpan[] = [];
  let text = "";
  const walker = root.ownerDocument.createTreeWalker(root, NodeFilter.SHOW_TEXT);
  for (let node = walker.nextNode(); node !== null; node = walker.nextNode()) {
    const t = node as Text;
    spans.push({ node: t, start: text.length });
    text += t.data;
  }
  return { text, spans };
}

/**
 * Wrap the first occurrence of each of WORDS in ROOT's text in a `<mark>` with
 * CLASS_NAME (one mark per text node the occurrence covers). Answers the words
 * it found no occurrence of.
 */
export function markWords(root: HTMLElement, words: readonly string[], className: string): string[] {
  const missing: string[] = [];
  for (const word of words) {
    if (word === "") continue;
    const { text, spans } = textOf(root);
    const at = text.indexOf(word);
    if (at < 0) {
      missing.push(word);
      continue;
    }
    wrapRange(root, spans, at, at + word.length, className);
  }
  return missing;
}

/** Wrap the text in [FROM, TO) of ROOT's text, node by node. */
function wrapRange(root: HTMLElement, spans: TextSpan[], from: number, to: number, className: string): void {
  // Collected first: wrapping splits nodes, which would move the spans after.
  const pieces: { node: Text; start: number; end: number }[] = [];
  for (const span of spans) {
    const end = span.start + span.node.data.length;
    if (end <= from || span.start >= to) continue;
    pieces.push({ node: span.node, start: Math.max(from, span.start) - span.start, end: Math.min(to, end) - span.start });
  }
  for (const piece of pieces) {
    let target = piece.node;
    if (piece.end < target.data.length) target.splitText(piece.end);
    if (piece.start > 0) target = target.splitText(piece.start);
    const mark = root.ownerDocument.createElement("mark");
    mark.className = className;
    target.replaceWith(mark);
    mark.append(target);
  }
}

/**
 * The words around a selection in ROOT: the text of the block (paragraph, list
 * item, cell, heading) that holds its start, so a question about a few words
 * carries what gives them meaning. Bounded, so a selection inside one huge
 * block does not ship the whole answer.
 */
export function contextOf(root: HTMLElement, node: Node, limit = 600): string {
  let at: Node | null = node;
  while (at !== null && at !== root) {
    if (at instanceof HTMLElement && /^(P|LI|TD|TH|H[1-6]|BLOCKQUOTE|PRE)$/.test(at.tagName)) break;
    at = at.parentNode;
  }
  const text = (at ?? root).textContent ?? "";
  return text.length > limit ? text.slice(0, limit) : text;
}
