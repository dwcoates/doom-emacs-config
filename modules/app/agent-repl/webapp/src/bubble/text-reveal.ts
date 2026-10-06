/**
 * text-reveal — show only the first N characters of a rendered subtree.
 *
 * THE TYPE-OUT CUTS THE RENDERED TEXT, NEVER THE MARKDOWN SOURCE. Cutting the
 * source mid-construct renders the half-typed syntax itself: a `**` shows as
 * two asterisks until its closer arrives, an opening fence turns the rest of
 * the bubble into a code block for a few frames. And re-rendering the source
 * every frame costs a full markdown parse per frame, which grows with the
 * response. So the prose is rendered ONCE per push, whole, and the reveal
 * walks that render and truncates its text nodes in document order:
 *
 *   - a text node before the cut shows whole, one across it shows its prefix,
 *     one after it shows nothing;
 *   - an element whose text has not begun (a list item, a code block, a table
 *     row still ahead of the cut) wears `UNREVEALED_ATTRIBUTE`, which the
 *     stylesheet draws as `display: none`, so its box (a bullet, a code
 *     block's frame, a rule) does not appear before its text does. A textless
 *     element (a rule, a line break) appears when the cut reaches it.
 *
 * Counts are in UTF-16 code units of the rendered text, the unit `revealSlice`
 * cuts in, so a surrogate pair is never split.
 *
 * The full text of every truncated node is remembered here, so the reveal can
 * be undone (`clearTextReveal`) before the body painter reconciles the slot
 * against a fresh render: the reconcile then compares full text with full
 * text, keeps every unchanged node, and the reveal is applied again after.
 */
import { revealSlice } from "../smooth.js";

/** The attribute an element wears while none of its text is revealed. */
export const UNREVEALED_ATTRIBUTE = "data-unrevealed";

/** Each truncated text node's full text, keyed by the node. */
const fullTexts = new WeakMap<Text, string>();

/** The full text of NODE: its remembered text when truncated, else its own. */
function fullOf(node: Text): string {
  return fullTexts.get(node) ?? node.data;
}

/**
 * Show the first SHOWN characters of ROOT's rendered text and hide the rest.
 * Answers the full length of ROOT's text, which is what a reveal paces toward.
 */
export function applyTextReveal(root: Element, shown: number): number {
  return walk(root, 0, shown);
}

/** Restore every text node and element under ROOT to its unrevealed whole. */
export function clearTextReveal(root: Element): void {
  const texts = root.ownerDocument.createTreeWalker(root, NodeFilter.SHOW_TEXT);
  for (let node = texts.nextNode() as Text | null; node !== null; node = texts.nextNode() as Text | null) {
    const full = fullTexts.get(node);
    if (full === undefined) continue;
    if (node.data !== full) node.data = full;
    fullTexts.delete(node);
  }
  for (const hidden of Array.from(root.querySelectorAll(`[${UNREVEALED_ATTRIBUTE}]`))) {
    hidden.removeAttribute(UNREVEALED_ATTRIBUTE);
  }
}

/** The full rendered text under ROOT, truncated nodes counted whole. */
export function fullTextOf(root: Node): string {
  let out = "";
  for (const child of Array.from(root.childNodes)) {
    if (child.nodeType === Node.TEXT_NODE) out += fullOf(child as Text);
    else if (child.nodeType === Node.ELEMENT_NODE) out += fullTextOf(child);
  }
  return out;
}

/**
 * Reveal PARENT's children from text position POS on, given SHOWN characters
 * are visible. Answers the position after PARENT's last character.
 */
function walk(parent: Node, pos: number, shown: number): number {
  for (const child of Array.from(parent.childNodes)) {
    if (child.nodeType === Node.TEXT_NODE) {
      const node = child as Text;
      const full = fullOf(node);
      const visible = revealSlice(full, shown - pos);
      if (visible.length < full.length) fullTexts.set(node, full);
      else fullTexts.delete(node);
      if (node.data !== visible) node.data = visible;
      pos += full.length;
    } else if (child.nodeType === Node.ELEMENT_NODE) {
      const start = pos;
      pos = walk(child, pos, shown);
      // An element with text appears with its first character; a textless one
      // (a rule, a break) when the cut reaches where it stands.
      const reached = pos > start ? shown > start : shown >= start;
      (child as Element).toggleAttribute(UNREVEALED_ATTRIBUTE, !reached);
    }
  }
  return pos;
}
