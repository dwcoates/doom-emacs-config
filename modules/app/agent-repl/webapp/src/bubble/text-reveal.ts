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
import { revealSlice } from "../reveal-pace.js";
import { queryAll } from "../dom.js";

/** The attribute an element wears while none of its text is revealed. */
export const UNREVEALED_ATTRIBUTE = "data-unrevealed";

/** Each truncated text node's full text, keyed by the node. */
const fullTexts = new WeakMap<Text, string>();

/** The full text of NODE: its remembered text when truncated, else its own. */
function fullOf(node: Text): string {
  return fullTexts.get(node) ?? node.data;
}

/**
 * NODE's whole text, whether a reveal has truncated it or not: what it reads
 * once its type-out ends.
 */
export function wholeTextOf(node: Text): string {
  return fullOf(node);
}

/**
 * What a reveal shows: the first `shown` characters, and every character from
 * `tailFrom` on. A REVISED response types out only what its revision changed
 * (feed/cards/response.ts): the text before the change and the text after it
 * were already on screen, so both stay shown while the middle types out. An
 * ordinary arriving response has no tail, which `tailFrom` at Infinity says.
 */
export interface RevealWindow {
  readonly shown: number;
  readonly tailFrom: number;
}

/**
 * Show the first SHOWN characters of ROOT's rendered text and every character
 * from TAILFROM on, and hide the rest. Answers the full length of ROOT's text.
 */
export function applyTextReveal(root: Element, shown: number, tailFrom = Number.POSITIVE_INFINITY): number {
  return walk(root, 0, { shown, tailFrom });
}

/**
 * The characters of FULL a reveal shows when FULL starts at text position POS:
 * its head up to the shown count, and its tail from the window's tail on,
 * never splitting a surrogate pair at either cut.
 */
export function visibleIn(full: string, pos: number, window: RevealWindow): string {
  const head = revealSlice(full, window.shown - pos);
  if (window.tailFrom === Number.POSITIVE_INFINITY) return head;
  let cut = Math.max(head.length, Math.ceil(window.tailFrom - pos));
  if (cut >= full.length) return head;
  // A cut before a low surrogate would split its pair; the pair is shown whole.
  const unit = full.charCodeAt(cut);
  if (cut > head.length && unit >= 0xdc00 && unit <= 0xdfff) cut -= 1;
  return head + full.slice(cut);
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
  for (const hidden of Array.from(queryAll(root, `[${UNREVEALED_ATTRIBUTE}]`))) {
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
 * Reveal PARENT's children from text position POS on through WINDOW. Answers
 * the position after PARENT's last character.
 */
function walk(parent: Node, pos: number, window: RevealWindow): number {
  for (const child of Array.from(parent.childNodes)) {
    if (child.nodeType === Node.TEXT_NODE) {
      const node = child as Text;
      const full = fullOf(node);
      const visible = visibleIn(full, pos, window);
      if (visible.length < full.length) fullTexts.set(node, full);
      else fullTexts.delete(node);
      if (node.data !== visible) node.data = visible;
      pos += full.length;
    } else if (child.nodeType === Node.ELEMENT_NODE) {
      const start = pos;
      pos = walk(child, pos, window);
      // An element with text appears with its first character, or stands in
      // the tail; a textless one (a rule, a break) when the cut reaches where
      // it stands, or it stands in the tail.
      const reached = pos > start ? window.shown > start || pos > window.tailFrom : window.shown >= start || start >= window.tailFrom;
      (child as Element).toggleAttribute(UNREVEALED_ATTRIBUTE, !reached);
    }
  }
  return pos;
}

/** Whether UNIT is the high half of a surrogate pair. */
function isHighSurrogate(unit: number): boolean {
  return unit >= 0xd800 && unit <= 0xdbff;
}

/**
 * The window a settled bubble REDRAWN WITH DIFFERENT TEXT types out (a
 * revision): BEFORE is the rendered text it showed, AFTER the text it shows
 * now, RESUMED how much of BEFORE was already shown.
 *
 * The text both share at the start stays shown, as far as the earlier draw had
 * shown it. When the earlier draw had shown all of BEFORE, the text both share
 * at the end stays shown too, and only the changed middle types out. A window
 * whose `shown` reaches its `tailFrom` has nothing left to type.
 */
export function revisedWindow(before: string, after: string, resumed: number): RevealWindow {
  let prefix = 0;
  const most = Math.min(before.length, after.length);
  while (prefix < most && before.charCodeAt(prefix) === after.charCodeAt(prefix)) prefix += 1;
  // A shared high surrogate whose low halves differ is not shared text.
  if (prefix > 0 && prefix < after.length && isHighSurrogate(after.charCodeAt(prefix - 1))) prefix -= 1;
  const shown = Math.min(prefix, Math.floor(resumed));
  if (resumed < before.length) return { shown, tailFrom: Number.POSITIVE_INFINITY };
  let suffix = 0;
  const room = Math.min(before.length, after.length) - prefix;
  while (suffix < room && before.charCodeAt(before.length - 1 - suffix) === after.charCodeAt(after.length - 1 - suffix)) suffix += 1;
  return { shown, tailFrom: Math.max(shown, after.length - suffix) };
}
