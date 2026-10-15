/**
 * ellipsis-trim — a one-line clamp's `…` never follows a space or a stray mark.
 *
 * OWNER REQUEST, 2026-10-15: a tool card's title and an ellipsis bubble's body
 * end their collapsed line in `…`, and the engine writes it right after the
 * last character that leaves it room, whatever that character is. Since the
 * line breaks anywhere (styles.css "THE ONE-LINE CLAMP'S SHAPE") that is often
 * a space, a period, a comma or a hyphen: "hello …", "hello world.…",
 * "hello world. …". The `…` belongs after the nearest word character to the
 * left of the cut instead: "hello world…".
 *
 * THE ENGINE STILL WRITES THE `…`; this only moves the line's end. CSS cannot
 * say where a clamp truncates, and truncating the text itself would fight the
 * type-out (bubble/text-reveal.ts) and the body painter, which both own the
 * clamp's text nodes. So the text is never touched: the clamp's first line is
 * measured, and when its cut would land after a run of TRIMMABLE characters,
 * the clamp's content box is narrowed (`ELLIPSIS_TRIM_PROPERTY`, drawn by the
 * stylesheet as a transparent right border, so the box and its background
 * stay where they were) until the last word character plus the `…` is all
 * that fits. The engine then drops the trimmable run itself, character by
 * character, to make room for the `…`.
 *
 * The measurer is bubble-more.ts's `refreshHasMore`, which already runs at
 * every moment a clamp's overflow can change, so the trim is cleared and
 * remeasured at exactly those moments, and a clamp that does not overflow (or
 * whose fold is open) wears no trim at all.
 */
import { computedPx } from "../dom.js";
import { log } from "../log.js";

/** The custom property the stylesheet draws as the clamp's narrowing border. */
export const ELLIPSIS_TRIM_PROPERTY = "--ellipsis-trim";

/** The operation an unmeasurable trim is recorded under. */
export const ELLIPSIS_TRIM_UNMEASURABLE = "feed.ellipsis-trim.unmeasurable";

/**
 * How far a character's measured right edge may sit past its true one, in px:
 * WebKit answers a text range's rects snapped OUT to whole pixels, so the true
 * edge lies within this of the measured one, to its left. A character fits
 * beside the `…` when its true edge could, so a cut is never placed a
 * character short of the engine's (which would drop a word character the
 * engine keeps); over-reading a fit only narrows a line the engine already
 * ends after its word character, which moves nothing.
 */
export const RECT_SNAP = 1;

/**
 * The px the narrowed line keeps beyond the last word character's measured
 * edge and the `…`, so a rounding difference between the canvas's `…` and the
 * engine's never drops that character too. With `RECT_SNAP` it stays under
 * the width of any trimmable mark, so the first one after the kept character
 * never fits beside the `…`.
 */
export const ELLIPSIS_TRIM_SLACK = 0.5;

/** True when CH may never stand right before the `…`. */
export function isTrimmable(ch: string): boolean {
  return /^[\s.,-]$/u.test(ch);
}

/** One rendered character of a clamp's first line: its text, and its right edge in px. */
export interface LineChar {
  readonly ch: string;
  readonly right: number;
}

/** What the trim reads off a laid-out clamp; the DOM's own unless a test says otherwise. */
export interface ClampGeometry {
  /** The rendered characters of CLAMP's first line, in order. */
  firstLine(clamp: HTMLElement): LineChar[];
  /** The right edge of CLAMP's content box, in px. */
  contentRight(clamp: HTMLElement): number;
  /** The width of the `…` the engine writes in CLAMP, in px. */
  ellipsisWidth(clamp: HTMLElement): number;
}

/**
 * How far to narrow a clamp's line, in px, so its `…` follows a word
 * character: 0 when the engine's own cut already does (or when the line holds
 * no word character to keep, so there is nothing better to move to).
 *
 * The engine keeps the characters whose right edge leaves room for the `…`
 * (CHARS up to the last such one, read with `RECT_SNAP`), then writes it. The kept line is walked
 * back over its trailing trimmable characters, and the line's end is moved to
 * just past the last word character's `…`.
 */
export function ellipsisTrim(chars: readonly LineChar[], contentRight: number, ellipsisWidth: number): number {
  let cut = -1;
  while (cut + 1 < chars.length && chars[cut + 1].right - RECT_SNAP + ellipsisWidth <= contentRight) cut++;
  let kept = cut;
  while (kept >= 0 && isTrimmable(chars[kept].ch)) kept--;
  if (kept === cut || kept < 0) return 0;
  return Math.max(0, contentRight - (chars[kept].right + ellipsisWidth + ELLIPSIS_TRIM_SLACK));
}

/** Drop CLAMP's trim, so it lays out its line at its full width. */
export function clearEllipsisTrim(clamp: HTMLElement): void {
  clamp.style.removeProperty(ELLIPSIS_TRIM_PROPERTY);
}

/**
 * Trim CLAMP, an untrimmed (`clearEllipsisTrim`) collapsed clamp whose text
 * overflows its line, so its `…` follows a word character.
 */
export function applyEllipsisTrim(clamp: HTMLElement, geometry: ClampGeometry = domClampGeometry): void {
  const chars = geometry.firstLine(clamp);
  if (chars.length === 0) return;
  const trim = ellipsisTrim(chars, geometry.contentRight(clamp), geometry.ellipsisWidth(clamp));
  if (trim > 0) clamp.style.setProperty(ELLIPSIS_TRIM_PROPERTY, `${trim}px`);
}

/** Record that CLAMP cannot be measured, and fail: a laid-out clamp always can be. */
function unmeasurable(clamp: HTMLElement, cause: string, context: Record<string, unknown> = {}): never {
  log.error("a clamp's ellipsis cannot be placed: its line cannot be measured", {
    operation: ELLIPSIS_TRIM_UNMEASURABLE,
    context: { clamp: clamp.className, cause, ...context },
  });
  throw new Error(`ellipsis trim unmeasurable: ${cause}`);
}

/** One reusable 2D context per page, for measuring the `…` in a clamp's font. */
const measuringContexts = new WeakMap<Document, CanvasRenderingContext2D>();

/** The geometry of a clamp the engine has laid out. */
export const domClampGeometry: ClampGeometry = {
  firstLine(clamp) {
    const doc = clamp.ownerDocument;
    const texts = doc.createTreeWalker(clamp, NodeFilter.SHOW_TEXT);
    const range = doc.createRange();
    const chars: LineChar[] = [];
    let lineBottom: number | null = null;
    for (let node = texts.nextNode() as Text | null; node !== null; node = texts.nextNode() as Text | null) {
      for (let i = 0; i < node.data.length; ) {
        const ch = String.fromCodePoint(node.data.codePointAt(i) ?? 0);
        range.setStart(node, i);
        range.setEnd(node, i + ch.length);
        i += ch.length;
        // A character with no inked box (collapsed whitespace, text under a
        // hidden element) is not on any line. WebKit also answers a
        // zero-width rect at the END of the previous line for the character
        // that opens the next, so only a rect with width places a character.
        const rect = Array.from(range.getClientRects())
          .filter((r) => r.width > 0)
          .at(-1);
        if (rect === undefined) continue;
        lineBottom ??= rect.bottom;
        if ((rect.top + rect.bottom) / 2 >= lineBottom) return chars;
        chars.push({ ch, right: rect.right });
      }
    }
    return chars;
  },
  contentRight(clamp) {
    const style = clamp.ownerDocument.defaultView?.getComputedStyle(clamp);
    const border = computedPx(style?.borderRightWidth ?? "");
    const padding = computedPx(style?.paddingRight ?? "");
    if (border === null || padding === null) {
      unmeasurable(clamp, "its right border or padding is not a px length", {
        border: style?.borderRightWidth,
        padding: style?.paddingRight,
      });
    }
    return clamp.getBoundingClientRect().right - border - padding;
  },
  ellipsisWidth(clamp) {
    const doc = clamp.ownerDocument;
    let ctx = measuringContexts.get(doc);
    if (ctx === undefined) {
      const made = doc.createElement("canvas").getContext("2d");
      if (made === null) unmeasurable(clamp, "the page has no 2D canvas to measure the ellipsis");
      ctx = made;
      measuringContexts.set(doc, ctx);
    }
    const style = doc.defaultView?.getComputedStyle(clamp);
    if (style === undefined) unmeasurable(clamp, "the clamp has no computed style");
    ctx.font = `${style.fontStyle} ${style.fontWeight} ${style.fontSize} ${style.fontFamily}`;
    return ctx.measureText("…").width;
  },
};
