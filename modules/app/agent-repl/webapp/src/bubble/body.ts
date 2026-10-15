/**
 * body — THE BUBBLE BODY: the element a bubble's content is painted into, and
 * the machinery that keeps a metaprompt tree in it wrapped to the bubble's cap.
 *
 * ONE BODY PIPELINE FOR EVERY BUBBLE. A prompt's blocks, a response's prose,
 * an agentic card's badges and plan, a peer message, a compaction summary and a
 * held prompt all go through `paintBody`: the content's nodes are placed, and
 * every MARKDOWN SLOT among them (`markdownSlot`) is rendered through
 * `proseHtml`, so a metaprompt tree in ANY bubble is wrapped by the metaprompt
 * engine at that bubble's own cap — the budget `measureTreeCols` resolves from
 * the bubble's CSS `max-width` (one token for every bubble) — and a bubble
 * below its cap never wraps. It paints on attach when it needs a width, and
 * re-wraps only when the containing block's width moves the budget.
 *
 * THE FEED TEXT ZOOM CHANGES ONLY THE TEXT (owner ruling, 2026-10-15). A
 * bubble's breadth is the one it has at 100% zoom, whatever the zoom: under
 * any other zoom the painter HOLDS the bubble at that breadth (`holdBreadth`),
 * and the text, trees included, re-wraps inside it.
 */
import { createConnectHookElement, type ConnectHookElement } from "../connect-hook.js";
import { log } from "../log.js";
import { inline, hasFencedTree, renderMarkdown, type TreeCols } from "../markdown.js";
import { findTreeRegion, renderTreeHtml, type TreeIssue } from "../metaprompt-tree.js";
import { computedPx, pageResizeObserver, placeChildren, scrollbarWidthPx } from "../dom.js";
import { FEED_TEXT_SCALE_DEFAULT, FEED_TEXT_SCALE_PROPERTY, feedTextScale } from "../feed/feed-text-scale.js";
import { onDiscard, stopTicking } from "../feed/ticking.js";
import { applyTextReveal, clearTextReveal, fullTextOf } from "./text-reveal.js";

/** The webapp surfaces a wrap issue through the client logger. */
const logTreeIssue: TreeIssue = (message, context) => {
  log.debug(message, { operation: "bubble.body.tree", context });
};

/**
 * Markdown → HTML, with the metaprompt TLDR tree WRAPPED to the measured COLS.
 *
 * The tree arrives unwrapped, one physical line per branch; the wrapper
 * (metaprompt-tree.ts, a port of the daemon's treefmt) breaks every branch too
 * wide for COLS onto continuation lines under its own text column, so the tree
 * fills the bubble and re-flows when the bubble's width changes. Stray prose
 * around the tree keeps the markdown path — which itself wraps a fenced tree to
 * the same COLS — so a model that wrapped the tree in a sentence still reads
 * correctly. COLS is asked for only when a tree is actually drawn.
 */
export function proseHtml(markdown: string, cols: TreeCols): string {
  const region = findTreeRegion(markdown);
  if (region === null) return renderMarkdown(markdown, cols);
  const before = region.before.trim() === "" ? "" : renderMarkdown(region.before, cols);
  const after = region.after.trim() === "" ? "" : renderMarkdown(region.after, cols);
  return `${before}<div class="mp-tree">${renderTreeHtml(region.tree, inline, cols(), logTreeIssue)}</div>${after}`;
}

/**
 * Whether drawing MARKDOWN needs a measured column budget: it holds a tree,
 * bare or fenced. Prose that holds none draws the same at every width, so it
 * can be drawn before the body has a containing block.
 */
export function proseNeedsWidth(markdown: string): boolean {
  return findTreeRegion(markdown) !== null || hasFencedTree(markdown);
}

/** The operation every unmeasurable-width violation is recorded under. */
export const TREE_WIDTH_UNMEASURABLE = "bubble.body.tree-width-unmeasurable";

/**
 * A tree's column budget could not be measured: record it once, with what was
 * read, and fail. There is no default width to fall back to — a guessed width is
 * how trees came to wrap at 105 columns inside bubbles with room for 140.
 */
function unmeasurable(reason: string, context: Record<string, unknown>): never {
  log.error(`a metaprompt tree's column budget could not be measured: ${reason}`, {
    operation: TREE_WIDTH_UNMEASURABLE,
    context: { reason, ...context },
  });
  throw new Error(`metaprompt tree width unmeasurable: ${reason}`);
}

/** A computed length, which a laid-out element always reports in px. */
function computedLength(el: Element, style: CSSStyleDeclaration, property: ComputedLength): number {
  const value = style[property];
  const px = computedPx(value);
  if (px === null) {
    unmeasurable("a computed length is not in px", { element: el.className, property, value });
  }
  return px;
}

/** The computed lengths the budget reads. */
type ComputedLength =
  | "paddingLeft"
  | "paddingRight"
  | "borderLeftWidth"
  | "borderRightWidth"
  | "marginLeft"
  | "marginRight";

/** One element's horizontal padding (plus border, when BORDER), from computed styles. */
function insetXPx(el: Element, view: Window, parts: { border: boolean; margin: boolean }): number {
  const s = view.getComputedStyle(el);
  let px = computedLength(el, s, "paddingLeft") + computedLength(el, s, "paddingRight");
  if (parts.border) px += computedLength(el, s, "borderLeftWidth") + computedLength(el, s, "borderRightWidth");
  if (parts.margin) px += computedLength(el, s, "marginLeft") + computedLength(el, s, "marginRight");
  return px;
}

/**
 * The column budget a tree in BODY wraps to: the body's content width WHEN THE
 * BUBBLE IS AT ITS CAP, in columns of the tree's monospace font.
 *
 * We never scroll horizontally; lines wider than the max bubble width wrap instead.
 *
 * The budget derives from the CAP — the bubble's `max-width` resolved against
 * its containing block, or the breadth a zoom holds the bubble at
 * (`holdBreadth`) where that is narrower — less the bubble's fixed chrome
 * summed from computed styles and the EXPANDED scroll box's scrollbar gutter,
 * and never from the bubble's current fit-content width or fold, so a tree
 * below the cap never wraps, a shrink cannot feed back into a re-wrap, and an
 * expand (whose scrollbar appears past 50vh) never re-wraps it.
 *
 * The column is the tree font's at AT: the zoom the feed is drawn at, or 100%
 * for the read of a bubble's 100% breadth (`fitAll`). It is read off the page's
 * standing probe for that zoom (`treeFontProbe`), so a measure only READS the
 * layout and a whole batch of bubbles is measured in one layout.
 *
 * It measures only a body that is IN THE DOCUMENT, and anything it cannot read
 * is an invariant violation (`unmeasurable`), never a default.
 *
 * REGRESSION WATCH (breadcrumb, per AGENTS.md): this used to run while the
 * bubble was still DETACHED (drawFeedResponse paints before feed-view attaches
 * the row) and fall back to 105 columns when it read nothing. A real engine
 * neither lays out nor styles a detached node — the character probe measured
 * 0px and `getComputedStyle` answered empty strings — so EVERY first paint
 * wrapped at the fallback, whatever the bubble's room. jsdom styles detached
 * nodes and the unit suite stubbed geometry regardless of attachment, which is
 * why the tests never saw it. The first paint of a tree now waits for the body
 * to be laid out (`armPainter`). This is a watch flag, not a lock.
 */
export function measureTreeCols(body: HTMLElement, at: TreeFontZoom = "zoomed"): number {
  if (!body.isConnected) unmeasurable("the bubble body is not in the document", {});
  const view = body.ownerDocument.defaultView;
  if (view === null) unmeasurable("the bubble body's document has no window", {});
  const bubble = requireBubble(body);
  const containing = containingBlockOf(bubble);

  const charPx = treeFontProbe(body.ownerDocument, at).getBoundingClientRect().width / TREE_PROBE_COLUMNS;
  if (!(charPx > 0)) unmeasurable("the tree font's column measured no width", { char_px: charPx });

  const containingPx =
    containing.clientWidth - insetXPx(containing, view, { border: false, margin: false });
  if (!(containingPx > 0)) {
    unmeasurable("the bubble's containing block has no width", { containing_px: containingPx });
  }
  const maxWidth = view.getComputedStyle(bubble).maxWidth;
  const maxPx = resolveCapPx(maxWidth, containingPx);
  if (maxPx === null) unmeasurable("the bubble's max-width does not resolve", { max_width: maxWidth });
  const heldPx = heldBreadthPx(bubble);
  const capPx = heldPx === null ? maxPx : Math.min(maxPx, heldPx);

  // The fixed chrome from the bubble's border box in to the body's content box:
  // every border and padding on the way, plus the margins of the boxes inside
  // the bubble. None of it depends on content.
  let chromePx = insetXPx(bubble, view, { border: true, margin: false });
  for (let el: HTMLElement | null = body; el !== null && el !== bubble; el = el.parentElement) {
    chromePx += insetXPx(el, view, { border: true, margin: true });
  }
  // WRAP AS IF EXPANDED (owner ruling, 2026-09-23): the gutter an expanded
  // bubble's scrollbar takes inside the scroll box comes off ALWAYS, so a tree
  // is wrapped the same collapsed and expanded and never re-wraps on a click.
  const gutterPx = scrollbarGutterPx(body, view);
  chromePx += gutterPx;

  const contentPx = capPx - chromePx;
  const cols = Math.floor(contentPx / charPx);
  const context = {
    cols,
    char_px: charPx,
    cap_px: capPx,
    held_px: heldPx,
    chrome_px: chromePx,
    gutter_px: gutterPx,
    containing_px: containingPx,
  };
  if (cols < 1) unmeasurable("the bubble's content width at its cap holds no column", context);
  log.debug("measured a metaprompt tree's column budget", {
    operation: "bubble.body.tree-cols",
    context,
  });
  return cols;
}

/** How many columns of the tree font a probe holds: its width over this is one column. */
const TREE_PROBE_COLUMNS = 100;

/** The zoom a tree-font probe is drawn at: the feed's, or 100%. */
export type TreeFontZoom = "zoomed" | "100%";

/** The class every standing tree-font probe wears. */
export const TREE_FONT_PROBE_CLASS = "mp-tree-font-probe";

/** Each page's standing tree-font probes, by the zoom each is drawn at. */
const treeFontProbes = new WeakMap<Document, Map<TreeFontZoom, HTMLElement>>();

/**
 * The page's STANDING TREE-FONT PROBE at AT: a hidden, unwrapped run of
 * TREE_PROBE_COLUMNS columns in the tree font (`.mp-tree`), whose width is the
 * tree font's column width times the run. Laid out off-screen and invisible,
 * so it never paints, scrolls or takes a click. The "100%" probe sets its own
 * `--feed-text-scale` to 1, so it reads the column a bubble's 100% breadth is
 * read at, whatever the feed's zoom.
 *
 * The "zoomed" probe is also what every painter's observer watches beside its
 * containing block (`observeBudgetInputs`). A tree's column budget has two
 * inputs: the column the cap is a percentage of, and the tree font's column
 * width. The feed text zoom moves only the second, so the probe, a box whose
 * width IS that column width, is how the same ResizeObserver that sees the
 * column move sees the zoom move, whatever moved it.
 *
 * A probe is kept in the document's body; one that has left the document (a
 * page that replaced its body) is replaced by a fresh one, so a new watch never
 * observes a box that can no longer change.
 */
export function treeFontProbe(doc: Document, at: TreeFontZoom = "zoomed"): HTMLElement {
  let probes = treeFontProbes.get(doc);
  if (probes === undefined) {
    probes = new Map();
    treeFontProbes.set(doc, probes);
  }
  const standing = probes.get(at);
  if (standing?.isConnected === true) return standing;
  const probe = doc.createElement("div");
  probe.className = `mp-tree ${TREE_FONT_PROBE_CLASS}`;
  probe.setAttribute("aria-hidden", "true");
  probe.style.cssText = "position:absolute;visibility:hidden;white-space:pre;left:-9999px;top:0;pointer-events:none;";
  if (at === "100%") probe.style.setProperty(FEED_TEXT_SCALE_PROPERTY, String(FEED_TEXT_SCALE_DEFAULT));
  probe.textContent = "0".repeat(TREE_PROBE_COLUMNS);
  doc.body.append(probe);
  probes.set(at, probe);
  return probe;
}

/**
 * The width the bubble scroll box's scrollbar gutter takes from its content,
 * in px, MEASURED off the `.bubble-box` ancestor of BODY:
 * `offsetWidth - clientWidth - borderLeft - borderRight`. The box declares
 * `scrollbar-gutter: stable` and styles no bar of its own (owner ruling,
 * 2026-09-24), so this is the SYSTEM bar's width -- 0px overlay, about 14px
 * classic -- reserved identically collapsed and expanded. A body with no scroll
 * box, or a measurement that is negative or not finite, is unmeasurable, never
 * a clamped or guessed width.
 */
function scrollbarGutterPx(body: HTMLElement, view: Window): number {
  const scroll = body.closest<HTMLElement>(".bubble-box");
  if (scroll === null) unmeasurable("the bubble body has no scroll box", {});
  const s = view.getComputedStyle(scroll);
  const borderPx = computedLength(scroll, s, "borderLeftWidth") + computedLength(scroll, s, "borderRightWidth");
  const offsetPx = scroll.offsetWidth;
  const clientPx = scroll.clientWidth;
  const gutterPx = scrollbarWidthPx(scroll, borderPx);
  if (!Number.isFinite(gutterPx) || gutterPx < 0) {
    unmeasurable("the scroll box's scrollbar gutter measured negative or not finite", {
      offset_px: offsetPx,
      client_px: clientPx,
      border_px: borderPx,
      gutter_px: gutterPx,
    });
  }
  return gutterPx;
}

/** The block the bubble's percentage cap resolves against: its parent. */
function containingBlockOf(bubble: HTMLElement): HTMLElement {
  const containing = bubble.parentElement;
  if (containing === null) unmeasurable("the bubble has no containing block", {});
  return containing;
}

/**
 * The bubble's `max-width`, resolved to px against CONTAININGPX. Per CSSOM the
 * resolved value of `max-width` is the COMPUTED value (a percentage stays a
 * percentage, a calc of one percentage serializes as `calc(N%)`), so a
 * percentage is resolved here; a browser that hands back px is honored as is.
 * Anything else answers `null`.
 */
function resolveCapPx(maxWidth: string, containingPx: number): number | null {
  const px = computedPx(maxWidth);
  if (px !== null) return px;
  const pct = /^(?:calc\()?\s*(-?[\d.]+)%\s*\)?$/.exec(maxWidth);
  if (pct !== null) return (Number.parseFloat(pct[1]) / 100) * containingPx;
  return null;
}

/** The attribute a bubble wears while a zoom holds its breadth (`holdBreadth`). */
export const HELD_BREADTH_ATTRIBUTE = "data-held-breadth";

/** The property carrying the held breadth, in px, which the stylesheet sizes the bubble to. */
export const HELD_BREADTH_PROPERTY = "--bubble-held-breadth";

/**
 * The attribute a bubble wears for the instant its 100%-zoom breadth is read,
 * so the stylesheet keeps any hover-only widening (an interim's usage reveal)
 * out of the breadth.
 */
export const BREADTH_MEASURING_ATTRIBUTE = "data-breadth-measuring";

/** The breadth a zoom holds BUBBLE at, in px, or null when none holds it. */
function heldBreadthPx(bubble: HTMLElement): number | null {
  if (!bubble.hasAttribute(HELD_BREADTH_ATTRIBUTE)) return null;
  const value = bubble.style.getPropertyValue(HELD_BREADTH_PROPERTY);
  const px = computedPx(value);
  if (px === null) unmeasurable("a held bubble's breadth is not in px", { held_breadth: value });
  return px;
}

/** The tag of every bubble's body: a custom element, so it knows when it joins the document. */
export const BUBBLE_BODY_TAG = "bubble-body";

/** The class the stylesheet's body rules key on. */
export const BUBBLE_BODY_CLASS = "bubble-body";

/** The attribute every markdown slot wears; its source is held beside it. */
export const MARKDOWN_SLOT_ATTRIBUTE = "data-markdown";

/**
 * A bubble's body, which runs its hooks every time it joins the document
 * (connect-hook.ts). `connectedCallback` runs synchronously inside the DOM
 * operation that attaches the row, which is what lets a tree's first paint
 * wait for a containing block without ever showing a wrong-width frame.
 */
export type BubbleBody = ConnectHookElement;

/** What paints one body: all of its slots, or one of them, at the cap's budget. */
interface Painter {
  paint(only?: HTMLElement): void;
}

/** Every body's painter, armed once when the body is created. */
const painters = new WeakMap<Element, Painter>();

/** Every markdown slot's source, held beside the element it paints. */
const slotSources = new WeakMap<Element, string>();

/**
 * What a slot's kind adds to its rendered markdown before it is placed: a
 * prompt's known slash commands marked, say. It runs on the DETACHED render of
 * every paint, so a repaint reconciles against marked text and never drops a
 * mark it would otherwise have to add back.
 */
export type SlotMark = (rendered: HTMLElement) => void;

/** Every marked slot's mark, held beside the element it paints. */
const slotMarks = new WeakMap<Element, SlotMark>();

/** A fresh bubble body, the custom element defined on first use, its painter armed. */
export function createBubbleBody(): BubbleBody {
  const body = createConnectHookElement(BUBBLE_BODY_TAG);
  body.className = BUBBLE_BODY_CLASS;
  painters.set(body, armPainter(body));
  return body;
}

/**
 * A MARKDOWN SLOT: the one way a bubble's content says "this is prose". Every
 * kind's markdown — a response's prose, a prompt's text block, a plan, a peer
 * message, a compaction summary — is a slot, and only the body paints one, so
 * every tree in every bubble is wrapped by the metaprompt engine at the
 * bubble's own cap. A slot is an ordinary element (CLASSNAME is the kind's), so
 * a kind can nest it in whatever structure it draws around its prose.
 */
export function markdownSlot(className: string, markdown: string, mark?: SlotMark): HTMLElement {
  const slot = document.createElement("div");
  slot.className = className;
  slot.setAttribute(MARKDOWN_SLOT_ATTRIBUTE, "");
  slotSources.set(slot, markdown);
  if (mark !== undefined) slotMarks.set(slot, mark);
  return slot;
}

/**
 * Paint BODY with CONTENT: its nodes, every markdown slot among them drawn.
 * Answers the nodes the body now holds, position for position with CONTENT.
 *
 * A REPAINT IS IN PLACE (owner rule, 2026-09-23: the user owns the scroll). On
 * a body that already holds content, a top-level markdown slot of the same
 * class keeps its element and takes the new source, which the painter then
 * RECONCILES node by node, so prose that did not change keeps its nodes. Every
 * other node is the new content's own — a kind's badges and links carry
 * listeners bound to the push that drew them, so a stale one is never kept —
 * and nothing already in place is moved (`placeChildren`). Every paint takes
 * the body's next generation (`paintGeneration`).
 */
export function paintBody(body: BubbleBody, content: readonly ChildNode[]): readonly ChildNode[] {
  const painter = painterOf(body);
  const live = [...body.childNodes];
  const next = content.map((want, i) => {
    const have = live[i];
    if (isSlot(have) && isSlot(want) && have.className === want.className) {
      slotSources.set(have, sourceOf(want));
      const mark = slotMarks.get(want);
      if (mark === undefined) slotMarks.delete(have);
      else slotMarks.set(have, mark);
      return have;
    }
    return want;
  });
  const kept = new Set<Node>(next);
  for (const have of live) {
    if (!kept.has(have) && have instanceof Element) stopTicking(have);
  }
  placeChildren(body, next);
  generations.set(body, paintGeneration(body) + 1);
  painter.paint();
  return next;
}

/** Every body's paint generation, taken by each `paintBody`. */
const generations = new WeakMap<Element, number>();

/**
 * BODY's current paint generation. A loop that repaints a body over time (the
 * response's type-out) reads it when it starts and stops the moment it moved:
 * a later `paintBody` — an in-place update — owns the body from then on.
 */
export function paintGeneration(body: BubbleBody): number {
  return generations.get(body) ?? 0;
}

/** Whether NODE is a markdown slot this pipeline made. */
function isSlot(node: ChildNode | undefined): node is HTMLElement {
  return node instanceof HTMLElement && slotSources.has(node);
}

/** Whether EL is a bubble body this pipeline made (and so can repaint). */
export function isBubbleBody(el: Element | null | undefined): el is BubbleBody {
  return el !== null && el !== undefined && painters.has(el);
}

/** Each revealing slot's shown length in rendered characters; absent = whole. */
const slotReveals = new WeakMap<HTMLElement, number>();

/** The slots the painter has drawn at least once. */
const paintedSlots = new WeakSet<HTMLElement>();

/** Callbacks waiting for a slot's first paint. */
const paintWaiters = new WeakMap<HTMLElement, (() => void)[]>();

/**
 * Show only the first SHOWN rendered characters of SLOT, already in BODY, or
 * the whole of it for undefined: the arriving response's type-out reveals
 * through here every frame (src/bubble/text-reveal.ts). The slot is rendered
 * once per push and only its text is cut, so no frame re-parses markdown and
 * no half-typed syntax is ever drawn. The painter re-applies the reveal after
 * every paint (a push, a re-wrap), so a repaint never flashes the whole text.
 */
export function revealSlot(body: BubbleBody, slot: HTMLElement, shown: number | undefined): void {
  if (!slotSources.has(slot) || !body.contains(slot)) {
    invariant("a reveal named an element that is not a markdown slot of this body", {
      slot: slot.className,
    });
  }
  if (shown === undefined) {
    slotReveals.delete(slot);
    clearTextReveal(slot);
    return;
  }
  slotReveals.set(slot, shown);
  if (paintedSlots.has(slot)) applyTextReveal(slot, shown);
}

/**
 * SLOT's whole rendered text, or undefined while the painter has not drawn it
 * yet (prose holding a tree waits for a layout, see `armPainter`).
 */
export function slotText(slot: HTMLElement): string | undefined {
  return paintedSlots.has(slot) ? fullTextOf(slot) : undefined;
}

/** Run FN once, right after SLOT's first paint. */
export function whenSlotPainted(slot: HTMLElement, fn: () => void): void {
  if (paintedSlots.has(slot)) {
    fn();
    return;
  }
  paintWaiters.set(slot, [...(paintWaiters.get(slot) ?? []), fn]);
}

/** The source SLOT paints, which every slot has from its making. */
function sourceOf(slot: Element): string {
  const source = slotSources.get(slot);
  if (source === undefined) {
    invariant("a markdown slot carries no source", { slot: slot.className });
  }
  return source;
}

/** BODY's painter, which every body made by `createBubbleBody` has. */
function painterOf(body: BubbleBody): Painter {
  const painter = painters.get(body);
  if (painter === undefined) invariant("a bubble body was painted that has no painter", {});
  return painter;
}

/** The operation a body pipeline invariant violation is recorded under. */
export const BODY_INVARIANT = "bubble.body.invariant";

/** A body-pipeline invariant broke: record it once, and fail. */
function invariant(reason: string, context: Record<string, unknown>): never {
  log.error(`the bubble body pipeline broke an invariant: ${reason}`, {
    operation: BODY_INVARIANT,
    context: { reason, ...context },
  });
  throw new Error(`bubble body invariant: ${reason}`);
}

/**
 * Keep BODY's slots painted, every tree in them wrapped to the cap, and the
 * bubble at its 100%-zoom breadth, for the body's whole life.
 *
 * A PAINT THAT NEEDS A WIDTH WAITS FOR A LAYOUT. Prose with no tree draws the
 * same at every width, so it paints at once, detached or not. Prose holding a
 * tree waits until the bubble is LAID OUT — in the document, and not inside
 * anything `display: none` (a folded compaction summary, a collapsed sub-feed):
 * `getClientRects()` is empty exactly when an element has no box — and then
 * paints at the budget measured from the cap. Waiting is not a fallback: no
 * width is guessed, and a laid-out bubble whose width cannot be read is still
 * the `unmeasurable` violation.
 *
 * Every attach (a tool-group re-arrange moves the row) asks for a fit, and a
 * `ResizeObserver` on the budget's two inputs asks for one when either moves:
 * the CONTAINING BLOCK — the box the cap is a percentage of, not the
 * fit-content body, which keeps its width when the column grows around a
 * bubble below its cap — and the page's TREE-FONT PROBE (`treeFontProbe`),
 * whose width moves with the feed text zoom. The same observer is what sees a
 * hidden bubble come back. It is torn down with the body through
 * `stopTicking` (see ticking.ts).
 *
 * A fit (`fitAll`) re-wraps a tree at 100% zoom only when the column moved its
 * budget, and under any other zoom HOLDS THE BREADTH. While a breadth is held a
 * `MutationObserver` on the bubble asks for a fit on every change to what the
 * bubble draws (a repaint, a typed-out frame, an expand, a corner arriving),
 * so the held breadth is always the breadth the content has at 100%.
 */
function armPainter(body: BubbleBody): Painter {
  let cols: number | null = null;
  let deferred = false;
  let observer: ResizeObserver | null = null;
  let content: MutationObserver | null = null;
  let lastInputs: string | null = null;

  const budget: TreeCols = () => {
    cols ??= measureTreeCols(body);
    return cols;
  };
  const slots = (): HTMLElement[] => [
    ...body.querySelectorAll<HTMLElement>(`[${MARKDOWN_SLOT_ATTRIBUTE}]`),
  ];
  const draw = (targets: readonly HTMLElement[]): void => {
    for (const slot of targets) {
      const target = document.createElement("div");
      target.innerHTML = proseHtml(sourceOf(slot), budget);
      slotMarks.get(slot)?.(target);
      // The reconcile compares whole text with whole text, so a revealing slot
      // is restored first and cut again after (`revealSlot`).
      clearTextReveal(slot);
      reconcileChildren(slot, target, null);
      paintedSlots.add(slot);
      const shown = slotReveals.get(slot);
      if (shown !== undefined) applyTextReveal(slot, shown);
      const waiters = paintWaiters.get(slot);
      paintWaiters.delete(slot);
      for (const fn of waiters ?? []) fn();
    }
  };
  const drawsTree = (): boolean => body.querySelector(".mp-tree") !== null;
  const needsWidth = (): boolean => slots().some((slot) => proseNeedsWidth(sourceOf(slot)));
  const laidOut = (): boolean =>
    body.isConnected && requireBubble(body).getClientRects().length > 0;

  const fitter: Fitter = {
    body,
    idle: () => !deferred && content === null && !drawsTree(),
    laidOut,
    deferred: () => deferred,
    paint: () => paint(),
    trees: needsWidth,
    drawAt(next: number): void {
      cols = next;
      draw(slots());
    },
    rewrapAt(next: number): void {
      if (next === cols) return;
      cols = next;
      draw(slots());
    },
    follow(): void {
      content ??= watchContent(requireBubble(body));
      // The hold's own redraws are not content changes: drop them, or every
      // hold would ask for the next.
      content.takeRecords();
    },
    unfollow(): void {
      content?.disconnect();
      content = null;
    },
  };

  const watch = (): void => {
    if (observer === null) {
      onDiscard(body, () => {
        observer?.disconnect();
        content?.disconnect();
      });
    } else {
      observer.disconnect();
    }
    observer = observeBudgetInputs(body, () => {
      // A delivery for a height alone (a row growing under its text, a hold
      // re-flowing it) moves nothing a fit reads: ask only when an input moved.
      const inputs = budgetInputsOf(body);
      if (inputs === lastInputs) return;
      lastInputs = inputs;
      requestFit(fitter, "frame");
    });
  };

  function paint(only?: HTMLElement): void {
    if (needsWidth() && !laidOut()) {
      deferred = true;
      log.debug("a bubble's tree waits for the bubble to be laid out", {
        operation: "bubble.body.paint-deferred",
        context: { connected: body.isConnected },
      });
      if (body.isConnected) watch();
      return;
    }
    const whole = only === undefined || deferred;
    deferred = false;
    draw(whole ? slots() : [only]);
    if (body.isConnected && observer === null) watch();
  }

  /**
   * Ask for a fit on every change to what BUBBLE draws, while a zoom holds its
   * breadth. The bubble's own inline style is not what it draws: the hold
   * writes it, and the prompt wave paints its fill's position there.
   */
  function watchContent(bubble: HTMLElement): MutationObserver {
    const view = body.ownerDocument.defaultView;
    if (view === null || typeof view.MutationObserver !== "function") {
      unmeasurable("the page has no MutationObserver to follow a held bubble's content", {});
    }
    const watcher = new view.MutationObserver((records) => {
      if (records.some((record) => drawsChange(record, bubble))) requestFit(fitter, "now");
    });
    watcher.observe(bubble, { subtree: true, childList: true, characterData: true, attributes: true });
    return watcher;
  }

  body.onConnect(() => {
    // A tree's FIRST paint is synchronous with the attach, so no frame ever
    // shows its slot empty; every other fit waits for the batch.
    if (deferred && laidOut()) paint();
    requestFit(fitter, "now");
    watch();
  });

  return { paint };
}

/** One painter's part in a fit, which `fitAll` runs phase by phase across a batch. */
interface Fitter {
  readonly body: BubbleBody;
  /** Nothing a fit could do at 100%: no tree, nothing held, no paint waiting. */
  idle(): boolean;
  /** Whether the bubble has a box to fit. */
  laidOut(): boolean;
  /** Whether a tree's first paint still waits for a layout. */
  deferred(): boolean;
  paint(): void;
  /** Whether a slot holds a tree, drawn or waiting. */
  trees(): boolean;
  /** Draw every slot with its trees wrapped to COLS. */
  drawAt(cols: number): void;
  /** Draw every slot again only when COLS moved the budget. */
  rewrapAt(cols: number): void;
  /** Follow the bubble's content while its breadth is held. */
  follow(): void;
  unfollow(): void;
}

/** The painters waiting for the next fit. */
const pendingFits = new Set<Fitter>();

/**
 * Ask for FITTER to be fitted, in the next batch: NOW (a microtask, still
 * ahead of the next paint, for an attach or a content change) or at the next
 * FRAME (for a resize, which a frame coalesces and keeps out of the observer's
 * own delivery). The first batch to run fits every painter waiting, so every
 * resize of one delivery is fitted together, and a later batch finds nothing
 * left. No "already scheduled" state is kept: a frame that never comes (a
 * torn-down page) cannot leave a later request unscheduled.
 */
function requestFit(fitter: Fitter, when: "now" | "frame"): void {
  pendingFits.add(fitter);
  const view = fitter.body.ownerDocument.defaultView;
  const raf = view?.requestAnimationFrame?.bind(view);
  if (when === "frame" && raf !== undefined) raf(flushFits);
  else queueMicrotask(flushFits);
}

/** Fit every painter waiting, a document at a time. */
function flushFits(): void {
  const fitters = [...pendingFits].filter((f) => f.body.isConnected);
  pendingFits.clear();
  const byDocument = new Map<Document, Fitter[]>();
  for (const f of fitters) byDocument.set(f.body.ownerDocument, [...(byDocument.get(f.body.ownerDocument) ?? []), f]);
  for (const [doc, batch] of byDocument) fitAll(doc, batch);
}

/**
 * FIT A BATCH of bubbles to the zoom and the column, in phases that each only
 * write or only read, so the whole batch costs a few layouts rather than a few
 * per bubble.
 *
 * At 100% every held breadth is let go, and each tree re-wraps only when the
 * column moved its budget.
 *
 * Under any other zoom every bubble is HELD AT ITS 100% BREADTH: drawn at 100%
 * for one read — its own `--feed-text-scale` set to 1 and its trees wrapped to
 * the 100% budget — its border-box width held as its width, and then the zoom
 * comes back and every tree re-wraps to the columns the held breadth holds at
 * the zoomed size. It all runs ahead of the next paint, so the reader only ever
 * sees the zoomed text inside the 100% breadth.
 */
function fitAll(doc: Document, batch: readonly Fitter[]): void {
  const zoomed = feedTextScale(doc) !== FEED_TEXT_SCALE_DEFAULT;
  const ready = batch.filter((f) => !(f.idle() && !zoomed) && f.laidOut());
  for (const f of ready) if (f.deferred()) f.paint();
  if (ready.length === 0) return;
  if (!zoomed) {
    let released = 0;
    for (const f of ready) {
      f.unfollow();
      if (releaseBreadth(requireBubble(f.body))) released++;
    }
    const trees = ready.filter((f) => f.trees());
    const cols = trees.map((f) => measureTreeCols(f.body));
    trees.forEach((f, i) => f.rewrapAt(cols[i]));
    if (released > 0) {
      log.debug("released bubbles' held breadths at 100% zoom", {
        operation: "bubble.body.breadth-released",
        context: { bubbles: released },
      });
    }
    return;
  }
  const bubbles = ready.map((f) => requireBubble(f.body));
  for (const bubble of bubbles) beginBreadthRead(bubble);
  const cols100 = ready.map((f) => (f.trees() ? measureTreeCols(f.body, "100%") : null));
  ready.forEach((f, i) => {
    const at = cols100[i];
    if (at !== null) f.drawAt(at);
  });
  const breadths = bubbles.map((bubble) => bubble.getBoundingClientRect().width);
  for (const bubble of bubbles) endBreadthRead(bubble);
  const unread = breadths.findIndex((px) => !(px > 0));
  if (unread >= 0) {
    unmeasurable("the bubble's breadth at 100% zoom measured no width", { breadth_px: breadths[unread] });
  }
  bubbles.forEach((bubble, i) => holdBreadth(bubble, breadths[i]));
  const cols = ready.map((f) => (f.trees() ? measureTreeCols(f.body) : null));
  ready.forEach((f, i) => {
    const at = cols[i];
    if (at !== null) f.drawAt(at);
  });
  for (const f of ready) f.follow();
  log.debug("held bubbles at their 100% zoom breadth", {
    operation: "bubble.body.breadth-held",
    context: { bubbles: bubbles.length, scale: feedTextScale(doc), breadths_px: breadths },
  });
}

/** Draw BUBBLE at 100%, its breadth let go and any hover-only widening kept out, for the read. */
function beginBreadthRead(bubble: HTMLElement): void {
  bubble.removeAttribute(HELD_BREADTH_ATTRIBUTE);
  bubble.style.removeProperty(HELD_BREADTH_PROPERTY);
  bubble.setAttribute(BREADTH_MEASURING_ATTRIBUTE, "");
  bubble.style.setProperty(FEED_TEXT_SCALE_PROPERTY, String(FEED_TEXT_SCALE_DEFAULT));
}

/** Draw BUBBLE at the feed's zoom again, after the read. */
function endBreadthRead(bubble: HTMLElement): void {
  bubble.style.removeProperty(FEED_TEXT_SCALE_PROPERTY);
  bubble.removeAttribute(BREADTH_MEASURING_ATTRIBUTE);
}

/** Hold BUBBLE at BREADTHPX, which the stylesheet sizes it to. */
function holdBreadth(bubble: HTMLElement, breadthPx: number): void {
  bubble.style.setProperty(HELD_BREADTH_PROPERTY, `${String(breadthPx)}px`);
  bubble.setAttribute(HELD_BREADTH_ATTRIBUTE, "");
}

/** Let BUBBLE fit its content again; whether a breadth was held. */
function releaseBreadth(bubble: HTMLElement): boolean {
  if (!bubble.hasAttribute(HELD_BREADTH_ATTRIBUTE)) return false;
  bubble.removeAttribute(HELD_BREADTH_ATTRIBUTE);
  bubble.style.removeProperty(HELD_BREADTH_PROPERTY);
  return true;
}

/** The bubble attributes a breadth hold itself writes, which are no change to what it draws. */
const HOLD_WRITES: ReadonlySet<string> = new Set(["style", HELD_BREADTH_ATTRIBUTE, BREADTH_MEASURING_ATTRIBUTE]);

/** Whether RECORD changes what BUBBLE draws: anything but an attribute a hold or the wave writes on the bubble itself. */
export function drawsChange(record: MutationRecord, bubble: HTMLElement): boolean {
  return !(record.type === "attributes" && record.target === bubble && HOLD_WRITES.has(record.attributeName ?? ""));
}

/**
 * What a fit of BODY reads, as one comparable key: whether its bubble is laid
 * out, its containing block's width, and the tree font's column width at the
 * feed's zoom. Read inside a ResizeObserver delivery, when layout is clean.
 */
function budgetInputsOf(body: BubbleBody): string {
  const bubble = requireBubble(body);
  const shown = bubble.getClientRects().length > 0;
  const columnPx = containingBlockOf(bubble).clientWidth;
  const fontPx = treeFontProbe(body.ownerDocument).getBoundingClientRect().width;
  return `${String(shown)}:${String(columnPx)}:${String(fontPx)}`;
}

/**
 * Run CHANGED whenever one of the budget's inputs changes size: BODY's
 * containing block, or the page's tree-font probe.
 */
function observeBudgetInputs(body: HTMLElement, changed: () => void): ResizeObserver {
  const Observer = pageResizeObserver(body);
  if (Observer === null) {
    unmeasurable("the page has no ResizeObserver to follow the column's width", {});
  }
  const observer = new Observer(changed);
  observer.observe(containingBlockOf(requireBubble(body)));
  observer.observe(treeFontProbe(body.ownerDocument));
  return observer;
}

/** The bubble around BODY, which a drawn bubble body always has. */
function requireBubble(body: HTMLElement): HTMLElement {
  const bubble = body.closest<HTMLElement>(".bubble");
  if (bubble === null) unmeasurable("the body has no bubble around it", {});
  return bubble;
}

/**
 * Reconcile PARENT's children in place so they match TARGET's, preserving the
 * identity of every node that did not change.
 *
 * This is the whole anti-flicker mechanism. The reveal renders the authoritative
 * `proseHtml(slice, cols)` into a DETACHED target each frame; rather than swap
 * the live subtree for it (which destroys and recreates every settled node under
 * the reader), this walks the two child lists in order and, position by
 * position:
 *
 *   - a node that `isEqualNode` the target's is left exactly as it is — a stable
 *     prose paragraph or a final tree line keeps its identity and never repaints;
 *   - a node that only changed inside (same tag and attributes, or a text node)
 *     is PATCHED in place recursively — a growing paragraph's text node gets its
 *     data updated, a `.mp-tree` is descended into and line-diffed, the one tail
 *     line whose wrap can still change is patched — so the node stays put and
 *     only its altered interior moves;
 *   - anything else is replaced, extra target nodes are appended, and trailing
 *     nodes the target no longer has are removed.
 *
 * Because every position ends structurally equal to the target, the reconciled
 * result is byte-identical to a fresh `proseHtml(slice, cols)` — the same string
 * a settled paint writes.
 *
 * TAIL, when given, is a node kept as PARENT's last child: it is never matched
 * against the target and never removed, so the prose reconciles ahead of it and
 * it stays put. The streaming reveal passes `null` (no trailing node), and the
 * in-place tree line-diff (`patchNode`) does too.
 */
export function reconcileChildren(parent: Node, target: Node, tail: Node | null): void {
  const goal = Array.from(target.childNodes);
  let i = 0;
  for (; i < goal.length; i++) {
    const want = goal[i];
    // `.item` returns null past the end at run time even where the DOM lib types
    // it non-null, so the range check below is real, not dead.
    let existing: ChildNode | null = parent.childNodes.item(i);
    // Reaching the preserved tail means the prose ran out: the rest is new and
    // is inserted ahead of the tail rather than matched against it.
    if (existing === tail) existing = null;
    if (existing === null) {
      parent.insertBefore(want, tail);
      continue;
    }
    if (existing.isEqualNode(want)) continue;
    if (patchable(existing, want)) {
      patchNode(existing, want);
      continue;
    }
    parent.replaceChild(want, existing);
  }
  // Drop any prose nodes the target no longer carries, without touching the tail.
  let extra: ChildNode | null = parent.childNodes.item(i);
  while (extra !== null && extra !== tail) {
    parent.removeChild(extra);
    extra = parent.childNodes.item(i);
  }
}

/**
 * Whether EXISTING can be patched into WANT in place (keeping EXISTING's node
 * identity) rather than replaced wholesale: two text nodes, or two elements of
 * the same tag carrying the same attributes. A tag or attribute difference means
 * the node genuinely changed shape and is replaced instead.
 */
function patchable(existing: Node, want: Node): boolean {
  if (existing.nodeType !== want.nodeType) return false;
  if (existing.nodeType === Node.TEXT_NODE) return true;
  if (existing.nodeType !== Node.ELEMENT_NODE) return false;
  const a = existing as Element;
  const b = want as Element;
  return a.tagName === b.tagName && sameAttributes(a, b);
}

/** Whether two elements carry the same attribute set, name for name and value. */
function sameAttributes(a: Element, b: Element): boolean {
  if (a.attributes.length !== b.attributes.length) return false;
  for (const attr of Array.from(a.attributes)) {
    if (b.getAttribute(attr.name) !== attr.value) return false;
  }
  return true;
}

/** Patch EXISTING to match WANT in place: text data for a text node, otherwise
 * recurse so the element's own children reconcile the same way. */
function patchNode(existing: Node, want: Node): void {
  if (existing.nodeType === Node.TEXT_NODE) {
    const from = existing as Text;
    const to = want as Text;
    if (from.data !== to.data) from.data = to.data;
    return;
  }
  reconcileChildren(existing, want, null);
}
