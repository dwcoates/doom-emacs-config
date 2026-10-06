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
 */
import { log } from "../log.js";
import { inline, hasFencedTree, renderMarkdown, type TreeCols } from "../markdown.js";
import { findTreeRegion, renderTreeHtml, type TreeIssue } from "../metaprompt-tree.js";
import { placeChildren, scrollbarWidthPx } from "../dom.js";
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
function computedPx(el: Element, style: CSSStyleDeclaration, property: ComputedLength): number {
  const value = style[property];
  const px = /^(-?\d+(?:\.\d+)?)px$/.exec(value);
  if (px === null) {
    unmeasurable("a computed length is not in px", { element: el.className, property, value });
  }
  return Number.parseFloat(px[1]);
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
  let px = computedPx(el, s, "paddingLeft") + computedPx(el, s, "paddingRight");
  if (parts.border) px += computedPx(el, s, "borderLeftWidth") + computedPx(el, s, "borderRightWidth");
  if (parts.margin) px += computedPx(el, s, "marginLeft") + computedPx(el, s, "marginRight");
  return px;
}

/**
 * The column budget a tree in BODY wraps to: the body's content width WHEN THE
 * BUBBLE IS AT ITS CAP, in columns of the tree's monospace font.
 *
 * We never scroll horizontally; lines wider than the max bubble width wrap instead.
 *
 * The budget derives from the CAP ALONE — the bubble's `max-width` resolved
 * against its containing block, less the bubble's fixed chrome summed from
 * computed styles and the EXPANDED scroll box's scrollbar gutter — and never
 * from the bubble's current fit-content width or fold, so a tree below the cap
 * never wraps, a shrink cannot feed back into a re-wrap, and an expand (whose
 * scrollbar appears past 50vh) never re-wraps it.
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
export function measureTreeCols(body: HTMLElement): number {
  if (!body.isConnected) unmeasurable("the bubble body is not in the document", {});
  const view = body.ownerDocument.defaultView;
  if (view === null) unmeasurable("the bubble body's document has no window", {});
  const bubble = requireBubble(body);
  const containing = containingBlockOf(bubble);

  const probe = body.ownerDocument.createElement("div");
  probe.className = "mp-tree";
  probe.style.cssText = "position:absolute;visibility:hidden;white-space:pre;left:-9999px;top:0;";
  probe.textContent = "0".repeat(100);
  body.appendChild(probe);
  const charPx = probe.getBoundingClientRect().width / 100;
  probe.remove();
  if (!(charPx > 0)) unmeasurable("the tree font's column measured no width", { char_px: charPx });

  const containingPx =
    containing.clientWidth - insetXPx(containing, view, { border: false, margin: false });
  if (!(containingPx > 0)) {
    unmeasurable("the bubble's containing block has no width", { containing_px: containingPx });
  }
  const maxWidth = view.getComputedStyle(bubble).maxWidth;
  const capPx = resolveCapPx(maxWidth, containingPx);
  if (capPx === null) unmeasurable("the bubble's max-width does not resolve", { max_width: maxWidth });

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
  const borderPx = computedPx(scroll, s, "borderLeftWidth") + computedPx(scroll, s, "borderRightWidth");
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
  const px = /^(-?[\d.]+)px$/.exec(maxWidth);
  if (px !== null) return Number.parseFloat(px[1]);
  const pct = /^(?:calc\()?\s*(-?[\d.]+)%\s*\)?$/.exec(maxWidth);
  if (pct !== null) return (Number.parseFloat(pct[1]) / 100) * containingPx;
  return null;
}

/** The tag of every bubble's body: a custom element, so it knows when it joins the document. */
export const BUBBLE_BODY_TAG = "bubble-body";

/** The class the stylesheet's body rules key on. */
export const BUBBLE_BODY_CLASS = "bubble-body";

/** The attribute every markdown slot wears; its source is held beside it. */
export const MARKDOWN_SLOT_ATTRIBUTE = "data-markdown";

/** A bubble's body, which runs its hooks every time it joins the document. */
export interface BubbleBody extends HTMLElement {
  onConnect(hook: () => void): void;
}

/**
 * The body as a custom element. `connectedCallback` runs synchronously inside
 * the DOM operation that attaches the row — before any frame is painted, and
 * whether or not the webview is visible — which is what lets a tree's first
 * paint wait for a containing block without ever showing a wrong-width frame.
 */
function bubbleBodyClass(): CustomElementConstructor {
  return class extends HTMLElement implements BubbleBody {
    private readonly hooks: Array<() => void> = [];

    onConnect(hook: () => void): void {
      this.hooks.push(hook);
    }

    connectedCallback(): void {
      for (const hook of this.hooks) hook();
    }
  };
}

/** What paints one body: all of its slots, or one of them, at the cap's budget. */
interface Painter {
  paint(only?: HTMLElement): void;
}

/** Every body's painter, armed once when the body is created. */
const painters = new WeakMap<Element, Painter>();

/** Every markdown slot's source, held beside the element it paints. */
const slotSources = new WeakMap<Element, string>();

/** A fresh bubble body, the custom element defined on first use, its painter armed. */
export function createBubbleBody(): BubbleBody {
  if (customElements.get(BUBBLE_BODY_TAG) === undefined) {
    customElements.define(BUBBLE_BODY_TAG, bubbleBodyClass());
  }
  const body = document.createElement(BUBBLE_BODY_TAG) as BubbleBody;
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
export function markdownSlot(className: string, markdown: string): HTMLElement {
  const slot = document.createElement("div");
  slot.className = className;
  slot.setAttribute(MARKDOWN_SLOT_ATTRIBUTE, "");
  slotSources.set(slot, markdown);
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
 * Keep BODY's slots painted, and every tree in them wrapped to the cap, for the
 * body's whole life.
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
 * Every later attach (a tool-group re-arrange moves the row) re-measures, and a
 * `ResizeObserver` on the CONTAINING BLOCK — the box the cap is a percentage
 * of, not the fit-content body, which keeps its width when the column grows
 * around a bubble below its cap — re-wraps only when the column's width moves
 * the budget. The same observer is what sees a hidden bubble come back. It is
 * torn down with the body through `stopTicking` (see ticking.ts).
 */
function armPainter(body: BubbleBody): Painter {
  let cols: number | null = null;
  let deferred = false;
  let observer: ResizeObserver | null = null;

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
  const watch = (): void => {
    if (observer === null) onDiscard(body, () => observer?.disconnect());
    else observer.disconnect();
    observer = observeContainingBlock(body, rewrap);
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
    if (body.isConnected && observer === null && drawsTree()) watch();
  }

  function rewrap(): void {
    // Not laid out (hidden, or detached): there is no width to wrap to, and
    // the observer fires again when the bubble has a box once more.
    if (!body.isConnected || (!deferred && !drawsTree()) || !laidOut()) return;
    if (deferred) {
      paint();
      return;
    }
    const next = measureTreeCols(body);
    if (next === cols) return;
    cols = next;
    draw(slots());
  }

  body.onConnect(() => {
    rewrap();
    if (deferred || drawsTree()) watch();
  });

  return { paint };
}

/**
 * Run REWRAP whenever BODY's containing block changes size. The recompute is
 * deferred to an animation frame so a burst of resizes coalesces and the
 * repaint's own height change is not re-observed inside the same delivery; it
 * repaints only when the integer column count moved.
 */
function observeContainingBlock(body: HTMLElement, rewrap: () => void): ResizeObserver {
  const view = body.ownerDocument.defaultView;
  if (view === null || typeof view.ResizeObserver !== "function") {
    unmeasurable("the page has no ResizeObserver to follow the column's width", {});
  }
  const containing = containingBlockOf(requireBubble(body));
  let scheduled = false;
  const recompute = (): void => {
    scheduled = false;
    rewrap();
  };
  const observer = new view.ResizeObserver(() => {
    if (scheduled) return;
    scheduled = true;
    const raf = view.requestAnimationFrame?.bind(view);
    if (raf !== undefined) raf(recompute);
    else recompute();
  });
  observer.observe(containing);
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
