/**
 * body — THE BUBBLE BODY: the element a bubble's content is painted into, and
 * the machinery that keeps a metaprompt tree in it wrapped to the bubble's cap.
 *
 * Extracted from the response card (response.ts), where it was the response
 * bubble's alone: the custom element that knows when it joins the document,
 * the cap measurement (`measureTreeCols`), the tree wrap that waits for a
 * containing block and follows the column's width, and the in-place reconcile
 * the type-out paints through.
 */
import { log } from "../log.js";
import { inline, hasFencedTree, renderMarkdown, type TreeCols } from "../markdown.js";
import { findTreeRegion, renderTreeHtml, type TreeIssue } from "../metaprompt-tree.js";
import { onDiscard } from "../feed/ticking.js";

/** The webapp surfaces a wrap issue through the client logger. */
const logTreeIssue: TreeIssue = (message, context) => {
  log.debug(message, { operation: "feed.cards.response.tree", context });
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
export const TREE_WIDTH_UNMEASURABLE = "feed.cards.response.tree-width-unmeasurable";

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
 * computed styles — and never from the bubble's current fit-content width, so
 * a tree below the cap never wraps and a shrink cannot feed back into a re-wrap.
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
 * to join the document (`createTreeWrap`). This is a watch flag, not a lock.
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

  const contentPx = capPx - chromePx;
  const cols = Math.floor(contentPx / charPx);
  const context = { cols, char_px: charPx, cap_px: capPx, chrome_px: chromePx, containing_px: containingPx };
  if (cols < 1) unmeasurable("the bubble's content width at its cap holds no column", context);
  log.debug("measured a metaprompt tree's column budget", {
    operation: "feed.cards.response.tree-cols",
    context,
  });
  return cols;
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

/** The tag of a response's body: a custom element, so it knows when it joins the document. */
export const RESPONSE_BODY_TAG = "response-body";

/** A response's body, which runs its hooks every time it joins the document. */
export interface ResponseBody extends HTMLElement {
  onConnect(hook: () => void): void;
}

/**
 * The response body as a custom element. `connectedCallback` runs synchronously
 * inside the DOM operation that attaches the row — before any frame is painted,
 * and whether or not the webview is visible — which is what lets a tree's first
 * paint wait for a containing block without ever showing a wrong-width frame.
 */
function responseBodyClass(): CustomElementConstructor {
  return class extends HTMLElement implements ResponseBody {
    private readonly hooks: Array<() => void> = [];

    onConnect(hook: () => void): void {
      this.hooks.push(hook);
    }

    connectedCallback(): void {
      for (const hook of this.hooks) hook();
    }
  };
}

/** A fresh response body, the custom element defined on first use. */
export function createResponseBody(): ResponseBody {
  if (customElements.get(RESPONSE_BODY_TAG) === undefined) {
    customElements.define(RESPONSE_BODY_TAG, responseBodyClass());
  }
  const body = document.createElement(RESPONSE_BODY_TAG) as ResponseBody;
  body.className = "bubble-body";
  return body;
}

/** What a body's painters draw through, so a tree is only ever drawn at a measured width. */
export interface TreeWrap {
  /** The body's measured column budget, measured on first ask after each attach. */
  readonly cols: TreeCols;
  /**
   * Draw TEXT through DRAW: at once, unless TEXT holds a tree and the body is
   * not in the document yet, in which case the draw runs the moment it joins.
   * The latest draw is also what a width change re-runs.
   */
  paint(text: string, draw: () => void): void;
}

/**
 * Keep BODY's tree wrapped to the cap for its whole life.
 *
 * The FIRST paint of a tree waits for the body to join the document, since only
 * then is there a containing block to measure against. Every later attach (a
 * tool-group re-arrange moves the row) re-measures, and a `ResizeObserver` on
 * the CONTAINING BLOCK — the box the cap is a percentage of, not the
 * fit-content body, which keeps its width when the column grows around a bubble
 * below its cap — re-wraps when the column's width moves the budget. The
 * observer is torn down with the body through `stopTicking` (see ticking.ts).
 */
export function createTreeWrap(body: ResponseBody): TreeWrap {
  let cols: number | null = null;
  let latest: (() => void) | null = null;
  let deferred = false;
  let observer: ResizeObserver | null = null;

  const drawsTree = (): boolean => body.querySelector(".mp-tree") !== null;
  const rewrap = (): void => {
    const next = measureTreeCols(body);
    if (next === cols) return;
    cols = next;
    latest?.();
  };
  const watch = (): void => {
    if (observer === null) onDiscard(body, () => observer?.disconnect());
    else observer.disconnect();
    observer = observeContainingBlock(body, rewrap);
  };

  body.onConnect(() => {
    if (deferred) {
      deferred = false;
      latest?.();
    } else if (drawsTree()) {
      rewrap();
    }
    if (drawsTree()) watch();
  });

  return {
    cols: () => {
      cols ??= measureTreeCols(body);
      return cols;
    },
    paint(text, draw) {
      latest = draw;
      if (!body.isConnected && proseNeedsWidth(text)) {
        deferred = true;
        return;
      }
      draw();
      if (body.isConnected && observer === null && drawsTree()) watch();
    },
  };
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

/** The bubble around BODY, which a drawn response body always has. */
function requireBubble(body: HTMLElement): HTMLElement {
  const bubble = body.closest<HTMLElement>(".bubble");
  if (bubble === null) unmeasurable("the body has no bubble around it", {});
  return bubble;
}

/**
 * Write the whole settled prose into the body, wrapped to the cap (see
 * `createTreeWrap`). Plain prose reflows on its own (CSS), so it takes no
 * observer and no measurement.
 */
export function paintWhole(body: ResponseBody, markdown: string): void {
  const wrap = createTreeWrap(body);
  wrap.paint(markdown, () => {
    body.innerHTML = proseHtml(markdown, wrap.cols);
  });
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
 * `paintWhole` writes in one shot.
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
