/**
 * DOM helpers shared across the feed: the one ancestor walk the feed's pointer
 * logic needs, and the one way a redraw puts elements in order.
 *
 * THE ANCESTOR WALK.
 *
 * Both pointer gestures over the feed answer the same question — which
 * section, if any, does the pointer sit inside? — and differ only in what
 * makes a node a section: a wheel wants the innermost SCROLL BOX
 * (scroll.ts), a click wants the innermost CAPPED SECTION (expand.ts).
 * The walk itself is shared, so it lives here once.
 *
 * Generic over the node shape (`parentElement` is all it touches), so it
 * runs against a real DOM and against a plain object in a test alike.
 */

import { log } from "./log.js";

/** The SVG namespace every drawn glyph's elements are created in. */
export const SVG_NS = "http://www.w3.org/2000/svg";

/**
 * Innermost node at or above `start` that satisfies `match`, stopping
 * below `stop` — `stop` itself is never a candidate, and neither is
 * anything above it. Null when nothing between `start` and `stop`
 * matches.
 */
export function ancestorMatching<T extends { parentElement: T | null }>(
  start: T | null,
  stop: T,
  match: (node: T) => boolean,
): T | null {
  for (let node = start; node && node !== stop; node = node.parentElement) {
    if (match(node)) return node;
  }
  return null;
}

/**
 * THE WIDTH A BOX'S VERTICAL SCROLLBAR TAKES FROM ITS CONTENT, in px: its
 * border-box width less its padding-box width (`clientWidth`) less its two side
 * borders. The one formula every scrollbar measurement shares — the bubble
 * body's column budget (bubble/body.ts) and the scrollbar hit test
 * (expand.ts) — so the two cannot disagree about what a bar is. An overlay bar
 * takes no layout width and reads 0. Validating the answer is the caller's.
 */
export function scrollbarWidthPx(box: { offsetWidth: number; clientWidth: number }, sideBordersPx: number): number {
  return box.offsetWidth - box.clientWidth - sideBordersPx;
}

/**
 * A computed CSS length's pixels, or null when VALUE is not a length in px.
 * A laid-out element reports every computed length in px, so null means the
 * element is not laid out, or the length resolved to something else (a
 * percentage, a keyword); what that means is the caller's to say. The one
 * parse every computed-length read shares, so no two readers can disagree
 * about what a px length is.
 */
export function computedPx(value: string): number | null {
  const px = /^(-?\d+(?:\.\d+)?)px$/.exec(value);
  return px === null ? null : Number.parseFloat(px[1]);
}

/**
 * Make PARENT's children exactly DESIRED, in order, MOVING ONLY WHAT IS OUT OF
 * PLACE. Answers how many elements it had to insert or move.
 *
 * WHY NOT `replaceChildren`. Removing an element from the document and putting
 * it back is not a no-op to a browser: its layout box is torn down and rebuilt,
 * which RESETS the scroll position of every scroll box inside it. A feed that re-laid every row with
 * `replaceChildren` on each live push therefore snapped every expanded bubble
 * the reader was scrolled inside back to its top, several times a second
 * (owner rule, 2026-09-23: the user owns the scroll). So an element already in
 * its place is never touched: a push that appends a row inserts that row and
 * nothing else, and a push that only redrew a row inside its chrome moves
 * nothing at all. Only a genuine reorder or reparent moves an element.
 */
export function placeChildren(parent: Element, desired: readonly ChildNode[]): number {
  const wanted = new Set<Node>(desired);
  for (const child of [...parent.childNodes]) {
    if (!wanted.has(child)) child.remove();
  }
  let placed = 0;
  let cursor: ChildNode | null = parent.firstChild;
  for (const el of desired) {
    if (cursor === el) {
      cursor = cursor.nextSibling;
      continue;
    }
    parent.insertBefore(el, cursor);
    placed += 1;
  }
  return placed;
}

/**
 * Paint EL with CLASS_NAME and TEXT, touching neither when EL already holds
 * it: the one in-place repaint of a small drawn mark (a state glyph).
 *
 * WHY NOT ASSIGN UNCONDITIONALLY. An element redrawn on every push of its
 * state — a merge tab's or a test suite's breathing dot, re-pushed many times
 * a second — must keep running whatever its class animates; a write that
 * changes nothing is skipped rather than trusted to be inert.
 */
export function repaintMark(el: HTMLElement, className: string, text: string): void {
  if (el.className !== className) el.className = className;
  if (el.textContent !== text) el.textContent = text;
}

/**
 * Bring HOST's optional child matching SELECTOR to NEXT: dropped while NEXT is
 * null, put in place by INSERT the first time it is known, and replaced only
 * when its text changed, so a child the push did not change keeps its element.
 * The one placement of a small element a component draws only while the
 * daemon knows its value (a suite's counts, a reused suite's source).
 */
export function placeOptional(
  host: HTMLElement,
  selector: string,
  next: HTMLElement | null,
  insert: (el: HTMLElement) => void,
): void {
  const drawn = host.querySelector(selector);
  if (next === null) drawn?.remove();
  else if (drawn === null) insert(next);
  else if (drawn.textContent !== next.textContent) drawn.replaceWith(next);
}

/**
 * The `ResizeObserver` of the page EL lives in, or null when that page has
 * none (a detached document, or a host without the capability). Every
 * measurer asks through here; what an absent observer means is each
 * caller's own to say.
 */
export function pageResizeObserver(el: Node): typeof ResizeObserver | null {
  const view = el.ownerDocument?.defaultView;
  if (view === null || view === undefined || typeof view.ResizeObserver !== "function") return null;
  return view.ResizeObserver;
}

/**
 * The page's `IntersectionObserver`, or null when it has none (a host without
 * the capability). Every viewport watch asks through here; what an absent
 * observer means is each caller's own to say.
 */
export function pageIntersectionObserver(): typeof IntersectionObserver | null {
  const Ctor = (globalThis as { IntersectionObserver?: typeof IntersectionObserver }).IntersectionObserver;
  return Ctor ?? null;
}

/**
 * NODE as `tag.class` (`div.bubble.md`), named from the element a text node
 * sits in: how a diagnosis record names the element a change or a callback
 * concerns.
 */
export function nodeLabel(node: Node): string {
  const el = node instanceof Element ? node : node.parentElement;
  if (el === null) return node.nodeName.toLowerCase();
  const classes = [...el.classList].join(".");
  return classes === "" ? el.tagName.toLowerCase() : `${el.tagName.toLowerCase()}.${classes}`;
}

/**
 * Every element under ROOT that matches SELECTOR, in document order: what
 * `querySelectorAll` answers, and the ONE way this webapp asks it.
 *
 * WHY NOT `querySelectorAll` (WebKit, measured 2026-10-16). A
 * `querySelectorAll` that matches anything leaves its answer cached on ROOT,
 * and nothing but a later change under ROOT drops it. A document unloaded
 * while such an answer stands is never freed: the cached list holds its
 * elements and they hold their document, so every webhost reload and every
 * page reload kept the old document's whole realm alive (537 documents and
 * 11.6 GB in one web process after 26 hours), and each full garbage
 * collection marked all of it. Walking the elements and asking each whether
 * it `matches` answers the same set and caches nothing.
 *
 * `:scope` IS REFUSED. `matches` reads `:scope` as the element being tested,
 * not ROOT, so a scoped selector would silently match nothing here; a caller
 * after ROOT's own children asks `childrenMatching`.
 */
export function queryAll<E extends Element = Element>(root: ParentNode, selector: string): E[] {
  refuseScope("queryAll", selector);
  const doc = root instanceof Document ? root : root.ownerDocument;
  if (doc === null) throw new Error(`queryAll: a ${root.nodeName} root belongs to no document`);
  // A malformed selector throws the SyntaxError `querySelectorAll` would,
  // even under a ROOT with nothing to test; `querySelector` caches nothing.
  doc.createDocumentFragment().querySelector(selector);
  const found: E[] = [];
  const walker = doc.createTreeWalker(root, NodeFilter.SHOW_ELEMENT);
  for (let node = walker.nextNode(); node !== null; node = walker.nextNode()) {
    const el = node as Element;
    if (el.matches(selector)) found.push(el as E);
  }
  return found;
}

/**
 * PARENT's own children that match SELECTOR, in order: the `:scope > X` of a
 * `querySelectorAll`, asked without one (see `queryAll`).
 */
export function childrenMatching<E extends Element = Element>(parent: Element, selector: string): E[] {
  refuseScope("childrenMatching", selector);
  return [...parent.children].filter((child): child is E => child.matches(selector));
}

/** Refuse a `:scope` selector, which `matches` would silently misread. */
function refuseScope(helper: string, selector: string): void {
  if (!selector.includes(":scope")) return;
  log.error(`${helper} was handed a :scope selector, which matches() reads as the tested element`, {
    operation: "dom.query-scope-refused",
    context: { helper, selector },
  });
  throw new Error(`${helper}: ":scope" selectors are refused (${selector})`);
}

/** One target a page-scoped observer watches, with the options it watches it under. */
interface Watched {
  readonly target: Node;
  readonly options: MutationObserverInit;
}

/** A `MutationObserver` the page disconnects as it unloads (see `pageMutationObserver`). */
export interface PageMutationObserver {
  /** Watch TARGET under OPTIONS, as `MutationObserver.observe` does. */
  observe(target: Node, options: MutationObserverInit): void;
  /** Stop watching everything, as `MutationObserver.disconnect` does. */
  disconnect(): void;
}

/**
 * A `MutationObserver` over DOC's page that is disconnected the moment the
 * page hides, and the ONE way this webapp observes mutations.
 *
 * WHY (WebKit, measured 2026-10-17). A page torn down with an observer still
 * connected goes on mutating its own DOM after its script context stops: a
 * visibilitychange handler writes an attribute, a cancelled stream redraws its
 * failure. The observer queues a record it can never deliver, WebKit parks it
 * for good, and its pending nodes keep the whole document alive. Every hidden
 * page held a whole-document observer (host/unshown-animations.ts), so every
 * webhost reload and every page unload stranded nearly every hidden page.
 * `pagehide` comes before the context stops, so every observer the page made
 * is disconnected there, whatever the page goes on to mutate.
 *
 * A page restored from the back/forward cache (`pageshow` with `persisted`)
 * watches everything again; a target asked for while the page is hidden is
 * watched from that restore on.
 */
export function pageMutationObserver(doc: Document, callback: MutationCallback): PageMutationObserver {
  const win = doc.defaultView;
  if (win === null) {
    log.error("a mutation observer was asked for a document with no window", {
      operation: "dom.page-observer-windowless",
      context: { document: doc.URL },
    });
    throw new Error(`pageMutationObserver: ${doc.URL} has no window`);
  }
  // The constructor of the document's own window, which is the page's.
  const observer = new win.MutationObserver(callback);
  let watched: Watched[] = [];
  let hidden = false;
  // Each observer holds its OWN listeners rather than sharing one per page:
  // nothing can strip a listener this observer is not also told about.
  const hide = (): void => {
    hidden = true;
    observer.disconnect();
  };
  const show = (event: PageTransitionEvent): void => {
    if (!event.persisted) return;
    hidden = false;
    for (const { target, options } of watched) observer.observe(target, options);
  };
  let listening = false;
  const listen = (): void => {
    if (listening) return;
    listening = true;
    win.addEventListener("pagehide", hide);
    win.addEventListener("pageshow", show);
  };
  const unlisten = (): void => {
    listening = false;
    win.removeEventListener("pagehide", hide);
    win.removeEventListener("pageshow", show);
  };
  // From creation on, so an observer made before the page hid and first used
  // after it knows the page is hidden.
  listen();
  return {
    observe(target, options) {
      listen();
      watched.push({ target, options });
      if (!hidden) observer.observe(target, options);
    },
    disconnect() {
      observer.disconnect();
      watched = [];
      unlisten();
    },
  };
}
