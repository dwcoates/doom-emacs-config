/**
 * Edge-gated inner scrolling.
 *
 * Every capped section in the feed (Read previews, Bash command/output,
 * diffs, tool input/output) is its own scroll box, so a wheel gesture
 * aimed at the feed gets swallowed whenever the pointer happens to sit
 * over one of them. The gate: a section only takes the wheel when the
 * pointer is in its left- or right-most gutter (EDGE_PX wide). Anywhere
 * else over the section the wheel is redirected to the feed, so
 * scrolling past a section is the default and scrolling the section
 * itself is the deliberate act.
 *
 * installEdgeScroll is the only DOM-facing piece; every decision it
 * makes lives in the pure helpers above it.
 *
 * The feed's own tail-following metric (isPinnedToBottom) lives here too:
 * it is the other half of the same question of who owns the scroll
 * position, the user or the feed.
 */
import { ancestorMatching } from "./dom.js";

/** Width of the left/right gutters that arm a section's own scrolling. */
export const EDGE_PX = 32;

/** Class marking the whole section whose gutters are lit while armed. */
export const ZONE_CLASS = "scroll-zone";

/** Class marking the armed scroll box itself, which carries the cursor. */
export const BOX_CLASS = "scroll-zone-box";

/**
 * Classes of the feed's sections: the bordered blocks that hold scroll
 * boxes. A tool card holds up to three (input, progress, output); a
 * permission card holds its preview; a response bubble holds its own
 * height-capped body, and naming the bubble here is what puts the lit
 * gutters on the bubble's edges rather than inset at its body's.
 */
export const SECTION_CLASSES = ["tool-card", "permission", "bubble"];

/** Slack below which the feed still counts as parked at its tail. */
export const PIN_PX = 40;

/** Wheel deltaMode units (WheelEvent.DOM_DELTA_*). */
const DELTA_LINE = 1;
const DELTA_PAGE = 2;
/** Line height assumed when a wheel event reports its delta in lines. */
const LINE_PX = 16;

/** The horizontal span of an element's box. */
export interface Box {
  left: number;
  right: number;
}

/** The geometry that makes an element a scroll box. */
export interface ScrollMetrics {
  scrollHeight: number;
  clientHeight: number;
  overflowY: string;
}

/** Where a scroll box currently sits along its own scrollable height. */
export interface ScrollPosition {
  scrollHeight: number;
  scrollTop: number;
  clientHeight: number;
}

/**
 * True when the box is parked at its tail (within PIN_PX of the bottom).
 * A pinned feed follows new content; an unpinned one holds the user's
 * place, so this is what a render consults before moving scrollTop.
 */
export function isPinnedToBottom(pos: ScrollPosition, pinPx: number = PIN_PX): boolean {
  return pos.scrollHeight - pos.scrollTop - pos.clientHeight < pinPx;
}

/**
 * Tail-following is FROZEN while the user reads content they deliberately
 * opened — a nested view inside a bubble (an agent's stream in a monitoring
 * bubble). A frozen feed stops chasing its tail, so streaming output can no
 * longer yank the view off what the user is reading. The freeze is the same
 * question isPinnedToBottom answers — who owns the scroll position, the user
 * or the feed — so its two transitions live here beside it.
 */

/**
 * The freeze after a fold toggle: opening a nested view BEGINS the freeze,
 * and closing one holds whatever state was already in force (only a return
 * to the tail lifts it, see freezeOnScroll). So a user who opens one view,
 * closes it, and opens another stays frozen throughout.
 */
export function freezeOnToggle(frozen: boolean, opened: boolean): boolean {
  return opened || frozen;
}

/**
 * The freeze after a user scroll: it ENDS the moment the feed is back at its
 * tail, which is the user asking to follow new content again. Anywhere short
 * of the tail keeps the freeze, so a scroll that merely reads more of the
 * opened content does not resume tail-following.
 */
export function freezeOnScroll(frozen: boolean, pinned: boolean): boolean {
  return frozen && !pinned;
}

/** The one mutable field parking a box at its tail touches. */
export interface ScrollTail {
  scrollTop: number;
  scrollHeight: number;
}

/**
 * Put a scroll box at its tail in a single jump. Assigning scrollTop is
 * what makes it a jump rather than an animation: the tail is simply THERE
 * on the next frame, with no crawl down the history to watch. Every site
 * that wants the newest content on screen goes through here — the
 * restored-session render, the tail-following render, and the Emacs
 * host's workspace-switch snap (host.ts).
 */
export function parkAtTail(box: ScrollTail): void {
  box.scrollTop = box.scrollHeight;
}

/** Registering a listener for a box's own scroll events. */
export type SubscribeScroll = (onScroll: () => void) => void;

/** Registering a listener for a box's own size changes. */
export type SubscribeResize = (onResize: () => void) => void;

/** Everything the tail re-anchor reads and writes on the box it guards. */
export type ReanchorBox = ScrollTail & ScrollPosition;

/**
 * Keep BOX parked at its tail across every resize of BOX itself.
 *
 * A workspace switch relayouts the feed: Emacs re-displays the webview's
 * window alongside the input panel, and the WKWebView is resized to the new
 * geometry. That resize is asynchronous relative to the lisp that triggered
 * it, so the host's switch snap (`agentReplParkAtTail`, host.ts) and the
 * resize can land in either order — and a snap that lands FIRST is undone by
 * the resize, which grows the scrollable height under a scrollTop that stays
 * put and leaves the feed short of the bottom.
 *
 * Re-anchoring on the resize event itself removes the ordering question
 * rather than betting on one order: snap-then-resize re-parks here, and
 * resize-then-snap parks against the settled layout. Neither needs the
 * resize to have finished by any particular moment.
 *
 * The decision is the pre-resize sample of `isPinnedToBottom`, not a fresh
 * one: by the time the resize fires, the new geometry has already moved the
 * box off its tail, so reading it then would answer about the damage instead
 * of about what the reader wanted. A box the user had deliberately scrolled
 * up in keeps its place, exactly as it does under streaming output.
 */
export function installTailReanchor(
  box: ReanchorBox,
  subscribeScroll: SubscribeScroll,
  subscribeResize: SubscribeResize,
  pinPx: number = PIN_PX,
): void {
  let pinned = isPinnedToBottom(box, pinPx);
  subscribeScroll(() => {
    pinned = isPinnedToBottom(box, pinPx);
  });
  subscribeResize(() => {
    if (pinned) parkAtTail(box);
  });
}

/** Where a revealed node lands: flush with the top, or as little as possible. */
export type RevealBlock = "start" | "nearest";

/** The one method bringing a node into view needs. */
export interface RevealTarget {
  scrollIntoView(arg: { block: RevealBlock }): void;
}

/**
 * Bring NODE into view inside the feed. The single "show me this bubble"
 * primitive: the roster's agent reveal (render.ts), the keyboard cycle
 * (nav.ts), and any later match-stepping (iterative search) must agree on
 * the mechanic, or the feed lurches differently depending on which one
 * moved it.
 *
 * `start` puts the node flush with the top, for a jump ARRIVING from
 * elsewhere. `nearest` scrolls only as far as it must, which is what a
 * cycle wants: a target already fully on screen should not be yanked
 * anywhere, since the current-marker is what says where the cycle sits.
 */
export function revealNode(node: RevealTarget, block: RevealBlock = "nearest"): void {
  node.scrollIntoView({ block });
}

/** True when the element both clips its content and scrolls it vertically. */
export function isScrollBox(m: ScrollMetrics): boolean {
  if (m.overflowY !== "auto" && m.overflowY !== "scroll") return false;
  return m.scrollHeight - m.clientHeight > 1;
}

/** True when clientX sits in the box's left or right gutter. */
export function inEdgeZone(box: Box, clientX: number, edgePx: number = EDGE_PX): boolean {
  return clientX - box.left <= edgePx || box.right - clientX <= edgePx;
}

/**
 * True when a wheel over `scroller` belongs to the feed instead.
 * A null scroller means the pointer is over no section at all, and an
 * unscrollable feed means there is nowhere to redirect the wheel to.
 */
export function redirectsToFeed(opts: {
  scroller: Box | null;
  clientX: number;
  feedScrollable: boolean;
  edgePx?: number;
}): boolean {
  if (!opts.scroller || !opts.feedScrollable) return false;
  return !inEdgeZone(opts.scroller, opts.clientX, opts.edgePx ?? EDGE_PX);
}

/** Wheel delta in pixels, whatever unit the event reported it in. */
export function wheelDeltaPx(e: { deltaY: number; deltaMode: number }, viewportPx: number): number {
  if (e.deltaMode === DELTA_LINE) return e.deltaY * LINE_PX;
  if (e.deltaMode === DELTA_PAGE) return e.deltaY * viewportPx;
  return e.deltaY;
}

/**
 * The whole wheel decision: null leaves the event to the browser, a
 * number is the pixel delta to add to the feed's scrollTop instead.
 * A purely horizontal wheel is always the browser's, so a wide code
 * block inside a section still pans on shift-wheel.
 */
export function wheelAction(opts: {
  scroller: Box | null;
  clientX: number;
  deltaY: number;
  deltaMode: number;
  feedScrollable: boolean;
  feedHeight: number;
  edgePx?: number;
}): number | null {
  if (opts.deltaY === 0) return null;
  if (
    !redirectsToFeed({
      scroller: opts.scroller,
      clientX: opts.clientX,
      feedScrollable: opts.feedScrollable,
      edgePx: opts.edgePx,
    })
  ) {
    return null;
  }
  return wheelDeltaPx(opts, opts.feedHeight);
}

/**
 * Innermost scroll box at or above `start`, stopping below `feed`.
 * Returns null when nothing between the pointer and the feed scrolls.
 */
export function innerScrollerAt<T extends { parentElement: T | null }>(
  start: T | null,
  feed: T,
  metrics: (node: T) => ScrollMetrics,
): T | null {
  return ancestorMatching(start, feed, (node) => isScrollBox(metrics(node)));
}

/**
 * The section a scroll box belongs to: the nearest enclosing card, or
 * the box itself when no card encloses it. The lit gutters ride the
 * section rather than the box, so they sit flush with the section's
 * left/right edges and run its FULL height, not just the height of
 * whichever sub-box the pointer happens to be over.
 */
export function sectionFor<T extends { parentElement: T | null }>(
  box: T,
  feed: T,
  isSection: (node: T) => boolean,
): T {
  for (let node: T | null = box; node && node !== feed; node = node.parentElement) {
    if (isSection(node)) return node;
  }
  return box;
}

const domMetrics = (el: HTMLElement): ScrollMetrics => ({
  scrollHeight: el.scrollHeight,
  clientHeight: el.clientHeight,
  overflowY: getComputedStyle(el).overflowY,
});

/**
 * Arm edge-gated scrolling on `feed`: a wheel over a section's middle
 * scrolls the feed, a wheel over its gutters scrolls the section.
 * Hovering a gutter marks the enclosing section `.scroll-zone` and the
 * scroll box `.scroll-zone-box`, so the armed state is visible before
 * the wheel turns — the bars on the section, the cursor on the box.
 */
export function installEdgeScroll(feed: HTMLElement, edgePx: number = EDGE_PX): void {
  const scrollerUnder = (target: EventTarget | null): HTMLElement | null =>
    innerScrollerAt(target instanceof HTMLElement ? target : null, feed, domMetrics);

  feed.addEventListener(
    "wheel",
    (e: WheelEvent) => {
      const scroller = scrollerUnder(e.target);
      const delta = wheelAction({
        scroller: scroller ? scroller.getBoundingClientRect() : null,
        clientX: e.clientX,
        deltaY: e.deltaY,
        deltaMode: e.deltaMode,
        feedScrollable: feed.scrollHeight - feed.clientHeight > 1,
        feedHeight: feed.clientHeight,
        edgePx,
      });
      if (delta === null) return;
      e.preventDefault();
      feed.scrollTop += delta;
    },
    { capture: true, passive: false },
  );

  const isSection = (el: HTMLElement): boolean =>
    SECTION_CLASSES.some((cls) => el.classList.contains(cls));

  let armedBox: HTMLElement | null = null;
  let armedSection: HTMLElement | null = null;
  feed.addEventListener("pointermove", (e: PointerEvent) => {
    const scroller = scrollerUnder(e.target);
    const hit =
      scroller && inEdgeZone(scroller.getBoundingClientRect(), e.clientX, edgePx) ? scroller : null;
    if (hit === armedBox) return;
    armedBox?.classList.remove(BOX_CLASS);
    armedSection?.classList.remove(ZONE_CLASS);
    armedBox = hit;
    armedSection = hit ? sectionFor(hit, feed, isSection) : null;
    armedBox?.classList.add(BOX_CLASS);
    armedSection?.classList.add(ZONE_CLASS);
  });
}
