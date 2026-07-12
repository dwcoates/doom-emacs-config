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
 */

/** Width of the left/right gutters that arm a section's own scrolling. */
export const EDGE_PX = 32;

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
  for (let node = start; node && node !== feed; node = node.parentElement) {
    if (isScrollBox(metrics(node))) return node;
  }
  return null;
}

const domMetrics = (el: HTMLElement): ScrollMetrics => ({
  scrollHeight: el.scrollHeight,
  clientHeight: el.clientHeight,
  overflowY: getComputedStyle(el).overflowY,
});

/**
 * Arm edge-gated scrolling on `feed`: a wheel over a section's middle
 * scrolls the feed, a wheel over its gutters scrolls the section.
 * Hovering a gutter marks the section `.scroll-zone`, so the armed
 * state is visible before the wheel turns.
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

  let armed: HTMLElement | null = null;
  feed.addEventListener("pointermove", (e: PointerEvent) => {
    const scroller = scrollerUnder(e.target);
    const hit =
      scroller && inEdgeZone(scroller.getBoundingClientRect(), e.clientX, edgePx) ? scroller : null;
    if (hit === armed) return;
    armed?.classList.remove("scroll-zone");
    hit?.classList.add("scroll-zone");
    armed = hit;
  });
}
