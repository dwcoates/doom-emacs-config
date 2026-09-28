/**
 * The feed's overscan buffer — the cure for the first-scroll jitter that
 * `content-visibility: auto` brings.
 *
 * Every `.feed-item` carries `content-visibility: auto` (styles.css), so the
 * browser skips the layout and paint of a row far from the viewport and stands
 * in the `contain-intrinsic-size` guess (320px) for its height. The first time
 * the reader scrolls a skipped row into view it lays out for the first time,
 * its real height replaces the guess, and everything below it shifts — the
 * classic content-visibility jitter.
 *
 * The band keeps the perf win for rows FAR from the viewport but PRE-RENDERS
 * (forces the layout of) the rows within about five viewport heights above and
 * below it, so by the time the reader reaches one it is already laid out at its
 * true height. That first layout still changes a row's height while it is
 * ABOVE the reader, which moves the content under them unless something anchors
 * the view: Chromium's native scroll anchoring does, WebKit (the Emacs webview)
 * has none, so the feed's tail owner anchors it (scroll.ts, `TailFollow`, "THE
 * FEED OWNS ITS SCROLL ANCHORING"). An `IntersectionObserver` rooted
 * on the feed's scroll box, its margin blown out to that band, is what reports
 * a row entering or leaving it; a row inside wears `OVERSCAN_CLASS`, which
 * turns its `content-visibility` back to `visible` (styles.css), and a row that
 * leaves loses the class and is skippable again.
 *
 * The percentage `rootMargin` is resolved against the ROOT's box, so "five
 * pages" tracks the scroll box's own height with no resize bookkeeping.
 */

/** How many viewport heights of rows to keep pre-rendered on each side. */
export const OVERSCAN_PAGES = 5;

/** The class a row wears while it sits inside the pre-render band. */
export const OVERSCAN_CLASS = "overscan";

/**
 * The `rootMargin` that grows the root's box by PAGES viewport heights on the
 * top and bottom (and not at all on the sides). Percentages resolve against the
 * root, so the band is PAGES × the scroll box's own height.
 */
export function overscanRootMargin(pages: number): string {
  return `${(pages * 100).toString()}% 0px`;
}

/** Observes feed rows so those near the viewport are pre-rendered. */
export interface Overscan {
  /** Start watching ROW; it gains/loses OVERSCAN_CLASS as it enters/leaves. */
  observe(row: HTMLElement): void;
  /** Stop watching ROW — called when the feed drops it, so nothing leaks. */
  unobserve(row: HTMLElement): void;
  /** Tear the observer down when the feed disposes. */
  dispose(): void;
}

/**
 * Build the overscan observer rooted on the feed's scroll BOX.
 *
 * Returns `null` when the environment ships no `IntersectionObserver` (a bare
 * jsdom with no stub), so its absence is a no-op: the feed still works, it just
 * loses the pre-render band — the same standing the app gives a missing
 * `ResizeObserver`.
 */
export function createOverscan(box: HTMLElement, pages: number = OVERSCAN_PAGES): Overscan | null {
  const Ctor = (globalThis as { IntersectionObserver?: typeof IntersectionObserver })
    .IntersectionObserver;
  if (Ctor === undefined) return null;
  const observer = new Ctor(
    (entries) => {
      for (const entry of entries) {
        (entry.target as HTMLElement).classList.toggle(OVERSCAN_CLASS, entry.isIntersecting);
      }
    },
    { root: box, rootMargin: overscanRootMargin(pages) },
  );
  return {
    observe: (row) => observer.observe(row),
    unobserve: (row) => observer.unobserve(row),
    dispose: () => observer.disconnect(),
  };
}
