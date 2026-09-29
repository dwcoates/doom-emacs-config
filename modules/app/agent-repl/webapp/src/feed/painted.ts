/**
 * painted — when the root feed's rows reached the screen.
 *
 * A row is PAINTED once it is in the document and a frame has been painted
 * with it: the second `requestAnimationFrame` after its insert runs after the
 * first frame that drew it. The footer holds an ended quiet-stretch line until
 * the row that ended it is painted (`footer/quiet-hold.ts`), so the line never
 * clears before its successor is visible.
 *
 * ROOT FEED ONLY: the daemon holds a line only on a root-feed row, the one
 * feed the page always shows.
 */

/** When the root feed's rows were painted, and a feed of paint edges. */
export interface PaintWatch {
  /** When row ID was painted (`Date.now()` ms), or null if it has not been. */
  paintedAt(id: string): number | null;
  /** Observe every paint edge. Returns its unsubscriber. */
  onPainted(fn: (id: string, at: number) => void): () => void;
}

/** The feed side of a PaintWatch: what the root controller reports into. */
export interface PaintReporter {
  readonly watch: PaintWatch;
  /** The root controller painted IDS at AT. */
  report(ids: readonly string[], at: number): void;
}

/** Build the paint watch over the root controller's own record of its rows. */
export function createPaintReporter(paintedAt: (id: string) => number | null): PaintReporter {
  const listeners = new Set<(id: string, at: number) => void>();
  return {
    watch: {
      paintedAt,
      onPainted(fn) {
        listeners.add(fn);
        return () => {
          listeners.delete(fn);
        };
      },
    },
    report(ids, at) {
      for (const id of ids) for (const fn of [...listeners]) fn(id, at);
    },
  };
}
