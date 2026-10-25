/**
 * timers — the one timer seam: a suite drives what waits on a timer without a
 * real clock, and production hands in the page's own (`WINDOW_TIMERS`).
 */

/** Schedule and cancel a one-shot timer. */
export interface Timers {
  setTimeout(fn: () => void, ms: number): number;
  clearTimeout(handle: number): void;
}

/** The page's own timers, which is what production always uses. */
export const WINDOW_TIMERS: Timers = {
  setTimeout: (fn, ms) => globalThis.setTimeout(fn, ms) as unknown as number,
  clearTimeout: (handle) => globalThis.clearTimeout(handle),
};
