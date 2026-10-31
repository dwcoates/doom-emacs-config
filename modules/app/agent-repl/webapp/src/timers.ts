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

/** One-shot timers held by key: at most one pending per key. */
export interface KeyedTimers<K> {
  /** Run FN after MS under KEY, replacing any timer KEY already had pending. */
  start(key: K, ms: number, fn: () => void): void;
  /** Cancel KEY's pending timer, if it has one. */
  cancel(key: K): void;
  /** Cancel every pending timer. */
  cancelAll(): void;
  /** Whether KEY has a timer pending. */
  pending(key: K): boolean;
}

/**
 * Keyed one-shot timers on TIMERS. A timer is forgotten the moment it fires,
 * before its function runs, so the function sees its key as no longer pending.
 */
export function createKeyedTimers<K>(timers: Timers = WINDOW_TIMERS): KeyedTimers<K> {
  const handles = new Map<K, number>();
  const cancel = (key: K): void => {
    const handle = handles.get(key);
    if (handle === undefined) return;
    timers.clearTimeout(handle);
    handles.delete(key);
  };
  return {
    start(key, ms, fn) {
      cancel(key);
      handles.set(
        key,
        timers.setTimeout(() => {
          handles.delete(key);
          fn();
        }, ms),
      );
    },
    cancel,
    cancelAll() {
      for (const key of [...handles.keys()]) cancel(key);
    },
    pending: (key) => handles.has(key),
  };
}
