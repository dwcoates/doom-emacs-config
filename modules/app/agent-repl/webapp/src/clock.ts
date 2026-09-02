/**
 * The app's ONE clock.
 *
 * CLOCKS TICK CLIENT-SIDE. The wire ships instants — a turn's start, a cron's
 * next fire, a cold gate's deadline — and the client animates the count-up or
 * countdown itself, formatting through `src/duration.ts`. Nothing about a
 * running clock is ever pushed.
 *
 * WHY ONE SHARED INTERVAL RATHER THAN ONE PER CLOCK. A busy workspace has a
 * turn timer, a footer clock, several live tool cards and a tray of ages all
 * ticking at once; a `setInterval` each means N wakeups a second, N separate
 * `Date.now()` readings, and a screen whose seconds visibly disagree with each
 * other because each interval started on its own phase. One interval hands
 * every subscriber the SAME instant, so the whole page steps together.
 *
 * The interval exists only while somebody is subscribed: a page with no live
 * clock does no work at all, and the last unsubscriber stops it rather than
 * leaving a timer running against a torn-down view.
 */

export interface Ticker {
  /** Call FN with the current instant, once per interval. Returns its unsubscriber. */
  subscribe(fn: (nowMs: number) => void): () => void;
  /** The current instant, for a first paint before the first tick lands. */
  now(): number;
}

/** How often the shared clock steps. The finest unit anything renders is seconds. */
export const DEFAULT_TICK_MS = 1000;

/** Build a ticker. INTERVALMS is injected by tests running on fake timers. */
export function createTicker(intervalMs: number = DEFAULT_TICK_MS): Ticker {
  const subscribers = new Set<(nowMs: number) => void>();
  let handle: ReturnType<typeof setInterval> | null = null;

  const step = (): void => {
    const nowMs = Date.now();
    // A copy: a subscriber may unsubscribe (or add one) from inside its tick.
    for (const fn of [...subscribers]) fn(nowMs);
  };

  return {
    subscribe(fn: (nowMs: number) => void): () => void {
      subscribers.add(fn);
      if (handle === null) handle = setInterval(step, intervalMs);
      return () => {
        if (!subscribers.delete(fn)) return;
        if (subscribers.size > 0 || handle === null) return;
        clearInterval(handle);
        handle = null;
      };
    },
    now(): number {
      return Date.now();
    },
  };
}
