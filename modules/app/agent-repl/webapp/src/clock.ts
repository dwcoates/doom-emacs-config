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
import { createListeners } from "./listeners.js";
import type { Quiet } from "./quiet.js";

export interface Ticker {
  /** Call FN with the current instant, once per interval. Returns its unsubscriber. */
  subscribe(fn: (nowMs: number) => void): () => void;
  /** The current instant, for a first paint before the first tick lands. */
  now(): number;
}

/** How often the shared clock steps. The finest unit anything renders is seconds. */
export const DEFAULT_TICK_MS = 1000;

/**
 * Build a ticker. INTERVALMS is injected by tests running on fake timers.
 *
 * THE CLOCK STOPS WHILE QUIET STANDS (quiet.ts): nobody can see a count-up
 * with the lid closed, so the interval is cleared, and when the quiet state
 * ends every subscriber is stepped at once (the clocks jump to now, never
 * replaying the seconds they missed) and the interval resumes.
 */
export function createTicker(intervalMs: number = DEFAULT_TICK_MS, quiet?: Quiet): Ticker {
  const subscribers = createListeners<[number]>();
  let handle: ReturnType<typeof setInterval> | null = null;

  const step = (): void => {
    const nowMs = Date.now();
    // A subscriber may unsubscribe (or add one) from inside its tick.
    subscribers.notify(nowMs);
  };
  const stop = (): void => {
    if (handle === null) return;
    clearInterval(handle);
    handle = null;
  };
  const start = (): void => {
    if (quiet?.quiet() === true || subscribers.size === 0) return;
    handle ??= setInterval(step, intervalMs);
  };
  quiet?.onChange((standing) => {
    if (standing) {
      stop();
      return;
    }
    if (subscribers.size === 0) return;
    step();
    start();
  });

  return {
    subscribe(fn: (nowMs: number) => void): () => void {
      const unsubscribe = subscribers.add(fn);
      start();
      return () => {
        if (!unsubscribe()) return;
        if (subscribers.size > 0) return;
        stop();
      };
    },
    now(): number {
      return Date.now();
    },
  };
}

/**
 * Whether A_MS and B_MS fall on the same LOCAL calendar date: the ONE test
 * every "today" in the app (a merge's, the rail's re-fit at midnight) asks.
 */
export function sameLocalDate(aMs: number, bMs: number): boolean {
  const a = new Date(aMs);
  const b = new Date(bMs);
  return a.getFullYear() === b.getFullYear() && a.getMonth() === b.getMonth() && a.getDate() === b.getDate();
}
