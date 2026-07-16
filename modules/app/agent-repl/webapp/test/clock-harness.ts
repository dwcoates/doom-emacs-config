/**
 * A hand-driven clock and scheduler for the tick-lifecycle tests: the
 * `TimerHost` that the header's `TaskTimer` and the bubbles' `AgentClock`
 * both run on, with the interval table exposed for start/stop-edge
 * assertions and a manual `advance()` in place of real elapsed time — a
 * test that waited a second on a wall clock would be both slow and flaky.
 *
 * Each test file layers its own paint sink and clock factory on top; only
 * the host, the interval table, and the advance loop are shared.
 */
import { TICK_MS, TimerHost } from "../src/timer.js";

export interface ClockHarness {
  host: TimerHost;
  /** Live intervals by id, for asserting the tick's start/stop edges. */
  intervals: Map<number, { handler: () => void; ms: number }>;
  /** Advance the clock and fire every live interval once per tick crossed. */
  advance(ms: number): void;
}

export function clockHarness(startMs: number): ClockHarness {
  let now = startMs;
  let nextId = 1;
  const intervals = new Map<number, { handler: () => void; ms: number }>();

  const host: TimerHost = {
    setInterval: (handler, ms) => {
      const id = nextId++;
      intervals.set(id, { handler, ms });
      return id;
    },
    clearInterval: (id) => {
      intervals.delete(id);
    },
    now: () => now,
  };

  return {
    host,
    intervals,
    advance(ms: number): void {
      for (let elapsed = 0; elapsed < ms; elapsed += TICK_MS) {
        now += TICK_MS;
        for (const interval of [...intervals.values()]) interval.handler();
      }
    },
  };
}
