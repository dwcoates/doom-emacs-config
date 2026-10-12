/**
 * animation-frames — a hand-driven `requestAnimationFrame` for a unit test.
 *
 * vitest's faked frames fire on a clock grid that does not restart per test,
 * so "advance one frame" can run none or two. This queue runs exactly the
 * frames a test asks for, each FRAME_MS of the faked `Date` after the last.
 */
import { vi } from "vitest";

/** One animation frame of the faked clock. */
export const FRAME_MS = 17;

/** The installed queue: `run(count)` runs that many frames. */
export interface AnimationFrames {
  run(count: number): void;
}

/** Fake `Date` from zero and stub the page's frames with a queue. */
export function installAnimationFrames(): AnimationFrames {
  let queued: (() => void)[] = [];
  vi.useFakeTimers({ toFake: ["Date"] });
  vi.setSystemTime(0);
  vi.stubGlobal("requestAnimationFrame", (fn: () => void) => {
    queued.push(fn);
    return queued.length;
  });
  return {
    run(count: number): void {
      for (let i = 0; i < count; i += 1) {
        vi.advanceTimersByTime(FRAME_MS);
        const due = queued;
        queued = [];
        for (const fn of due) fn();
      }
    },
  };
}
