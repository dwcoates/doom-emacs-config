/**
 * frame-queue — a frame scheduler a unit test runs by hand.
 *
 * A module taking a frame seam (`frame.ts`) is handed `queue.frame`; the
 * callbacks it schedules wait until the test starts a frame with `run()`, or
 * runs frames until none are left with `drain()`. One frame runs only the
 * callbacks waiting when it starts, as a browser's does: a callback scheduling
 * another puts it on the NEXT frame.
 */

/** The hand-run queue: the seam to hand a module, and the test's controls. */
export interface FrameQueue {
  /** The seam: schedule CALLBACK for the next frame. */
  frame: (callback: () => void) => void;
  /** Start one frame: run every callback waiting when it starts. */
  run: () => void;
  /** Start frames until no callback is waiting. */
  drain: () => void;
  /** How many callbacks wait for the next frame. */
  pending: () => number;
  /** How many callbacks were ever scheduled. */
  asked: () => number;
}

/** A frame queue with nothing scheduled. */
export function frameQueue(): FrameQueue {
  let waiting: Array<() => void> = [];
  let asked = 0;
  const run = (): void => {
    const due = waiting;
    waiting = [];
    for (const callback of due) callback();
  };
  return {
    frame: (callback) => {
      asked += 1;
      waiting.push(callback);
    },
    run,
    drain: () => {
      while (waiting.length > 0) run();
    },
    pending: () => waiting.length,
    asked: () => asked,
  };
}
