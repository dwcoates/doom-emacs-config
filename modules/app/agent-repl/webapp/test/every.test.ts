// Run a callback every interval on a rescheduled one-shot timer. See src/every.ts.
import { describe, expect, it } from "vitest";

import { every, type After } from "../src/every.js";

/** A timer the test fires by hand. */
function manualTimers(): { after: After; pending: () => number; fire: () => void } {
  let timers: { fn: () => void; ms: number }[] = [];
  return {
    after: (fn, ms) => {
      const timer = { fn, ms };
      timers.push(timer);
      return () => {
        timers = timers.filter((t) => t !== timer);
      };
    },
    pending: () => timers.length,
    fire: () => {
      const next = timers.shift();
      next?.fn();
    },
  };
}

describe("every", () => {
  it("runs the callback once per firing, rescheduling after each run", () => {
    // Arrange
    const timers = manualTimers();
    let runs = 0;
    every(timers.after, 1000, () => void runs++);

    // Act
    timers.fire();
    timers.fire();

    // Assert
    expect(runs).toBe(2);
    expect(timers.pending()).toBe(1);
  });

  it("schedules nothing before the first interval runs", () => {
    // Arrange
    const timers = manualTimers();
    let runs = 0;

    // Act
    every(timers.after, 1000, () => void runs++);

    // Assert
    expect(runs).toBe(0);
    expect(timers.pending()).toBe(1);
  });

  it("leaves no timer pending once stopped", () => {
    // Arrange
    const timers = manualTimers();
    const stop = every(timers.after, 1000, () => undefined);
    timers.fire();

    // Act
    stop();

    // Assert
    expect(timers.pending()).toBe(0);
  });
});
