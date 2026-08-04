/**
 * The topbar task timer's lifecycle.
 *
 * The clock and the scheduler are injected, so the tick is driven by hand
 * here rather than by real elapsed time: a test that waited a second on a
 * wall clock would be both slow and flaky.
 */
import { describe, expect, it } from "vitest";

import { IDLE_LABEL, TICK_MS, TaskTimer, timerLabel } from "../src/timer.js";
import { clockHarness } from "./clock-harness.js";

/** The turn start the daemon stamps, as an envelope ts. */
const START = "2026-07-13T12:00:00.000Z";
const START_MS = Date.parse(START);

/** The shared clock harness, plus the paints the timer made. */
function harness() {
  const core = clockHarness(START_MS);
  const painted: string[] = [];
  return {
    ...core,
    painted,
    timer(): TaskTimer {
      return new TaskTimer(core.host, (label) => painted.push(label));
    },
  };
}

describe("timerLabel", () => {
  it("reads the idle label when no turn is running", () => {
    // Arrange + Act + Assert
    expect(timerLabel(null, START_MS)).toBe(IDLE_LABEL);
  });

  it("reads the elapsed time since the turn's daemon stamp", () => {
    // Arrange + Act + Assert
    expect(timerLabel(START, START_MS + 330_000)).toBe("5m 30s");
  });

  it("reads zero seconds the instant a turn starts", () => {
    // Arrange + Act + Assert
    expect(timerLabel(START, START_MS)).toBe("0s");
  });
});

describe("TaskTimer", () => {
  it("paints nothing on an idle sync against an already-idle header", () => {
    // Arrange
    const h = harness();
    const timer = h.timer();
    // Act — every frame of an idle session syncs, and the renderer already
    // writes the idle label itself.
    timer.sync(null);
    timer.sync(null);
    // Assert
    expect(h.painted).toEqual([]);
  });

  it("starts no interval while no turn is running", () => {
    // Arrange
    const h = harness();
    const timer = h.timer();
    // Act
    timer.sync(null);
    // Assert — an idle header has no digit to move.
    expect(h.intervals.size).toBe(0);
  });

  it("paints the turn's elapsed time immediately when the turn starts", () => {
    // Arrange
    const h = harness();
    const timer = h.timer();
    // Act — the header must not trail the spinner by a whole tick.
    timer.sync(START);
    // Assert
    expect(h.painted).toEqual(["0s"]);
  });

  it("repaints the elapsed time once a second while the turn runs", () => {
    // Arrange
    const h = harness();
    const timer = h.timer();
    timer.sync(START);
    // Act
    h.advance(3 * TICK_MS);
    // Assert
    expect(h.painted).toEqual(["0s", "1s", "2s", "3s"]);
  });

  it("ticks at one-second resolution", () => {
    // Arrange
    const h = harness();
    const timer = h.timer();
    // Act
    timer.sync(START);
    // Assert — a finer tick would repaint a digit that cannot have moved.
    expect([...h.intervals.values()][0].ms).toBe(TICK_MS);
  });

  it("resumes a turn already under way at the elapsed time it is really at", () => {
    // Arrange — a tab that joins mid-turn replays the original user-turn stamp.
    const h = harness();
    const timer = h.timer();
    h.advance(0);
    const joined = new Date(START_MS - 90_000).toISOString();
    // Act
    timer.sync(joined);
    // Assert — 90s in, not restarted from zero.
    expect(h.painted).toEqual(["1m 30s"]);
  });

  it("does not restart the interval on a re-sync of the same turn", () => {
    // Arrange — renderChrome syncs on EVERY frame of a streaming turn.
    const h = harness();
    const timer = h.timer();
    timer.sync(START);
    const running = [...h.intervals.keys()][0];
    // Act
    timer.sync(START);
    timer.sync(START);
    // Assert
    expect([...h.intervals.keys()]).toEqual([running]);
  });

  it("does not repaint on a re-sync of the same turn", () => {
    // Arrange
    const h = harness();
    const timer = h.timer();
    timer.sync(START);
    h.painted.length = 0;
    // Act — a frame arriving mid-second must not move the count early.
    timer.sync(START);
    // Assert
    expect(h.painted).toEqual([]);
  });

  it("paints the idle label when the turn ends", () => {
    // Arrange
    const h = harness();
    const timer = h.timer();
    timer.sync(START);
    h.advance(2 * TICK_MS);
    h.painted.length = 0;
    // Act — the result frame clears the store's turn start.
    timer.sync(null);
    // Assert
    expect(h.painted).toEqual([IDLE_LABEL]);
  });

  it("stops ticking when the turn ends", () => {
    // Arrange
    const h = harness();
    const timer = h.timer();
    timer.sync(START);
    // Act
    timer.sync(null);
    h.advance(5 * TICK_MS);
    // Assert — no interval survives, so nothing repaints over the idle label.
    expect(h.intervals.size).toBe(0);
  });

  it("counts a fresh turn from its own start rather than the previous turn's", () => {
    // Arrange
    const h = harness();
    const timer = h.timer();
    timer.sync(START);
    h.advance(10 * TICK_MS);
    timer.sync(null);
    h.painted.length = 0;
    // Act
    timer.sync(new Date(h.host.now()).toISOString());
    // Assert
    expect(h.painted).toEqual(["0s"]);
  });

  it("re-arms the interval for a turn that follows an earlier one", () => {
    // Arrange — the first turn's interval was cleared at its result.
    const h = harness();
    const timer = h.timer();
    timer.sync(START);
    timer.sync(null);
    // Act
    timer.sync(new Date(h.host.now()).toISOString());
    h.painted.length = 0;
    h.advance(TICK_MS);
    // Assert
    expect(h.painted).toEqual(["1s"]);
  });

  it("goes idle when the view is torn down mid-turn", () => {
    // Arrange — the view is explicitly torn down while a turn is active.
    const h = harness();
    const timer = h.timer();
    timer.sync(START);
    h.painted.length = 0;
    // Act
    timer.stop();
    // Assert — a torn-down view retains no active turn timer.
    expect(h.painted).toEqual([IDLE_LABEL]);
  });

  it("stops ticking when the view is torn down mid-turn", () => {
    // Arrange
    const h = harness();
    const timer = h.timer();
    timer.sync(START);
    timer.stop();
    h.painted.length = 0;
    // Act
    h.advance(5 * TICK_MS);
    // Assert — nothing repaints over the idle label.
    expect(h.painted).toEqual([]);
  });
});
