/**
 * The agent bubbles' elapsed-tick lifecycle.
 *
 * The clock and the scheduler are injected (the same TimerHost the header's
 * TaskTimer runs on), so the tick is driven by hand here rather than by
 * real elapsed time.
 */
import { describe, expect, it } from "vitest";

import { AgentClock } from "../src/agent-clock.js";
import { TICK_MS } from "../src/timer.js";
import { AgentClockEntry } from "../src/topbar.js";
import { clockHarness } from "./clock-harness.js";

/** When the fixture agents started, as their tool-use-start stamps. */
const START = "2026-07-13T12:00:00.000Z";
const START_MS = Date.parse(START);

/** A live agent entry, as runningAgentClocks projects one. */
function liveAgent(id = "a1", startedAt = START): AgentClockEntry {
  return { id, startedAt };
}

/** The shared clock harness, plus the paints the clock made. */
function harness() {
  const core = clockHarness(START_MS);
  const painted: Array<{ id: string; label: string }> = [];
  return {
    ...core,
    painted,
    clock(): AgentClock {
      return new AgentClock(core.host, (id, label) => painted.push({ id, label }));
    },
  };
}

describe("AgentClock", () => {
  it("starts one interval when a live agent appears", () => {
    // Arrange
    const h = harness();
    // Act
    h.clock().sync([liveAgent()]);
    // Assert
    expect(h.intervals.size).toBe(1);
  });

  it("keeps the one interval across repeated syncs", () => {
    // Arrange
    const h = harness();
    const clock = h.clock();
    clock.sync([liveAgent()]);
    // Act — the per-frame re-sync a streaming turn drives.
    clock.sync([liveAgent()]);
    // Assert
    expect(h.intervals.size).toBe(1);
  });

  it("stops the interval when the last agent settles", () => {
    // Arrange
    const h = harness();
    const clock = h.clock();
    clock.sync([liveAgent()]);
    // Act
    clock.sync([]);
    // Assert
    expect(h.intervals.size).toBe(0);
  });

  it("starts no interval for an empty live set", () => {
    // Arrange + Act
    const h = harness();
    h.clock().sync([]);
    // Assert
    expect(h.intervals.size).toBe(0);
  });

  it("paints nothing on sync, whose render already baked fresh labels", () => {
    // Arrange + Act
    const h = harness();
    h.clock().sync([liveAgent()]);
    // Assert
    expect(h.painted).toEqual([]);
  });

  it("paints an agent's elapsed label on the tick", () => {
    // Arrange
    const h = harness();
    h.clock().sync([liveAgent()]);
    // Act
    h.advance(TICK_MS);
    // Assert
    expect(h.painted).toEqual([{ id: "a1", label: "1s" }]);
  });

  it("paints every live agent each tick", () => {
    // Arrange
    const h = harness();
    h.clock().sync([liveAgent("a1"), liveAgent("a2")]);
    // Act
    h.advance(TICK_MS);
    // Assert
    expect(h.painted.map((p) => p.id)).toEqual(["a1", "a2"]);
  });

  it("counts each agent from its own start stamp", () => {
    // Arrange — a2 started a minute after a1.
    const h = harness();
    const later = new Date(START_MS + 60_000).toISOString();
    h.clock().sync([liveAgent("a1"), liveAgent("a2", later)]);
    // Act — 90s past a1's start, 30s past a2's.
    h.advance(90_000);
    // Assert — the final tick's pair.
    expect(h.painted.slice(-2)).toEqual([
      { id: "a1", label: "1m 30s" },
      { id: "a2", label: "30s" },
    ]);
  });

  it("drops a settled agent from the next tick's paints", () => {
    // Arrange
    const h = harness();
    const clock = h.clock();
    clock.sync([liveAgent("a1"), liveAgent("a2")]);
    // Act — a1 settles; a2 keeps running.
    clock.sync([liveAgent("a2")]);
    h.advance(TICK_MS);
    // Assert
    expect(h.painted.map((p) => p.id)).toEqual(["a2"]);
  });

  it("paints nothing after stop", () => {
    // Arrange
    const h = harness();
    const clock = h.clock();
    clock.sync([liveAgent()]);
    // Act
    clock.stop();
    h.advance(TICK_MS);
    // Assert
    expect(h.painted).toEqual([]);
  });
});
