// The footer's breathing signal: a size oscillation that never resets, and a
// color ramp that steps once per daemon-resolved progress view.
import { describe, expect, it } from "vitest";

import { BREATH_PERIOD_MS, BREATH_SHADES, BreathingTicker, breathColor } from "../src/breathing.js";

const NOW = Date.parse("2024-05-01T12:00:00.000Z");

/** A distinct progress-view stand-in: identity is all the ticker reads. */
function view(): object {
  return {};
}

/** The hue degrees out of an `hsl(H …)` color. */
function hue(color: string): number {
  const got = /^hsl\((\d+)/.exec(color);
  if (got === null) throw new Error(`not an hsl color: ${color}`);
  return Number(got[1]);
}

describe("breathColor: the green → purple ramp", () => {
  it("starts the ramp at green", () => {
    // Arrange / Act
    const got = hue(breathColor(0));
    // Assert — the green end of the cool axis.
    expect(got).toBe(140);
  });

  it("ends the ramp at purple", () => {
    // Arrange / Act
    const got = hue(breathColor(BREATH_SHADES - 1));
    // Assert
    expect(got).toBe(285);
  });

  it("offers exactly the configured number of distinct shades", () => {
    // Arrange
    const stops = Array.from({ length: BREATH_SHADES }, (_, i) => breathColor(i));
    // Act
    const distinct = new Set(stops);
    // Assert
    expect(distinct.size).toBe(BREATH_SHADES);
  });

  it("spaces the stops evenly across the ramp", () => {
    // Arrange — even spacing is what makes each tick read as one step.
    const hues = Array.from({ length: BREATH_SHADES }, (_, i) => hue(breathColor(i)));
    // Act
    const gaps = hues.slice(1).map((h, i) => h - hues[i]);
    // Assert — rounding to whole degrees allows at most one degree of slop.
    expect(Math.max(...gaps) - Math.min(...gaps)).toBeLessThanOrEqual(1);
  });

  it("never wanders into the warm hues the footer reserves for alarm", () => {
    // Arrange — yellow, orange, and red belong to the rate-limit rungs and the
    // failure row, so a working phase must never borrow them.
    const hues = Array.from({ length: BREATH_SHADES }, (_, i) => hue(breathColor(i)));
    // Act / Assert
    expect(Math.min(...hues)).toBeGreaterThanOrEqual(140);
  });

  it("wraps an index past the last stop back onto the ramp", () => {
    // Arrange / Act — the ramp is a cycle the tick walks forever.
    const got = breathColor(BREATH_SHADES);
    // Assert
    expect(got).toBe(breathColor(0));
  });

  it("wraps a negative index back onto the ramp", () => {
    // Arrange / Act — JavaScript's `%` keeps the sign, which would emit a
    // negative hue rather than a color.
    const got = breathColor(-1);
    // Assert
    expect(got).toBe(breathColor(BREATH_SHADES - 1));
  });
});

describe("BreathingTicker: the color ramp steps per arrival", () => {
  it("starts on the ramp's first stop", () => {
    // Arrange
    const ticker = new BreathingTicker();
    // Act
    ticker.observe(view());
    // Assert — the first view adopts stop 0 rather than stepping off it.
    expect(ticker.state(NOW).shade).toBe(0);
  });

  it("steps one stop when a new view arrives", () => {
    // Arrange
    const ticker = new BreathingTicker();
    ticker.observe(view());
    // Act
    ticker.observe(view());
    // Assert
    expect(ticker.state(NOW).shade).toBe(1);
  });

  it("holds the shade across renders that carry the same view", () => {
    // Arrange — the footer re-renders on the chrome cadence, not per arrival.
    const ticker = new BreathingTicker();
    const same = view();
    ticker.observe(same);
    // Act
    ticker.observe(same);
    ticker.observe(same);
    // Assert
    expect(ticker.state(NOW).shade).toBe(0);
  });

  it("treats a re-sent view with identical numbers as its own tick", () => {
    // Arrange — the store adopts each ProgressView as a fresh object, so a
    // re-send is still traffic worth showing.
    const ticker = new BreathingTicker();
    ticker.observe({ turnStartedAtMs: 7 });
    // Act
    ticker.observe({ turnStartedAtMs: 7 });
    // Assert
    expect(ticker.state(NOW).shade).toBe(1);
  });

  it("wraps the ramp after its last stop", () => {
    // Arrange
    const ticker = new BreathingTicker();
    // Act — one adoption plus a full lap of steps.
    for (let i = 0; i <= BREATH_SHADES; i += 1) ticker.observe(view());
    // Assert
    expect(ticker.state(NOW).shade).toBe(0);
  });

  it("ignores a null view, which is not an arrival", () => {
    // Arrange — the footer renders before the daemon has resolved anything.
    const ticker = new BreathingTicker();
    ticker.observe(view());
    // Act
    ticker.observe(null);
    // Assert
    expect(ticker.state(NOW).shade).toBe(0);
  });
});

describe("BreathingTicker: the size oscillation never resets", () => {
  it("stamps its epoch on the first read", () => {
    // Arrange
    const ticker = new BreathingTicker();
    // Act
    const got = ticker.state(NOW);
    // Assert — the cycle starts where the footer first painted.
    expect(got.elapsedMs).toBe(0);
  });

  it("reports the time since that epoch, not since the last read", () => {
    // Arrange
    const ticker = new BreathingTicker();
    ticker.state(NOW);
    ticker.state(NOW + 500);
    // Act
    const got = ticker.state(NOW + 1500);
    // Assert
    expect(got.elapsedMs).toBe(1500);
  });

  it("leaves the epoch untouched when a new view arrives", () => {
    // Arrange — the whole point: a tick must not restart the breath.
    const ticker = new BreathingTicker();
    ticker.observe(view());
    ticker.state(NOW);
    // Act
    ticker.observe(view());
    // Assert
    expect(ticker.state(NOW + 900).elapsedMs).toBe(900);
  });

  it("keeps accumulating past a full breath period", () => {
    // Arrange — the negative delay seeks into the cycle, so it is never
    // reduced modulo the period on this side.
    const ticker = new BreathingTicker();
    ticker.state(NOW);
    // Act
    const got = ticker.state(NOW + BREATH_PERIOD_MS * 3 + 100);
    // Assert
    expect(got.elapsedMs).toBe(BREATH_PERIOD_MS * 3 + 100);
  });

  it("floors a clock that goes backwards at zero", () => {
    // Arrange — a negative elapsed would emit a POSITIVE animation-delay and
    // stall the breath until the clock caught up.
    const ticker = new BreathingTicker();
    ticker.state(NOW);
    // Act
    const got = ticker.state(NOW - 5000);
    // Assert
    expect(got.elapsedMs).toBe(0);
  });
});
