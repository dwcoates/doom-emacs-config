// The footer's breathing signal: a size oscillation that never resets, and a
// color ramp that steps once per daemon-resolved progress view.
import { describe, expect, it } from "vitest";

import {
  BREATH_PERIOD_MS,
  BREATH_SHADES,
  BUBBLE_BREATH_PERIOD_MS,
  BreathingTicker,
  BubbleBreath,
  breathColor,
  bubbleBreath,
  bubbleBreathStyle,
} from "../src/breathing.js";

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

describe("BubbleBreath: the prompt bubble's phase survives a rebuild", () => {
  it("starts the cycle where the page first painted a bubble", () => {
    // Arrange
    const breath = new BubbleBreath();
    // Act
    const got = breath.delayMs(NOW);
    // Assert — the epoch is the first read, so the first bubble seeks nowhere.
    expect(got).toBe(0);
  });

  it("advances with the clock, against the epoch rather than the last read", () => {
    // Arrange
    const breath = new BubbleBreath();
    breath.delayMs(NOW);
    breath.delayMs(NOW + 400);
    // Act
    const got = breath.delayMs(NOW + 1200);
    // Assert
    expect(got).toBe(1200);
  });

  it("gives two bubbles rendered at the same instant the same phase", () => {
    // Arrange — one epoch for the whole page, so the feed breathes in unison.
    const breath = new BubbleBreath();
    breath.delayMs(NOW);
    // Act
    const first = breath.delayMs(NOW + 2000);
    const second = breath.delayMs(NOW + 2000);
    // Assert
    expect(second).toBe(first);
  });

  it("wraps back to the start of the cycle after a full period", () => {
    // Arrange
    const breath = new BubbleBreath();
    breath.delayMs(NOW);
    // Act
    const got = breath.delayMs(NOW + BUBBLE_BREATH_PERIOD_MS);
    // Assert — a whole cycle later is the same point in the cycle.
    expect(got).toBe(0);
  });

  it("keeps the phase inside one period however long the page has been open", () => {
    // Arrange — hours of uptime must not emit an ever-growing delay.
    const breath = new BubbleBreath();
    breath.delayMs(NOW);
    // Act
    const got = breath.delayMs(NOW + BUBBLE_BREATH_PERIOD_MS * 1000 + 137);
    // Assert
    expect(got).toBe(137);
  });

  it("separates a rebuild at t and one at t+delta by exactly delta", () => {
    // Arrange — a rebuild is just another read of the same never-moved epoch.
    const breath = new BubbleBreath();
    const delta = 1234;
    const before = breath.delayMs(NOW);
    // Act
    const after = breath.delayMs(NOW + delta);
    // Assert
    expect((after - before + BUBBLE_BREATH_PERIOD_MS) % BUBBLE_BREATH_PERIOD_MS).toBe(delta);
  });

  it("separates rebuilds a period apart by delta modulo the period", () => {
    // Arrange — the wrap must not break the rebuild-equals-elapsed property.
    const breath = new BubbleBreath();
    const delta = BUBBLE_BREATH_PERIOD_MS + 900;
    const before = breath.delayMs(NOW);
    // Act
    const after = breath.delayMs(NOW + delta);
    // Assert
    expect((after - before + BUBBLE_BREATH_PERIOD_MS) % BUBBLE_BREATH_PERIOD_MS).toBe(
      delta % BUBBLE_BREATH_PERIOD_MS,
    );
  });

  it("floors a clock that goes backwards at zero", () => {
    // Arrange — a negative elapsed would emit a POSITIVE animation-delay and
    // stall the bubble at the deflated end of the swing.
    const breath = new BubbleBreath();
    breath.delayMs(NOW);
    // Act
    const got = breath.delayMs(NOW - 5000);
    // Assert
    expect(got).toBe(0);
  });

  it("never moves the epoch once stamped", () => {
    // Arrange — the immovability IS the continuity guarantee.
    const breath = new BubbleBreath();
    breath.delayMs(NOW);
    breath.delayMs(NOW + 3000);
    // Act — a later read still measures from the original epoch.
    const got = breath.delayMs(NOW + 100);
    // Assert
    expect(got).toBe(100);
  });
});

describe("bubbleBreathStyle: what a render stamps on the bubble", () => {
  it("emits a negative animation-delay, which is what seeks into the cycle", () => {
    // Arrange / Act
    const got = bubbleBreathStyle(Date.now());
    // Assert — a positive delay would DEFER the breath rather than resume it.
    expect(got).toMatch(/^animation-delay:-\d+ms$/);
  });

  it("emits the page-global breath's phase, not a fresh one per call", () => {
    // Arrange — every bubble reads the one shared epoch.
    const at = Date.now() + 777;
    // Act
    const got = bubbleBreathStyle(at);
    // Assert
    expect(got).toBe(`animation-delay:-${Math.round(bubbleBreath.delayMs(at))}ms`);
  });

  it("stays within one period, so the attribute never grows without bound", () => {
    // Arrange / Act — a page open for a very long time.
    const got = bubbleBreathStyle(Date.now() + BUBBLE_BREATH_PERIOD_MS * 500);
    const ms = Number(/-(\d+)ms$/.exec(got)?.[1]);
    // Assert
    expect(ms).toBeLessThan(BUBBLE_BREATH_PERIOD_MS);
  });
});
