// @vitest-environment jsdom
// The footer's breathing signal: a size oscillation that never resets, and a
// color ramp that steps once per daemon-resolved progress view. Plus the
// status word's letter wave, which shares the breath's epoch mechanic.
import { describe, expect, it } from "vitest";

import {
  AnimationEpoch,
  BREATH_PERIOD_MS,
  BREATH_SHADES,
  BreathingTicker,
  breathColor,
  STATUS_COLOR_CYCLE_MS,
  STATUS_COLOR_PERIOD_MS,
  STATUS_WAVE_CYCLE_MS,
  STATUS_WAVE_LETTER_OFFSET_MS,
  STATUS_WAVE_PERIOD_MS,
  StatusWave,
  WAVING_STATUS_ARMS,
  statusColorStyle,
  statusWaveStyle,
  statusWordWaves,
} from "../src/breathing.js";
import { FOOTER_STATUS_CASES } from "../src/footer/tones.js";

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

describe("AnimationEpoch: one immovable start time", () => {
  it("stamps the epoch on its first read, so the first render seeks nowhere", () => {
    // Arrange
    const epoch = new AnimationEpoch();
    // Act
    const got = epoch.elapsedMs(NOW);
    // Assert
    expect(got).toBe(0);
  });

  it("measures every later read from that first stamp, not from the last read", () => {
    // Arrange
    const epoch = new AnimationEpoch();
    epoch.elapsedMs(NOW);
    epoch.elapsedMs(NOW + 400);
    // Act
    const got = epoch.elapsedMs(NOW + 1200);
    // Assert
    expect(got).toBe(1200);
  });

  it("never moves the epoch once stamped, which IS the continuity guarantee", () => {
    // Arrange
    const epoch = new AnimationEpoch();
    epoch.elapsedMs(NOW);
    epoch.elapsedMs(NOW + 3000);
    // Act — an earlier-but-still-forward read measures from the original stamp.
    const got = epoch.elapsedMs(NOW + 100);
    // Assert
    expect(got).toBe(100);
  });

  it("floors a clock that goes backwards at zero", () => {
    // Arrange — a negative elapsed becomes a POSITIVE animation-delay, which
    // stalls the animation at its start until the clock catches up.
    const epoch = new AnimationEpoch();
    epoch.elapsedMs(NOW);
    // Act
    const got = epoch.elapsedMs(NOW - 5000);
    // Assert
    expect(got).toBe(0);
  });

  it("keeps two epochs independent, so one animation's start never moves another's", () => {
    // Arrange — the footer and the bubble each own one.
    const first = new AnimationEpoch();
    const second = new AnimationEpoch();
    first.elapsedMs(NOW);
    // Act — the second stamps later, at its own first read.
    const got = second.elapsedMs(NOW + 900);
    // Assert
    expect(got).toBe(0);
  });
});

describe("AnimationEpoch: both animations really share it", () => {
  // The extraction is only worth anything if neither consumer kept its own
  // hand-rolled epoch: a divergent copy would drift from this table silently.
  // Each entry reads its consumer at a wall-clock instant and reports the
  // elapsed time that consumer measured, in the units it reports it in.
  const consumers: Array<{ name: string; elapsedAt: (at: number) => number }> = [
    {
      name: "the footer's breathing ticker",
      elapsedAt: (() => {
        const ticker = new BreathingTicker();
        return (at: number) => ticker.state(at).elapsedMs;
      })(),
    },
    {
      name: "the footer status word's colour sweep",
      elapsedAt: (() => {
        const wave = new StatusWave();
        // Reported modulo the cycle, so the table's deltas stay inside one cycle.
        return (at: number) => wave.colorDelayMs(at);
      })(),
    },
  ];

  for (const { name, elapsedAt } of consumers) {
    it(`stamps ${name} on its first read`, () => {
      // Arrange / Act
      const got = elapsedAt(NOW);
      // Assert
      expect(got).toBe(0);
    });

    it(`measures ${name} from the immovable stamp rather than the last read`, () => {
      // Arrange
      elapsedAt(NOW);
      elapsedAt(NOW + 400);
      // Act
      const got = elapsedAt(NOW + 1200);
      // Assert
      expect(got).toBe(1200);
    });

    it(`floors ${name} at zero when the clock goes backwards`, () => {
      // Arrange
      elapsedAt(NOW);
      // Act
      const got = elapsedAt(NOW - 5000);
      // Assert
      expect(got).toBe(0);
    });
  }
});


describe("StatusWave: the footer status word's travelling letter bulge", () => {
  it("lags each letter behind the one before it by exactly one offset", () => {
    // Arrange
    const wave = new StatusWave();
    wave.delayMs(NOW, 0);

    // Act
    const first = wave.delayMs(NOW, 0);
    const second = wave.delayMs(NOW, 1);

    // Assert
    expect(second - first).toBe(STATUS_WAVE_LETTER_OFFSET_MS);
  });

  it("reduces the delay modulo TWO periods, because the rule alternates", () => {
    // Arrange — index 10 is exactly one period along; a one-period modulus
    // would fold it back to zero and flip the bulge's direction.
    const wave = new StatusWave();

    // Act
    const got = wave.delayMs(NOW, 10);

    // Assert
    expect(got).toBe(STATUS_WAVE_PERIOD_MS);
  });

  it("emits a reverse-pass delay on a redraw during the backward half, so the sweep reverses", () => {
    // Arrange — the epoch is stamped, then the footer redraws one period plus a
    // bit later: that instant is in the animation's BACKWARD iteration.
    const wave = new StatusWave();
    wave.delayMs(NOW, 0);

    // Act — the redraw's emitted delay for the head letter.
    const got = wave.delayMs(NOW + STATUS_WAVE_PERIOD_MS + 500, 0);

    // Assert — >= one period, which seeks the rebuilt letter into the reverse
    // (odd-parity) iteration rather than folding it back onto the forward pass.
    expect(got).toBe(STATUS_WAVE_PERIOD_MS + 500);
    expect(got).toBeGreaterThanOrEqual(STATUS_WAVE_PERIOD_MS);
  });

  it("wraps at the end of the forward-and-back cycle", () => {
    // Arrange
    const wave = new StatusWave();
    wave.delayMs(NOW, 0);

    // Act
    const got = wave.delayMs(NOW + STATUS_WAVE_CYCLE_MS, 0);

    // Assert
    expect(got).toBe(0);
  });

  it("carries the elapsed time into every letter, so a redraw continues the wave", () => {
    // Arrange
    const wave = new StatusWave();
    wave.delayMs(NOW, 0);

    // Act
    const got = wave.delayMs(NOW + 900, 2);

    // Assert
    expect(got).toBe(900 + 2 * STATUS_WAVE_LETTER_OFFSET_MS);
  });

  it("floors the wave at zero when the clock goes backwards", () => {
    // Arrange
    const wave = new StatusWave();
    wave.delayMs(NOW, 0);

    // Act
    const got = wave.delayMs(NOW - 5000, 0);

    // Assert
    expect(got).toBe(0);
  });
});

describe("StatusWave: the footer status word's slower colour sweep", () => {
  it("runs the colour period 40% slower than the letter wave, in whole milliseconds", () => {
    // Arrange / Act / Assert — exactly 3640, matching the CSS's 3.64s.
    expect(STATUS_COLOR_PERIOD_MS).toBe(3640);
    expect(STATUS_COLOR_PERIOD_MS / STATUS_WAVE_PERIOD_MS).toBeCloseTo(1.4);
  });

  it("reduces the colour delay modulo TWO colour periods, because the rule alternates", () => {
    // Arrange
    const wave = new StatusWave();
    wave.colorDelayMs(NOW);

    // Act
    const got = wave.colorDelayMs(NOW + STATUS_COLOR_PERIOD_MS + 500);

    // Assert — still on the backward pass, not folded onto the forward one.
    expect(got).toBe(STATUS_COLOR_PERIOD_MS + 500);
  });

  it("wraps at the end of the colour sweep's forward-and-back cycle", () => {
    // Arrange
    const wave = new StatusWave();
    wave.colorDelayMs(NOW);

    // Act
    const got = wave.colorDelayMs(NOW + STATUS_COLOR_CYCLE_MS);

    // Assert
    expect(got).toBe(0);
  });

  it("shares the letter wave's epoch, whichever of the two is read first", () => {
    // Arrange — the colour sweep stamps the epoch.
    const wave = new StatusWave();
    wave.colorDelayMs(NOW);

    // Act
    const got = wave.delayMs(NOW + 900, 0);

    // Assert
    expect(got).toBe(900);
  });

  it("floors the colour delay at zero when the clock goes backwards", () => {
    // Arrange
    const wave = new StatusWave();
    wave.colorDelayMs(NOW);

    // Act
    const got = wave.colorDelayMs(NOW - 5000);

    // Assert
    expect(got).toBe(0);
  });
});

describe("statusColorStyle and statusWaveStyle: the inline delays", () => {
  it.each([
    ["the colour sweep", () => statusColorStyle(NOW)],
    ["a letter's bulge", () => statusWaveStyle(3, NOW)],
  ])("renders %s as one whole-millisecond negative animation-delay", (_name, style) => {
    // Arrange / Act
    const got = style();

    // Assert — both share one style shape.
    expect(got).toMatch(/^animation-delay:-\d+ms$/);
  });
});

describe("statusWordWaves: which statuses mean progress", () => {
  it.each([...WAVING_STATUS_ARMS].map((arm) => [arm]))("waves the %s arm", (arm) => {
    // Arrange / Act / Assert
    expect(statusWordWaves(arm)).toBe(true);
  });

  it.each(
    FOOTER_STATUS_CASES.filter((arm) => !WAVING_STATUS_ARMS.has(arm)).map((arm) => [arm]),
  )("holds the %s arm still", (arm) => {
    // Arrange / Act / Assert
    expect(statusWordWaves(arm)).toBe(false);
  });

  it("names only arms the contract declares, so a renamed arm cannot rot the set", () => {
    // Arrange / Act
    const unknown = [...WAVING_STATUS_ARMS].filter((arm) => !FOOTER_STATUS_CASES.includes(arm));

    // Assert
    expect(unknown).toEqual([]);
  });
});
