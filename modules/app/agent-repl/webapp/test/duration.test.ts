import { describe, expect, it } from "vitest";

import {
  formatDuration,
  formatDurationCeil,
  formatElapsed,
} from "../src/duration.js";

describe("formatDuration", () => {
  it("keeps a sub-second duration in whole milliseconds", () => {
    // Arrange + Act + Assert — no finer unit exists to promote a fraction into.
    expect(formatDuration(850)).toBe("850ms");
  });

  it("reports a zero duration in milliseconds", () => {
    // Arrange + Act + Assert
    expect(formatDuration(0)).toBe("0ms");
  });

  it("carries a second's leftover in whole milliseconds", () => {
    // Arrange + Act + Assert — not the fractional 1.03s.
    expect(formatDuration(1033)).toBe("1s 33ms");
  });

  it("drops the leftover off a whole second count", () => {
    // Arrange + Act + Assert
    expect(formatDuration(1000)).toBe("1s");
  });

  it("promotes a millisecond count that rounds up to a full second", () => {
    // Arrange + Act + Assert — 999.6ms would otherwise render as 1000ms.
    expect(formatDuration(999.6)).toBe("1s");
  });

  it("carries a minute's leftover in whole seconds", () => {
    // Arrange + Act + Assert — not the fractional 5.5m.
    expect(formatDuration(330_000)).toBe("5m 30s");
  });

  it("rounds a minute's fractional second leftover to a whole second", () => {
    // Arrange + Act + Assert — 93.6s is 1m plus 33.6s.
    expect(formatDuration(93_600)).toBe("1m 34s");
  });

  it("drops the leftover off a whole minute count", () => {
    // Arrange + Act + Assert
    expect(formatDuration(120_000)).toBe("2m");
  });

  it("carries an hour's leftover in whole minutes", () => {
    // Arrange + Act + Assert — not the fractional 1.5h.
    expect(formatDuration(5_400_000)).toBe("1h 30m");
  });

  it("promotes a leftover that rounds up to a full major unit", () => {
    // Arrange + Act + Assert — 59m 59.999s renders as 1h, never 59m 60s.
    expect(formatDuration(3_599_999)).toBe("1h");
  });

  it("keeps a three-digit hour count in whole hours", () => {
    // Arrange + Act + Assert
    expect(formatDuration(360_000_000)).toBe("100h");
  });
});

describe("formatElapsed", () => {
  it("reports a turn that has not run a whole second yet as zero seconds", () => {
    // Arrange + Act + Assert — `0ms` is the millisecond scale's answer, not
    // the second scale's.
    expect(formatElapsed(0)).toBe("0s");
  });

  it("truncates a part-second rather than rounding it up", () => {
    // Arrange + Act + Assert — 1.9s of elapsed time is 1 whole second of it.
    expect(formatElapsed(1_900)).toBe("1s");
  });

  it("drops the milliseconds a live count would only churn", () => {
    // Arrange + Act + Assert — never the `1s 312ms` the duration scale gives.
    expect(formatElapsed(1_312)).toBe("1s");
  });

  it("carries a minute's leftover in whole seconds", () => {
    // Arrange + Act + Assert — never the fractional 5.5m.
    expect(formatElapsed(330_400)).toBe("5m 30s");
  });

  it("keeps the last second before a minute in seconds", () => {
    // Arrange + Act + Assert
    expect(formatElapsed(59_999)).toBe("59s");
  });

  it("keeps the last second before an hour in minutes and seconds", () => {
    // Arrange + Act + Assert — truncation cannot promote 59m 59s to 1h.
    expect(formatElapsed(3_599_999)).toBe("59m 59s");
  });

  it("drops the seconds once a turn passes an hour", () => {
    // Arrange + Act + Assert — the coarse scale carries the leftover.
    expect(formatElapsed(3_900_000)).toBe("1h 5m");
  });

  it("floors a start stamped ahead of the reader's clock to zero", () => {
    // Arrange + Act + Assert — a skewed clock counts from zero, never backwards.
    expect(formatElapsed(-2_000)).toBe("0s");
  });
});

describe("formatDurationCeil", () => {
  it("rounds a part-second up to the next whole second", () => {
    // Arrange + Act + Assert — 5984ms is closer to six seconds than five.
    expect(formatDurationCeil(5_984)).toBe("6s");
  });

  it("leaves a whole-second count unchanged", () => {
    // Arrange + Act + Assert — nothing to round when the span is exact.
    expect(formatDurationCeil(6_000)).toBe("6s");
  });

  it("never emits a millisecond remainder", () => {
    // Arrange + Act + Assert — 1033ms rounds up rather than reading 1s 33ms.
    expect(formatDurationCeil(1_033)).toBe("2s");
  });

  it("rounds a part-second above a minute up into whole seconds", () => {
    // Arrange + Act + Assert — 90500ms is 1m plus 30.5s, rounded to 31s.
    expect(formatDurationCeil(90_500)).toBe("1m 31s");
  });

  it("counts any positive sub-second span as a whole second", () => {
    // Arrange + Act + Assert — a single millisecond still rounds up to 1s.
    expect(formatDurationCeil(1)).toBe("1s");
  });

  it("floors a zero span to zero seconds", () => {
    // Arrange + Act + Assert — `0ms` is the millisecond scale's answer, not
    // the second scale's.
    expect(formatDurationCeil(0)).toBe("0s");
  });

  it("floors a negative span to zero seconds", () => {
    // Arrange + Act + Assert — a skewed clock counts from zero, never backwards.
    expect(formatDurationCeil(-2_000)).toBe("0s");
  });
});
