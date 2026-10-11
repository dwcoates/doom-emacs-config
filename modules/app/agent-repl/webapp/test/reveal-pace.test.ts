import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedResponseRevealPaceSchema } from "../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../src/rpc/malformed.js";
import {
  FOLLOW_SECONDS,
  advanceReveal,
  revealPaceOf,
  revealSlice,
  revealSpeed,
  type RevealPace,
} from "../src/reveal-pace.js";

/** The thinking pace the daemon serves by default: 45-55 chars/sec, 1.5s bank. */
const PACE: RevealPace = { minCps: 45, maxCps: 55, bankLimitSeconds: 1.5 };

describe("revealPaceOf", () => {
  it("answers no pace for a draw that carries none", () => {
    // Act / Assert
    expect(revealPaceOf(undefined, "p")).toBeUndefined();
  });

  it("reads the bounds and the bank limit in seconds", () => {
    // Arrange
    const wire = create(FeedResponseRevealPaceSchema, { minCharsPerSecond: 45, maxCharsPerSecond: 55, bankLimitMs: 1500 });
    // Act
    const pace = revealPaceOf(wire, "p");
    // Assert
    expect(pace).toEqual(PACE);
  });

  it.each([
    ["a zero minimum", { minCharsPerSecond: 0, maxCharsPerSecond: 55, bankLimitMs: 1500 }, "p.min_chars_per_second"],
    ["a zero maximum", { minCharsPerSecond: 0, maxCharsPerSecond: 0, bankLimitMs: 1500 }, "p.min_chars_per_second"],
    ["a maximum of zero above a minimum", { minCharsPerSecond: 5, maxCharsPerSecond: 0, bankLimitMs: 1500 }, "p.max_chars_per_second"],
    ["a minimum above the maximum", { minCharsPerSecond: 60, maxCharsPerSecond: 55, bankLimitMs: 1500 }, "p.min_chars_per_second"],
    ["a zero bank limit", { minCharsPerSecond: 45, maxCharsPerSecond: 55, bankLimitMs: 0 }, "p.bank_limit_ms"],
  ])("refuses %s as malformed", (_name, init, field) => {
    // Arrange
    const wire = create(FeedResponseRevealPaceSchema, init);
    // Act
    const read = (): RevealPace | undefined => revealPaceOf(wire, "p");
    // Assert
    expect(read).toThrow(MalformedView);
    expect(read).toThrow(field);
  });
});

describe("revealSpeed", () => {
  it("is zero for an empty bank", () => {
    // Act / Assert
    expect(revealSpeed(0, PACE)).toBe(0);
  });

  it("follows the arrival speed a steady bank stands for, within the bounds", () => {
    // Arrange: text arriving at 50 chars/sec keeps a bank of 50 × FOLLOW_SECONDS.
    const bank = 50 * FOLLOW_SECONDS;
    // Act / Assert
    expect(revealSpeed(bank, PACE)).toBeCloseTo(50);
  });

  it("never types slower than the minimum while the bank holds text", () => {
    // Act / Assert: one character banked would follow at 3.3 chars/sec.
    expect(revealSpeed(1, PACE)).toBe(45);
  });

  it("types at the maximum when the bank stands for a faster arrival", () => {
    // Act / Assert: 30 banked would follow at 100 chars/sec.
    expect(revealSpeed(30, PACE)).toBe(55);
  });

  it("stays at the maximum up to the bank limit", () => {
    // Arrange: exactly the limit's worth of typing at the maximum.
    const bank = 55 * 1.5;
    // Act / Assert
    expect(revealSpeed(bank, PACE)).toBe(55);
  });

  it("goes past the maximum by a bank's excess over its limit, caught up on the follow time", () => {
    // Act / Assert: 165 banked is 82.5 past the 82.5-character limit.
    expect(revealSpeed(165, PACE)).toBeCloseTo(55 + 82.5 / FOLLOW_SECONDS);
  });

  it("types a whole block landing at once out within a few seconds", () => {
    // Arrange: 3200 characters arrive in one push.
    let shown = 0;
    let elapsed = 0;
    // Act: frames of 16ms until done.
    while (shown < 3200) {
      shown = advanceReveal(shown, 3200, 16, PACE);
      elapsed += 16;
    }
    // Assert: the excess is caught up quickly and the last limit's worth types at the maximum.
    expect(elapsed).toBeLessThan(4000);
  });
});

describe("advanceReveal", () => {
  it("steps by one frame at the bank's speed", () => {
    // Act: 60 banked types at the maximum, 55 chars/sec, for 100ms.
    const shown = advanceReveal(0, 60, 100, PACE);
    // Assert
    expect(shown).toBeCloseTo(5.5);
  });

  it("never steps past what has arrived", () => {
    // Act / Assert
    expect(advanceReveal(98, 100, 1000, PACE)).toBe(100);
  });

  it("leaves a caught-up reveal where it is", () => {
    // Act / Assert
    expect(advanceReveal(100, 100, 50, PACE)).toBe(100);
  });

  it("does not move for a frame of no time", () => {
    // Act / Assert
    expect(advanceReveal(10, 100, -5, PACE)).toBe(10);
  });

  it("finishes a block the stream outran at the maximum, with no burst at the end", () => {
    // Arrange: 80 chars banked at the settle, within the 82.5-char limit.
    let shown = 0;
    const speeds: number[] = [];
    // Act: frames of 16ms until done.
    while (shown < 80) {
      speeds.push(revealSpeed(80 - shown, PACE));
      shown = advanceReveal(shown, 80, 16, PACE);
    }
    // Assert: never faster than the maximum.
    expect(Math.max(...speeds)).toBeLessThanOrEqual(55);
  });

  it("holds a stream faster than the maximum just past the bank limit", () => {
    // Arrange: text arriving at 100 chars/sec for 10s, in 16ms frames.
    let shown = 0;
    let arrived = 0;
    // Act
    for (let t = 0; t < 10_000; t += 16) {
      arrived += 100 * 0.016;
      shown = advanceReveal(shown, arrived, 16, PACE);
    }
    // Assert: the bank settles just past its 82.5-character limit.
    const bank = arrived - shown;
    expect(bank).toBeGreaterThan(82.5);
    expect(bank).toBeLessThan(100);
  });
});

describe("revealSlice", () => {
  it("reveals nothing for a zero count", () => {
    // Arrange / Act / Assert
    expect(revealSlice("hello", 0)).toBe("");
  });

  it("reveals the whole string once the count reaches its length", () => {
    // Arrange / Act / Assert
    expect(revealSlice("hello", 5)).toBe("hello");
  });

  it("reveals a leading prefix for a mid-string count", () => {
    // Arrange / Act / Assert
    expect(revealSlice("hello", 3)).toBe("hel");
  });

  it("drops a high surrogate rather than cut a pair in half", () => {
    // Arrange — "a😀b": the emoji is a UTF-16 surrogate pair at units 1..2.
    // Act — cut right between the pair's halves.
    const shown = revealSlice("a😀b", 2);
    // Assert — the lone high surrogate is dropped, not shown as a glyph.
    expect(shown).toBe("a");
  });

  it("keeps a surrogate pair whole once its low half is included", () => {
    // Arrange / Act
    const shown = revealSlice("a😀b", 3);
    // Assert
    expect(shown).toBe("a😀");
  });
});

