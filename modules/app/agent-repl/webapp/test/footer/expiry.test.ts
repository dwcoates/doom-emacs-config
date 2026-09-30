import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createTicker } from "../../src/clock.js";
import { MAX_TIMEOUT_MS, createTransientExpiry } from "../../src/footer/expiry.js";

const NOW = 1_800_000_000_000;

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(NOW);
});
afterEach(() => {
  vi.useRealTimers();
});

/** A timer and the count of re-renders it has asked for. */
function expiryUnderTest(): { expiry: ReturnType<typeof createTransientExpiry>; fired: () => number } {
  let count = 0;
  const expiry = createTransientExpiry(createTicker(1000), () => {
    count += 1;
  });
  return { expiry, fired: () => count };
}

describe("createTransientExpiry", () => {
  it("re-renders at the scheduled instant", () => {
    // Arrange
    const { expiry, fired } = expiryUnderTest();
    expiry.schedule(NOW + 10_000);
    // Act
    vi.advanceTimersByTime(10_000);
    // Assert
    expect(fired()).toBe(1);
  });

  it("does not re-render before the scheduled instant", () => {
    const { expiry, fired } = expiryUnderTest();
    expiry.schedule(NOW + 10_000);
    vi.advanceTimersByTime(9_999);
    expect(fired()).toBe(0);
  });

  it("re-renders once, not on every interval after it", () => {
    const { expiry, fired } = expiryUnderTest();
    expiry.schedule(NOW + 10_000);
    vi.advanceTimersByTime(60_000);
    expect(fired()).toBe(1);
  });

  it("replaces a pending re-render rather than stacking a second one", () => {
    // Arrange
    const { expiry, fired } = expiryUnderTest();
    expiry.schedule(NOW + 5_000);
    // Act
    expiry.schedule(NOW + 10_000);
    vi.advanceTimersByTime(5_000);
    // Assert: the first instant passed and nothing fired.
    expect(fired()).toBe(0);
  });

  it("fires the replacement at its own instant", () => {
    const { expiry, fired } = expiryUnderTest();
    expiry.schedule(NOW + 5_000);
    expiry.schedule(NOW + 10_000);
    vi.advanceTimersByTime(10_000);
    expect(fired()).toBe(1);
  });

  it("drops the pending re-render on cancel", () => {
    const { expiry, fired } = expiryUnderTest();
    expiry.schedule(NOW + 10_000);
    expiry.cancel();
    vi.advanceTimersByTime(10_000);
    expect(fired()).toBe(0);
  });

  it("re-renders at once for an instant already past", () => {
    const { expiry, fired } = expiryUnderTest();
    expiry.schedule(NOW - 1_000);
    vi.advanceTimersByTime(0);
    expect(fired()).toBe(1);
  });

  it("reports a re-render pending while one is scheduled", () => {
    const { expiry } = expiryUnderTest();
    expiry.schedule(NOW + 10_000);
    expect(expiry.pending()).toBe(true);
  });

  it("reports nothing pending once the re-render has fired", () => {
    const { expiry } = expiryUnderTest();
    expiry.schedule(NOW + 10_000);
    vi.advanceTimersByTime(10_000);
    expect(expiry.pending()).toBe(false);
  });

  it("treats a cancel with nothing pending as a no-op", () => {
    const { expiry } = expiryUnderTest();
    expiry.cancel();
    expect(expiry.pending()).toBe(false);
  });

  it("clamps an instant past the timer's range rather than firing at once", () => {
    // Arrange: an unclamped delay this long overflows and fires immediately.
    const { expiry, fired } = expiryUnderTest();
    expiry.schedule(NOW + MAX_TIMEOUT_MS * 4);
    // Act
    vi.advanceTimersByTime(1_000);
    // Assert
    expect(fired()).toBe(0);
  });

  it("fires a clamped instant at the end of the timer's range", () => {
    const { expiry, fired } = expiryUnderTest();
    expiry.schedule(NOW + MAX_TIMEOUT_MS * 4);
    vi.advanceTimersByTime(MAX_TIMEOUT_MS);
    expect(fired()).toBe(1);
  });
});
