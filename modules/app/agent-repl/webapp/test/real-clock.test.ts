/**
 * `realNow` (test/real-clock.ts) — the one real clock the suites read.
 */
import { afterEach, describe, expect, it, vi } from "vitest";

import { realNow, realSetImmediate } from "./real-clock";

afterEach(() => {
  vi.useRealTimers();
});

describe("realNow", () => {
  it("does not move when the page's fake clock is advanced", () => {
    // Arrange
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "Date"] });
    const before = realNow();

    // Act
    vi.advanceTimersByTime(60_000);

    // Assert: real time passed, but nowhere near the minute the page's clock did.
    expect(realNow() - before).toBeLessThan(60_000);
  });

  it("moves forward with real time", async () => {
    // Arrange
    const before = realNow();

    // Act: one real turn of the event loop.
    await new Promise<void>((resolve) => setImmediate(resolve));

    // Assert
    expect(realNow()).toBeGreaterThanOrEqual(before);
  });
});

describe("realSetImmediate", () => {
  it("runs with the page's fake clock installed and never advanced", async () => {
    // Arrange
    vi.useFakeTimers({ toFake: ["setImmediate", "clearImmediate"] });
    let ran = false;

    // Act
    await new Promise<void>((resolve) =>
      realSetImmediate(() => {
        ran = true;
        resolve();
      }),
    );

    // Assert
    expect(ran).toBe(true);
  });
});
