/**
 * `realNow` (test/real-clock.ts) — the one real clock the suites read.
 */
import { readFileSync } from "node:fs";
import { relative, resolve } from "node:path";
import { afterEach, describe, expect, it, vi } from "vitest";

import { filesUnder } from "./files-under";
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

  it("is the only real clock the suites capture", () => {
    // Arrange
    const root = resolve(process.cwd(), "test");

    // Act: every capture of the real clock outside this module.
    const captures = filesUnder(root)
      .filter((path) => !path.endsWith("real-clock.ts") && !path.endsWith("real-clock.test.ts"))
      .filter((path) => readFileSync(path, "utf8").includes("performance.now.bind("))
      .map((path) => relative(root, path));

    // Assert: a second hand-rolled capture would drift from this one.
    expect(captures).toEqual([]);
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

  it("is the only real setImmediate the suites capture", () => {
    // Arrange
    const root = resolve(process.cwd(), "test");

    // Act
    const captures = filesUnder(root)
      .filter((path) => !path.endsWith("real-clock.ts") && !path.endsWith("real-clock.test.ts"))
      .filter((path) => /=\s*globalThis\.setImmediate\b/.test(readFileSync(path, "utf8")))
      .map((path) => relative(root, path));

    // Assert
    expect(captures).toEqual([]);
  });
});
