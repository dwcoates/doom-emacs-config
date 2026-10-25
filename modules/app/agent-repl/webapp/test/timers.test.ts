// @vitest-environment jsdom
/** The one timer seam (src/timers.ts). */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it, vi } from "vitest";
import { filesUnder } from "./files-under.js";
import { WINDOW_TIMERS } from "../src/timers.js";

describe("WINDOW_TIMERS", () => {
  afterEach(() => vi.useRealTimers());

  it("runs a scheduled function once its time comes", () => {
    vi.useFakeTimers();
    const fn = vi.fn();
    WINDOW_TIMERS.setTimeout(fn, 50);
    vi.advanceTimersByTime(50);
    expect(fn).toHaveBeenCalledTimes(1);
  });

  it("runs nothing it was told to clear", () => {
    vi.useFakeTimers();
    const fn = vi.fn();
    WINDOW_TIMERS.clearTimeout(WINDOW_TIMERS.setTimeout(fn, 50));
    vi.advanceTimersByTime(50);
    expect(fn).not.toHaveBeenCalled();
  });
});

describe("the timer seam's call sites", () => {
  it("declare no timer seam of their own beside src/timers.ts", () => {
    const src = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../src");
    const own = filesUnder(src, [".ts", ".tsx"]).filter(
      (file) => !file.endsWith(`${path.sep}timers.ts`) && /setTimeout\(fn: \(\) => void, ms: number\): number;/.test(readFileSync(file, "utf8")),
    );
    expect(own.map((file) => path.relative(src, file))).toEqual([]);
  });
});
