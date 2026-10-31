// @vitest-environment jsdom
/** The one timer seam (src/timers.ts). */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it, vi } from "vitest";
import { filesUnder } from "./files-under.js";
import { createKeyedTimers, WINDOW_TIMERS } from "../src/timers.js";

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

describe("createKeyedTimers", () => {
  afterEach(() => vi.useRealTimers());

  it("runs a started timer's function once its time comes", () => {
    // ARRANGE
    vi.useFakeTimers();
    const timers = createKeyedTimers<string>();
    const fn = vi.fn();
    // ACT
    timers.start("a", 50, fn);
    vi.advanceTimersByTime(50);
    // ASSERT
    expect(fn).toHaveBeenCalledTimes(1);
  });

  it("replaces the timer a key already had pending", () => {
    // ARRANGE
    vi.useFakeTimers();
    const timers = createKeyedTimers<string>();
    const first = vi.fn();
    const second = vi.fn();
    timers.start("a", 50, first);
    // ACT
    timers.start("a", 50, second);
    vi.runAllTimers();
    // ASSERT
    expect([first.mock.calls.length, second.mock.calls.length]).toEqual([0, 1]);
  });

  it("keeps each key's timer apart", () => {
    // ARRANGE
    vi.useFakeTimers();
    const timers = createKeyedTimers<string>();
    const a = vi.fn();
    const b = vi.fn();
    timers.start("a", 50, a);
    timers.start("b", 50, b);
    // ACT
    timers.cancel("a");
    vi.runAllTimers();
    // ASSERT
    expect([a.mock.calls.length, b.mock.calls.length]).toEqual([0, 1]);
  });

  it("runs nothing for a key cancelled before its time", () => {
    // ARRANGE
    vi.useFakeTimers();
    const timers = createKeyedTimers<string>();
    const fn = vi.fn();
    timers.start("a", 50, fn);
    // ACT
    timers.cancel("a");
    vi.runAllTimers();
    // ASSERT
    expect([fn.mock.calls.length, timers.pending("a"), vi.getTimerCount()]).toEqual([0, false, 0]);
  });

  it("ignores a cancel for a key with nothing pending", () => {
    // ARRANGE
    const timers = createKeyedTimers<string>();
    // ACT
    timers.cancel("a");
    // ASSERT
    expect(timers.pending("a")).toBe(false);
  });

  it("cancels every pending timer at once", () => {
    // ARRANGE
    vi.useFakeTimers();
    const timers = createKeyedTimers<string>();
    const fn = vi.fn();
    timers.start("a", 50, fn);
    timers.start("b", 50, fn);
    // ACT
    timers.cancelAll();
    vi.runAllTimers();
    // ASSERT
    expect([fn.mock.calls.length, timers.pending("a"), timers.pending("b")]).toEqual([0, false, false]);
  });

  it("forgets a timer before its function runs", () => {
    // ARRANGE
    vi.useFakeTimers();
    const timers = createKeyedTimers<string>();
    let pendingInside: boolean | undefined;
    timers.start("a", 50, () => {
      pendingInside = timers.pending("a");
    });
    // ACT
    vi.advanceTimersByTime(50);
    // ASSERT
    expect(pendingInside).toBe(false);
  });

  it("schedules on the timers it was handed", () => {
    // ARRANGE
    const scheduled: number[] = [];
    const timers = createKeyedTimers<string>({
      setTimeout: (_fn, ms) => scheduled.push(ms),
      clearTimeout: () => undefined,
    });
    // ACT
    timers.start("a", 70, () => undefined);
    // ASSERT
    expect(scheduled).toEqual([70]);
  });
});

describe("the timer seam's call sites", () => {
  it("hold no map of timer handles of their own beside createKeyedTimers", () => {
    const src = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../src");
    const own = filesUnder(src, [".ts", ".tsx"]).filter((file) =>
      /new Map<[^>]*ReturnType<typeof setTimeout>>/.test(readFileSync(file, "utf8")),
    );
    expect(own.map((file) => path.relative(src, file))).toEqual([]);
  });

  it("declare no timer seam of their own beside src/timers.ts", () => {
    const src = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../src");
    const own = filesUnder(src, [".ts", ".tsx"]).filter(
      (file) => !file.endsWith(`${path.sep}timers.ts`) && /setTimeout\(fn: \(\) => void, ms: number\): number;/.test(readFileSync(file, "utf8")),
    );
    expect(own.map((file) => path.relative(src, file))).toEqual([]);
  });
});
