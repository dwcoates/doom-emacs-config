import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { DEFAULT_TICK_MS, createTicker } from "../src/clock.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

describe("createTicker", () => {
  it("ticks a subscriber once per interval", () => {
    // ARRANGE
    const ticker = createTicker(1000);
    const fn = vi.fn();
    ticker.subscribe(fn);
    // ACT
    vi.advanceTimersByTime(1000);
    // ASSERT
    expect(fn).toHaveBeenCalledOnce();
  });

  it("ticks repeatedly, not once", () => {
    const ticker = createTicker(1000);
    const fn = vi.fn();
    ticker.subscribe(fn);
    vi.advanceTimersByTime(3000);
    expect(fn).toHaveBeenCalledTimes(3);
  });

  it("does not tick before its interval elapses", () => {
    const ticker = createTicker(1000);
    const fn = vi.fn();
    ticker.subscribe(fn);
    vi.advanceTimersByTime(999);
    expect(fn).not.toHaveBeenCalled();
  });

  it("hands the subscriber the current instant", () => {
    vi.setSystemTime(new Date(1_700_000_000_000));
    const ticker = createTicker(1000);
    const seen: number[] = [];
    ticker.subscribe((now) => seen.push(now));
    vi.advanceTimersByTime(1000);
    expect(seen).toEqual([1_700_000_001_000]);
  });

  it("hands EVERY subscriber the same instant, so the page steps together", () => {
    const ticker = createTicker(1000);
    const a: number[] = [];
    const b: number[] = [];
    ticker.subscribe((now) => a.push(now));
    ticker.subscribe((now) => b.push(now));
    vi.advanceTimersByTime(1000);
    expect(a).toEqual(b);
  });

  it("runs ONE interval for many subscribers", () => {
    const spy = vi.spyOn(globalThis, "setInterval");
    const ticker = createTicker(1000);
    ticker.subscribe(() => {});
    ticker.subscribe(() => {});
    ticker.subscribe(() => {});
    expect(spy).toHaveBeenCalledOnce();
  });

  it("starts no interval before the first subscriber", () => {
    const spy = vi.spyOn(globalThis, "setInterval");
    createTicker(1000);
    expect(spy).not.toHaveBeenCalled();
  });

  it("stops ticking a subscriber that unsubscribed", () => {
    const ticker = createTicker(1000);
    const fn = vi.fn();
    ticker.subscribe(fn)();
    vi.advanceTimersByTime(3000);
    expect(fn).not.toHaveBeenCalled();
  });

  it("keeps ticking the others when one unsubscribes", () => {
    const ticker = createTicker(1000);
    const kept = vi.fn();
    ticker.subscribe(() => {})();
    ticker.subscribe(kept);
    vi.advanceTimersByTime(1000);
    expect(kept).toHaveBeenCalledOnce();
  });

  it("stops the interval when the LAST subscriber leaves", () => {
    const spy = vi.spyOn(globalThis, "clearInterval");
    const ticker = createTicker(1000);
    ticker.subscribe(() => {})();
    expect(spy).toHaveBeenCalledOnce();
  });

  it("does not stop the interval while a subscriber remains", () => {
    const ticker = createTicker(1000);
    const kept = vi.fn();
    ticker.subscribe(kept);
    ticker.subscribe(() => {})();
    vi.advanceTimersByTime(1000);
    expect(kept).toHaveBeenCalledOnce();
  });

  it("restarts the interval when a subscriber arrives after the last one left", () => {
    const ticker = createTicker(1000);
    ticker.subscribe(() => {})();
    const fn = vi.fn();
    ticker.subscribe(fn);
    vi.advanceTimersByTime(1000);
    expect(fn).toHaveBeenCalledOnce();
  });

  it("ignores a second call to the same unsubscriber", () => {
    const ticker = createTicker(1000);
    const kept = vi.fn();
    const unsubscribe = ticker.subscribe(() => {});
    ticker.subscribe(kept);
    unsubscribe();
    unsubscribe();
    vi.advanceTimersByTime(1000);
    expect(kept).toHaveBeenCalledOnce();
  });

  it("survives a subscriber that unsubscribes from inside its own tick", () => {
    const ticker = createTicker(1000);
    const unsubscribe = ticker.subscribe(() => unsubscribe());
    expect(() => vi.advanceTimersByTime(1000)).not.toThrow();
  });

  it("reads the current instant without waiting for a tick", () => {
    vi.setSystemTime(new Date(1_700_000_000_000));
    expect(createTicker(1000).now()).toBe(1_700_000_000_000);
  });

  it("defaults to a one-second step, the finest unit anything renders", () => {
    const ticker = createTicker();
    const fn = vi.fn();
    ticker.subscribe(fn);
    vi.advanceTimersByTime(DEFAULT_TICK_MS);
    expect(fn).toHaveBeenCalledOnce();
  });
});
