// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createTicker } from "../../src/clock.js";
import { TICKING_ATTRIBUTE, stopTicking, tick } from "../../src/feed/ticking.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

describe("tick", () => {
  it("runs once immediately, so the element never paints blank", () => {
    const el = document.createElement("span");
    const seen: number[] = [];
    tick(el, createTicker(1000), (nowMs) => seen.push(nowMs));
    expect(seen).toHaveLength(1);
  });

  it("runs again on every tick of the shared clock", () => {
    const el = document.createElement("span");
    const seen: number[] = [];
    tick(el, createTicker(1000), (nowMs) => seen.push(nowMs));
    vi.advanceTimersByTime(2000);
    expect(seen).toHaveLength(3);
  });

  it("marks the element, so whoever discards it can find the subscription", () => {
    const el = document.createElement("span");
    tick(el, createTicker(1000), () => {});
    expect(el.hasAttribute(TICKING_ATTRIBUTE)).toBe(true);
  });

  it("holds several subscriptions on one element", () => {
    const el = document.createElement("span");
    const ticker = createTicker(1000);
    let a = 0;
    let b = 0;
    tick(el, ticker, () => (a += 1));
    tick(el, ticker, () => (b += 1));
    vi.advanceTimersByTime(1000);
    expect([a, b]).toEqual([2, 2]);
  });
});

describe("stopTicking", () => {
  it("unsubscribes the element itself", () => {
    const el = document.createElement("span");
    let ticks = 0;
    tick(el, createTicker(1000), () => (ticks += 1));
    stopTicking(el);
    vi.advanceTimersByTime(5000);
    expect(ticks).toBe(1);
  });

  it("unsubscribes marked descendants, which is where a row's clocks live", () => {
    const row = document.createElement("div");
    const clock = document.createElement("span");
    row.append(clock);
    let ticks = 0;
    tick(clock, createTicker(1000), () => (ticks += 1));
    stopTicking(row);
    vi.advanceTimersByTime(5000);
    expect(ticks).toBe(1);
  });

  it("clears the marker, so a re-drawn element is not mistaken for a live one", () => {
    const el = document.createElement("span");
    tick(el, createTicker(1000), () => {});
    stopTicking(el);
    expect(el.hasAttribute(TICKING_ATTRIBUTE)).toBe(false);
  });

  it("is inert on an element that never subscribed", () => {
    const el = document.createElement("span");
    expect(() => stopTicking(el)).not.toThrow();
  });
});
