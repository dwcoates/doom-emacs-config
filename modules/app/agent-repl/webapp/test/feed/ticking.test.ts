// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createTicker } from "../../src/clock.js";
import {
  TICKING_ATTRIBUTE,
  replaceTicking,
  stopTicking,
  tick,
} from "../../src/feed/ticking.js";

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

describe("stopTicking: the count it reports", () => {
  it("counts the element it stopped, so a backstop can say it stopped something", () => {
    const el = document.createElement("span");
    tick(el, createTicker(1000), () => {});
    expect(stopTicking(el)).toBe(1);
  });

  it("counts each marked descendant it stopped", () => {
    const row = document.createElement("div");
    const a = document.createElement("span");
    const b = document.createElement("span");
    row.append(a, b);
    tick(a, createTicker(1000), () => {});
    tick(b, createTicker(1000), () => {});
    expect(stopTicking(row)).toBe(2);
  });

  it("counts nothing when nothing was ticking, which is the quiet case", () => {
    expect(stopTicking(document.createElement("span"))).toBe(0);
  });

  it("counts nothing the second time, so a repeated sweep stays silent", () => {
    const el = document.createElement("span");
    tick(el, createTicker(1000), () => {});
    stopTicking(el);
    expect(stopTicking(el)).toBe(0);
  });
});

describe("replaceTicking", () => {
  it("stops a child it drops", () => {
    const host = document.createElement("div");
    const dropped = document.createElement("span");
    host.append(dropped);
    let ticks = 0;
    tick(dropped, createTicker(1000), () => (ticks += 1));
    replaceTicking(host, [document.createElement("span")]);
    vi.advanceTimersByTime(5000);
    expect(ticks).toBe(1);
  });

  it("keeps a child it re-places, because that child was MOVED and not discarded", () => {
    const host = document.createElement("div");
    const kept = document.createElement("span");
    host.append(kept);
    let ticks = 0;
    tick(kept, createTicker(1000), () => (ticks += 1));
    replaceTicking(host, [kept]);
    vi.advanceTimersByTime(2000);
    expect(ticks).toBe(3);
  });

  it("stops a clock held by a DESCENDANT of a dropped child", () => {
    const host = document.createElement("div");
    const dropped = document.createElement("div");
    const clock = document.createElement("span");
    dropped.append(clock);
    host.append(dropped);
    let ticks = 0;
    tick(clock, createTicker(1000), () => (ticks += 1));
    replaceTicking(host, []);
    vi.advanceTimersByTime(5000);
    expect(ticks).toBe(1);
  });

  it("reports how many dropped children were still ticking", () => {
    const host = document.createElement("div");
    const dropped = document.createElement("span");
    host.append(dropped);
    tick(dropped, createTicker(1000), () => {});
    expect(replaceTicking(host, [])).toBe(1);
  });

  it("empties the host when no replacement is given", () => {
    const host = document.createElement("div");
    host.append(document.createElement("span"));
    replaceTicking(host);
    expect(host.children).toHaveLength(0);
  });
});
