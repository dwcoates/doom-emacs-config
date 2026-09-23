// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createTicker } from "../../src/clock.js";
import {
  DISCARD_ATTRIBUTE,
  TICKING_ATTRIBUTE,
  onDiscard,
  replaceTicking,
  stopClocks,
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

describe("onDiscard", () => {
  it("marks the element as holding a discard hook", () => {
    // Arrange
    const el = document.createElement("span");

    // Act
    onDiscard(el, () => {});

    // Assert
    expect(el.hasAttribute(DISCARD_ATTRIBUTE)).toBe(true);
  });

  it("never marks the element as ticking, since a hook is not a clock", () => {
    // Arrange
    const el = document.createElement("span");

    // Act
    onDiscard(el, () => {});

    // Assert
    expect(el.hasAttribute(TICKING_ATTRIBUTE)).toBe(false);
  });

  it("runs every hook one element registered when it is discarded", () => {
    // Arrange
    const el = document.createElement("span");
    const ran: string[] = [];
    onDiscard(el, () => ran.push("a"));
    onDiscard(el, () => ran.push("b"));

    // Act
    stopTicking(el);

    // Assert
    expect(ran).toEqual(["a", "b"]);
  });

  it("runs a descendant's hook when its ancestor is discarded", () => {
    // Arrange
    const row = document.createElement("div");
    const box = document.createElement("div");
    row.append(box);
    let ran = 0;
    onDiscard(box, () => (ran += 1));

    // Act
    stopTicking(row);

    // Assert
    expect(ran).toBe(1);
  });

  it("runs a hook once, however often its element is discarded", () => {
    // Arrange
    const el = document.createElement("span");
    let ran = 0;
    onDiscard(el, () => (ran += 1));

    // Act
    stopTicking(el);
    stopTicking(el);

    // Assert
    expect(ran).toBe(1);
  });

  it("clears the marker once the hooks have run", () => {
    // Arrange
    const el = document.createElement("span");
    onDiscard(el, () => {});

    // Act
    stopTicking(el);

    // Assert
    expect(el.hasAttribute(DISCARD_ATTRIBUTE)).toBe(false);
  });

  it("is not counted as a stopped clock by a discard", () => {
    // Arrange
    const el = document.createElement("span");
    onDiscard(el, () => {});

    // Act / Assert
    expect(stopTicking(el)).toBe(0);
  });
});

describe("stopClocks", () => {
  it("unsubscribes a descendant's clock", () => {
    // Arrange
    const row = document.createElement("div");
    const clock = document.createElement("span");
    row.append(clock);
    let ticks = 0;
    tick(clock, createTicker(1000), () => (ticks += 1));

    // Act
    stopClocks(row);
    vi.advanceTimersByTime(5000);

    // Assert
    expect(ticks).toBe(1);
  });

  it("counts the elements whose clocks it stopped", () => {
    // Arrange
    const row = document.createElement("div");
    const clock = document.createElement("span");
    row.append(clock);
    tick(row, createTicker(1000), () => {});
    tick(clock, createTicker(1000), () => {});

    // Act / Assert
    expect(stopClocks(row)).toBe(2);
  });

  it("leaves a discard hook in place, because the element stays on screen", () => {
    // Arrange
    const row = document.createElement("div");
    const box = document.createElement("div");
    row.append(box);
    let ran = 0;
    onDiscard(box, () => (ran += 1));

    // Act
    stopClocks(row);

    // Assert
    expect(ran).toBe(0);
  });

  it("leaves the hook for the discard that follows it", () => {
    // Arrange
    const row = document.createElement("div");
    let ran = 0;
    onDiscard(row, () => (ran += 1));
    stopClocks(row);

    // Act
    stopTicking(row);

    // Assert
    expect(ran).toBe(1);
  });
});
