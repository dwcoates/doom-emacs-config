// @vitest-environment jsdom
// Each page's tally of where its callback time goes. See src/busy-probe.ts.
import { afterEach, describe, expect, it } from "vitest";

import { BUSY_TOP, installBusyProbe, type BusyProbe, type BusyWindow } from "../src/busy-probe.js";

/** A window whose registrations hand their callbacks back to the test to run. */
function fakeWindow(): BusyWindow & { run: () => void } {
  const pending: (() => void)[] = [];
  const later = (fn: () => void): number => pending.push(fn);
  return {
    setTimeout: ((handler: TimerHandler) => later(() => (handler as () => void)())) as Window["setTimeout"],
    setInterval: ((handler: TimerHandler) => later(() => (handler as () => void)())) as Window["setInterval"],
    requestAnimationFrame: (callback: FrameRequestCallback) => later(() => callback(0)),
    MutationObserver: window.MutationObserver,
    ResizeObserver: class {
      constructor(private readonly callback: ResizeObserverCallback) {
        pending.push(() => this.callback([], this));
      }
      observe(): void {}
      unobserve(): void {}
      disconnect(): void {}
    },
    run: () => {
      for (const fn of pending.splice(0)) fn();
    },
  };
}

/** A clock that advances STEP ms on every read. */
function steppingClock(step: number): () => number {
  let t = 0;
  return () => {
    const at = t;
    t += step;
    return at;
  };
}

let probe: BusyProbe | undefined;
afterEach(() => probe?.stop());

describe("installBusyProbe", () => {
  it.each([
    {
      kind: "timer",
      register: (win: BusyWindow) => {
        win.setTimeout(function tick() {}, 10);
      },
      label: "tick",
    },
    {
      kind: "timer",
      register: (win: BusyWindow) => {
        win.setInterval(function step() {}, 1000);
      },
      label: "step",
    },
    {
      kind: "frame",
      register: (win: BusyWindow) => {
        win.requestAnimationFrame(function paint() {});
      },
      label: "paint",
    },
  ])("tallies a $kind callback under its function's name", ({ kind, register, label }) => {
    // Arrange
    const win = fakeWindow();
    probe = installBusyProbe(win, steppingClock(4));
    register(win);

    // Act
    win.run();
    const report = probe.take();

    // Assert
    expect([report.kinds[kind as "timer" | "frame"], report.top]).toEqual([
      { count: 1, ms: 4 },
      [{ kind, label, count: 1, ms: 4 }],
    ]);
  });

  it("names an unnamed callback anonymous", () => {
    // Arrange
    const win = fakeWindow();
    probe = installBusyProbe(win, steppingClock(1));
    win.setTimeout(() => undefined, 0);

    // Act
    win.run();

    // Assert
    expect(probe.take().top.map((t) => t.label)).toEqual(["anonymous"]);
  });

  it("tallies a MutationObserver callback under the element it first observes", async () => {
    // Arrange
    const win = fakeWindow();
    probe = installBusyProbe(win, steppingClock(2));
    const box = document.createElement("div");
    box.className = "bubble md";
    const observer = new win.MutationObserver(() => undefined);
    observer.observe(box, { childList: true });

    // Act
    box.append(document.createElement("p"));
    await Promise.resolve();
    observer.disconnect();

    // Assert
    expect(probe.take().top).toEqual([{ kind: "mutation", label: "div.bubble.md", count: 1, ms: 2 }]);
  });

  it("tallies a ResizeObserver callback under the element it first observes", () => {
    // Arrange
    const win = fakeWindow();
    probe = installBusyProbe(win, steppingClock(3));
    const box = document.createElement("section");
    new win.ResizeObserver(() => undefined).observe(box);

    // Act
    win.run();

    // Assert
    expect(probe.take().top).toEqual([{ kind: "resize", label: "section", count: 1, ms: 3 }]);
  });

  it("starts the next tally at every take", () => {
    // Arrange
    const win = fakeWindow();
    probe = installBusyProbe(win, steppingClock(1));
    win.setTimeout(() => undefined, 0);
    win.run();
    probe.take();

    // Act
    const next = probe.take();

    // Assert
    expect([next.busy_ms, next.top]).toEqual([0, []]);
  });

  it("tallies a callback that throws, and lets the throw through", () => {
    // Arrange
    const win = fakeWindow();
    probe = installBusyProbe(win, steppingClock(5));
    win.setTimeout(function boom() {
      throw new Error("boom");
    }, 0);

    // Act
    const run = (): void => win.run();

    // Assert
    expect(run).toThrow("boom");
    expect(probe.take().kinds.timer).toEqual({ count: 1, ms: 5 });
  });

  it("passes a string handler through untimed", () => {
    // Arrange
    const seen: TimerHandler[] = [];
    const win = fakeWindow();
    win.setTimeout = ((handler: TimerHandler) => seen.push(handler)) as Window["setTimeout"];
    probe = installBusyProbe(win, steppingClock(1));

    // Act
    win.setTimeout("noop()", 0);

    // Assert
    expect(seen).toEqual(["noop()"]);
  });

  it("names at most BUSY_TOP labels, costliest first", () => {
    // Arrange: each callback costs as many ms as its index plus one.
    const win = fakeWindow();
    let t = 0;
    let spent = 0;
    probe = installBusyProbe(win, () => {
      t += spent;
      spent = 0;
      return t;
    });
    for (let i = 0; i < BUSY_TOP + 2; i += 1) {
      const label = `cb${i}`;
      win.setTimeout({ [label]: () => void (spent = i + 1) }[label], 0);
    }

    // Act
    win.run();
    const top = probe.take().top;

    // Assert
    expect(top.map((tally) => [tally.label, tally.ms])).toEqual(
      Array.from({ length: BUSY_TOP }, (_, k) => [`cb${BUSY_TOP + 1 - k}`, BUSY_TOP + 2 - k]),
    );
  });

  it("puts every wrapped global back on stop", () => {
    // Arrange
    const win = fakeWindow();
    const before = { ...win };
    const installed = installBusyProbe(win, steppingClock(1));

    // Act
    installed.stop();

    // Assert
    expect([win.setTimeout, win.setInterval, win.requestAnimationFrame, win.MutationObserver, win.ResizeObserver]).toEqual([
      before.setTimeout,
      before.setInterval,
      before.requestAnimationFrame,
      before.MutationObserver,
      before.ResizeObserver,
    ]);
  });
});
