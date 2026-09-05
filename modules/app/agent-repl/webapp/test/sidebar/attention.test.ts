// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  ATTENTION_BLINKS,
  ATTENTION_PHASES,
  ATTENTION_PHASE_MS,
  AttentionRegistry,
  WINDOW_TIMERS,
  blinkState,
} from "../../src/sidebar/attention.js";
import { fakeTimers } from "./harness.js";

/** One element standing in for a row drawn by a pass. */
function element(): HTMLElement {
  return document.createElement("div");
}

describe("the cadence frontend.v1.RosterRowAttention specifies", () => {
  it("blinks twice before going steady", () => {
    expect(ATTENTION_BLINKS).toBe(2);
    expect(ATTENTION_PHASES).toBe(4);
  });

  it("holds each phase for half a second", () => {
    expect(ATTENTION_PHASE_MS).toBe(500);
  });

  it.each([
    [0, "on"],
    [1, "off"],
    [2, "on"],
    [3, "off"],
    // The settled marker STANDS LIT: steady is not a third visual state.
    [4, "on"],
    [9, "on"],
  ] as const)("draws phase %i as %s", (phase, state) => {
    expect(blinkState(phase)).toBe(state);
  });

  it.each([
    [3, "false"],
    [4, "true"],
  ] as const)("reports phase %i as settled=%s", (phase, settled) => {
    const timers = fakeTimers();
    const registry = new AttentionRegistry(timers);
    const el = element();
    registry.beginPass();
    registry.mark("ws-1", el);
    registry.endPass();
    for (let i = 0; i < phase; i++) timers.run();
    expect(el.getAttribute("data-settled")).toBe(settled);
  });
});

describe("a marker's first appearance", () => {
  it("starts lit", () => {
    const registry = new AttentionRegistry(fakeTimers());
    const el = element();
    registry.beginPass();
    registry.mark("ws-1", el);
    registry.endPass();
    expect(el.getAttribute("data-blink")).toBe("on");
  });

  it.each([
    [1, "off"],
    [2, "on"],
    [3, "off"],
    [4, "on"],
  ] as const)("is %s after %i phases", (phases, state) => {
    const timers = fakeTimers();
    const registry = new AttentionRegistry(timers);
    const el = element();
    registry.beginPass();
    registry.mark("ws-1", el);
    registry.endPass();
    for (let i = 0; i < phases; i++) timers.run();
    expect(el.getAttribute("data-blink")).toBe(state);
  });

  it("arms no further timer once it is steady", () => {
    const timers = fakeTimers();
    const registry = new AttentionRegistry(timers);
    registry.beginPass();
    registry.mark("ws-1", element());
    registry.endPass();
    for (let i = 0; i < ATTENTION_PHASES; i++) expect(timers.run()).toBe(true);
    expect(timers.run()).toBe(false);
  });
});

describe("a re-push that keeps the marker", () => {
  it("does not restart the cadence", () => {
    const timers = fakeTimers();
    const registry = new AttentionRegistry(timers);
    registry.beginPass();
    registry.mark("ws-1", element());
    registry.endPass();
    timers.run();

    const redrawn = element();
    registry.beginPass();
    registry.mark("ws-1", redrawn);
    registry.endPass();
    expect(redrawn.getAttribute("data-blink")).toBe("off");
  });

  it("reaches steady on the phase it would have anyway", () => {
    const timers = fakeTimers();
    const registry = new AttentionRegistry(timers);
    registry.beginPass();
    registry.mark("ws-1", element());
    registry.endPass();
    timers.run();
    timers.run();

    const redrawn = element();
    registry.beginPass();
    registry.mark("ws-1", redrawn);
    registry.endPass();
    timers.run();
    timers.run();
    expect(redrawn.getAttribute("data-blink")).toBe("on");
    expect(redrawn.getAttribute("data-settled")).toBe("true");
  });

  it("drives every element the pass drew for one workspace", () => {
    const timers = fakeTimers();
    const registry = new AttentionRegistry(timers);
    const inRepoPane = element();
    const inTaskPane = element();
    registry.beginPass();
    registry.mark("ws-1", inRepoPane);
    registry.mark("ws-1", inTaskPane);
    registry.endPass();
    timers.run();
    expect(inRepoPane.getAttribute("data-blink")).toBe("off");
    expect(inTaskPane.getAttribute("data-blink")).toBe("off");
  });
});

describe("a marker the daemon cleared", () => {
  it("is forgotten when the next pass does not carry it", () => {
    const timers = fakeTimers();
    const registry = new AttentionRegistry(timers);
    registry.beginPass();
    registry.mark("ws-1", element());
    registry.endPass();

    registry.beginPass();
    registry.endPass();
    expect(registry.stateOf("ws-1")).toBeNull();
  });

  it("blinks again when it comes back, because it is a new notification", () => {
    const timers = fakeTimers();
    const registry = new AttentionRegistry(timers);
    registry.beginPass();
    registry.mark("ws-1", element());
    registry.endPass();
    for (let i = 0; i < ATTENTION_PHASES; i++) timers.run();

    registry.beginPass();
    registry.endPass();

    const returned = element();
    registry.beginPass();
    registry.mark("ws-1", returned);
    registry.endPass();
    expect(returned.getAttribute("data-blink")).toBe("on");
  });

  it("cancels its pending phase, so no timer outlives it", () => {
    const timers = fakeTimers();
    const registry = new AttentionRegistry(timers);
    registry.beginPass();
    registry.mark("ws-1", element());
    registry.endPass();
    registry.beginPass();
    registry.endPass();
    expect(timers.run()).toBe(false);
  });
});

describe("disposing the registry", () => {
  it("drops every marker", () => {
    const timers = fakeTimers();
    const registry = new AttentionRegistry(timers);
    registry.beginPass();
    registry.mark("ws-1", element());
    registry.endPass();
    registry.dispose();
    expect(registry.stateOf("ws-1")).toBeNull();
  });
});

describe("a marker nothing is drawing", () => {
  it("has no blink state at all, rather than a resting one", () => {
    expect(new AttentionRegistry(fakeTimers()).stateOf("ws-never-marked")).toBeNull();
  });
});

describe("a marker the roster is drawing", () => {
  it("reports the phase it is lit in", () => {
    const registry = new AttentionRegistry(fakeTimers());
    registry.mark("ws-1", element());
    expect(registry.stateOf("ws-1")).toBe("on");
  });

  it("reports the dark half of the blink once a phase has elapsed", () => {
    const timers = fakeTimers();
    const registry = new AttentionRegistry(timers);
    registry.mark("ws-1", element());
    timers.run();
    expect(registry.stateOf("ws-1")).toBe("off");
  });
});

describe("the page's own timers, which production always uses", () => {
  // The unit run shares one jsdom per worker, so the fake clock is uninstalled
  // in this same file rather than left standing for whatever runs next.
  afterEach(() => {
    vi.useRealTimers();
  });

  it("advances the cadence a phase at a time on the page's clock", () => {
    vi.useFakeTimers();
    const registry = new AttentionRegistry(WINDOW_TIMERS);
    const el = element();
    registry.mark("ws-1", el);
    vi.advanceTimersByTime(ATTENTION_PHASE_MS);
    expect(el.getAttribute("data-blink")).toBe("off");
  });

  it("cancels the page's pending phase when the registry is disposed", () => {
    vi.useFakeTimers();
    const registry = new AttentionRegistry(WINDOW_TIMERS);
    const el = element();
    registry.mark("ws-1", el);
    registry.dispose();
    vi.advanceTimersByTime(ATTENTION_PHASE_MS * ATTENTION_PHASES);
    expect(el.getAttribute("data-blink")).toBe("on");
  });
});
