// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import {
  ATTENTION_BLINKS,
  ATTENTION_PHASES,
  ATTENTION_PHASE_MS,
  AttentionRegistry,
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
    [4, "steady"],
    [9, "steady"],
  ] as const)("draws phase %i as %s", (phase, state) => {
    expect(blinkState(phase)).toBe(state);
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
    [4, "steady"],
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
    expect(redrawn.getAttribute("data-blink")).toBe("steady");
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
