// @vitest-environment jsdom
/**
 * The chat's pull past its end (src/chat/overscroll.ts): a wheel pushing down
 * at the limit pulls the content up with mounting resistance, and the pull
 * springs back once the wheel is quiet.
 */
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { installChatOverscroll, OVERSCROLL_RELEASE_MS, OVERSCROLL_SPRING, rubberBand } from "../../src/chat/overscroll.js";

describe("rubberBand", () => {
  it.each([
    ["no push", 0, 0],
    ["a pull back past nothing", -40, 0],
  ])("draws nothing for %s", (_name, pushed, want) => {
    expect(rubberBand(pushed, 400)).toBe(want);
  });

  it("follows a small push almost one for one", () => {
    expect(rubberBand(10, 400)).toBeGreaterThan(5);
  });

  it("resists: twice the push draws less than twice the pull", () => {
    expect(rubberBand(400, 400)).toBeLessThan(2 * rubberBand(200, 400));
  });

  it("never reaches the box's height", () => {
    expect(rubberBand(1e9, 400)).toBeLessThan(400);
  });
});

describe("installChatOverscroll", () => {
  let scroll: HTMLElement;
  let content: HTMLElement;
  let top: number;
  let pending: Map<number, () => void>;
  let next: number;
  let teardown: () => void;
  const timers = {
    setTimeout: (fn: () => void, ms: number): number => {
      expect(ms).toBe(OVERSCROLL_RELEASE_MS);
      pending.set(++next, fn);
      return next;
    },
    clearTimeout: (id: number): void => void pending.delete(id),
  };
  const wheel = (deltaY: number): void => void scroll.dispatchEvent(new WheelEvent("wheel", { deltaY }));
  const quiet = (): void => {
    const fns = [...pending.values()];
    pending.clear();
    for (const fn of fns) fn();
  };

  beforeEach(() => {
    scroll = document.createElement("div");
    content = document.createElement("div");
    scroll.append(content);
    top = 900;
    Object.defineProperty(scroll, "scrollHeight", { configurable: true, get: () => 1000 });
    Object.defineProperty(scroll, "clientHeight", { configurable: true, get: () => 100 });
    Object.defineProperty(scroll, "scrollTop", { configurable: true, get: () => top });
    pending = new Map();
    next = 0;
    teardown = installChatOverscroll(scroll, content, timers);
  });

  afterEach(() => teardown());

  it("pulls the content up when the wheel pushes past the limit", () => {
    wheel(30);
    expect(content.style.transform).toBe(`translateY(${-rubberBand(30, 100)}px)`);
  });

  it("pulls nothing while the box can still scroll down", () => {
    top = 500;
    wheel(30);
    expect(content.style.transform).toBe("");
  });

  it("pulls nothing for a wheel up at the limit", () => {
    wheel(-30);
    expect(content.style.transform).toBe("");
  });

  it("springs back once the wheel is quiet", () => {
    wheel(30);
    quiet();
    expect([content.style.transform, content.style.transition]).toEqual(["", OVERSCROLL_SPRING]);
  });

  it("waits for the wheel's last event before springing back", () => {
    wheel(30);
    wheel(30);
    expect(pending.size).toBe(1);
  });

  it("eases the pull back as the wheel turns up again", () => {
    wheel(60);
    wheel(-30);
    expect(content.style.transform).toBe(`translateY(${-rubberBand(30, 100)}px)`);
  });

  it("leaves the content as it was when torn down", () => {
    wheel(30);
    teardown();
    expect([content.style.transform, pending.size]).toEqual(["", 0]);
  });
});
