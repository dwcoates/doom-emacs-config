// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import { COLLAPSE_MS, easeIntoLook, readBubbleLook, type BubbleLook } from "../../src/bubble/collapse-motion.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

/** A connected bubble standing HEIGHT px tall with FILL. */
function bubble(height: number, fill = "rgb(240, 236, 255)"): HTMLElement {
  const el = document.createElement("div");
  el.style.backgroundColor = fill;
  el.getBoundingClientRect = () => ({ height }) as DOMRect;
  document.body.append(el);
  return el;
}

/** Deps that hold the next frame until RUN is called. */
function frames(reduced = false) {
  const queued: (() => void)[] = [];
  return {
    deps: { nextFrame: (fn: () => void) => queued.push(fn), reducedMotion: () => reduced },
    run: () => {
      for (const fn of queued.splice(0)) fn();
    },
  };
}

const FROM: BubbleLook = { height: 180, background: "rgb(240, 236, 255)", boxShadow: "rgba(0, 0, 0, 0.1) 0px 1px 2px 0px" };

afterEach(() => {
  document.body.replaceChildren();
  vi.restoreAllMocks();
});

describe("readBubbleLook", () => {
  it("reads the bubble's height, fill and shadow as drawn", () => {
    // Arrange
    const el = bubble(42, "rgb(1, 2, 3)");
    el.style.boxShadow = "none";

    // Act
    const look = readBubbleLook(el);

    // Assert
    expect(look).toEqual({ height: 42, background: "rgb(1, 2, 3)", boxShadow: "none" });
  });
});

describe("easeIntoLook", () => {
  it("animates from the old look to the bubble's own, at the next frame", () => {
    // Arrange
    const el = bubble(30, "rgb(255, 255, 255)");
    const animate = vi.spyOn(el, "animate");
    const { deps, run } = frames();

    // Act
    easeIntoLook(el, FROM, deps);
    const before = animate.mock.calls.length;
    run();

    // Assert
    expect(before).toBe(0);
    const [keyframes, options] = animate.mock.calls[0] ?? [];
    expect([keyframes, options]).toEqual([
      [
        { boxSizing: "border-box", overflow: "hidden", height: "180px", backgroundColor: FROM.background, boxShadow: FROM.boxShadow },
        { boxSizing: "border-box", overflow: "hidden", height: "30px", backgroundColor: "rgb(255, 255, 255)", boxShadow: getComputedStyle(el).boxShadow },
      ],
      { duration: COLLAPSE_MS, easing: "ease-out" },
    ]);
  });

  it("records the easing at DEBUG with both heights", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const el = bubble(30);
    const { deps, run } = frames();

    // Act
    easeIntoLook(el, FROM, deps);
    run();

    // Assert
    const record = await forwardedRecord(capture, "feed.bubble.collapse-eased");
    expect(record.context).toMatchObject({ from_height: 180, to_height: 30, duration_ms: COLLAPSE_MS });
  });

  it("changes at once for a reader who asked for reduced motion", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const el = bubble(30);
    const animate = vi.spyOn(el, "animate");
    const { deps, run } = frames(true);

    // Act
    easeIntoLook(el, FROM, deps);
    run();

    // Assert
    const record = await forwardedRecord(capture, "feed.bubble.collapse-skipped");
    expect([animate.mock.calls.length, record.context]).toMatchObject([0, { why: "reduced-motion" }]);
  });

  it("eases nothing for a bubble that left the feed before its first frame", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const el = bubble(30);
    const animate = vi.spyOn(el, "animate");
    const { deps, run } = frames();

    // Act
    easeIntoLook(el, FROM, deps);
    el.remove();
    run();

    // Assert
    const record = await forwardedRecord(capture, "feed.bubble.collapse-skipped");
    expect([animate.mock.calls.length, record.context]).toMatchObject([0, { why: "detached" }]);
  });
});
