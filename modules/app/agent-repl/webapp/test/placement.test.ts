// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { applyPlacement, placeBeside, viewportOf } from "../src/placement.js";
import type { Rect } from "../src/topbar/clamp.js";

describe("viewportOf", () => {
  it("reads the window's inner size", () => {
    const win = { innerWidth: 1200, innerHeight: 700 } as Window;
    expect(viewportOf(win)).toEqual({ width: 1200, height: 700 });
  });

  it("reads this page's own window when given none", () => {
    expect(viewportOf()).toEqual({ width: window.innerWidth, height: window.innerHeight });
  });
});

describe("applyPlacement", () => {
  it("stands the box at the placement's left, top and height cap, in pixels", () => {
    // ARRANGE
    const el = document.createElement("div");
    // ACT
    applyPlacement(el, { left: 12, top: 40, maxHeight: 300 });
    // ASSERT
    expect([el.style.left, el.style.top, el.style.maxHeight]).toEqual(["12px", "40px", "300px"]);
  });
});

/** A rectangle at LEFT, TOP, WIDTH wide and HEIGHT tall. */
const rect = (left: number, top: number, width: number, height: number): Rect => ({
  left,
  top,
  width,
  height,
  right: left + width,
  bottom: top + height,
});

describe("placeBeside", () => {
  it("stands the box on the anchor's right edge, its top on the anchor's line", () => {
    // ARRANGE
    const el = document.createElement("div");
    // ACT
    const at = placeBeside(el, rect(0, 100, 200, 20), { width: 1000, height: 800 }, () => rect(0, 0, 150, 60));
    // ASSERT
    expect([at, el.style.left, el.style.top, el.style.maxHeight]).toEqual([
      { left: 200, top: 100, maxHeight: at.maxHeight },
      "200px",
      "100px",
      `${at.maxHeight}px`,
    ]);
  });

  it("slides the box up only as far as the window's bottom demands", () => {
    // ARRANGE
    const el = document.createElement("div");
    // ACT
    const at = placeBeside(el, rect(0, 700, 200, 20), { width: 1000, height: 800 }, () => rect(0, 0, 150, 300));
    // ASSERT
    expect(at.top + 300).toBeLessThanOrEqual(800);
  });

  it("measures the box at its natural height, with the last placement's cap taken off", () => {
    // ARRANGE
    const el = document.createElement("div");
    el.style.maxHeight = "40px";
    let capWhenMeasured: string | null = null;
    // ACT
    placeBeside(el, rect(0, 100, 200, 20), { width: 1000, height: 800 }, (box) => {
      capWhenMeasured = (box as HTMLElement).style.maxHeight;
      return rect(0, 0, 150, 60);
    });
    // ASSERT
    expect(capWhenMeasured).toBe("");
  });
});
