// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { drawWorkDot } from "../../src/feed/work-dot.js";

describe("drawWorkDot", () => {
  it("draws a colored dot filled", () => {
    const dot = drawWorkDot("red", false);
    expect([dot.getAttribute("data-dot"), dot.textContent, dot.classList.contains("tone-red")]).toEqual([
      "filled",
      "●",
      true,
    ]);
  });

  it("draws a dot that spends no color hollow", () => {
    const dot = drawWorkDot("none", false);
    expect([dot.getAttribute("data-dot"), dot.textContent, dot.classList.contains("tone-none")]).toEqual([
      "hollow",
      "○",
      true,
    ]);
  });

  it("breathes a live dot", () => {
    expect(drawWorkDot("green", true).classList.contains("work-dot-live")).toBe(true);
  });

  it("keeps a settled dot still", () => {
    expect(drawWorkDot("none", false).classList.contains("work-dot-live")).toBe(false);
  });

  it("hides the dot from assistive tech, the head's words carrying the state", () => {
    expect(drawWorkDot("green", true).getAttribute("aria-hidden")).toBe("true");
  });
});
