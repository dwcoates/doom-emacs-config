// @vitest-environment jsdom
//
// The topbar's one glyph-on-disc drawing, shared by the persistent-wifi and
// remote-control chips.
import { describe, expect, it } from "vitest";
import { DISC_RADIUS, DISC_VIEW_BOX, drawDiscGlyph } from "../../src/topbar/disc-glyph.js";

const draw = (): SVGSVGElement =>
  drawDiscGlyph({ glyphClass: "g", discClass: "d", paths: ["M1 1h1", "M2 2h2"] });

describe("drawDiscGlyph", () => {
  it("sizes the viewBox to the disc's square around (12, 12)", () => {
    expect(DISC_VIEW_BOX).toBe(`${12 - DISC_RADIUS} ${12 - DISC_RADIUS} ${2 * DISC_RADIUS} ${2 * DISC_RADIUS}`);
  });

  it("draws the svg over the disc's viewBox", () => {
    expect(draw().getAttribute("viewBox")).toBe(DISC_VIEW_BOX);
  });

  it("hides the glyph from assistive technology", () => {
    expect(draw().getAttribute("aria-hidden")).toBe("true");
  });

  it("carries the glyph's class on the svg", () => {
    expect(draw().getAttribute("class")).toBe("g");
  });

  it("draws the disc first, so the paths paint over it", () => {
    expect(draw().firstElementChild?.getAttribute("class")).toBe("d");
  });

  it("centers the disc on the grid's center at the disc radius", () => {
    const disc = draw().querySelector("circle");
    expect([disc?.getAttribute("cx"), disc?.getAttribute("cy"), disc?.getAttribute("r")]).toEqual([
      "12",
      "12",
      String(DISC_RADIUS),
    ]);
  });

  it("draws the glyph's paths in order", () => {
    expect([...draw().querySelectorAll("path")].map((p) => p.getAttribute("d"))).toEqual(["M1 1h1", "M2 2h2"]);
  });
});
