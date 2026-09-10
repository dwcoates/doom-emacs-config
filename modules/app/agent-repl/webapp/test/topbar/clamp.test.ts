import { describe, expect, it } from "vitest";
import { REVEAL_MARGIN_PX, clampReveal, type Rect } from "../../src/topbar/clamp.js";

const rect = (left: number, top: number, width: number, height: number): Rect => ({
  left,
  top,
  width,
  height,
  right: left + width,
  bottom: top + height,
});

const VIEWPORT = { width: 1000, height: 800 };
/** A 32px-tall strip element, the shape every anchor has. */
const anchorAt = (left: number, width: number): Rect => rect(left, 0, width, 32);

describe("clampReveal", () => {
  it("puts the reveal directly under the strip, never above it", () => {
    expect(clampReveal(anchorAt(100, 80), rect(0, 0, 200, 300), VIEWPORT).top).toBe(32);
  });

  it("keeps the anchor's own left edge when the reveal fits", () => {
    expect(clampReveal(anchorAt(100, 80), rect(0, 0, 200, 300), VIEWPORT).left).toBe(100);
  });

  it("slides left when the reveal would overflow the right edge", () => {
    // ARRANGE: anchored at 900, a 200-wide reveal would end at 1100.
    const placement = clampReveal(anchorAt(900, 60), rect(0, 0, 200, 300), VIEWPORT);
    // ASSERT: held one margin back from the right edge.
    expect(placement.left).toBe(1000 - REVEAL_MARGIN_PX - 200);
  });

  it("holds a left-edge anchor one margin clear of the left edge", () => {
    expect(clampReveal(anchorAt(0, 60), rect(0, 0, 200, 300), VIEWPORT).left).toBe(REVEAL_MARGIN_PX);
  });

  it("overflows right rather than left for a reveal wider than the viewport", () => {
    // A reveal that cannot fit must still start where the reader's text does.
    expect(clampReveal(anchorAt(400, 60), rect(0, 0, 1200, 300), VIEWPORT).left).toBe(
      REVEAL_MARGIN_PX,
    );
  });

  it("caps the height at what is left below the strip, so it scrolls instead of overflowing", () => {
    expect(clampReveal(anchorAt(100, 60), rect(0, 0, 200, 4000), VIEWPORT).maxHeight).toBe(
      800 - 32 - REVEAL_MARGIN_PX,
    );
  });

  it("never answers a negative height, however short the window", () => {
    expect(clampReveal(anchorAt(100, 60), rect(0, 0, 200, 300), { width: 1000, height: 10 })
      .maxHeight).toBe(0);
  });
});
