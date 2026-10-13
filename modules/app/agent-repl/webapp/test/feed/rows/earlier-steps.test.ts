// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { drawFeedEarlierSteps, EARLIER_STEPS_LABEL } from "../../../src/feed/rows/earlier-steps.js";

describe("drawFeedEarlierSteps", () => {
  it("draws the fold under its label", () => {
    // Act
    const el = drawFeedEarlierSteps(() => undefined);

    // Assert
    expect([el.textContent, el.hasAttribute("data-earlier-steps")]).toEqual([EARLIER_STEPS_LABEL, true]);
  });

  it("runs the walk when activated", () => {
    // Arrange
    let ran = 0;
    const el = drawFeedEarlierSteps(() => {
      ran += 1;
    });

    // Act
    el.click();

    // Assert
    expect(ran).toBe(1);
  });
});
