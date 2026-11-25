/**
 * THE PAGE'S LOG LEVEL IS APPLIED THROUGH `LevelWindow.apply`
 * (`src/host/embed.ts`), never opened or closed by hand.
 */
import { describe, expect, it } from "vitest";
import { levelApplyShapeOf } from "./level-apply-shape.js";

describe("the page's log level", () => {
  it("is applied through LevelWindow.apply, never opened or closed by hand", () => {
    // Act
    const shape = levelApplyShapeOf("host/embed.ts");

    // Assert
    expect(shape).toEqual({ applies: true, handRolls: false });
  });
});
