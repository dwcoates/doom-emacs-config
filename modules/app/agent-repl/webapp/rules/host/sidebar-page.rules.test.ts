/**
 * THE SIDEBAR PAGE'S LOG LEVEL IS APPLIED THROUGH THE LEVEL WINDOW
 * (`src/host/sidebar-page.ts`), never by hand.
 */
import { describe, expect, it } from "vitest";
import { levelApplyShapeOf } from "./level-apply-shape.js";

describe("setLogLevel", () => {
  it("applies the level through the level window, never by hand", () => {
    // Act
    const shape = levelApplyShapeOf("host/sidebar-page.ts");

    // Assert
    expect(shape).toEqual({ applies: true, handRolls: false });
  });
});
