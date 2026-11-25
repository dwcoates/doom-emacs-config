/**
 * BOTH FADES FIND A WORKSPACE'S DRAWN ROWS THROUGH `drawnRowsOf`: their
 * sources read for the lookup.
 */
import { describe, expect, it } from "vitest";
import selectionFadeSource from "../../src/sidebar/selection-fade.ts?raw";
import foldFadeSource from "../../src/sidebar/fold-fade.ts?raw";

describe("drawnRowsOf", () => {
  it("is the lookup both fades paint through", () => {
    // Arrange
    const sources = [selectionFadeSource, foldFadeSource];

    // Assert
    expect(sources.map((source) => [source.includes("drawnRowsOf(root, id)"), source.includes('getAttribute("data-roster-row") === id')])).toEqual([
      [true, false],
      [true, false],
    ]);
  });
});
