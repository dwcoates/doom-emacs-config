// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { TOOL_EXIT_LINE_CLASS, drawToolExitLine } from "../../../src/feed/cards/exit-line.js";

describe("drawToolExitLine", () => {
  it("puts the chip on a line of its own", () => {
    // Arrange
    const chip = document.createElement("span");
    chip.textContent = "exit 3";

    // Act
    const line = drawToolExitLine(chip);

    // Assert
    expect([line.localName, line.className, line.firstElementChild === chip, line.children.length]).toEqual([
      "div",
      TOOL_EXIT_LINE_CLASS,
      true,
      1,
    ]);
  });
});
