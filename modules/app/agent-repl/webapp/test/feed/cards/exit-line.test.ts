// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { TOOL_EXIT_LINE_CLASS, drawToolExitLine } from "../../../src/feed/cards/exit-line.js";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../../src");

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

  it.each(["feed/cards/tool-call.ts", "feed/cards/hook.ts"])("is how %s draws its exit code", (file) => {
    expect(readFileSync(path.join(SRC, file), "utf8")).toContain("drawToolExitLine(");
  });
});
