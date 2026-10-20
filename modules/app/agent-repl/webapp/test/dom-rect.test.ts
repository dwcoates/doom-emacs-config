import { describe, expect, it } from "vitest";
import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { boxRect } from "./dom-rect.js";
import { codeUnder } from "./files-under.js";

describe("boxRect", () => {
  it("derives every edge from the box it is stated by", () => {
    // Arrange
    const init = { left: 10, top: 20, width: 30, height: 40 };
    // Act
    const rect = boxRect(init);
    // Assert
    expect({ ...rect, toJSON: undefined }).toEqual({
      left: 10,
      top: 20,
      width: 30,
      height: 40,
      right: 40,
      bottom: 60,
      x: 10,
      y: 20,
      toJSON: undefined,
    });
  });

  it("states an unstated length as 0", () => {
    // Arrange
    const init = { top: 5, height: 15 };
    // Act
    const rect = boxRect(init);
    // Assert
    expect([rect.left, rect.width, rect.right, rect.bottom, rect.x, rect.y]).toEqual([0, 0, 0, 20, 0, 5]);
  });

  it("is the one scripted DOMRect: no suite hand-rolls its own", () => {
    // Arrange
    const suites = codeUnder(dirname(fileURLToPath(import.meta.url)));
    // Act
    const handRolled = [...suites].filter(([path, code]) => path !== "dom-rect.ts" && /toJSON:\s*\(\)\s*=>/.test(code)).map(([path]) => path);
    // Assert
    expect(handRolled).toEqual([]);
  });
});
