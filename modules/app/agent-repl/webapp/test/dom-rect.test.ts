// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { boxRect, settableScrollTop } from "./dom-rect.js";
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

describe("settableScrollTop", () => {
  it("starts at the value it is given", () => {
    // Arrange
    const el = document.createElement("div");
    // Act
    settableScrollTop(el, 40);
    // Assert
    expect(el.scrollTop).toBe(40);
  });

  it("keeps what is written to it, unclamped", () => {
    // Arrange
    const el = document.createElement("div");
    settableScrollTop(el);
    // Act
    el.scrollTop = 9_999;
    // Assert
    expect(el.scrollTop).toBe(9_999);
  });

  it("is the one settable scrollTop of the feed suites: none hand-rolls its own", () => {
    // Arrange
    const suites = codeUnder(dirname(fileURLToPath(import.meta.url)));
    // Act
    const handRolled = [...suites]
      .filter(([path, code]) => path.startsWith("feed/") && /defineProperty\([^,]+,\s*"scrollTop"/.test(code))
      .map(([path]) => path);
    // Assert
    expect(handRolled).toEqual([]);
  });
});
