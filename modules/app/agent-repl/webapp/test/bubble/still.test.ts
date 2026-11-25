// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { isStill } from "./still.js";

describe("isStill", () => {
  it.each([
    ["a bare bubble", {}, true],
    ["a bubble wearing the wave marker", { "data-wave": "working" }, false],
    ["a bubble carrying an inline style", { style: "animation-delay:-5ms" }, false],
  ] as const)("answers %s", (_label, attrs, still) => {
    // Arrange
    const bubble = document.createElement("div");
    for (const [name, value] of Object.entries(attrs)) bubble.setAttribute(name, value);
    // Act
    const got = isStill(bubble);
    // Assert
    expect(got).toBe(still);
  });
});
