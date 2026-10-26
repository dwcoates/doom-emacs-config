// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

import { filesUnder } from "../files-under.js";
import { isStill } from "./still.js";

const TEST_ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");

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

  it("is the one reading of the wave marker among the tests", () => {
    // Arrange
    const here = fileURLToPath(import.meta.url);
    const helper = path.join(path.dirname(here), "still.ts");
    // Act
    const handRolled = filesUnder(TEST_ROOT)
      .filter((file) => file !== here && file !== helper)
      .filter((file) => readFileSync(file, "utf8").includes('hasAttribute("data-wave")'));
    // Assert
    expect(handRolled).toEqual([]);
  });
});
