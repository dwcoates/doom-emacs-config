// @vitest-environment jsdom
import { readdirSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

import { isStill } from "./still.js";

const TEST_ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");

/** Every TypeScript file under DIR, recursively. */
function sources(dir: string): string[] {
  return readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) return sources(full);
    return entry.name.endsWith(".ts") ? [full] : [];
  });
}

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
    const handRolled = sources(TEST_ROOT)
      .filter((file) => file !== here && file !== helper)
      .filter((file) => readFileSync(file, "utf8").includes('hasAttribute("data-wave")'));
    // Assert
    expect(handRolled).toEqual([]);
  });
});
