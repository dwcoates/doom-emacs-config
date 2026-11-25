/**
 * Every test that overrides some elements' computed style does it through
 * overrideComputedStyle (test/computed-style.ts).
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { testCodeFiles } from "./files-under.js";

describe("every test that overrides some elements' computed style", () => {
  it("does it through overrideComputedStyle, never a spy of its own over the real method", () => {
    const here = fileURLToPath(import.meta.url);
    const handRolled = testCodeFiles([".test.ts", ".test.tsx"]).filter(
      (path) => path !== here && readFileSync(path, "utf8").includes("window.getComputedStyle.bind(window)"),
    );

    expect(handRolled).toEqual([]);
  });
});
