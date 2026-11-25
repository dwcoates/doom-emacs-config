/**
 * THE TREE HIGHLIGHTS SHARE ONE RESPONSE-ROW CHECK: a rule over the sources
 * (src/feed/bubble-selection.ts, src/feed/branch-hover.ts).
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

describe("isResponseRow", () => {
  it("is the one response-row check every tree highlight shares", () => {
    // Arrange
    const files = ["src/feed/bubble-selection.ts", "src/feed/branch-hover.ts"];
    // Act
    const sources = files.map((f) => readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "../..", f), "utf8"));
    // Assert
    expect(sources.map((src) => src.match(/getAttribute\("data-unit"\) [!=]== "response"/g)?.length ?? 0)).toEqual([1, 0]);
  });
});
