/**
 * THE RENDERERS' RULES over their sources: the arranger draws a row one way,
 * and the row context is declared once.
 */
import { readFileSync } from "node:fs";
import { join, relative } from "node:path";
import { describe, expect, it } from "vitest";
import { filesUnder } from "../files-under.js";
import renderersSource from "../../src/feed/renderers.ts?raw";

describe("drawPlaced", () => {
  it("is the arranger's only way to draw a row", () => {
    const draws = renderersSource.match(/view\.drawRow\(/g) ?? [];
    expect(draws).toHaveLength(1);
  });
});

describe("the row context", () => {
  it("is declared once, in cards/context.ts, and only re-exported elsewhere", () => {
    // Arrange: every webapp source.
    const root = join(process.cwd(), "src");
    const sources = filesUnder(root);
    // Act
    const declaring = sources
      .filter((path) => /\binterface RowContext\b/.test(readFileSync(path, "utf8")))
      .map((path) => relative(root, path));
    // Assert
    expect(declaring).toEqual(["feed/cards/context.ts"]);
  });
});
