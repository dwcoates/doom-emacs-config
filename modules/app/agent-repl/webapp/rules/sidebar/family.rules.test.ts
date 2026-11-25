/**
 * EVERY SIDEBAR LIST WALKS ITS ROWS THROUGH `pathedFamilies`: the drawing
 * sources read for the call.
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { codeOf, functionBody } from "../../test/source-text.js";

describe("pathedFamilies", () => {
  it.each([
    ["roster.ts", "drawRosterRows"],
    ["entries.ts", "pathedOpenRows"],
    ["row.ts", "appendRowKids"],
  ])("is how %s walks a list's rows", (file, fn) => {
    // Arrange
    const source = codeOf(readFileSync(join(__dirname, "..", "..", "src", "sidebar", file), "utf8"));

    // Act
    const body = functionBody(source, fn);

    // Assert
    expect(body.includes("pathedFamilies(")).toBe(true);
  });
});
