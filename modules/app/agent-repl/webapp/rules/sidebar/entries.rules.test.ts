/**
 * THE SIDEBAR'S OPEN ROWS HAVE ONE READ AND ONE WALK: the slot painters key
 * rows through `openRowWorkspaces`, and no source recurses over `openKin` on
 * its own instead of taking `familyRows`.
 */
import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";

describe("openRowWorkspaces", () => {
  it("is the read every slot painter keys rows by", () => {
    // Arrange
    const source = (file: string): string => readFileSync(new URL(`../../src/sidebar/${file}`, import.meta.url), "utf8");

    // Act / Assert
    for (const file of ["tab-numbers.ts", "recency-gutters.ts"]) {
      expect(source(file)).toContain("openRowWorkspaces(");
      expect(source(file)).not.toContain("openKin(");
    }
  });
});

describe("familyRows", () => {
  // THE ONE WALK: every reader of a family's nested rows takes familyRows,
  // so no hand-rolled recursion over openKin can walk them differently.
  it.each(["subsections.ts", "keyed-rows.ts", "tab-numbers.ts", "roster.ts", "row.ts"])("leaves %s no recursion of its own over openKin", (file) => {
    // Arrange
    const source = readFileSync(new URL(`../../src/sidebar/${file}`, import.meta.url), "utf8");

    // Act
    const walks = source.match(/openKin\(/g) ?? [];

    // Assert
    expect(walks).toEqual([]);
  });
});
