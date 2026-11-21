// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { pathedFamilies } from "../../src/sidebar/family.js";
import { codeOf } from "../source-text.js";
import { openRow, row } from "./harness.js";

describe("pathedFamilies", () => {
  it("gives each family its index under the region's path", () => {
    const got = pathedFamilies([openRow(row({ id: "a" })), openRow(row({ id: "b" }))], "R.rows");
    expect(got.map((e) => [e.family.row?.workspace?.workspace?.id, e.path])).toEqual([
      ["a", "R.rows[0]"],
      ["b", "R.rows[1]"],
    ]);
  });

  it.each([
    ["roster.ts", "drawRosterRows"],
    ["entries.ts", "pathedOpenRows"],
    ["row.ts", "appendRowKids"],
  ])("is how %s walks a list's rows", (file, fn) => {
    // Arrange
    const source = codeOf(readFileSync(join(__dirname, "..", "..", "src", "sidebar", file), "utf8"));
    const start = source.indexOf(`function ${fn}(`);

    // Act
    const body = source.slice(start, source.indexOf("\n}\n", start));

    // Assert
    expect(body.includes("pathedFamilies(")).toBe(true);
  });
});
