/**
 * A DROPPED ROW'S PLACEMENT IS BUILT IN ONE PLACE: the subsections source read
 * for the request's one builder.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { codeOf } from "../../test/source-text.js";

describe("the placement a dropped row asks for", () => {
  it("is built in one place, so the top-level row's drop and the freed child's drop ask the same shape", () => {
    // Arrange
    const source = codeOf(readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src/sidebar/subsections.ts"), "utf8"));

    // Act
    const built = source.match(/case: "placeWorkspace"/g) ?? [];
    const asked = source.match(/placement\(workspace, l, before, refs\)/g) ?? [];

    // Assert
    expect([built.length, asked.length]).toEqual([1, 2]);
  });
});
