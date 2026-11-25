/** No test code subscribes a tail owner by hand: tailTriggers (test/tail-triggers.ts) does. */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { testCodeFiles } from "./files-under.js";

describe("tailTriggers", () => {
  it("is the only test code that subscribes a tail owner by hand", () => {
    // Arrange
    const self = fileURLToPath(import.meta.url);
    const files = testCodeFiles([".test.ts"]).filter((f) => f !== self);
    // Act
    const offenders = files.filter((f) => /\btail\.observe\(/.test(readFileSync(f, "utf8")));
    // Assert
    expect(offenders).toEqual([]);
  });
});
