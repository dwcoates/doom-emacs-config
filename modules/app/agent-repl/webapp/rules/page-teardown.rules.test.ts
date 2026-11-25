/** page-teardown.ts alone ends a document at its unload. */
import { describe, expect, it } from "vitest";
import path from "node:path";
import { codeUnder } from "./files-under.js";

describe("who ends a document at its unload", () => {
  it("is page-teardown.ts alone, besides the observers dom.ts disconnects", () => {
    // Arrange
    const owners = new Set(["page-teardown.ts", "dom.ts"]);

    // Act
    const others = [...codeUnder(path.join(__dirname, "..", "src"))]
      .filter(([file, code]) => !owners.has(file) && code.includes('"pagehide"'))
      .map(([file]) => file);

    // Assert
    expect(others).toEqual([]);
  });
});
