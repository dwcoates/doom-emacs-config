/** pageSize is the one size measure: no caller counts elements or feed rows itself. */
import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

describe("pageSize", () => {
  it("is the one size measure: no caller counts elements or feed rows itself", () => {
    // Arrange
    const callers = ["../src/page-presence.ts", "../src/host/load-census.ts", "../src/page-census.ts"].map((path) =>
      readFileSync(fileURLToPath(new URL(path, import.meta.url)), "utf8"),
    );

    // Act
    const handRolled = callers.filter((source) => source.includes('getElementsByTagName("*")'));

    // Assert
    expect(handRolled).toEqual([]);
  });
});
