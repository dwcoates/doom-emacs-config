/** The webapp's tests await events (test/events.ts), never polling a condition with vi.waitFor. */
import { readFileSync } from "node:fs";
import { relative } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { testCodeFiles, webappRoot } from "./files-under.js";

describe("the webapp's tests", () => {
  it("never poll a condition with vi.waitFor: they await its event (events.ts)", () => {
    // Arrange
    const self = fileURLToPath(import.meta.url);

    // Act
    const polling = testCodeFiles([".ts", ".tsx"]).filter(
      (file) => file !== self && readFileSync(file, "utf8").includes("vi.waitFor("),
    );

    // Assert
    expect(polling.map((file) => relative(webappRoot, file))).toEqual([]);
  });
});

