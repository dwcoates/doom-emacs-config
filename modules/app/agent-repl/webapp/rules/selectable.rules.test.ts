/** The shared `selectable` stamp (test/selectable.ts) is the one every selection test uses. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { testDir } from "./files-under.js";

describe("selectable", () => {
  it("is the one stamp every selection test uses", () => {
    // Arrange
    const users = ["feed/feed-view.test.ts", "integration/bubble-selection.integration.test.ts"];
    // Act
    const handRolled = users.filter((file) =>
      readFileSync(path.join(testDir, file), "utf8").includes("FeedRowSelectableSchema"),
    );
    // Assert
    expect(handRolled).toEqual([]);
  });
});
