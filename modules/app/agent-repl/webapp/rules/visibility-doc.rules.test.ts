/** visibilityDoc (test/visibility-doc.ts) is the one way the suites fake a page's visibility. */
import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import { join } from "node:path";

describe("the test-set visibility document", () => {
  it("is the one way the suites fake a page's visibility", () => {
    // Arrange
    const files = ["test/page-seen.test.ts", "test/sidebar/unseen.test.ts", "test/sidebar/sidebar.test.ts"];
    // Act
    const handRolled = files.filter((file) =>
      readFileSync(join(process.cwd(), file), "utf8").includes('defineProperty(doc, "visibilityState"'),
    );
    // Assert
    expect(handRolled).toEqual([]);
  });
});
