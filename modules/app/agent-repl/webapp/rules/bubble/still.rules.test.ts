/**
 * THE ONE READING OF THE WAVE MARKER (`test/bubble/still.ts`): no test reads
 * `data-wave` by hand.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

import { testCodeFiles, testDir } from "../files-under.js";

describe("isStill", () => {
  it("is the one reading of the wave marker among the tests", () => {
    // Arrange
    const here = fileURLToPath(import.meta.url);
    const helper = path.join(testDir, "bubble", "still.ts");
    // Act
    const handRolled = testCodeFiles()
      .filter((file) => file !== here && file !== helper)
      .filter((file) => readFileSync(file, "utf8").includes('hasAttribute("data-wave")'));
    // Assert
    expect(handRolled).toEqual([]);
  });
});
