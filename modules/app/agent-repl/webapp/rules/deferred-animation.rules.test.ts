/** deferredAnimation (test/deferred-animation.ts) is the one deferred animation: no suite hand-rolls a pending finished. */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { testCodeFiles, testDir } from "./files-under.js";

describe("deferredAnimation's call sites", () => {
  it("is the one deferred animation: no suite hand-rolls a pending finished", () => {
    // Arrange
    const helper = join(testDir, "deferred-animation.ts");
    // Act
    const handRolled = testCodeFiles().filter(
      (path) => path !== helper && /\{ finished \} as unknown as Animation/.test(readFileSync(path, "utf8")),
    );
    // Assert
    expect(handRolled).toEqual([]);
  });
});
