/** servedProse (test/prose.ts) is the one helper every fixture builder that takes either kind uses. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { testDir } from "./files-under.js";

describe("servedProse", () => {
  it("is the one helper every fixture builder that takes either kind uses", () => {
    // Arrange — a builder handing a text variable to plainProse would refuse a
    // tree only when one is passed, so no builder does.
    const builders = ["feed/harness.ts", "integration/fixtures.ts"];
    // Act
    const handRolled = builders.filter((file) =>
      /plainProse\([a-z][A-Za-z]*\)/.test(readFileSync(path.join(testDir, file), "utf8")),
    );
    // Assert
    expect(handRolled).toEqual([]);
  });
});
