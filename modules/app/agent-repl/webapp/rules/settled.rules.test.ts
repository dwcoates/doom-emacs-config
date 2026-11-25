/** settledByNextTask (test/settled.ts) is how every never-settles test asks. */
import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import path from "node:path";
import { testDir } from "./files-under.js";

describe("settledByNextTask", () => {
  it("is how every never-settles test asks, so no suite hand-rolls the check", () => {
    // Arrange
    const users = ["page-teardown.test.ts", "rpc/unary.test.ts", "rpc/client-log-sink.test.ts"];

    // Act
    const handRolled = users.filter((file) => readFileSync(path.join(testDir, file), "utf8").includes("let settled"));

    // Assert
    expect(handRolled).toEqual([]);
  });
});
