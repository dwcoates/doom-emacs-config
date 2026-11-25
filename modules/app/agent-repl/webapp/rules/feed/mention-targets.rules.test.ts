/**
 * THE ONLY WAY A TEST BUILDS SERVED MENTION TARGETS (test/feed/mention-targets.ts).
 */
import { readFileSync } from "node:fs";
import { join, relative } from "node:path";
import { describe, expect, it } from "vitest";
import { testCodeFiles, webappRoot } from "../files-under.js";

describe("mention-targets", () => {
  it("is the only way a test builds served targets", () => {
    // ARRANGE: the malformed table in workspace-mentions.test.ts builds raw
    // targets on purpose, since a malformed target is what it tests.
    const allowed = new Set(["test/feed/mention-targets.ts", "test/feed/workspace-mentions.test.ts"]);
    // Spelled in two pieces so this file does not match its own search.
    const schema = ["WorkspaceMentionTargets", "Schema"].join("");
    // ACT
    const handRolled = testCodeFiles([".ts", ".tsx"])
      .map((full) => relative(webappRoot, full))
      .filter((f) => !allowed.has(f) && readFileSync(join(webappRoot, f), "utf8").includes(schema));
    // ASSERT
    expect(handRolled).toEqual([]);
  });
});
