/**
 * The merge bubble's jumps go through their one site: no merge module calls
 * SelectInEditor itself, and no caller picks between the two jumps itself
 * (src/feed/merge/jump.ts).
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { codeOf } from "../../../test/source-text.js";

describe("jumpToWorkspace", () => {
  it("is the one SelectInEditor call site in the merge bubble", () => {
    for (const file of ["queue.ts", "merge.ts", "merge-body.ts", "workspace-switch.ts", "step-tabs.ts"]) {
      const source = codeOf(readFileSync(join(process.cwd(), "src/feed/merge", file), "utf8"));
      expect(source.includes("selectInEditor("), file).toBe(false);
    }
  });
});

describe("jumpForStanding", () => {
  it("is the one choice of jump by standing: no caller picks between the two jumps itself", () => {
    for (const file of ["feed/workspace-mentions.ts", "feed/pull-request-owner.ts"]) {
      const source = codeOf(readFileSync(join(process.cwd(), "src", file), "utf8"));
      expect([source.includes("jumpForStanding("), source.includes("openAndJumpToWorkspace")], file).toEqual([true, false]);
    }
  });
});
