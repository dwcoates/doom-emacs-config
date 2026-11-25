/**
 * The merge bubble draws a plain merge commit in one place
 * (src/feed/merge/merge.ts, drawMergeCommitText).
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

describe("drawMergeCommitText", () => {
  it("is the one place a plain merge commit is drawn: no merge source hand-rolls its own", () => {
    const dir = path.join(path.dirname(fileURLToPath(import.meta.url)), "..", "..", "..", "src", "feed", "merge");
    const count = (file: string) =>
      (readFileSync(path.join(dir, file), "utf8").match(/className = "merge-commit"/g) ?? []).length;
    expect([count("merge.ts"), count("step-tabs.ts")]).toEqual([1, 0]);
  });
});
