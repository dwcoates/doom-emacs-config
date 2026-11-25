/**
 * The links of src/link.ts are the only ones: every href-less link shares its
 * click claim, every OpenInEditor link is armed there, and only it builds a
 * commit_log target.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { codeOf } from "../test/source-text.js";
import { codeUnder } from "./files-under.js";

describe("claimsClick", () => {
  it("is the claim every href-less link shares, never re-spelled", () => {
    // ARRANGE
    const mentions = codeOf(readFileSync(path.join(process.cwd(), "src/feed/workspace-mentions.ts"), "utf8"));
    // ACT + ASSERT
    expect(mentions).toContain("claimsClick(event)");
    expect(mentions).not.toMatch(/event\.button\s*!==\s*0|event\.metaKey/);
  });
});

describe("editorTargetAnchor", () => {
  it("is the one place an OpenInEditor link is armed: no link hand-rolls its own", () => {
    const here = path.dirname(fileURLToPath(import.meta.url));
    const source = readFileSync(path.join(here, "..", "src", "link.ts"), "utf8");
    expect(source.match(/armButtonRole\(/g)).toHaveLength(1);
  });
});

describe("renderMergeCommitLink", () => {
  it("is the one place a page builds a commit_log target", () => {
    const here = path.dirname(fileURLToPath(import.meta.url));
    const spelling = [...codeUnder(path.join(here, "..", "src"))]
      .filter(([, code]) => code.includes('"commitLog"'))
      .map(([file]) => path.basename(file));
    expect(spelling).toEqual(["link.ts"]);
  });

});
