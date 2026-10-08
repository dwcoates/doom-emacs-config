// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { drawClassicDiff, parseUnifiedDiff, type ClassicDiffLine } from "../../../src/feed/cards/diff-view.js";

/** A git diff of one file with one hunk: a context, a removed and an added line. */
const GIT_DIFF = [
  "diff --git a/x.ts b/x.ts",
  "index 1111111..2222222 100644",
  "--- a/x.ts",
  "+++ b/x.ts",
  "@@ -1,2 +1,2 @@",
  " keep",
  "-old",
  "+new",
  "",
].join("\n");

describe("parseUnifiedDiff: what is a diff", () => {
  it.each([
    ["a git diff", GIT_DIFF],
    ["a plain unified diff with no git header", "--- a\n+++ b\n@@ -1 +1 @@\n-x\n+y\n"],
    ["a diff whose last hunk the output cut short", "--- a\n+++ b\n@@ -1,5 +1,5 @@\n-x\n+y\n"],
    ["a new file's diff", "--- /dev/null\n+++ b/n.ts\n@@ -0,0 +1,2 @@\n+a\n+b\n"],
    ["a diff of two files", `${GIT_DIFF}diff --git a/y b/y\n--- a/y\n+++ b/y\n@@ -1 +1 @@\n-p\n+q\n`],
  ])("reads %s as a diff", (_name, text) => {
    // Arrange / Act
    const lines = parseUnifiedDiff(text);

    // Assert
    expect(lines).not.toBeNull();
  });

  it.each([
    ["ordinary command output", "ok  \tclaude-repld/internal/feed\t0.12s\n"],
    ["a markdown list of changes", "- removed the old path\n+ added a new one\n"],
    ["a hunk with no file header before it", "@@ -1 +1 @@\n-x\n+y\n"],
    ["file headers with no hunk", "--- a\n+++ b\n"],
    ["a diff followed by other output", `${GIT_DIFF}2 files changed\n`],
    ["other output before a diff", `On branch main\n${GIT_DIFF}`],
    ["a hunk line with no marker", "--- a\n+++ b\n@@ -1,2 +1,2 @@\n-x\nnot a diff line\n"],
    ["an empty text", ""],
  ])("refuses %s", (_name, text) => {
    // Arrange / Act
    const lines = parseUnifiedDiff(text);

    // Assert
    expect(lines).toBeNull();
  });
});

describe("parseUnifiedDiff: each line's kind and text", () => {
  it("reads every line of a git diff with its kind, markers stripped from hunk lines", () => {
    // Arrange / Act
    const lines = parseUnifiedDiff(GIT_DIFF);

    // Assert
    expect(lines).toEqual([
      { kind: "meta", text: "diff --git a/x.ts b/x.ts" },
      { kind: "meta", text: "index 1111111..2222222 100644" },
      { kind: "meta", text: "--- a/x.ts" },
      { kind: "meta", text: "+++ b/x.ts" },
      { kind: "header", text: "@@ -1,2 +1,2 @@" },
      { kind: "context", text: "keep" },
      { kind: "removed", text: "old" },
      { kind: "added", text: "new" },
    ]);
  });

  it("reads a removed line that itself reads like a file header by the hunk's counts", () => {
    // Arrange / Act
    const lines = parseUnifiedDiff("--- a\n+++ b\n@@ -1 +1 @@\n--- x\n+++ y\n");

    // Assert
    expect(lines?.slice(3)).toEqual([
      { kind: "removed", text: "-- x" },
      { kind: "added", text: "++ y" },
    ]);
  });

  it("reads an empty line inside a hunk as an empty context line", () => {
    // Arrange / Act
    const lines = parseUnifiedDiff("--- a\n+++ b\n@@ -1,2 +1,2 @@\n\n-x\n+y\n");

    // Assert
    expect(lines?.[3]).toEqual({ kind: "context", text: "" });
  });

  it("reads a no-newline marker as meta", () => {
    // Arrange / Act
    const lines = parseUnifiedDiff("--- a\n+++ b\n@@ -1 +1 @@\n-x\n\\ No newline at end of file\n+y\n");

    // Assert
    expect(lines?.[4]).toEqual({ kind: "meta", text: "\\ No newline at end of file" });
  });
});

describe("drawClassicDiff", () => {
  const LINES: readonly ClassicDiffLine[] = [
    { kind: "meta", text: "--- a/x.ts" },
    { kind: "header", text: "@@ -1,3 +1,3 @@" },
    { kind: "context", text: "keep" },
    { kind: "removed", text: "old" },
    { kind: "added", text: "new" },
  ];

  it.each([
    ["meta", "meta"],
    ["header", "hunk"],
    ["context", "ctx"],
    ["removed", "del"],
    ["added", "add"],
  ])("draws a %s line with the %s class", (kind, cls) => {
    // Arrange / Act
    const el = drawClassicDiff(LINES, "p");

    // Assert
    expect(el.querySelector(`[data-diff-line="${kind}"]`)?.classList.contains(cls)).toBe(true);
  });

  it("draws every line in order, each its own text and no marker", () => {
    // Arrange / Act
    const el = drawClassicDiff(LINES, "p");

    // Assert
    expect([...el.children].map((c) => c.textContent)).toEqual(["--- a/x.ts", "@@ -1,3 +1,3 @@", "keep", "old", "new"]);
  });

  it("draws no +/- marker before an added or removed line", () => {
    // Arrange / Act
    const el = drawClassicDiff(LINES, "p");

    // Assert
    const changed = [...el.querySelectorAll('[data-diff-line="added"], [data-diff-line="removed"]')];
    expect(changed.map((c) => c.textContent?.charAt(0))).toEqual(["o", "n"]);
  });

  it("wears the classic diff classes on the output section", () => {
    // Arrange / Act
    const el = drawClassicDiff(LINES, "p");

    // Assert
    expect(el.className).toBe("tool-output diff-output diff diff-classic");
  });
});
