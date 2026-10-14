/**
 * THE SHARED RECURSIVE FILE WALK (`test/files-under.ts`), AND THE PROOF EVERY
 * SCAN USES IT.
 */
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, relative } from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { filesUnder } from "./files-under.js";

const here = dirname(fileURLToPath(import.meta.url));

describe("filesUnder", () => {
  let root = "";

  beforeEach(() => {
    root = mkdtempSync(join(tmpdir(), "files-under-"));
    mkdirSync(join(root, "b", "deep"), { recursive: true });
    for (const file of ["a.ts", "a.html", "b/c.ts", "b/deep/d.ts", "b/deep/e.css", "z.ts"]) writeFileSync(join(root, file), "");
  });

  afterEach(() => {
    rmSync(root, { recursive: true, force: true });
  });

  it.each([
    { name: "keeps only .ts files by default, at every depth, in walk order", suffixes: undefined, want: ["a.ts", "b/c.ts", "b/deep/d.ts", "z.ts"] },
    { name: "keeps a file ending with any of several suffixes", suffixes: [".ts", ".html"], want: ["a.html", "a.ts", "b/c.ts", "b/deep/d.ts", "z.ts"] },
    { name: "keeps nothing when no file ends with the suffix", suffixes: [".json"], want: [] },
  ])("$name", ({ suffixes, want }) => {
    // Arrange / Act
    const files = filesUnder(root, suffixes).map((file) => relative(root, file));
    // Assert
    expect(files).toEqual(want);
  });

  it("answers full paths under the directory", () => {
    // Arrange / Act
    const files = filesUnder(join(root, "b"));
    // Assert
    expect(files).toEqual([join(root, "b", "c.ts"), join(root, "b", "deep", "d.ts")]);
  });

  it("throws when the directory does not exist", () => {
    // Arrange / Act / Assert
    expect(() => filesUnder(join(root, "missing"))).toThrow(/ENOENT/);
  });
});

describe("the call sites", () => {
  it("walk a directory recursively only through filesUnder", () => {
    // Arrange: a recursive walk is a directory read that also descends.
    const owners = new Set([join(here, "files-under.ts"), fileURLToPath(import.meta.url)]);
    const descends = /isDirectory\(\)|recursive:\s*true/;
    // Act
    const handRolled = filesUnder(here)
      .filter((file) => !owners.has(file))
      .filter((file) => {
        const source = readFileSync(file, "utf8");
        return /\breaddir(?:Sync)?\(/.test(source) && descends.test(source);
      })
      .map((file) => relative(here, file));
    // Assert
    expect(handRolled).toEqual([]);
  });
});
