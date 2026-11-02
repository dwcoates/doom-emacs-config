/**
 * The shim tests' one recursive walk.
 */
import { mkdirSync, mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, relative } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { filesUnder } from "./files-under.js";

describe("filesUnder", () => {
  it("lists every file under a directory, nested ones included, sorted", () => {
    // Arrange
    const root = mkdtempSync(join(tmpdir(), "files-under-"));
    mkdirSync(join(root, "b"));
    writeFileSync(join(root, "c.ts"), "");
    writeFileSync(join(root, "b", "a.ts"), "");
    writeFileSync(join(root, "a.ts"), "");

    // Act
    const files = filesUnder(root).map((file) => relative(root, file));

    // Assert
    expect(files).toEqual(["a.ts", join("b", "a.ts"), "c.ts"]);
  });

  it("is the one walk the tests use: no test file hand-rolls a readdirSync recursion", () => {
    // Arrange
    const dir = dirname(fileURLToPath(import.meta.url));

    // Act
    const handRolled = filesUnder(dir).filter(
      (file) =>
        !file.endsWith("files-under.ts") &&
        (file.endsWith(".ts") || file.endsWith(".mjs")) &&
        /isDirectory\(\)\s*\)?\s*(\?|walk\()/.test(readFileSync(file, "utf8")),
    );

    // Assert
    expect(handRolled.map((file) => relative(dir, file))).toEqual([]);
  });
});
