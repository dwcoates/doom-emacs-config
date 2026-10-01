/**
 * THE SHARED COMMENT STRIPPERS (`test/source-text.ts`), AND THE PROOF EVERY
 * SCAN USES THEM.
 *
 * The last case fails any test file that hand-rolls its own comment strip or
 * stylesheet rule scan instead of going through `source-text.ts` and
 * `stylesheet.ts`, so the stripping rule cannot drift between scans again.
 */
import { readdirSync, readFileSync, statSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { codeOf, withoutBlockComments } from "./source-text.js";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("withoutBlockComments", () => {
  it.each([
    { name: "removes a block comment", text: "a /* note */ b", want: "a  b" },
    { name: "removes a block comment spanning lines", text: "a /* one\ntwo */ b", want: "a  b" },
    { name: "removes each of several block comments", text: "/* x */a/* y */", want: "a" },
    { name: "keeps a line comment", text: "a // note", want: "a // note" },
  ])("$name", ({ text, want }) => {
    // Arrange / Act
    const stripped = withoutBlockComments(text);
    // Assert
    expect(stripped).toBe(want);
  });
});

describe("codeOf", () => {
  it.each([
    { name: "removes a block comment", source: "f(); /* note */", want: "f(); " },
    { name: "removes a whole-line comment", source: "f();\n  // note\ng();", want: "f();\n\ng();" },
    { name: "keeps a trailing slash pair, as in a URL", source: 'u("https://x"); // note', want: 'u("https://x"); // note' },
  ])("$name", ({ source, want }) => {
    // Arrange / Act
    const code = codeOf(source);
    // Assert
    expect(code).toBe(want);
  });
});

describe("the call sites", () => {
  /** Every TypeScript file under test/, recursively. */
  function testFiles(dir: string): string[] {
    return readdirSync(dir).flatMap((name) => {
      const full = path.join(dir, name);
      if (statSync(full).isDirectory()) return testFiles(full);
      return full.endsWith(".ts") ? [full] : [];
    });
  }

  it("strip comments and scan stylesheet rules only through the shared readers", () => {
    // Arrange: the hand-rolled shapes the shared readers replaced.
    const shapes = [
      String.raw`/\/\*[\s\S]*?\*\//g`,
      String.raw`/^\s*\/\/.*$/gm`,
      String.raw`/([^{}]+)\{([^{}]*)\}/g`,
    ];
    const owners = new Set([
      path.join(here, "source-text.ts"),
      path.join(here, "stylesheet.ts"),
      fileURLToPath(import.meta.url),
    ]);
    // Act
    const handRolled = testFiles(here).filter(
      (file) => !owners.has(file) && shapes.some((shape) => readFileSync(file, "utf8").includes(shape)),
    );
    // Assert
    expect(handRolled.map((file) => path.relative(here, file))).toEqual([]);
  });
});
