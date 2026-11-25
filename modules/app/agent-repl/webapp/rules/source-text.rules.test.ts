/**
 * EVERY SCAN USES THE SHARED COMMENT STRIPPERS (`test/source-text.ts`) and the
 * stylesheet's text parsers (`test/css-text.ts`): a test file that
 * hand-rolls its own comment strip or stylesheet rule scan fails here, so the
 * stripping rule cannot drift between scans again.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { testCodeFiles, testDir, webappRoot } from "./files-under.js";

describe("the call sites", () => {

  it("strip comments, scan stylesheet rules, find media blocks and cut function bodies only through the shared readers", () => {
    // Arrange: the hand-rolled shapes the shared readers replaced.
    const shapes = [
      String.raw`/\/\*[\s\S]*?\*\//g`,
      String.raw`/^\s*\/\/.*$/gm`,
      String.raw`/([^{}]+)\{([^{}]*)\}/g`,
      'indexOf("@media',
      'split("@media',
      String.raw`indexOf("\n}\n"`,
    ];
    const owners = new Set([
      path.join(testDir, "source-text.ts"),
      path.join(testDir, "css-text.ts"),
      fileURLToPath(import.meta.url),
    ]);
    // Act
    const handRolled = testCodeFiles().filter(
      (file) => !owners.has(file) && shapes.some((shape) => readFileSync(file, "utf8").includes(shape)),
    );
    // Assert
    expect(handRolled.map((file) => path.relative(webappRoot, file))).toEqual([]);
  });

  it("import raw source text at collection, never inside a test", () => {
    // Arrange: a dynamic `?raw` import inside a test body pays Vite's transform
    // against that test's timeout, which a loaded run overruns.
    const inTest = /await import\([^)]*\?raw["'`]\)/;
    // Act
    const offenders = testCodeFiles().filter(
      (file) => file !== fileURLToPath(import.meta.url) && inTest.test(readFileSync(file, "utf8")),
    );
    // Assert
    expect(offenders.map((file) => path.relative(webappRoot, file))).toEqual([]);
  });
});
