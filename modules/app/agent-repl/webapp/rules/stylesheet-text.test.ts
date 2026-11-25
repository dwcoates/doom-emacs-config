/**
 * THE SHARED STYLESHEET-TEXT READERS (`rules/stylesheet-text.ts`) on the real
 * stylesheet, and the proof no test hand-rolls the parsers of
 * `test/css-text.ts`.
 *
 * Every rule that reads a rule off `src/styles.css` by text goes through
 * `rulesOf`, `isOwnRule`, `selectorOf` or `keyframesOf`, so a comment that names a selector can never be
 * credited to the rule after it. `source-text.rules.test.ts` pins that sharing.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { keyframesOf, mediaBlocksOf, selectorOf } from "./stylesheet-text.js";
import { testCodeFiles, testDir, webappRoot } from "./files-under.js";

describe("isOwnRule", () => {
  it("is the only place a test asks whether a rule is a selector's own", () => {
    // Arrange: the hand-rolled shape isOwnRule replaced.
    const shape = /selectors\.length === 1/;
    const owners = new Set([path.join(testDir, "css-text.ts"), fileURLToPath(import.meta.url)]);

    // Act
    const handRolled = testCodeFiles().filter((file) => !owners.has(file) && shape.test(readFileSync(file, "utf8")));

    // Assert
    expect(handRolled.map((file) => path.relative(webappRoot, file))).toEqual([]);
  });
});

describe("selectorOf", () => {
  it("answers the selector the pattern's first group captures from the real stylesheet", () => {
    // Arrange / Act
    const selector = selectorOf(/([^{}]*\.merge-strip)\s*\{/, "merge strip");
    // Assert
    expect(selector).toBe(".merge-strip");
  });

  it("fails loudly naming what it looked for when nothing matches", () => {
    // Arrange / Act / Assert
    expect(() => selectorOf(/(\.no-such-class-anywhere)\s*\{/, "phantom rule")).toThrow(
      "the stylesheet has no phantom rule",
    );
  });
});

describe("mediaBlocksOf", () => {
  it("reads the real stylesheet by default", () => {
    // Arrange / Act
    const blocks = mediaBlocksOf("prefers-color-scheme: dark");

    // Assert
    expect(blocks[0]?.body).toContain(":root");
  });
});

describe("keyframesOf", () => {
  it("reads the real stylesheet by default", () => {
    // Arrange / Act
    const body = keyframesOf("ws-selection-number-rise");
    // Assert
    expect(body).toMatch(/from\s*\{/);
  });

  it("fails loudly naming the block when the stylesheet has none", () => {
    // Arrange / Act / Assert
    expect(() => keyframesOf("no-such-keyframes")).toThrow("the stylesheet has no @keyframes no-such-keyframes");
  });

  it("is the only place a test cuts a keyframes block out of the stylesheet", () => {
    // Arrange: the hand-rolled shape keyframesOf replaced, a regex opening on a keyframes block.
    const shape = /\/@keyframes [\w-]+ \\\{/;
    const owners = new Set([path.join(testDir, "css-text.ts"), fileURLToPath(import.meta.url)]);

    // Act
    const handRolled = testCodeFiles().filter((file) => !owners.has(file) && shape.test(readFileSync(file, "utf8")));

    // Assert
    expect(handRolled.map((file) => path.relative(webappRoot, file))).toEqual([]);
  });
});
