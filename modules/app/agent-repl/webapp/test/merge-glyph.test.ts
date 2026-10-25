import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { MERGE_GLYPH, MERGE_GLYPH_NAMES } from "../src/merge-glyph.js";
import { FEED_MERGE_HEAD_GLYPH } from "../src/vocab.js";
import { filesUnder } from "./files-under.js";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("merge-glyph", () => {
  it("draws the merge glyph as the arrow pair", () => {
    expect(MERGE_GLYPH).toBe("⇄");
  });

  it("draws the vocabulary's feed merge head glyph name as the merge glyph", () => {
    expect(MERGE_GLYPH_NAMES[FEED_MERGE_HEAD_GLYPH]).toBe(MERGE_GLYPH);
  });

  it("freezes the name table", () => {
    expect(Object.isFrozen(MERGE_GLYPH_NAMES)).toBe(true);
  });
});

describe("every merge mark draws the one merge glyph", () => {
  it("spells the glyph as a string literal in no src file's code but merge-glyph.ts", () => {
    const src = path.join(here, "..", "src");
    const literal = /["'`]⇄/;
    // A comment may show the glyph as it reads; only code draws it.
    const comment = /^\s*(\*|\/\*|\/\/)/;
    const spellsIt = (file: string): boolean =>
      readFileSync(file, "utf8")
        .split("\n")
        .some((line) => !comment.test(line) && literal.test(line));
    const offenders = filesUnder(src)
      .filter((file) => file.endsWith(".ts") && path.basename(file) !== "merge-glyph.ts")
      .filter(spellsIt)
      .map((file) => path.relative(src, file));
    expect(offenders).toEqual([]);
  });
});
