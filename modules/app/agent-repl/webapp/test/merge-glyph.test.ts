import { describe, expect, it } from "vitest";
import { MERGE_GLYPH, MERGE_GLYPH_NAMES } from "../src/merge-glyph.js";
import { FEED_MERGE_HEAD_GLYPH } from "../src/vocab.js";

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
