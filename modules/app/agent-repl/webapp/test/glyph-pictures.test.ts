import { readdirSync } from "node:fs";
import { describe, expect, it } from "vitest";
import { GLYPH_PICTURE_DIR, readGlyphPicture } from "./glyph-pictures";

describe("the shared glyph pictures", () => {
  it("reads a picture's SVG markup by its glyph name", () => {
    expect(readGlyphPicture("exclaim")).toMatch(/^<svg [^>]*viewBox="0 0 [0-9.]+ 16"/);
  });

  it("names the directory every picture lives in", () => {
    expect(readdirSync(GLYPH_PICTURE_DIR)).toEqual(
      expect.arrayContaining(["ask.svg", "dot.svg", "exclaim.svg", "merge.svg", "snowflake.svg", "target.svg"]),
    );
  });

  it("fails loudly for a name with no picture", () => {
    expect(() => readGlyphPicture("no-such-glyph")).toThrow(/ENOENT/);
  });
});
