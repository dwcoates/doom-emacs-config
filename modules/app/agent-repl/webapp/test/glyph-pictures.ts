/**
 * The shared glyph pictures, read off disk for the tests that check what the
 * rail draws against them: images/glyphs/<name>.svg at the module root, the
 * files the Emacs tab bar draws too (AGENTS.md, "Status glyphs are one set of
 * pictures, drawn as tall as a text row").
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

/** The module's images/glyphs/ directory. */
export const GLYPH_PICTURE_DIR = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../images/glyphs");

/** The SVG markup of glyph NAME's picture, exactly as the file holds it. */
export function readGlyphPicture(name: string): string {
  return readFileSync(path.join(GLYPH_PICTURE_DIR, `${name}.svg`), "utf8");
}
