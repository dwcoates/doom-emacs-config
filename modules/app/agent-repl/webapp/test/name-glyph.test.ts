// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { drawNameGlyph, NAME_GLYPH_CLASS } from "../src/name-glyph.js";
import { cascadedValue, installStylesheet } from "./stylesheet.js";

describe("drawNameGlyph", () => {
  it("draws the daemon's glyph verbatim in its own element", () => {
    // Act
    const glyph = drawNameGlyph({ text: "👍" }, "R.name.glyph");

    // Assert
    expect([glyph.tagName, glyph.className, glyph.textContent]).toEqual(["SPAN", NAME_GLYPH_CLASS, "👍"]);
  });

  it("refuses a glyph that carries none", () => {
    expect(() => drawNameGlyph({ text: "" }, "R.name.glyph")).toThrow("R.name.glyph: a name glyph carries its glyph");
  });

  it.each([
    ["font-size", "1em"],
    ["line-height", "1"],
  ])("is drawn at the name's own size (%s %s)", (property, value) => {
    // Arrange
    const uninstall = installStylesheet();
    const glyph = drawNameGlyph({ text: "👍" }, "R.name.glyph");
    document.body.append(glyph);

    // Act
    const cascaded = cascadedValue(glyph, property);
    uninstall();
    glyph.remove();

    // Assert
    expect(cascaded).toBe(value);
  });

  // EVERY SURFACE DRAWS THE GLYPH THROUGH THIS ONE HELPER: a surface that
  // built its own glyph element would draw it apart from the others.
  it.each(["src/sidebar/row.ts", "src/topbar/strip.ts"])("is the one drawing %s uses", (file) => {
    const source = readFileSync(join(__dirname, "..", file), "utf8");
    expect([source.includes("drawNameGlyph("), source.includes('"name-glyph"')]).toEqual([true, false]);
  });
});
