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
    ["font-size", "0.85em"],
    ["line-height", "1"],
  ])("is drawn a little smaller than the name, on the name's line (%s %s)", (property, value) => {
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

  // ONE WORD SPACE STANDS BETWEEN THE GLYPH AND THE NAME (owner request,
  // 2026-11-20). A sidebar row is a flex line whose gap stood there as well,
  // so the glyph takes the row's gap back.
  it("stands one word space before the name outside a sidebar row", () => {
    // Arrange
    const uninstall = installStylesheet();
    const glyph = drawNameGlyph({ text: "👍" }, "R.name.glyph");
    document.body.append(glyph);

    // Act
    const cascaded = [cascadedValue(glyph, "--name-glyph-space"), cascadedValue(glyph, "margin-right")];
    uninstall();
    glyph.remove();

    // Assert
    expect(cascaded).toEqual(["0.25em", "var(--name-glyph-space)"]);
  });

  it("takes a sidebar row's gap back, so one word space is all that stands before the name", () => {
    // Arrange
    const uninstall = installStylesheet();
    const sidebar = document.createElement("div");
    sidebar.id = "ws-sidebar";
    const row = document.createElement("div");
    row.className = "row";
    const glyph = drawNameGlyph({ text: "👍" }, "R.name.glyph");
    row.append(glyph);
    sidebar.append(row);
    document.body.append(sidebar);

    // Act
    const cascaded = [cascadedValue(row, "gap"), cascadedValue(row, "--row-gap"), cascadedValue(glyph, "margin-right")];
    uninstall();
    sidebar.remove();

    // Assert
    expect(cascaded).toEqual(["var(--row-gap)", "0.45rem", "calc(var(--name-glyph-space) - var(--row-gap))"]);
  });

  // IN A SIDEBAR ROW THE GLYPH IS SIZED FROM THE STATUS MARK (owner request,
  // 2026-11-20): only slightly bigger than the indicator to its left.
  it("is a quarter larger than the status mark in a sidebar row", () => {
    // Arrange
    const uninstall = installStylesheet();
    const sidebar = document.createElement("div");
    sidebar.id = "ws-sidebar";
    const row = document.createElement("div");
    row.className = "row";
    const glyph = drawNameGlyph({ text: "👍" }, "R.name.glyph");
    row.append(glyph);
    sidebar.append(row);
    document.body.append(sidebar);

    // Act
    const cascaded = [cascadedValue(sidebar, "--ws-mark-size"), cascadedValue(glyph, "font-size")];
    uninstall();
    sidebar.remove();

    // Assert
    expect(cascaded).toEqual(["9px", "calc(var(--ws-mark-size) * 1.25)"]);
  });

  // EVERY SURFACE DRAWS THE GLYPH THROUGH THIS ONE HELPER: a surface that
  // built its own glyph element would draw it apart from the others.
  it.each(["src/sidebar/row.ts", "src/topbar/strip.ts"])("is the one drawing %s uses", (file) => {
    const source = readFileSync(join(__dirname, "..", file), "utf8");
    expect([source.includes("drawNameGlyph("), source.includes('"name-glyph"')]).toEqual([true, false]);
  });
});
