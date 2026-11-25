// @vitest-environment jsdom
/**
 * A ROSTER ROW'S STYLESHEET RULES AND ITS ONE-WAY DRAWING, read off the
 * stylesheet and src/sidebar/: the current row's fill, rounding and weight,
 * the one highlight tint, the ⋯'s padding and hover, the detail value grey,
 * and the one place a row finds or makes its children's box.
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { drawRosterRow } from "../../src/sidebar/row.js";
import stylesheet from "../../src/styles.css?raw";
import { codeOf, functionBody } from "../../test/source-text.js";
import { cascadedValue, inRail } from "../../test/stylesheet.js";
import { moreOf } from "../../test/sidebar/row-parts.js";
import { row, sidebarContext } from "../../test/sidebar/harness.js";
import { rulesOf } from "../stylesheet-text.js";

describe("the highlight and the receded styling", () => {
  /** The declarations of every stylesheet rule that paints a current row. */
  const currentRowDeclarations = (): string[] =>
    rulesOf(stylesheet)
      .filter((rule) => rule.selectors.some((sel) => sel.includes(".ws.current")))
      .map((rule) => rule.declarations);

  it("draws no gutter bar on the current row (owner request, 2026-10-25)", () => {
    // Arrange / Act
    const declarations = currentRowDeclarations();
    // Assert
    expect(declarations.some((d) => /border-left/.test(d))).toBe(false);
  });

  it("rounds every row's fill subtly", () => {
    // Arrange / Act
    const row = rulesOf(stylesheet).find((rule) => rule.selectors.includes("#ws-sidebar .row"))?.declarations;
    // Assert
    expect(row).toMatch(/border-radius:\s*5px/);
  });

  it("fills the current row's line midway from --card toward the rail, 4.4% brighter", () => {
    // Arrange / Act
    const declarations = currentRowDeclarations();
    const token = (name: string): string | undefined =>
      rulesOf(stylesheet)
        .filter((rule) => rule.selectors.includes("#ws-sidebar"))
        .map((rule) => new RegExp(`${name}:\\s*([^;]+);`).exec(rule.declarations)?.[1])
        .find((value) => value !== undefined);
    // Assert
    expect([
      declarations.some((d) => /background-color:\s*var\(--row-highlight-tint\)/.test(d)),
      token("--row-highlight-tint"),
    ]).toEqual([true, "var(--highlight-grey)"]);
  });

  it("derives the shared highlight grey midway from --card toward the rail, 4.4% brighter", () => {
    // Arrange / Act
    const root = rulesOf(stylesheet)
      .filter((rule) => rule.selectors.includes(":root"))
      .map((rule) => /--highlight-grey:\s*([^;]+);/.exec(rule.declarations)?.[1])
      .find((value) => value !== undefined);
    // Assert
    expect(root).toBe("color-mix(in srgb, white 4.4%, color-mix(in srgb, var(--card) 69%, var(--sidebar-base)))");
  });

  // THE HOVER AND SELECTED HIGHLIGHTS ARE ONE BRIGHTNESS (owner request,
  // 2026-10-26), in the workspaces list and the chat list alike: every
  // hovered and every selected row fill names the one tint.
  it.each([
    "#ws-sidebar:not(.sb-hover-suspended) .row:hover",
    "#ws-sidebar:not(.sb-hover-suspended) .sb-outline-row:hover",
    "#ws-sidebar .ws.current > .row",
    "#ws-sidebar .sb-outline-row.shown",
  ])("fills %s with the one highlight tint", (selector) => {
    // Arrange / Act
    const fill = rulesOf(stylesheet)
      .filter((rule) => rule.selectors.includes(selector))
      .map((rule) => /background(?:-color)?:\s*([^;]+);/.exec(rule.declarations)?.[1])
      .filter((value) => value !== undefined);
    // Assert
    expect(fill).toEqual(["var(--row-highlight-tint)"]);
  });

  it("does not embolden the current row", () => {
    // Arrange / Act
    const declarations = currentRowDeclarations();
    // Assert
    expect(declarations.filter((d) => /\bfont-weight\s*:/.test(d))).toEqual([]);
  });
});

describe("the when-column", () => {
  it("stands an open row's ⋯ at the one right padding a row and a merge queue line share", () => {
    // Arrange
    const rules = rulesOf(stylesheet);
    const paddingRight = (selector: string): string | undefined =>
      /padding:\s*\S+\s+(\S+)/.exec(rules.find((rule) => rule.selectors.includes(selector))?.declarations ?? "")?.[1];
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    // Act
    const right = inRail(drawn, () => cascadedValue(moreOf(drawn), "right"));
    // Assert
    expect([right, paddingRight("#ws-sidebar .row"), paddingRight("#ws-sidebar .sb-mq-waiting")]).toEqual([
      "var(--row-pad-right)",
      "var(--row-pad-right)",
      "var(--row-pad-right)",
    ]);
  });

  it("draws a hovered open row's ⋯ in its duration's place", () => {
    // Arrange / Act
    const hiding = rulesOf(stylesheet).find((rule) =>
      rule.selectors.includes("#ws-sidebar:not(.sb-hover-suspended) .ws:not(.receded) > .row:hover > .when"),
    )?.declarations;
    // Assert
    expect(hiding).toMatch(/visibility:\s*hidden/);
  });
});

describe("the detail panel", () => {
  it("derives the value grey from the titles' grey", () => {
    const root = rulesOf(stylesheet).filter((rule) => rule.selectors.includes(":root"));
    const values = root.map((rule) => /--detail-value:\s*([^;]+);/.exec(rule.declarations)?.[1]).filter(Boolean);
    expect(values).toEqual(["color-mix(in srgb, var(--muted) 82%, #000000)"]);
  });
});

describe("kidsBoxOf", () => {
  it.each([
    ["row.ts", "appendRowKids"],
    ["subsections.ts", "nestLists"],
  ])("is how %s finds or makes a row's box", (file, fn) => {
    // Arrange
    const source = codeOf(readFileSync(join(__dirname, "..", "..", "src", "sidebar", file), "utf8"));

    // Act
    const body = functionBody(source, fn);

    // Assert
    expect([body.includes("kidsBoxOf"), body.includes("createElement")]).toEqual([true, false]);
  });
});

describe("appendRowKids", () => {
  it("is the one place a row nests its children", () => {
    // Arrange
    const source = readFileSync(join(__dirname, "../../src/sidebar/row.ts"), "utf8");

    // Act
    const handRolled = source.match(/kids\.className = /g) ?? [];

    // Assert
    expect(handRolled).toHaveLength(1);
  });
});
