/**
 * THE TOOL GROUP'S STYLES, read off src/styles.css as text: the tab strip's
 * size, the clock a grouped member's head hides, and a stacked ending's fit.
 */
import { describe, expect, it } from "vitest";
import stylesheet from "../../src/styles.css?raw";
import { TABLINE_HEIGHT_PROPERTY } from "../../src/feed/tool-group.js";

describe("the tab strip's size (styles.css)", () => {
  // Owner-prescribed, in two rulings. The tabs were first halved (2026-09-20);
  // at that size they were hard to see, so every dimension then grew by a
  // QUARTER (2026-09-21) — the type, the icon gap and both paddings alike, so
  // the tab scales evenly rather than stretching in one direction. Pinned here,
  // alongside the module that draws them, so a future resize of
  // `.feed-group-tabline`/`.feed-group-tab` is a deliberate edit to this test
  // rather than a silent drift.
  /** The declaration block for SELECTOR, exactly as it appears in the sheet. */
  function ruleBodyOf(selector: string): string {
    const pattern = new RegExp(
      `${selector.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}\\s*\\{([^}]*)\\}`,
    );
    const match = pattern.exec(stylesheet);
    if (match === null) throw new Error(`no rule found for ${selector}`);
    return match[1];
  }

  // The strip's size is the SHARED feed tab strip's (src/feed/tabs.ts), which
  // the group's strip wears beside its own positioning class.
  it("grows the strip's gap and padding by a quarter", () => {
    const body = ruleBodyOf(".feed-tab-strip");
    expect(body).toMatch(/(^|[;\s])gap\s*:\s*0\.156rem\s*;/);
    expect(body).toMatch(/(^|[;\s])padding\s*:\s*0\.062rem 0 0\.188rem\s*;/);
  });

  it("draws no outer bubble around the group", () => {
    const body = ruleBodyOf(".feed-group");
    expect(body).not.toMatch(/(^|[;\s])(background|border|padding)\s*:/);
  });

  it("draws no line between the tabs and the card's content", () => {
    expect(ruleBodyOf(".feed-group-tabline")).not.toMatch(/border/);
  });

  it("lays the tabline over the member card, inset by the card's border and padding", () => {
    const body = ruleBodyOf(".feed-group-tabline");
    expect(body).toMatch(/(^|[;\s])position\s*:\s*absolute\s*;/);
    expect(body).toMatch(/(^|[;\s])top\s*:\s*calc\(1px \+ 0\.3rem\)\s*;/);
    expect(body).toMatch(/(^|[;\s])left\s*:\s*calc\(1px \+ 0\.75rem\)\s*;/);
  });

  it("grows the member card's top padding by the tabline's measured height", () => {
    const body = ruleBodyOf(".feed-group-panel > .feed-item > .tool-card");
    expect(body).toMatch(
      new RegExp(`padding-top\\s*:\\s*calc\\(0\\.3rem \\+ var\\(${TABLINE_HEIGHT_PROPERTY}\\) \\+ 0\\.3rem\\)\\s*;`),
    );
  });

  it("grows a tab's gap, padding and font-size by the same quarter", () => {
    const body = ruleBodyOf(".feed-group-tab");
    expect(body).toMatch(/(^|[;\s])gap\s*:\s*0\.156rem\s*;/);
    expect(body).toMatch(/(^|[;\s])padding\s*:\s*0\.094rem 0\.281rem\s*;/);
    expect(body).toMatch(/(^|[;\s])font-size\s*:\s*0\.625em\s*;/);
  });
});

describe("the tabline's clock", () => {
  it("hides the clock a grouped member's own head draws (styles.css)", () => {
    // Arrange
    const pattern = /\.feed-group-panel > \.feed-item > \.tool-card > \.tool-head > \.tool-clock\s*\{([^}]*)\}/;
    // Act
    const match = pattern.exec(stylesheet);
    // Assert
    expect(match?.[1]).toMatch(/(^|[;\s])display\s*:\s*none\s*;/);
  });
});

describe("the vendor-fault group", () => {
  it("lets a stacked ending's row fill the panel rather than take its own cap", () => {
    // Arrange
    const pattern = /\.feed-group-panel > \.feed-item > \.turn-ended\s*\{([^}]*)\}/;
    // Act
    const match = pattern.exec(stylesheet);
    // Assert
    expect(match?.[1]).toMatch(/(^|[;\s])width\s*:\s*auto\s*;/);
  });
});

describe("the vendor-fault group", () => {
  it("grows a stacked ending's card by the tabline, as a tool card member's", () => {
    // Arrange
    const pattern = /\.feed-group-panel > \.feed-item > \.turn-ended > \.tool-card\s*\{([^}]*)\}/;
    // Act
    const match = pattern.exec(stylesheet);
    // Assert
    expect(match?.[1]).toContain("var(--feed-group-tabline-h)");
  });
});
