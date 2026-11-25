/**
 * THE TOPBAR STRIP'S LAYOUT, GEOMETRY AND CELLS AS THE STYLESHEET DECLARES
 * THEM: read off `src/styles.css`, since jsdom lays nothing out.
 */
import { describe, expect, it } from "vitest";
import STYLESHEET from "../../src/styles.css?raw";
import { withoutBlockComments } from "../../test/source-text.js";

/**
 * THE STRIP'S LAYOUT (owner rulings 1-4, 2026-09-13).
 *
 * jsdom resolves the cascade but lays nothing out, so a layout claim is
 * asserted where it is DECIDED — the declarations in `src/styles.css` and the
 * DOM order `drawTopbarView` builds — rather than by measuring boxes that are
 * all zero here. The stylesheet is read raw for the same reason
 * `test/feed/rows/separation.test.ts` reads it: the token a rule was written
 * with is the assertion, and jsdom would hand back a resolved-away shorthand.
 */
function ruleBody(selector: string): string {
  const start = STYLESHEET.indexOf(`\n${selector} {`);
  expect(start).toBeGreaterThan(-1);
  const open = STYLESHEET.indexOf("{", start);
  const close = STYLESHEET.indexOf("}", open);
  return STYLESHEET.slice(open + 1, close);
}

/** The value of DECLARATION in SELECTOR's block, comments stripped. */
function declaration(selector: string, property: string): string {
  const body = withoutBlockComments(ruleBody(selector));
  const match = new RegExp(`(?:^|;|\\n)\\s*${property}\\s*:([^;]*);`).exec(body);
  expect(match).not.toBeNull();
  return (match?.[1] ?? "").trim();
}

/** The rem value of the root custom property NAME, as declared. */
function token(name: string): number {
  const match = new RegExp(`\\n\\s*${name}\\s*:\\s*([\\d.]+)rem\\s*;`).exec(STYLESHEET);
  expect(match).not.toBeNull();
  return Number.parseFloat(match?.[1] ?? "NaN");
}

describe("the strip's layout", () => {
  // RULING 1. Not "both are 0.5rem" — both are the SAME TOKEN, which is what
  // keeps them from drifting apart the next time one of them is tuned.
  it("pads the strip's edges with the very token that gaps its tracks", () => {
    // ARRANGE / ACT
    const padding = declaration(".topbar-row", "padding");
    const gap = declaration(".topbar-row", "gap");
    // ASSERT
    expect(padding).toBe(`0 ${gap}`);
  });

  // THE 2026-09-14 RULING: "a reasonable, and tight spacing between chips, and
  // ALL extra space to be around the center title section". The chips run on
  // their own tighter token while the frame — edge inset and track gap — keeps
  // the cell gap, so the room the chips give back falls to the title.
  it("gaps the chips inside a flank with a tighter token than the row's frame", () => {
    // ARRANGE / ACT
    const flanks = [declaration(".topbar-left", "gap"), declaration(".topbar-chips", "gap")];
    const row = declaration(".topbar-row", "gap");
    // ASSERT
    expect([...flanks, row]).toEqual([
      "var(--topbar-chip-gap)",
      "var(--topbar-chips-gap)",
      "var(--topbar-cell-gap)",
    ]);
  });

  // The chip runs are spaced by their shared class alone, so neither flank
  // group may carry a chip gap of its own that would compete with it.
  it("gaps the right group's chips from the chip run's rule alone", () => {
    expect(withoutBlockComments(ruleBody(".topbar-right"))).not.toMatch(/(?:^|;|\n)\s*gap\s*:/);
  });

  // THE 2026-10-02 REQUEST: the right-hand chips "30% closer". The space a
  // reader sees between two of them is the gap plus each side's inset and 1px
  // border, which was 0.3rem + 2 x (0.45rem + 1px) = 21.2px at the 16px root.
  it("spaces the right-hand chips at least 30% closer than the 21.2px they stood apart", () => {
    // ARRANGE
    const root = 16;
    // ACT
    const spacing = (token("--topbar-chips-gap") + 2 * token("--topbar-chips-inset")) * root + 2;
    // ASSERT
    expect(spacing).toBeLessThanOrEqual(21.2 * 0.7);
  });

  // THE 2026-11-10 REQUEST: the chip gaps "narrow by a quarter" again, every
  // rem going to the title's flanks. The rems of the 30%-closer spacing,
  // 0.2rem + 2 x 0.3rem, narrow by exactly a quarter; the 1px borders stay.
  it("narrows the chips' rem spacing by a quarter from the 30%-closer measure", () => {
    const rems = token("--topbar-chips-gap") + 2 * token("--topbar-chips-inset");
    expect(rems).toBeCloseTo((0.2 + 2 * 0.3) * 0.75, 6);
  });

  // ONE RULE FOR EVERY CHIP, keyed on its run: a button inside a control's
  // wrap, or a cell that holds no button.
  it("insets every chip from the chip run's one rule", () => {
    expect(declaration(".topbar-chips ar-button,\n.topbar-chips > :not(:has(ar-button))", "padding-inline")).toBe(
      "var(--topbar-chips-inset)",
    );
  });

  // THE 2026-09-14 MEASUREMENT: `header#topbar` spanned x 423-2036 while the
  // row inside it spanned only 423-1009. The header's one in-flow child is a
  // flex item, and a flex item with no width rule shrink-wraps to its content,
  // so the row's two free tracks were dividing the content width instead of
  // the header's. These two declarations are what stop a future edit
  // shrink-wrapping it again.
  it("spans the header rather than shrink-wrapping to its content", () => {
    // ARRANGE / ACT
    const width = declaration(".topbar-row", "width");
    // ASSERT
    expect(width).toBe("100%");
  });

  it("lets the strip between the header and the row take the header's width", () => {
    // ARRANGE / ACT
    const grow = declaration(".topbar-strip", "flex");
    // ASSERT
    expect(grow).toBe("1 1 0");
  });

  it("sets the chip gap tighter than the frame measure it was split from", () => {
    // ARRANGE / ACT
    const chip = token("--topbar-chip-gap");
    const cell = token("--topbar-cell-gap");
    // ASSERT
    expect(chip).toBeLessThan(cell);
  });
});

describe("the strip's geometry", () => {
  // jsdom lays out nothing, so the rules that make the centering hold are
  // asserted as declarations; test/webkit/topbar.webkit.test.ts measures it.
  it("lays the row out with equal side tracks around a shrinkable middle track", () => {
    expect(declaration(".topbar-row", "grid-template-columns")).toBe(
      "minmax(min-content, 1fr) minmax(0, auto) minmax(min-content, 1fr)",
    );
  });

  it("centers every cell of the row vertically in the strip", () => {
    expect(declaration(".topbar-row", "align-items")).toBe("center");
  });

  it.each([".topbar-left", ".topbar-right"])("leaves %s no height of its own", (selector) => {
    expect(withoutBlockComments(ruleBody(selector))).not.toMatch(/(?:^|;|\n)\s*height\s*:/);
  });

  it("hangs each flank group on its own edge of the strip", () => {
    expect([
      declaration(".topbar-left", "justify-self"),
      declaration(".topbar-right", "justify-self"),
    ]).toEqual(["start", "end"]);
  });

  // A group that would not compress with its floorless track would spill out
  // of it and over the title, which is the one thing the tracks are there to
  // prevent.
  it("lets each flank group compress with its own track", () => {
    expect([
      declaration(".topbar-left", "min-width"),
      declaration(".topbar-right", "min-width"),
    ]).toEqual(["0", "0"]);
  });
});

describe("the account cell", () => {
  // THE LOGIN GLYPH HUGS THE LOGIN NAME (owner request, 2026-11-10): no gap
  // inside the cell, and only the label's shrunken left inset between them.
  it("stands the glyph against its label with no gap inside the cell", () => {
    expect(declaration(".topbar-account-cell", "gap")).toBe("0");
  });

  it("insets the label from the glyph by 0.15rem alone", () => {
    expect(declaration(".topbar-account-cell > .topbar-account", "padding-left")).toBe("0.15rem");
  });
});

describe("the connectivity glyph's session dropdown", () => {
  it("wears a pointer only while it opens something", () => {
    expect(declaration(".topbar-connectivity[data-reveal-anchor]", "cursor")).toBe("pointer");
  });
});

describe("the no-session cells", () => {
  // A DASH IS A LABEL, NOT A CONTROL: it takes the right group's box so the
  // slot keeps its width and its neighbours do not move, and it takes neither
  // the cursor nor the hover border that would promise a click.
  it("boxes every no-session cell exactly as the strip's other right-hand cells", () => {
    expect(
      declaration(".topbar-model[data-no-session],\n.topbar-effort[data-no-session],\n.topbar-effort[data-effort-unsupported],\n.topbar-mode[data-no-session]", "padding-block"),
    ).toBe(
      declaration(
        ".topbar-model-button,\n.topbar-effort-button,\n.topbar-mode-button,\n.topbar-context-figure,\n.topbar-queue-bypass-button,\n.topbar-remote-control-button,\n.topbar-wifi-button",
        "padding-block",
      ),
    );
  });

  // The inline inset is the group's, so a cell's own rule setting the
  // `padding` shorthand would quietly win over it for that one cell.
  it.each([
    ".topbar-model[data-no-session],\n.topbar-effort[data-no-session],\n.topbar-effort[data-effort-unsupported],\n.topbar-mode[data-no-session]",
    ".topbar-model-button,\n.topbar-effort-button,\n.topbar-mode-button,\n.topbar-context-figure,\n.topbar-queue-bypass-button,\n.topbar-remote-control-button,\n.topbar-wifi-button",
  ])("leaves the inline inset of %s to the group", (selector) => {
    expect(withoutBlockComments(ruleBody(selector))).not.toMatch(/(?:^|[;\s])padding(?:-inline)?(?:-left|-right)?\s*:/);
  });
});
