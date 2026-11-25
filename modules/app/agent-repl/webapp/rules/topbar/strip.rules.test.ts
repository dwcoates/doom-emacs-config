/**
 * THE SUMMARY MENU'S LINK ROWS AS THE STYLESHEET PAINTS THEM: an owned row's
 * ground, each pull request badge's color and each mark's color, read off
 * `src/styles.css`.
 */
import { describe, expect, it } from "vitest";
import styles from "../../src/styles.css?raw";
import { withoutBlockComments } from "../../test/source-text.js";

describe("a summary menu row's owned tint and service icon", () => {
  it("tints an owned row on a ground between the menu's and the topbar's", () => {
    const rule = withoutBlockComments(styles).match(/\.topbar-title-link-option\.is-owned\s*\{([^}]*)\}/);
    expect(rule?.[1]).toMatch(/background:\s*color-mix\(in srgb, var\(--bg\) 50%, var\(--sidebar-ground\)\)/);
  });
});

describe("a summary menu row's pull request badge and owned label", () => {
  it.each([
    ["open", "--fg"],
    ["draft", "--muted"],
    ["merged", "--ok"],
    ["closed", "--err"],
  ])("colors the %s badge with %s", (state, token) => {
    const rule = withoutBlockComments(styles).match(
      new RegExp(`\\.topbar-title-link-badge\\[data-badge="${state}"\\]\\s*\\{([^}]*)\\}`),
    );
    expect(rule?.[1]).toMatch(new RegExp(`color:\\s*var\\(${token}\\)`));
  });
});

describe("a summary menu row's owned and subscribed marks and its untinted icon", () => {
  it.each([
    ["owned", "--fg"],
    ["subscribed", "--ok"],
  ])("draws the %s mark's text and border in %s", (mark, token) => {
    const css = withoutBlockComments(styles);
    const rule = css.match(new RegExp(`\\.topbar-title-link-mark\\[data-mark="${mark}"\\]\\s*\\{([^}]*)\\}`));
    const base = css.match(/\.topbar-title-link-mark\s*\{([^}]*)\}/);
    expect([rule?.[1].match(new RegExp(`color:\\s*var\\(${token}\\)`)) !== null, base?.[1].includes("border-color: currentColor")]).toEqual([
      true,
      true,
    ]);
  });
});
