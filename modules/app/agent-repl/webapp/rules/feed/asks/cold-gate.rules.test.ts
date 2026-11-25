/**
 * The cold gate's stylesheet rules (src/styles.css): its buttons' borders,
 * fills, greys and sizing, and its frame and divider in the card's heat.
 */
import { describe, expect, it } from "vitest";
// Imported at collection, not inside a test, so the transform never counts
// against a test's timeout.
import css from "../../../src/styles.css?raw";

describe("the standing gate", () => {
  it("gives compact the pay button's fill and a green border", () => {
    const rule = /\.cold-gate-buttons > ar-button\.cold-gate-compact\s*\{[^}]*\}/.exec(css)?.[0] ?? "";
    expect([rule.includes("background: var(--bg)"), rule.includes("border-color: var(--cold-gate-compact-border)")]).toEqual([true, true]);
  });

  it("draws clear's label in the shared colour, borders half again as thick", () => {
    expect([
      css.includes(".cold-gate-buttons > ar-button.cold-gate-clear { color: var(--fg); border-color: var(--info-tokens); }"),
      css.includes(".cold-gate-buttons > ar-button { border-width: 1.5px; }"),
      css.includes("--cold-gate-compact-border: #128a3d;"),
      css.includes(".cold-gate .cold-gate-actions:has(> .cold-gate-buttons) { margin-bottom: 0.45rem; }"),
    ]).toEqual([true, true, true, true]);
  });

  it("draws the frame and the submenu divider in the card's heat", () => {
    // Arrange
    // Act
    const frame = /\n\.cold-gate\s*\{[^}]*\}/.exec(css)?.[0] ?? "";
    const divider = /\.cold-gate-submenu\s*\{[^}]*\}/.exec(css)?.[0] ?? "";
    // Assert
    expect([
      frame.includes("border: 1px solid var(--cold-gate-heat, var(--revival-gate-border))"),
      divider.includes("border-top: 1px solid var(--cold-gate-heat, var(--revival-gate-border))"),
    ]).toEqual([true, true]);
  });

  it("draws pay and resume with the red border", () => {
    // Arrange
    // Act
    const rule = /\.cold-gate-buttons > ar-button\.cold-gate-direct\s*\{[^}]*\}/.exec(css)?.[0] ?? "";
    // Assert
    expect(rule.includes("border-color: var(--err)")).toBe(true);
  });

  it("sizes the buttons equal, to the widest label, centred", () => {
    const rule = /\.cold-gate-buttons\s*\{[^}]*\}/.exec(css)?.[0] ?? "";
    const label = /\.cold-gate-buttons > ar-button\s*\{[^}]*\}/.exec(css)?.[0] ?? "";
    expect([
      rule.includes("grid-auto-columns: 1fr"),
      rule.includes("width: max-content"),
      label.includes("text-align: center"),
    ]).toEqual([true, true, true]);
  });
});

describe("the hide workspace button", () => {
  it("is drawn in its own grey", () => {
    expect(
      css.includes(".cold-gate-buttons > ar-button.cold-gate-hide { color: var(--cold-gate-hide-grey); border-color: var(--cold-gate-hide-grey); }"),
    ).toBe(true);
  });

  it.each([
    ["light", "--muted: #6b7280;", "--cold-gate-hide-grey: #a3a8b1;"],
    ["dark", "--muted: #898782;", "--cold-gate-hide-grey: #bdbbb5;"],
  ])("wears a grey noticeably lighter than kill's in the %s theme", (_theme, muted, hide) => {
    // Arrange
    const lightness = (decl: string): number => {
      const hex = /#([0-9a-f]{6});$/.exec(decl)?.[1];
      if (hex === undefined) throw new Error(`no hex color in ${decl}`);
      const [r, g, b] = [0, 2, 4].map((i) => parseInt(hex.slice(i, i + 2), 16));
      return (Math.max(r, g, b) + Math.min(r, g, b)) / 2 / 255;
    };
    // Act
    const declared = [css.includes(muted), css.includes(hide)];
    // Assert
    expect(declared).toEqual([true, true]);
    expect(lightness(hide) - lightness(muted)).toBeGreaterThan(0.12);
  });
});

describe("the kill workspace button", () => {
  it("is drawn grey", () => {
    expect(
      css.includes(".cold-gate-buttons > ar-button.cold-gate-kill { color: var(--muted); border-color: var(--muted); }"),
    ).toBe(true);
  });
});
