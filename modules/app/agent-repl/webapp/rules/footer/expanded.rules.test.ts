/**
 * The expanded footer's rules: one search of the chip panels and one builder
 * of a row's text span (src/footer/expanded.ts), and the jump row's hover
 * wash (src/styles.css).
 */
import { describe, expect, it } from "vitest";
import { withoutBlockComments } from "../../test/source-text.js";
import stylesheet from "../../src/styles.css?raw";
import expandedSource from "../../src/footer/expanded.ts?raw";

describe("firstLiveChipPanel: the first chip panel with entries", () => {
  it("is the one place the chip panels are searched for entries", () => {
    // A resolver hand-rolling its own search would drift from the others.
    const searches = withoutBlockComments(expandedSource).match(/CHIP_PANELS\.(find|some|filter)\(/g) ?? [];
    expect(searches).toHaveLength(1);
  });
});

describe("a detached-work row is one hit target", () => {
  it("washes the whole row on hover, as the agent roster's pressable rows do", () => {
    // Arrange
    const rule = /\.footer-row-jump:hover\s*\{([^}]*)\}/.exec(withoutBlockComments(stylesheet));
    // Act
    const declarations = rule?.[1] ?? "";
    // Assert
    expect(declarations).toMatch(/background:\s*var\(--bg\)/);
  });
});

describe("rowText", () => {
  it("is the one way a row's text span is built", () => {
    const source = withoutBlockComments(expandedSource);
    expect(source).not.toMatch(/\.className = "footer-row-(label|description|command)";\n\s*\w+\.textContent/);
  });
});
