/**
 * The footer's stylesheet rules (src/styles.css): the divider's darker token
 * derived from the row delimiter's, and the section's height capped at the
 * markup's row count.
 */
import { describe, expect, it } from "vitest";
import STYLESHEET from "../../src/styles.css?raw";

describe("mountFooter: the strip on top, the expanded section under it", () => {
  it("takes its darker token from the row delimiter's own, one step down", () => {
    // Arrange / Act / Assert: same hue, derived — never a second unrelated grey.
    expect(STYLESHEET).toMatch(
      /--border-strong:\s*color-mix\(in srgb, var\(--border\) 85%, var\(--fg\)\);/,
    );
  });
});

describe("mountFooter: the expanded section's scroll is the reader's", () => {
  it("caps the section's height at the markup's row count in the stylesheet", () => {
    // Assert: the ceiling is rows x row height, with the count read from the markup.
    expect(STYLESHEET).toMatch(
      /\.pfooter-sheet \{[^}]*max-height: calc\(var\(--pfooter-row-h\) \* var\(--pfooter-sheet-rows\)\);/,
    );
  });
});
