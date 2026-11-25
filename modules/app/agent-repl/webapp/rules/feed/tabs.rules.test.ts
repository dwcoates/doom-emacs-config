/**
 * THE FEED TABS' STYLES, read off src/styles.css as text.
 */
import { describe, expect, it } from "vitest";
import stylesheet from "../../src/styles.css?raw";

describe("drawFeedTabStrip", () => {
  it("states the strip's size once, and no position", () => {
    // Arrange
    const match = /\.feed-tab-strip\s*\{([^}]*)\}/.exec(stylesheet);
    // Assert
    expect(match).not.toBeNull();
    expect(match?.[1]).toMatch(/(^|[;\s])gap\s*:\s*0\.156rem\s*;/);
    expect(match?.[1]).not.toMatch(/position\s*:/);
  });
});

describe("drawFeedTabline", () => {
  it("lays the strip and its clock out as one row, the clock after the tabs", () => {
    // Arrange
    const line = /\.feed-tabline\s*\{([^}]*)\}/.exec(stylesheet);
    const strip = /\.feed-tabline > \.feed-tab-strip\s*\{([^}]*)\}/.exec(stylesheet);
    // Assert
    expect(line?.[1]).toMatch(/(^|[;\s])display\s*:\s*flex\s*;/);
    expect(strip?.[1]).toMatch(/(^|[;\s])flex\s*:\s*1 1 auto\s*;/);
  });
});
