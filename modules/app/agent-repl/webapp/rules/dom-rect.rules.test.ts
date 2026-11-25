/**
 * The scripted DOM geometry of test/dom-rect.ts is the only one: no suite
 * hand-rolls its own DOMRect, element box or settable scrollTop.
 */
import { describe, expect, it } from "vitest";
import { codeUnder, rulesDir, testDir } from "./files-under.js";

/** Every test code file, test/ then rules/, as code keyed by its path under its own tree. */
function testCodeUnder(): [string, string][] {
  return [...codeUnder(testDir), ...codeUnder(rulesDir)];
}

describe("boxRect", () => {
  it("is the one scripted DOMRect: no suite hand-rolls its own", () => {
    // Arrange
    const suites = testCodeUnder();
    // Act
    const handRolled = [...suites].filter(([path, code]) => path !== "dom-rect.ts" && /toJSON:\s*\(\)\s*=>/.test(code)).map(([path]) => path);
    // Assert
    expect(handRolled).toEqual([]);
  });
});

describe("boxAt", () => {
  it("is how the sidebar's drag suite fixes one element's box: no copy of its own", () => {
    // Arrange
    const suites = codeUnder(testDir);
    // Act
    const code = suites.get("sidebar/subsections.test.ts") ?? "";
    const handRolled = code.match(/defineProperty\(el, "getBoundingClientRect", \{ configurable: true, value: \(\) => boxRect\(\{ top/g) ?? [];
    // Assert
    expect([code.includes("boxAt("), handRolled.length]).toEqual([true, 0]);
  });
});

describe("settableScrollTop", () => {
  it("is the one settable scrollTop of the feed suites: none hand-rolls its own", () => {
    // Arrange
    const suites = testCodeUnder();
    // Act
    const handRolled = [...suites]
      .filter(([path, code]) => path.startsWith("feed/") && /defineProperty\([^,]+,\s*"scrollTop"/.test(code))
      .map(([path]) => path);
    // Assert
    expect(handRolled).toEqual([]);
  });

});
