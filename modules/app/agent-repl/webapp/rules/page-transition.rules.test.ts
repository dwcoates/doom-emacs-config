/** The test code dispatches a page transition only through dispatchPageTransition (test/page-transition.ts). */
import { describe, expect, it } from "vitest";
import { codeUnder, rulesDir, testDir } from "./files-under.js";

describe("the call sites", () => {
  it("dispatch a page transition only through dispatchPageTransition", () => {
    // Arrange / Act
    const rolled = [...codeUnder(testDir), ...codeUnder(rulesDir)]
      .filter(([file, code]) => file !== "page-transition.ts" && /new\s+(?:[\w$]+\.)*PageTransitionEvent\(/.test(code))
      .map(([file]) => file);
    // Assert
    expect(rolled).toEqual([]);
  });
});
