/**
 * THE RAIL'S HOVER HIGHLIGHTS IN THE STYLESHEET are each gated on the rail not
 * being suspended (`HOVER_SUSPENDED_CLASS`, src/sidebar/transients.ts).
 */
import { describe, expect, it } from "vitest";
import { HOVER_SUSPENDED_CLASS } from "../../src/sidebar/transients.js";
import stylesheet from "../../src/styles.css?raw";
import { rulesOf } from "../stylesheet-text.js";

describe("the rail's hover highlights in the stylesheet", () => {
  const railHoverSelectors = rulesOf(stylesheet)
    .flatMap((rule) => rule.selectors)
    .filter((selector) => selector.startsWith("#ws-sidebar") && selector.includes(":hover"));

  it("has hover highlights in the rail to gate", () => {
    expect(railHoverSelectors.length).toBeGreaterThan(0);
  });

  it("draws every one only while the rail is not suspended", () => {
    expect(
      railHoverSelectors.filter((selector) => !selector.startsWith(`#ws-sidebar:not(.${HOVER_SUSPENDED_CLASS}) `)),
    ).toEqual([]);
  });
});
