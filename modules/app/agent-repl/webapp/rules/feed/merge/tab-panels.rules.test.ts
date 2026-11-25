/**
 * A merge tab panel's heading is swapped in one place
 * (src/feed/merge/tab-panels.ts, swapHeading).
 */
import { describe, expect, it } from "vitest";
import TAB_PANELS from "../../../src/feed/merge/tab-panels.ts?raw";

describe("swapHeading", () => {
  it("is the one way a panel's heading is swapped", () => {
    expect([TAB_PANELS.split("swapHeading(").length - 1, TAB_PANELS.split(".replaceWith(next)").length - 1]).toEqual([3, 1]);
  });
});
