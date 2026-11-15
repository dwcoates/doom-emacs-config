// @vitest-environment jsdom
// A workspace row's grey child count. See src/sidebar/child-count.ts.
import { describe, expect, it } from "vitest";
import { CHILD_COUNT_CLASS, CHILD_COUNT_THRESHOLD, childCountOf, drawChildCount } from "../../src/sidebar/child-count.js";
import { drawRosterFamily } from "../../src/sidebar/row.js";
import { appContext, openRow, row, sidebarContext } from "./harness.js";

const kids = (n: number) => Array.from({ length: n }, (_, i) => row({ id: `kid-${i}` }));
const drawnCount = (n: number): HTMLElement | null =>
  drawRosterFamily(openRow(row({ id: "p", children: kids(n) })), sidebarContext(appContext()), "R").querySelector(
    `:scope > .row > .${CHILD_COUNT_CLASS}`,
  );

describe("childCountOf", () => {
  it("counts own children", () => {
    // Act / Assert
    expect(childCountOf({ children: [{}, {}, {}] })).toBe(3);
  });
  it("counts nothing for no kin", () => {
    expect(childCountOf({})).toBe(0);
  });
});

describe("drawChildCount", () => {
  it("draws nothing at the threshold", () => {
    expect(drawChildCount({ children: Array.from({ length: CHILD_COUNT_THRESHOLD }, () => ({})) })).toBeUndefined();
  });
  it("draws the count past the threshold, as a subsection's count", () => {
    // Act
    const drawn = drawChildCount({ children: Array.from({ length: CHILD_COUNT_THRESHOLD + 1 }, () => ({})) });

    // Assert
    expect(drawn?.textContent).toBe("(6)");
    expect(drawn?.classList.contains("sb-count")).toBe(true);
  });
});

describe("a drawn row", () => {
  it("wears the count with more than five children", () => {
    expect(drawnCount(7)?.textContent).toBe("(7)");
  });
  it("wears no count with five children", () => {
    expect(drawnCount(5)).toBeNull();
  });
});
