// @vitest-environment jsdom
// The sidebar tests' shared harness. See test/sidebar/harness.ts.
import { describe, expect, it } from "vitest";
import { drawnEntries, scrollable } from "./harness.js";

describe("drawnEntries", () => {
  it("spells a section list's children, a row by its id and a subsection as [id]", () => {
    // Arrange
    const list = document.createElement("div");
    const rowEl = document.createElement("div");
    rowEl.setAttribute("data-roster-row", "a");
    const sub = document.createElement("div");
    sub.className = "sb-subsection";
    sub.setAttribute("data-subsection", "foo");
    list.append(rowEl, sub);

    // Act / Assert
    expect(drawnEntries(list)).toEqual(["a", "[foo]"]);
  });

  it("is the one spelling the sidebar tests read a section's list by", () => {
    // Arrange
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- see "is the one primary pointer the sidebar tests build".
    const sources = import.meta.glob("./*.test.ts", { query: "?raw", import: "default", eager: true }) as Record<string, string>;

    // Act
    const handRolled = Object.entries(sources)
      .filter(([, source]) => /getAttribute\("data-roster-row"\) \?\? `\[\$\{el\.getAttribute\("data-subsection"\)/.test(source))
      .map(([file]) => file);

    // Assert
    expect(handRolled).toEqual([]);
  });
});

describe("scrollable", () => {
  it.each([
    { name: "keeps an offset inside its range", to: 40, want: 40 },
    { name: "clamps an offset past the maximum", to: 140, want: 100 },
    { name: "clamps a negative offset to the top", to: -5, want: 0 },
  ])("$name", ({ to, want }) => {
    // Arrange
    const el = scrollable(document.createElement("div"), 100);
    // Act
    el.scrollTop = to;
    // Assert
    expect(el.scrollTop).toBe(want);
  });
});
