// @vitest-environment jsdom
// A workspace row's grey child count. See src/sidebar/child-count.ts.
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { RosterOpenRowTabSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { CHILD_COUNT_CLASS, childCountOf, drawChildCount } from "../../src/sidebar/child-count.js";
import type { RowFamily } from "../../src/sidebar/family.js";
import { drawRosterFamily } from "../../src/sidebar/row.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { openKin } from "../../src/sidebar/entries.js";
import { withRepositoryBlocks } from "../roster-families.js";
import { appContext, openRow, row, sidebarContext } from "./harness.js";
import { cascadedValue, inRail } from "../stylesheet.js";
import { FOLDED_CLASS } from "../../src/sidebar/folded-class.js";

const NUMBERED = create(RosterOpenRowTabSchema, { tab: { case: "numbered", value: { number: 1 } } });
const FOLDED = create(RosterOpenRowTabSchema, { tab: { case: "folded", value: {} } });
/** N children, the first WORKING of them on a thinking status and the rest ready. */
const children = (n: number, working = 0, merging = 0): RowFamily[] =>
  Array.from({ length: n }, (_, i) => ({
    tab: i % 2 ? NUMBERED : FOLDED,
    row: row({ id: `c-${i}`, status: { case: i < working ? "thinking" : i < working + merging ? "merging" : "ready", value: {} } }),
  }));
const kids = (n: number) => Array.from({ length: n }, (_, i) => row({ id: `kid-${i}` }));
/** The count a row with N children draws. */
const drawnCount = (n: number): HTMLElement | null => {
  const open = openRow(row({ id: "p", children: kids(n) }));
  return drawRosterFamily(open, sidebarContext(appContext()), "R").querySelector(`:scope > .row > .${CHILD_COUNT_CLASS}`);
};

describe("childCountOf", () => {
  it("counts own children and the synchronously merging ones", () => {
    // Act / Assert
    expect(childCountOf({ children: children(4, 1, 2) }, "R")).toEqual({ workspaces: 4, syncMerging: 2 });
  });
  it("counts a folded child's work as any other's", () => {
    // Arrange: child 0 is folded and working.
    // Act / Assert
    expect(childCountOf({ children: children(2, 1) }, "R")).toEqual({ workspaces: 2, syncMerging: 0 });
  });
  it("counts every repository block's rows", () => {
    // Arrange
    const blocks = [{ rows: [openRow(row({ id: "b-1" })), openRow(row({ id: "b-2" }))] }] as unknown as RowFamily["repositoryBlocks"];

    // Act / Assert
    expect(childCountOf({ children: children(1, 1), repositoryBlocks: blocks }, "R")).toEqual({ workspaces: 3, syncMerging: 0 });
  });
  it("counts exactly the rows openKin reads, the one walk of a row's kin", () => {
    // Arrange
    const open = withRepositoryBlocks(openRow(row({ id: "p", children: kids(2) })), [{ repo: "repo-2", rows: kids(3) }]);

    // Act / Assert
    expect(childCountOf(open, "R").workspaces).toBe(openKin(open, "R").length);
  });
  it("counts nothing for no kin", () => {
    expect(childCountOf({}, "R")).toEqual({ workspaces: 0, syncMerging: 0 });
  });
  it("refuses a child with no row", () => {
    expect(() => childCountOf({ children: [{}] }, "R")).toThrow(MalformedView);
  });
});

describe("drawChildCount", () => {
  it("draws nothing with no children", () => {
    expect(drawChildCount({ children: children(0) }, "R")).toBeUndefined();
  });
  it("draws the count with any children, as a subsection's count", () => {
    // Act
    const drawn = drawChildCount({ children: children(6, 2, 1) }, "R");

    // Assert
    expect([drawn?.textContent, drawn?.classList.contains("sb-count")]).toEqual(["(1/6)", true]);
  });
});

describe("a drawn row", () => {
  it("wears the count with several children", () => {
    expect(drawnCount(7)?.textContent).toBe("(7)");
  });
  it("wears the count with one child", () => {
    expect(drawnCount(1)?.textContent).toBe("(1)");
  });
  it("wears no count with no children", () => {
    expect(drawnCount(0)).toBeNull();
  });
});

describe("a row's child count, painted", () => {
  const paintedDisplay = (folded: boolean): string => {
    const drawn = drawRosterFamily(openRow(row({ id: "p", children: kids(3) })), sidebarContext(appContext()), "R");
    drawn.classList.toggle(FOLDED_CLASS, folded);
    return inRail(drawn, () => {
      const count = drawn.querySelector<HTMLElement>(`:scope > .row > .${CHILD_COUNT_CLASS}`);
      if (count === null) throw new Error("the row draws no child count");
      return cascadedValue(count, "display");
    });
  };
  it("is not shown while the children are shown", () => {
    expect(paintedDisplay(false)).toBe("none");
  });
  it("is shown while the children are folded", () => {
    expect(paintedDisplay(true)).toBe("inline");
  });
});

describe("a subsection's member count, painted", () => {
  const paintedDisplay = (folded: boolean): string => {
    const subsection = document.createElement("div");
    subsection.className = "sb-subsection";
    subsection.classList.toggle(FOLDED_CLASS, folded);
    const head = document.createElement("div");
    head.className = "sb-subsection-head";
    const count = document.createElement("span");
    count.className = "sb-count";
    head.append(count);
    subsection.append(head);
    return inRail(subsection, () => cascadedValue(count, "display"));
  };
  it("is not shown while the subsection is open", () => {
    expect(paintedDisplay(false)).toBe("none");
  });
  it("is shown while the subsection is folded", () => {
    expect(paintedDisplay(true)).toBe("inline");
  });
});
