// @vitest-environment jsdom
// A workspace row's grey child count. See src/sidebar/child-count.ts.
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { RosterOpenRowTabSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { CHILD_COUNT_CLASS, CHILD_COUNT_THRESHOLD, childCountOf, drawChildCount } from "../../src/sidebar/child-count.js";
import type { RowFamily } from "../../src/sidebar/family.js";
import { drawRosterFamily } from "../../src/sidebar/row.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { openKin } from "../../src/sidebar/entries.js";
import { withRepositoryBlocks } from "../roster-families.js";
import { appContext, openRow, row, sidebarContext } from "./harness.js";

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
  it("counts own children, the working ones and the synchronously merging ones", () => {
    // Act / Assert
    expect(childCountOf({ children: children(4, 1, 2) }, "R")).toEqual({ workspaces: 4, working: 1, syncMerging: 2 });
  });
  it("counts a folded child's work as any other's", () => {
    // Arrange: child 0 is folded and working.
    // Act / Assert
    expect(childCountOf({ children: children(2, 1) }, "R")).toEqual({ workspaces: 2, working: 1, syncMerging: 0 });
  });
  it("counts every repository block's rows", () => {
    // Arrange
    const blocks = [{ rows: [openRow(row({ id: "b-1" })), openRow(row({ id: "b-2" }))] }] as unknown as RowFamily["repositoryBlocks"];

    // Act / Assert
    expect(childCountOf({ children: children(1, 1), repositoryBlocks: blocks }, "R")).toEqual({ workspaces: 3, working: 1, syncMerging: 0 });
  });
  it("counts exactly the rows openKin reads, the one walk of a row's kin", () => {
    // Arrange
    const open = withRepositoryBlocks(openRow(row({ id: "p", children: kids(2) })), [{ repo: "repo-2", rows: kids(3) }]);

    // Act / Assert
    expect(childCountOf(open, "R").workspaces).toBe(openKin(open, "R").length);
  });
  it("counts nothing for no kin", () => {
    expect(childCountOf({}, "R")).toEqual({ workspaces: 0, working: 0, syncMerging: 0 });
  });
  it("refuses a child with no row", () => {
    expect(() => childCountOf({ children: [{}] }, "R")).toThrow(MalformedView);
  });
});

describe("drawChildCount", () => {
  it("draws nothing at the threshold", () => {
    expect(drawChildCount({ children: children(CHILD_COUNT_THRESHOLD) }, "R")).toBeUndefined();
  });
  it("draws the count past the threshold, as a subsection's count", () => {
    // Act
    const drawn = drawChildCount({ children: children(CHILD_COUNT_THRESHOLD + 1, 2, 1) }, "R");

    // Assert
    expect([drawn?.textContent, drawn?.classList.contains("sb-count")]).toEqual(["(2/1/6)", true]);
  });
});

describe("a drawn row", () => {
  it("wears the count with more than five children", () => {
    expect(drawnCount(7)?.textContent).toBe("(0/0/7)");
  });
  it("wears no count with five children", () => {
    expect(drawnCount(5)).toBeNull();
  });
});
