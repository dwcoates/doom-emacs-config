// @vitest-environment jsdom
// A workspace row's grey child count. See src/sidebar/child-count.ts.
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { RosterOpenRowTabSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { CHILD_COUNT_CLASS, CHILD_COUNT_THRESHOLD, childCountOf, drawChildCount } from "../../src/sidebar/child-count.js";
import type { RowFamily } from "../../src/sidebar/row.js";
import { drawRosterFamily } from "../../src/sidebar/row.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { appContext, openRow, row, sidebarContext } from "./harness.js";

const NUMBERED = create(RosterOpenRowTabSchema, { tab: { case: "numbered", value: { number: 1 } } });
const FOLDED = create(RosterOpenRowTabSchema, { tab: { case: "folded", value: {} } });
/** N children, the first UNFOLDED of them with a numbered tab and the rest folded. */
const children = (n: number, unfolded = n): RowFamily[] => Array.from({ length: n }, (_, i) => ({ tab: i < unfolded ? NUMBERED : FOLDED }));
const kids = (n: number) => Array.from({ length: n }, (_, i) => row({ id: `kid-${i}` }));
/** The count a row with N children draws, its children's tabs numbered when NUMBERED. */
const drawnCount = (n: number, numbered = false): HTMLElement | null => {
  const open = openRow(row({ id: "p", children: kids(n) }));
  if (numbered) open.children.forEach((child) => (child.tab = NUMBERED));
  return drawRosterFamily(open, sidebarContext(appContext()), "R").querySelector(`:scope > .row > .${CHILD_COUNT_CLASS}`);
};

describe("childCountOf", () => {
  it("counts own children, and the unfolded ones by their tabs", () => {
    // Act / Assert
    expect(childCountOf({ children: children(3, 1) }, "R")).toEqual({ workspaces: 3, unfolded: 1 });
  });
  it("counts every repository block's rows", () => {
    // Arrange
    const blocks = [{ rows: [openRow(row({ id: "b-1" })), openRow(row({ id: "b-2" }))] }] as unknown as RowFamily["repositoryBlocks"];

    // Act / Assert
    expect(childCountOf({ children: children(1), repositoryBlocks: blocks }, "R")).toEqual({ workspaces: 3, unfolded: 1 });
  });
  it("counts nothing for no kin", () => {
    expect(childCountOf({}, "R")).toEqual({ workspaces: 0, unfolded: 0 });
  });
  it("refuses a child with no tab", () => {
    expect(() => childCountOf({ children: [{}] }, "R")).toThrow(MalformedView);
  });
});

describe("drawChildCount", () => {
  it("draws nothing at the threshold", () => {
    expect(drawChildCount({ children: children(CHILD_COUNT_THRESHOLD) }, "R")).toBeUndefined();
  });
  it("draws the count past the threshold, as a subsection's count", () => {
    // Act
    const drawn = drawChildCount({ children: children(CHILD_COUNT_THRESHOLD + 1, 2) }, "R");

    // Assert
    expect([drawn?.textContent, drawn?.classList.contains("sb-count")]).toEqual(["(2/6)", true]);
  });
});

describe("a drawn row", () => {
  it("wears the count with more than five children", () => {
    expect(drawnCount(7, true)?.textContent).toBe("(7/7)");
  });
  it("counts its folded children out of the numerator", () => {
    expect(drawnCount(7)?.textContent).toBe("(0/7)");
  });
  it("wears no count with five children", () => {
    expect(drawnCount(5)).toBeNull();
  });
});
