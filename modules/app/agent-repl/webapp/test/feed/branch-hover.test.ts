// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import { BRANCH_HOVER_CLASS, HOVER_LEFT_PROPERTY, HOVER_RIGHT_PROPERTY, installBranchHover } from "../../src/feed/branch-hover.js";
import { SELECTABLE_ATTRIBUTE, SELECTION_GOVERNED_ATTRIBUTE } from "../../src/feed/bubble-selection.js";
import { TREE_CLASS } from "../../src/metaprompt-tree.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { boxedBy, drawnRow, exampleSubtreeWidth, exampleTree, lineOf } from "./tree-fixture.js";

const HOVER_BOX = { className: BRANCH_HOVER_CLASS, leftProperty: HOVER_LEFT_PROPERTY, rightProperty: HOVER_RIGHT_PROPERTY };

/** The example tree drawn in row r1, as a response (UNIT) the daemon has or has not made SELECTABLE. */
function response(opts: { unit?: string; selectable?: boolean } = {}): HTMLElement {
  const row = drawnRow("r1", exampleTree);
  row.setAttribute("data-unit", opts.unit ?? "response");
  row.toggleAttribute(SELECTION_GOVERNED_ATTRIBUTE, true);
  row.toggleAttribute(SELECTABLE_ATTRIBUTE, opts.selectable ?? true);
  const prose = document.createElement("p");
  prose.id = "prose";
  prose.textContent = "said";
  row.append(prose);
  return row;
}

/** The pointer moves onto EL. */
const over = (el: Element): void => {
  el.dispatchEvent(new MouseEvent("mouseover", { bubbles: true }));
};

/** The words of NODE's line in ROW. */
const words = (row: HTMLElement, node: string): Element => {
  const content = lineOf(row, node).querySelector(".mp-content");
  if (content === null) throw new Error(`${node}'s line draws no words`);
  return content;
};

let uninstall: (() => void) | undefined;
afterEach(() => {
  uninstall?.();
  uninstall = undefined;
});

describe("the pointer's box over a response branch", () => {
  it("boxes the hovered node's subtree from its label's column to the widest line", () => {
    // Arrange
    const row = response();
    uninstall = installBranchHover(document.body);
    // Act
    over(words(row, "1.1"));
    // Assert
    expect(boxedBy(row, HOVER_BOX)).toEqual([
      ["1.1", "4ch", `${exampleSubtreeWidth}ch`],
      ["1.1.1", "4ch", `${exampleSubtreeWidth}ch`],
    ]);
  });

  it("moves to the next node the pointer moves onto", () => {
    // Arrange
    const row = response();
    uninstall = installBranchHover(document.body);
    over(words(row, "1.1"));
    // Act
    over(words(row, "1.2"));
    // Assert
    expect(boxedBy(row, HOVER_BOX).map(([node]) => node)).toEqual(["1.2"]);
  });

  it("is taken down when the pointer moves off the tree", () => {
    // Arrange
    const row = response();
    uninstall = installBranchHover(document.body);
    over(words(row, "1.1"));
    // Act
    over(row.querySelector("#prose") as Element);
    // Assert
    expect(boxedBy(row, HOVER_BOX)).toEqual([]);
  });

  it("is taken down when the pointer leaves the feed", () => {
    // Arrange
    const row = response();
    uninstall = installBranchHover(document.body);
    over(words(row, "1.1"));
    // Act
    document.body.dispatchEvent(new MouseEvent("mouseleave"));
    // Assert
    expect(boxedBy(row, HOVER_BOX)).toEqual([]);
  });

  it("is drawn again on the same node after the row's prose repaints", () => {
    // Arrange
    const row = response();
    uninstall = installBranchHover(document.body);
    over(words(row, "1.1"));
    const tree = row.querySelector(`.${TREE_CLASS}`) as HTMLElement;
    tree.replaceWith(tree.cloneNode(true));
    for (const line of row.querySelectorAll(`.${BRANCH_HOVER_CLASS}`)) line.classList.remove(BRANCH_HOVER_CLASS);
    // Act
    over(words(row, "1.1"));
    // Assert
    expect(boxedBy(row, HOVER_BOX).map(([node]) => node)).toEqual(["1.1", "1.1.1"]);
  });

  it.each([
    ["a response the daemon has not made selectable", { selectable: false }],
    ["a row that is not a response", { unit: "tool" }],
  ])("boxes nothing in %s", (_name, opts) => {
    // Arrange
    const row = response(opts);
    uninstall = installBranchHover(document.body);
    // Act
    over(words(row, "1.1"));
    // Assert
    expect(boxedBy(row, HOVER_BOX)).toEqual([]);
  });

  it("takes the box down and stops boxing once uninstalled", () => {
    // Arrange
    const row = response();
    const off = installBranchHover(document.body);
    over(words(row, "1.1"));
    // Act
    off();
    over(words(row, "1.2"));
    // Assert
    expect(boxedBy(row, HOVER_BOX)).toEqual([]);
  });

  it("records each newly boxed branch at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const row = response();
    uninstall = installBranchHover(document.body);
    // Act
    over(words(row, "1.1"));
    // Assert
    const record = await forwardedRecord(capture, "feed.branch-hover");
    expect([record.level.case, record.context]).toEqual(["debug", expect.objectContaining({ row: "r1", label: "1.1" })]);
  });
});
