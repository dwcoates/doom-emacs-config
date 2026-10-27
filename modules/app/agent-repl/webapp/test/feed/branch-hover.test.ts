// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import {
  BRANCH_HOVER_CLASS,
  HOVER_LEFT_PROPERTY,
  HOVER_RIGHT_PROPERTY,
  REFERENCE_HOVER_CLASS,
  REFERENCE_LEFT_PROPERTY,
  REFERENCE_RIGHT_PROPERTY,
  citationAt,
  installBranchHover,
} from "../../src/feed/branch-hover.js";
import { SELECTABLE_ATTRIBUTE, SELECTION_GOVERNED_ATTRIBUTE } from "../../src/feed/bubble-selection.js";
import { TREE_CLASS, TREE_REFERENCE_CLASS } from "../../src/metaprompt-tree.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { linkAnswers, entryTreeHtml } from "../../src/feed/entries.js";
import { create } from "@bufbuild/protobuf";
import { FeedResponseAnswerSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { columnMeasure } from "../../src/metaprompt-tree.js";
import { inline } from "../../src/markdown.js";
import { promptProse } from "../prose.js";
import { boxedBy, drawnRow, exampleSubtreeWidth, exampleTree, labelOf, labelledTree, lineOf } from "./tree-fixture.js";

const HOVER_BOX = { className: BRANCH_HOVER_CLASS, leftProperty: HOVER_LEFT_PROPERTY, rightProperty: HOVER_RIGHT_PROPERTY };
const REFERENCE_BOX = {
  className: REFERENCE_HOVER_CLASS,
  leftProperty: REFERENCE_LEFT_PROPERTY,
  rightProperty: REFERENCE_RIGHT_PROPERTY,
};

/** A tree whose 2.1 cites 1.1, and 1.1 has a child. */
const citingTree = labelledTree([
  ["1", "The cause"],
  ["1.1", "The shim drops it", 0],
  ["1.1.1", "At fork time", 1],
  ["2", "The fix"],
  ["2.1", "Remap the boundary (1.1)", 3],
]);

/** TREE drawn in row r1, as a response (UNIT) the daemon has or has not made SELECTABLE. */
function response(opts: { unit?: string; selectable?: boolean } = {}, tree = exampleTree): HTMLElement {
  const row = drawnRow("r1", tree);
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

/** The citation in ROW. */
const citation = (row: HTMLElement): Element => {
  const el = row.querySelector(`.${TREE_REFERENCE_CLASS}`);
  if (el === null) throw new Error("the row draws no citation");
  return el;
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

describe("the box over the node a hovered citation cites", () => {
  it("boxes the cited node's subtree in the citation's box", () => {
    // Arrange
    const row = response({}, citingTree);
    uninstall = installBranchHover(document.body);
    // Act
    over(citation(row));
    // Assert
    expect(boxedBy(row, REFERENCE_BOX).map(([node]) => node)).toEqual(["1.1", "1.1.1"]);
  });

  it("does not box the branch holding the citation", () => {
    // Arrange
    const row = response({}, citingTree);
    uninstall = installBranchHover(document.body);
    // Act
    over(citation(row));
    // Assert
    expect(boxedBy(row, HOVER_BOX)).toEqual([]);
  });

  it("takes the pointer's box off the holding branch when the pointer moves onto its citation", () => {
    // Arrange
    const row = response({}, citingTree);
    uninstall = installBranchHover(document.body);
    over(words(row, "2.1"));
    // Act
    over(citation(row));
    // Assert
    expect([boxedBy(row, HOVER_BOX), boxedBy(row, REFERENCE_BOX).length]).toEqual([[], 2]);
  });

  it("gives way to the holding branch's box when the pointer moves off the citation onto its words", () => {
    // Arrange
    const row = response({}, citingTree);
    uninstall = installBranchHover(document.body);
    over(citation(row));
    // Act
    over(words(row, "2.1"));
    // Assert
    expect([boxedBy(row, REFERENCE_BOX), boxedBy(row, HOVER_BOX).map(([node]) => node)]).toEqual([[], ["2.1"]]);
  });

  it("is taken down when the pointer leaves the feed", () => {
    // Arrange
    const row = response({}, citingTree);
    uninstall = installBranchHover(document.body);
    over(citation(row));
    // Act
    document.body.dispatchEvent(new MouseEvent("mouseleave"));
    // Assert
    expect(boxedBy(row, REFERENCE_BOX)).toEqual([]);
  });

  it("boxes nothing in a row that is not a response", () => {
    // Arrange
    const row = response({ unit: "tool" }, citingTree);
    uninstall = installBranchHover(document.body);
    // Act
    over(citation(row));
    // Assert
    expect(boxedBy(row, REFERENCE_BOX)).toEqual([]);
  });

  it("records each newly boxed cited node at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const row = response({}, citingTree);
    uninstall = installBranchHover(document.body);
    // Act
    over(citation(row));
    // Assert
    const record = await forwardedRecord(capture, "feed.branch-hover.reference");
    expect([record.level.case, record.context]).toEqual(["debug", expect.objectContaining({ row: "r1", label: "1.1" })]);
  });
});

describe("citationAt", () => {
  it("answers the label a citation cites", () => {
    // Arrange
    const row = response({}, citingTree);
    // Act + Assert
    expect(citationAt(row, citation(row))).toBe("1.1");
  });

  it("answers null off a citation", () => {
    // Arrange
    const row = response({}, citingTree);
    // Act + Assert
    expect(citationAt(row, words(row, "1.1"))).toBeNull();
  });

  it("refuses a citation that names no label", () => {
    // Arrange
    const row = response({}, citingTree);
    const el = citation(row);
    el.removeAttribute("data-ref");
    // Act + Assert
    expect(() => citationAt(row, el)).toThrow("names no label");
  });
});

/** A response tree whose 3.1 the prompt p1 answers: 3, its child 3.1 and grandchild 3.1.1, then 3.2. */
const answeredTree = labelledTree([
  ["3", "The cause"],
  ["3.1", "The shim drops it", 0],
  ["3.1.1", "At fork time", 1],
  ["3.2", "A sibling", 0],
]);

/**
 * The response r1 drawing answeredTree with 3.1 answered by the prompt p1,
 * and p1 drawing ENTRIES, both governed rows of the document's body.
 */
function answeredFeed(entries = "3.1 - yes"): { response: HTMLElement; prompt: HTMLElement } {
  const row = response({}, answeredTree);
  linkAnswers(row, [create(FeedResponseAnswerSchema, { label: "3.1", prompt: { value: "p1" } })]);
  const block = promptProse(entries).blocks[0]?.block;
  if (block?.case !== "entries") throw new Error("the prompt draws no entries");
  const prompt = document.createElement("article");
  prompt.setAttribute("data-feed-row", "p1");
  prompt.toggleAttribute(SELECTION_GOVERNED_ATTRIBUTE, true);
  prompt.innerHTML = `<div class="${TREE_CLASS}">${entryTreeHtml(block.value, inline, columnMeasure(80))}</div>`;
  document.body.append(prompt);
  return { response: row, prompt };
}

describe("the box over the response node a hovered prompt entry answers", () => {
  it("boxes the answered node's subtree in the response, as the composer's box is drawn", () => {
    // Arrange
    const { response: row, prompt } = answeredFeed();
    uninstall = installBranchHover(document.body);
    // Act
    over(labelOf(prompt, "3.1"));
    // Assert
    const width = `    └── 3.1.1 At fork time`.length;
    expect(boxedBy(row, HOVER_BOX)).toEqual([
      ["3.1", "4ch", `${width}ch`],
      ["3.1.1", "4ch", `${width}ch`],
    ]);
  });

  it("boxes an implied ancestor's own subtree", () => {
    // Arrange
    const { response: row, prompt } = answeredFeed();
    uninstall = installBranchHover(document.body);
    // Act
    over(labelOf(prompt, "3"));
    // Assert
    expect(boxedBy(row, HOVER_BOX).map(([node]) => node)).toEqual(["3", "3.1", "3.1.1", "3.2"]);
  });

  it("boxes nothing in the prompt itself", () => {
    // Arrange
    const { prompt } = answeredFeed();
    uninstall = installBranchHover(document.body);
    // Act
    over(labelOf(prompt, "3.1"));
    // Assert
    expect(boxedBy(prompt, HOVER_BOX)).toEqual([]);
  });

  it("is taken down when the pointer moves off the label onto the entry's words", () => {
    // Arrange
    const { response: row, prompt } = answeredFeed();
    uninstall = installBranchHover(document.body);
    over(labelOf(prompt, "3.1"));
    // Act
    over(words(prompt, "3.1"));
    // Assert
    expect(boxedBy(row, HOVER_BOX)).toEqual([]);
  });

  it("boxes nothing for an entry no drawn response is answered by", () => {
    // Arrange
    const { response: row, prompt } = answeredFeed("3.1 - a\n4.2 - **b**\n5.5.1 - c");
    uninstall = installBranchHover(document.body);
    // Act
    over(labelOf(prompt, "4.2"));
    // Assert
    expect(boxedBy(row, HOVER_BOX)).toEqual([]);
  });

  it("records each newly boxed answered node at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { prompt } = answeredFeed();
    uninstall = installBranchHover(document.body);
    // Act
    over(labelOf(prompt, "3.1"));
    // Assert
    const record = await forwardedRecord(capture, "feed.branch-hover.entry");
    expect([record.level.case, record.context]).toEqual(["debug", expect.objectContaining({ row: "r1", label: "3.1" })]);
  });
});
