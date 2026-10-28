// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { boxSubtree, inSubtree, labelColumn, standingBox, subtreeSpan, unboxAll, type SubtreeBox } from "../../src/feed/subtree-box.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { boxedBy, drawnRow, exampleSubtreeWidth, exampleTree, labelledTree, lineOf } from "./tree-fixture.js";

const box: SubtreeBox = { className: "box-a", leftProperty: "--a-left", rightProperty: "--a-right" };
const other: SubtreeBox = { className: "box-b", leftProperty: "--b-left", rightProperty: "--b-right" };

describe("inSubtree", () => {
  it.each([
    ["the node itself", "1.1", true],
    ["a child", "1.1.1", true],
    ["a grandchild", "1.1.1.1", true],
    ["the parent", "1", false],
    ["a sibling", "1.2", false],
    ["a label merely opening on the node's characters", "1.10", false],
  ])("answers %s", (_name, node, want) => {
    // Act + Assert
    expect(inSubtree(node, "1.1")).toBe(want);
  });

  it("answers false for a line drawing no node", () => {
    // Act + Assert
    expect(inSubtree(null, "1.1")).toBe(false);
  });
});

describe("labelColumn", () => {
  it.each([
    ["a root", "1", "0ch"],
    ["a child, past its connector", "1.1", "4ch"],
    ["a grandchild, past its parent's rail and its connector", "1.1.1", "8ch"],
  ])("is the column %s's label starts at", (_name, node, want) => {
    // Arrange
    const row = drawnRow("r1", exampleTree);
    // Act + Assert
    expect(labelColumn(lineOf(row, node))).toBe(want);
  });
});

describe("boxSubtree", () => {
  it("boxes the subtree from its label's column to its widest line", () => {
    // Arrange
    const row = drawnRow("r1", exampleTree);
    // Act
    boxSubtree(row, "1.1", box);
    // Assert
    expect(boxedBy(row, box)).toEqual([
      ["1.1", "4ch", `${exampleSubtreeWidth}ch`],
      ["1.1.1", "4ch", `${exampleSubtreeWidth}ch`],
    ]);
  });

  it("boxes each tree holding the node on its own and skips a tree that does not", () => {
    // Arrange
    const row = drawnRow("r1", exampleTree, labelledTree([["2", "Elsewhere"]]), exampleTree);
    // Act
    boxSubtree(row, "1.2", box);
    // Assert
    expect(boxedBy(row, box).map(([node]) => node)).toEqual(["1.2", "1.2"]);
  });

  it("boxes nothing for a label no tree holds", () => {
    // Arrange
    const row = drawnRow("r1", exampleTree);
    // Act
    boxSubtree(row, "9.9", box);
    // Assert
    expect(boxedBy(row, box)).toEqual([]);
  });

  it("leaves another box over the same lines untouched", () => {
    // Arrange
    const row = drawnRow("r1", exampleTree);
    boxSubtree(row, "1", other);
    // Act
    boxSubtree(row, "1.1", box);
    // Assert
    expect(boxedBy(row, other).map(([, left]) => left)).toEqual(["0ch", "0ch", "0ch", "0ch", "0ch"]);
  });
});

describe("unboxAll", () => {
  it("takes the box's class and edges off every line", () => {
    // Arrange
    const row = drawnRow("r1", exampleTree);
    boxSubtree(row, "1", box);
    // Act
    unboxAll(row, box);
    // Assert
    const line = lineOf(row, "1.1");
    expect([boxedBy(row, box), line.style.getPropertyValue(box.leftProperty), line.style.getPropertyValue(box.rightProperty)]).toEqual([[], "", ""]);
  });

  it("leaves another box standing", () => {
    // Arrange
    const row = drawnRow("r1", exampleTree);
    boxSubtree(row, "1", box);
    boxSubtree(row, "1.1", other);
    // Act
    unboxAll(row, box);
    // Assert
    expect(boxedBy(row, other).map(([node]) => node)).toEqual(["1.1", "1.1.1"]);
  });
});

describe("every highlight of a response's node", () => {
  it.each(["src/feed/composer-entry.ts", "src/feed/branch-hover.ts", "src/feed/revealed-node.ts"])("draws its box through subtree-box in %s", (file) => {
    // Arrange
    const source = readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "../..", file), "utf8");
    // Act + Assert: no hand-rolled box, only the shared one.
    const shared = source.includes("boxSubtree(") || source.includes("standingBox(");
    expect([shared, source.includes("textWidth("), source.includes("labelColumn(")]).toEqual([true, false, false]);
  });
});

describe("subtree-box's edges", () => {
  it("are spelled by the tree's one cssLength, never by hand", () => {
    // Arrange
    const source = readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src/feed/subtree-box.ts"), "utf8");
    // Act + Assert
    expect([source.includes("cssLength("), /`\$\{[^`]*\}\$\{[^`]*unit\}`/.test(source)]).toEqual([true, false]);
  });
});

describe("standingBox", () => {
  it("boxes the stood node in its row and in no other", () => {
    // Arrange
    const standing = standingBox(box);
    const row = drawnRow("r1", exampleTree);
    // Act
    standing.stand(document, { row: "r1", label: "1.1" });
    // Assert
    expect(boxedBy(row, box).map(([node]) => node)).toEqual(["1.1", "1.1.1"]);
  });

  it("boxes nothing for a node of another row", () => {
    // Arrange
    const standing = standingBox(box);
    const row = drawnRow("r2", exampleTree);
    // Act
    standing.stand(document, { row: "r1", label: "1.1" });
    // Assert
    expect(boxedBy(row, box)).toEqual([]);
  });

  it("moves the box when another node is stood", () => {
    // Arrange
    const standing = standingBox(box);
    const row = drawnRow("r1", exampleTree);
    standing.stand(document, { row: "r1", label: "1.1" });
    // Act
    standing.stand(document, { row: "r1", label: "1.2" });
    // Assert
    expect(boxedBy(row, box).map(([node]) => node)).toEqual(["1.2"]);
  });

  it("takes the box down when no node is stood", () => {
    // Arrange
    const standing = standingBox(box);
    const row = drawnRow("r1", exampleTree);
    standing.stand(document, { row: "r1", label: "1.1" });
    // Act
    standing.stand(document, undefined);
    // Assert
    expect(boxedBy(row, box)).toEqual([]);
  });

  it("redraws the stood node on a repaint of its row's prose", () => {
    // Arrange
    const standing = standingBox(box);
    standing.stand(document, { row: "r1", label: "1.1" });
    // Act
    const row = drawnRow("r1", exampleTree);
    standing.mark(row, "r1");
    // Assert
    expect(boxedBy(row, box).map(([node]) => node)).toEqual(["1.1", "1.1.1"]);
  });

  it("keeps each standing box apart from another's", () => {
    // Arrange
    const first = standingBox(box);
    const second = standingBox(other);
    const row = drawnRow("r1", exampleTree);
    first.stand(document, { row: "r1", label: "1.1" });
    // Act
    second.stand(document, { row: "r1", label: "1.2" });
    // Assert
    expect([boxedBy(row, box).map(([node]) => node), boxedBy(row, other).map(([node]) => node)]).toEqual([["1.1", "1.1.1"], ["1.2"]]);
  });
});

describe("the edges a box is drawn from", () => {
  it("are the ones the tree was wrapped by, in its measure's unit", () => {
    // Arrange — a line drawn in px, as the bubble's measure draws it.
    const row = document.createElement("article");
    row.innerHTML = `<div class="mp-tree"><div class="mp-line" data-node="2" data-label-at="0px" data-text-end="88.5px"></div><div class="mp-line" data-node="2" data-text-end="120px"></div></div>`;
    document.body.replaceChildren(row);
    // Act
    boxSubtree(row, "2", box);
    // Assert
    expect(boxedBy(row, box)).toEqual([
      ["2", "0px", "120px"],
      ["2", "0px", "120px"],
    ]);
  });

  it("refuses a line that carries no edge, recording it through the canonical logger", async () => {
    // Arrange — a head line drawn by hand, with no text end.
    const capture = captureLogRecords();
    const row = document.createElement("article");
    row.innerHTML = `<div class="mp-tree"><div class="mp-line" data-node="2" data-label-at="0px"></div></div>`;
    document.body.replaceChildren(row);
    // Act
    expect(() => boxSubtree(row, "2", box)).toThrow(/subtree box/);
    // Assert
    const record = await forwardedRecord(capture, "feed.subtree-box.edge");
    expect(record).toMatchObject({ level: { case: "error" }, context: { attribute: "data-text-end", value: "", node: "2" } });
  });
});

/** The one drawn tree of ROW. */
const treeOf = (row: HTMLElement): HTMLElement => {
  const tree = row.querySelector<HTMLElement>(".mp-tree");
  if (tree === null) throw new Error("the row draws no tree");
  return tree;
};

describe("subtreeSpan", () => {
  it("spans a node's subtree from its label's column to its widest line", () => {
    // Arrange
    const row = drawnRow("r1", exampleTree);
    // Act
    const span = subtreeSpan(treeOf(row), "1.1");
    // Assert
    expect([span?.lines.map((l) => l.getAttribute("data-node")), span?.left, span?.right]).toEqual([
      ["1.1", "1.1.1"],
      "4ch",
      `${exampleSubtreeWidth}ch`,
    ]);
  });

  it("answers null for a node the tree does not draw", () => {
    // Arrange
    const row = drawnRow("r1", exampleTree);
    // Act + Assert
    expect(subtreeSpan(treeOf(row), "9")).toBeNull();
  });

  it.each(["1", "1.1", "1.1.1", "1.2", "1.10"])("is the very box boxSubtree draws over %s", (label) => {
    // Arrange — one place decides a box's edges, for the highlight and the hit test alike.
    const row = drawnRow("r1", exampleTree);
    const span = subtreeSpan(treeOf(row), label);
    // Act
    boxSubtree(row, label, box);
    // Assert
    expect(boxedBy(row, box)).toEqual(span?.lines.map((l) => [l.getAttribute("data-node"), span.left, span.right]));
  });

  it("is the only place subtree-box reads a box's edges from the lines", () => {
    // Arrange
    const source = readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src/feed/subtree-box.ts"), "utf8");
    // Act — reading every line's text end is how a box's right edge is found.
    const reads = source.split("edgeOf(l, TREE_TEXT_END_ATTRIBUTE)").length - 1;
    // Assert
    expect(reads).toBe(1);
  });
});
