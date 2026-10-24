// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { boxSubtree, inSubtree, labelColumn, unboxAll, type SubtreeBox } from "../../src/feed/subtree-box.js";
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
    ["a root", "1", 0],
    ["a child, past its connector", "1.1", 4],
    ["a grandchild, past its parent's rail and its connector", "1.1.1", 8],
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
  it.each(["src/feed/composer-entry.ts"])("draws its box through subtree-box in %s", (file) => {
    // Arrange
    const source = readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "../..", file), "utf8");
    // Act + Assert: no hand-rolled box, only the shared one.
    expect([source.includes("boxSubtree("), source.includes("textWidth("), source.includes("labelColumn(")]).toEqual([true, false, false]);
  });
});
