/**
 * SUBTREE-BOX'S RULES over its source and its callers: every node highlight
 * and hit test goes through it, and it spells and reads a box's edges one way.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

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

describe("subtreeSpan", () => {
  it("is the only place subtree-box reads a box's edges from the lines", () => {
    // Arrange
    const source = readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src/feed/subtree-box.ts"), "utf8");
    // Act — reading every line's text end is how a box's right edge is found.
    const reads = source.split("edgeOf(l, TREE_TEXT_END_ATTRIBUTE)").length - 1;
    // Assert
    expect(reads).toBe(1);
  });
});

describe("every hit test of a response's node", () => {
  it.each(["src/feed/branch-hover.ts", "src/feed/bubble-selection.ts"])("decides the node through subtree-box in %s", (file) => {
    // Arrange
    const source = readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "../..", file), "utf8");
    // Act + Assert: the hover and the click both answer by the drawn box's own edges, never a hand-rolled area.
    expect([source.includes("nodeBoxedAt("), source.includes("xInLine("), source.includes("subtreeSpan(")]).toEqual([true, true, false]);
  });
});
