// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { drawnRowsOf } from "../../src/sidebar/drawn-rows.js";

/** A ROOT holding rows for IDS, in order. */
const rail = (ids: string[]): HTMLElement => {
  const root = document.createElement("div");
  for (const id of ids) {
    const ws = document.createElement("div");
    ws.setAttribute("data-roster-row", id);
    root.appendChild(ws);
  }
  return root;
};

describe("drawnRowsOf", () => {
  it("answers every drawn copy of the workspace's row, in document order", () => {
    // Arrange
    const root = rail(["ws-1", "ws-2", "ws-1"]);

    // Act
    const rows = drawnRowsOf(root, "ws-1");

    // Assert
    expect(rows).toEqual([root.children[0], root.children[2]]);
  });

  it("answers none for a workspace with no drawn row", () => {
    // Act / Assert
    expect(drawnRowsOf(rail(["ws-2"]), "ws-1")).toEqual([]);
  });
});
