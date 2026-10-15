// @vitest-environment jsdom
// One page's size, as every size-bearing record states it. See src/page-size.ts.
import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { pageSize } from "../src/page-size.js";

/** A document holding ROWS feed rows inside one feed element. */
function documentWithRows(rows: number): Document {
  const doc = document.implementation.createHTMLDocument("t");
  const feed = doc.createElement("div");
  for (let i = 0; i < rows; i++) {
    const row = doc.createElement("div");
    row.setAttribute("data-feed-row", String(i));
    feed.append(row);
  }
  doc.body.append(feed);
  return doc;
}

describe("pageSize", () => {
  it.each([
    { rows: 0, nodes: 5 },
    { rows: 3, nodes: 8 },
  ])("counts $rows feed rows among $nodes elements", ({ rows, nodes }) => {
    // Arrange
    const doc = documentWithRows(rows);

    // Act
    const size = pageSize(doc);

    // Assert
    expect(size).toEqual({ dom_nodes: nodes, feed_rows: rows });
  });

  it("is the one size measure: no caller counts elements or feed rows itself", () => {
    // Arrange
    const callers = ["../src/page-presence.ts"].map((path) =>
      readFileSync(fileURLToPath(new URL(path, import.meta.url)), "utf8"),
    );

    // Act
    const handRolled = callers.filter((source) => source.includes('getElementsByTagName("*")'));

    // Assert
    expect(handRolled).toEqual([]);
  });
});
