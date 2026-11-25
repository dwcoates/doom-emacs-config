// @vitest-environment jsdom
/**
 * row-id — the one spelling of the feed row's identity attribute
 * (src/feed/row-id.ts).
 */
import { describe, expect, it } from "vitest";
import { FEED_ROW_ATTRIBUTE, FEED_ROW_SELECTOR, feedRowId, feedRowOf } from "../../src/feed/row-id.js";

/** A feed row carrying ID, holding a nested row carrying INNER, holding a span. */
function nested(id: string, inner: string) {
  const row = document.createElement("article");
  row.setAttribute(FEED_ROW_ATTRIBUTE, id);
  const child = document.createElement("div");
  child.setAttribute(FEED_ROW_ATTRIBUTE, inner);
  const span = document.createElement("span");
  child.append(span);
  row.append(child);
  return { row, child, span };
}

describe("FEED_ROW_SELECTOR", () => {
  it("matches exactly the elements carrying the attribute", () => {
    // Arrange
    const n = nested("r1", "r1.c0");
    // Act
    const found = [...n.row.querySelectorAll(FEED_ROW_SELECTOR)];
    // Assert
    expect(found).toEqual([n.child]);
  });
});

describe("feedRowId", () => {
  it("answers the FeedId the row carries", () => {
    // Arrange
    const n = nested("f1.r7", "f1.r7.c0");
    // Act / Assert
    expect(feedRowId(n.row)).toBe("f1.r7");
  });

  it("answers null for an element that carries none", () => {
    // Arrange
    const el = document.createElement("div");
    // Act / Assert
    expect(feedRowId(el)).toBeNull();
  });
});

describe("feedRowOf", () => {
  it("answers the innermost row an element is drawn in", () => {
    // Arrange
    const n = nested("r1", "r1.c0");
    // Act / Assert
    expect(feedRowOf(n.span)).toBe(n.child);
  });

  it("answers the element itself when it is a row", () => {
    // Arrange
    const n = nested("r1", "r1.c0");
    // Act / Assert
    expect(feedRowOf(n.row)).toBe(n.row);
  });

  it("answers null for an element drawn in no row", () => {
    // Arrange
    const el = document.createElement("div");
    // Act / Assert
    expect(feedRowOf(el)).toBeNull();
  });
});
