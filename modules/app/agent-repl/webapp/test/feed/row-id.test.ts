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

describe("the feed row attribute is spelled once", () => {
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
  const sources = import.meta.glob("../../src/**/*.ts", {
    query: "?raw",
    import: "default",
    eager: true,
  }) as Record<string, string>;
  const others = Object.entries(sources).filter(([path]) => !path.endsWith("/feed/row-id.ts"));

  it("scans a real set of modules, so an empty glob cannot pass it", () => {
    // Arrange + Act + Assert
    expect(others.length).toBeGreaterThan(5);
  });

  it("finds the attribute as a string literal in no other module", () => {
    // Arrange + Act -- a quoted or templated spelling, not a mention in prose.
    const found = others.filter(([, src]) => /["']\[?data-feed-row\b|`\[data-feed-row=/.test(src)).map(([path]) => path);
    // Assert
    expect(found).toEqual([]);
  });
});
