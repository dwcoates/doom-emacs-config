/**
 * THE DIGEST'S SHARED BUILDERS ARE ITS ONLY ONES (`src/news-digest/news-digest.ts`):
 * one heading, one prose block and one link row maker.
 */
import { describe, expect, it } from "vitest";
import source from "../../src/news-digest/news-digest.ts?raw";

describe("the digest's shared builders", () => {
  it("is the only place news-digest.ts builds a heading, a prose block or a link row", () => {
    // ARRANGE
    // ACT
    const counts = ['createElement("h2")', '"news-digest-summary md"', '"news-digest-links"', "renderMarkdown("].map(
      (shape) => source.split(shape).length - 1,
    );
    // ASSERT
    expect(counts).toEqual([1, 1, 1, 1]);
  });
});
