/**
 * THE FEED'S TAIL OWNER IS BUILT IN ONE PLACE: a rule over the sources and
 * the webkit pages.
 */
import { describe, expect, it } from "vitest";

describe("createFeedTail", () => {
  it("is the one place the feed's tail owner is built, in production and in the webkit pages", () => {
    // Arrange
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
    const sources = import.meta.glob(["../../src/**/*.ts", "../../test/webkit/*-page.ts"], {
      query: "?raw",
      import: "default",
      eager: true,
    }) as Record<string, string>;
    // Act
    const builders = Object.entries(sources)
      .filter(([, src]) => /new TailFollow\(/.test(src))
      .map(([path]) => path);
    // Assert
    expect([Object.keys(sources).length > 5, builders]).toEqual([true, ["../../src/feed/feed.ts"]]);
  });
});
