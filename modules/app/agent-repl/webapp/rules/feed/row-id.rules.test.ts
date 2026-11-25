/**
 * THE FEED ROW ATTRIBUTE IS SPELLED ONCE (src/feed/row-id.ts): a rule over
 * every other source.
 */
import { describe, expect, it } from "vitest";

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
