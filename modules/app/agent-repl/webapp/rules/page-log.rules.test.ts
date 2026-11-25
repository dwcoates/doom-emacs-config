/** installPageLogger (test/webkit/page-log.ts) is the one logger setup of the WebKit pages. */
import { describe, expect, it } from "vitest";

describe("installPageLogger", () => {
  it("is the one logger setup: no WebKit page installs a logger of its own", () => {
    // Arrange
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
    const pages = import.meta.glob("../test/webkit/*-page.ts", {
      query: "?raw",
      import: "default",
      eager: true,
    }) as Record<string, string>;
    const sharing = Object.values(pages).filter((src) => /installPageLogger\(/.test(src));
    // Act
    const own = Object.entries(pages)
      .filter(([, src]) => /\bsetLogger\(|\bbindLogContext\(/.test(src))
      .map(([path]) => path);
    // Assert
    expect([sharing.length >= 5, own]).toEqual([true, []]);
  });
});
