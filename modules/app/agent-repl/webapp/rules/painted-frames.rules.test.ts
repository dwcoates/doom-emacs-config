/** paintedFrames (test/webkit/painted-frames.ts) is the one painted-frame clock of the WebKit pages. */
import { describe, expect, it } from "vitest";

describe("paintedFrames", () => {
  it("is the one painted-frame clock: no WebKit page observes a resize of its own", () => {
    // Arrange
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
    const pages = import.meta.glob("../test/webkit/*-page.ts", {
      query: "?raw",
      import: "default",
      eager: true,
    }) as Record<string, string>;
    const measuring = Object.entries(pages).filter(([, src]) => /paintedFrames\(/.test(src));
    // Act
    const ownObservers = Object.entries(pages)
      .filter(([, src]) => /new ResizeObserver\(/.test(src))
      .map(([path]) => path);
    // Assert
    expect([measuring.length >= 2, ownObservers]).toEqual([true, []]);
  });
});
