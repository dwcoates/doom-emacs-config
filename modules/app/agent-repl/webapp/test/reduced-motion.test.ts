// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import { REDUCED_MOTION_QUERY, prefersReducedMotion } from "../src/reduced-motion.js";

afterEach(() => {
  vi.unstubAllGlobals();
});

describe("prefersReducedMotion", () => {
  it.each([true, false])("answers the media query's %s", (matches) => {
    // Arrange
    const asked: string[] = [];
    vi.stubGlobal("matchMedia", (query: string) => {
      asked.push(query);
      return { matches } as MediaQueryList;
    });

    // Act
    const got = prefersReducedMotion(document);

    // Assert
    expect([got, asked]).toEqual([matches, [REDUCED_MOTION_QUERY]]);
  });

  it("answers no for a host with no matchMedia", () => {
    // Arrange
    vi.stubGlobal("matchMedia", undefined);

    // Act / Assert
    expect(prefersReducedMotion(document)).toBe(false);
  });
});
