// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it, vi } from "vitest";
import { REDUCED_MOTION_QUERY, prefersReducedMotion } from "../src/reduced-motion.js";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "../src");

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

  it.each(["feed/send-glide.ts", "bubble/collapse-motion.ts"])("is how %s reads the reader's setting", (file) => {
    const source = readFileSync(path.join(SRC, file), "utf8");
    expect([source.includes("prefersReducedMotion("), source.includes("prefers-reduced-motion: reduce")]).toEqual([true, false]);
  });
});
