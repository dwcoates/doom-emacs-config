// @vitest-environment node
import { describe, expect, it } from "vitest";

describe("the digest overlay's surface", () => {
  it("is opaque, so the feed is not seen through it", async () => {
    // Arrange
    const css = (await import("../../src/styles.css?raw")).default;

    // Act
    const rule = /\[data-component="news-digest"\]\s*\{[^}]*\}/.exec(css)?.[0] ?? "";

    // Assert
    expect([rule.includes("background: var(--bg);"), rule.includes("transparent"), rule.includes("backdrop-filter")]).toEqual([
      true,
      false,
      false,
    ]);
  });
});
