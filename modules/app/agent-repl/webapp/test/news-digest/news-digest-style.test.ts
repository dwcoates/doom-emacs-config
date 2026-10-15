// @vitest-environment node
import { describe, expect, it } from "vitest";

// Imported at collection, not inside a test, so the stylesheet's transform
// never counts against a test's timeout.
import css from "../../src/styles.css?raw";

describe("the digest overlay's surface", () => {
  it("is opaque, so the feed is not seen through it", () => {
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

describe("the IPO outlook's color", () => {
  it("is defined for both themes and worn only by the digest", () => {
    // Act
    const definitions = css.match(/--news-digest-ipo:/g)?.length ?? 0;
    const users = [...css.matchAll(/^([^{}\n]*)\{[^}]*var\(--news-digest-ipo\)/gm)].map((m) => m[1].trim());

    // Assert
    expect([definitions, users.length > 0, users.every((selector) => selector.includes(".news-digest-ipo"))]).toEqual([
      2,
      true,
      true,
    ]);
  });
});
