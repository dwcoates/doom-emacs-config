/**
 * THE ONE HOVER STAMP RECORDER (src/feed/hover-stamp.ts): no other source
 * records a card's hover stamp.
 */
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { codeUnder } from "../files-under.js";

describe("the one recorder", () => {
  it("is the only place a card's hover stamp is recorded: no card records one of its own", () => {
    // Arrange: every source under src/ but the recorder's own module.
    const root = join(dirname(fileURLToPath(import.meta.url)), "../../src");
    const sources = [...codeUnder(root)].filter(([file]) => file !== join("feed", "hover-stamp.ts"));
    // Act
    const recorders = sources.filter(([, code]) => /\bsetRowHover\(|\brecordRowHover\(/.test(code)).map(([file]) => file);
    // Assert: the feed view's row chrome alone, for every row kind.
    expect(recorders).toEqual([join("feed", "feed-view.ts")]);
  });
});
