/** Every one-per-page thing is memoized through perDocument (src/per-document.ts) and disposed with its page. */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

describe("every one-per-page thing", () => {
  it.each([
    ["../src/presence.ts", "(presence) => presence.dispose()"],
    ["../src/focus-left.ts", "(source) => source.dispose()"],
  ])("%s, which listens, is disposed with its page", (path, dispose) => {
    // Arrange
    const source = readFileSync(fileURLToPath(new URL(path, import.meta.url)), "utf8");
    // Act / Assert
    expect(source).toContain(dispose);
  });

  it.each(["../src/presence.ts", "../src/focus-left.ts", "../src/expand.ts"])(
    "%s is memoized through perDocument, not a map of its own",
    (path) => {
      // Arrange
      const source = readFileSync(fileURLToPath(new URL(path, import.meta.url)), "utf8");
      // Act / Assert
      expect([source.includes("perDocument("), source.includes("new WeakMap<Document")]).toEqual([true, false]);
    },
  );
});
