// @vitest-environment jsdom
/**
 * per-document — one of a thing per page, made on first use.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { describe, expect, it, vi } from "vitest";
import { perDocument } from "../src/per-document.js";

describe("perDocument", () => {
  it("makes the thing on a document's first ask, from that document", () => {
    // Arrange
    const make = vi.fn((doc: Document) => ({ doc }));
    const of = perDocument(make);
    // Act
    const thing = of(document);
    // Assert
    expect([thing.doc, make.mock.calls.length]).toEqual([document, 1]);
  });

  it("answers the same thing for the same document, making it once", () => {
    // Arrange
    const make = vi.fn(() => ({}));
    const of = perDocument(make);
    // Act
    const [first, second] = [of(document), of(document)];
    // Assert
    expect([first === second, make.mock.calls.length]).toEqual([true, 1]);
  });

  it("makes another document its own", () => {
    // Arrange
    const of = perDocument(() => ({}));
    const other = document.implementation.createHTMLDocument("other");
    // Act / Assert
    expect(of(document)).not.toBe(of(other));
  });

  it("keeps each memo of its own", () => {
    // Arrange
    const a = perDocument(() => ({}));
    const b = perDocument(() => ({}));
    // Act / Assert
    expect(a(document)).not.toBe(b(document));
  });
});

describe("every one-per-page thing", () => {
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
