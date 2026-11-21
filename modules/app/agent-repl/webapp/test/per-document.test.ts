// @vitest-environment jsdom
/**
 * per-document — one of a thing per page, made on first use.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { describe, expect, it, vi } from "vitest";
import { perDocument } from "../src/per-document.js";
import { tearDownPage } from "../src/page-teardown.js";

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

describe("perDocument with a dispose", () => {
  it("disposes the thing when the page is torn down", () => {
    // Arrange
    const dispose = vi.fn();
    const of = perDocument(() => ({}), dispose);
    const thing = of(document);
    // Act
    tearDownPage();
    // Assert
    expect(dispose.mock.calls).toEqual([[thing]]);
  });

  it("makes the document a new one on the first ask after the teardown", () => {
    // Arrange
    const of = perDocument(() => ({}), () => undefined);
    const before = of(document);
    // Act
    tearDownPage();
    // Assert
    expect(of(document)).not.toBe(before);
  });

  it("disposes nothing it never made", () => {
    // Arrange
    const dispose = vi.fn();
    perDocument(() => ({}), dispose);
    // Act
    tearDownPage();
    // Assert
    expect(dispose).not.toHaveBeenCalled();
  });

  it("keeps a thing with no dispose across the teardown", () => {
    // Arrange
    const of = perDocument(() => ({}));
    const before = of(document);
    // Act
    tearDownPage();
    // Assert
    expect(of(document)).toBe(before);
  });
});

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
