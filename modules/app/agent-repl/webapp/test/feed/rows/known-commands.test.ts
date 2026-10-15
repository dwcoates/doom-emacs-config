// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import {
  KNOWN_COMMAND_CLASS,
  markKnownCommands,
  slashWords,
} from "../../../src/feed/rows/known-commands.js";

/** The names of TEXT's slash words, in order. */
function names(text: string): string[] {
  return slashWords(text).map((word) => word.name);
}

/** A detached root holding HTML. */
function rendered(html: string): HTMLElement {
  const root = document.createElement("div");
  root.innerHTML = html;
  return root;
}

/** The text of every mark under ROOT, in order. */
function marks(root: HTMLElement): string[] {
  return [...root.querySelectorAll(`.${KNOWN_COMMAND_CLASS}`)].map((el) => el.textContent ?? "");
}

describe("slashWords: the composer's slash word", () => {
  it.each([
    { name: "a word that is a slash and a name", text: "/merge-queue own", want: ["merge-queue"] },
    { name: "a word after opening punctuation", text: "(/merge-queue", want: ["merge-queue"] },
    { name: "a word before closing punctuation", text: "run /merge-queue).", want: ["merge-queue"] },
    { name: "every word of the text, in order", text: "/a then /b", want: ["a", "b"] },
    { name: "a slash inside a run is no word", text: "and/or a/b", want: [] },
    { name: "a path names its whole rest", text: "/usr/local", want: ["usr/local"] },
    { name: "a bare slash names nothing", text: "a / b", want: [""] },
    { name: "a slash before only closing punctuation names nothing", text: "/.", want: [""] },
  ])("$name", ({ text, want }) => {
    // Arrange / Act / Assert
    expect(names(text)).toEqual(want);
  });

  it("spans the word without its surrounding punctuation", () => {
    // Arrange / Act
    const [word] = slashWords('"/compact",');
    // Assert
    expect(word).toEqual({ start: 1, end: 9, name: "compact" });
  });
});

describe("markKnownCommands", () => {
  it("wraps a slash word whose name is known", () => {
    // Arrange
    const root = rendered("<p>/merge-queue own</p>");
    // Act
    markKnownCommands(root, new Set(["merge-queue"]));
    // Assert
    expect(marks(root)).toEqual(["/merge-queue"]);
  });

  it("keeps the text around the mark", () => {
    // Arrange
    const root = rendered("<p>run (/merge-queue) now</p>");
    // Act
    markKnownCommands(root, new Set(["merge-queue"]));
    // Assert
    expect(root.textContent).toBe("run (/merge-queue) now");
  });

  it("leaves a slash word whose name is not known", () => {
    // Arrange
    const root = rendered("<p>/usr/local and /merge-queue</p>");
    // Act
    markKnownCommands(root, new Set(["merge-queue"]));
    // Assert
    expect(marks(root)).toEqual(["/merge-queue"]);
  });

  it("marks every known word of a text node", () => {
    // Arrange
    const root = rendered("<p>/compact then /merge-queue</p>");
    // Act
    markKnownCommands(root, new Set(["merge-queue", "compact"]));
    // Assert
    expect(marks(root)).toEqual(["/compact", "/merge-queue"]);
  });

  it("marks a word inside nested markup", () => {
    // Arrange
    const root = rendered("<ul><li><code>/compact</code></li></ul>");
    // Act
    markKnownCommands(root, new Set(["compact"]));
    // Assert
    expect(root.querySelector(`code > .${KNOWN_COMMAND_CLASS}`)?.textContent).toBe("/compact");
  });

  it("marks nothing for no names", () => {
    // Arrange
    const root = rendered("<p>/compact</p>");
    // Act
    markKnownCommands(root, new Set());
    // Assert
    expect(root.innerHTML).toBe("<p>/compact</p>");
  });
});
