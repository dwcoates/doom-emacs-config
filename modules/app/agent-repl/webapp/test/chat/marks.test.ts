// @vitest-environment jsdom
/**
 * Marking the words the daemon names in a rendered answer, and the context a
 * selection carries.
 */
import { describe, expect, it } from "vitest";
import { contextOf, markWords } from "../../src/chat/marks.js";

function answer(html: string): HTMLElement {
  const el = document.createElement("div");
  el.innerHTML = html;
  return el;
}

describe("markWords", () => {
  it.each([
    ["words inside one text node", "<p>the sky is blue</p>", ["sky is"], '<p>the <mark class="m">sky is</mark> blue</p>'],
    [
      "words crossing an element boundary",
      "<p>the <strong>blue</strong> sky</p>",
      ["blue sky"],
      '<p>the <strong><mark class="m">blue</mark></strong><mark class="m"> sky</mark></p>',
    ],
    ["only the first occurrence", "<p>a b a</p>", ["a"], '<p><mark class="m">a</mark> b a</p>'],
    ["several entries", "<p>one two three</p>", ["one", "three"], '<p><mark class="m">one</mark> two <mark class="m">three</mark></p>'],
  ])("marks %s", (_name, html, words, want) => {
    // Arrange
    const el = answer(html);

    // Act
    const missing = markWords(el, words, "m");

    // Assert
    expect(el.innerHTML).toBe(want);
    expect(missing).toEqual([]);
  });

  it("answers the words it could not find, and marks nothing for them", () => {
    // Arrange
    const el = answer("<p>the sky</p>");

    // Act
    const missing = markWords(el, ["**sky**", ""], "m");

    // Assert
    expect(missing).toEqual(["**sky**"]);
    expect(el.innerHTML).toBe("<p>the sky</p>");
  });
});

describe("contextOf", () => {
  it("is the block holding the selection", () => {
    // Arrange
    const el = answer("<p>first</p><ul><li>the <em>blue</em> sky</li></ul>");
    const em = el.querySelector("em")?.firstChild as Node;

    // Act / Assert
    expect(contextOf(el, em)).toBe("the blue sky");
  });

  it("is bounded", () => {
    // Arrange
    const el = answer(`<p>${"x".repeat(50)}</p>`);

    // Act / Assert
    expect(contextOf(el, el.firstChild as Node, 10)).toBe("x".repeat(10));
  });
});
