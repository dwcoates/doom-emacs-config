// @vitest-environment jsdom
// The page's markup as a settle compares it. See test/integration/quiet-markup.ts.
import { describe, expect, it } from "vitest";
import { applyTextReveal } from "../../src/bubble/text-reveal.js";
import { REVEALED_ATTRIBUTE } from "../../src/feed/cards/response.js";
import { quietMarkupOf } from "./quiet-markup";

/** A root holding one prose bubble whose text is TEXT. */
function bubble(text: string): HTMLElement {
  const root = document.createElement("div");
  const prose = document.createElement("p");
  prose.textContent = text;
  root.appendChild(prose);
  return root;
}

describe("quietMarkupOf", () => {
  it("reads a text mid type-out as its whole text", () => {
    // Arrange
    const typing = bubble("hello world");
    const whole = bubble("hello world");
    applyTextReveal(typing.firstElementChild!, 3);

    // Act / Assert
    expect(quietMarkupOf(typing)).toBe(quietMarkupOf(whole));
  });

  it("reads two steps of one type-out the same", () => {
    // Arrange
    const root = bubble("hello world");
    applyTextReveal(root.firstElementChild!, 2);
    const before = quietMarkupOf(root);

    // Act
    applyTextReveal(root.firstElementChild!, 7);

    // Assert
    expect(quietMarkupOf(root)).toBe(before);
  });

  it("still sees text that arrived", () => {
    // Arrange
    const root = bubble("hello");
    const before = quietMarkupOf(root);

    // Act
    root.firstElementChild!.textContent = "hello world";

    // Assert
    expect(quietMarkupOf(root)).not.toBe(before);
  });

  it("drops the reveal's shown-length bookkeeping", () => {
    // Arrange
    const root = bubble("hello");
    root.setAttribute(REVEALED_ATTRIBUTE, "2");
    const inner = document.createElement("div");
    inner.appendChild(root);

    // Act
    const markup = quietMarkupOf(inner);

    // Assert
    expect(markup).not.toContain(REVEALED_ATTRIBUTE);
  });

  it("empties a ticking clock", () => {
    // Arrange
    const root = document.createElement("div");
    root.innerHTML = '<span data-ticking="">3s</span>';

    // Act / Assert
    expect(quietMarkupOf(root)).toBe('<span data-ticking=""></span>');
  });

  it("leaves the page itself untouched", () => {
    // Arrange
    const root = bubble("hello world");
    applyTextReveal(root.firstElementChild!, 3);
    const live = root.innerHTML;

    // Act
    quietMarkupOf(root);

    // Assert
    expect(root.innerHTML).toBe(live);
  });
});
