// @vitest-environment jsdom
/**
 * text-reveal — showing only the first N characters of a rendered subtree.
 */
import { describe, expect, it } from "vitest";
import {
  UNREVEALED_ATTRIBUTE,
  applyTextReveal,
  clearTextReveal,
  fullTextOf,
} from "../../src/bubble/text-reveal.js";

/** A detached root holding HTML. */
function rooted(html: string): HTMLElement {
  const root = document.createElement("div");
  root.innerHTML = html;
  return root;
}

describe("applyTextReveal", () => {
  it.each([
    { name: "shows nothing at zero", shown: 0, want: "" },
    { name: "cuts a text node across the cut", shown: 3, want: "hel" },
    { name: "shows text before the cut whole and the next node's prefix", shown: 7, want: "hello w" },
    { name: "shows everything at the full length", shown: 11, want: "hello world" },
  ])("$name", ({ shown, want }) => {
    // Arrange
    const root = rooted("<p>hello <strong>world</strong></p>");
    // Act
    applyTextReveal(root, shown);
    // Assert
    expect(root.textContent).toBe(want);
  });

  it("answers the full rendered length however much is shown", () => {
    // Arrange
    const root = rooted("<p>hello <strong>world</strong></p>");
    // Act
    const total = applyTextReveal(root, 2);
    // Assert
    expect(total).toBe(11);
  });

  it("hides an element whose text has not begun", () => {
    // Arrange
    const root = rooted("<ul><li>one</li><li>two</li></ul>");
    // Act
    applyTextReveal(root, 2);
    // Assert
    const items = [...root.querySelectorAll("li")].map((li) => li.hasAttribute(UNREVEALED_ATTRIBUTE));
    expect(items).toEqual([false, true]);
  });

  it("shows an element with its first character", () => {
    // Arrange
    const root = rooted("<ul><li>one</li><li>two</li></ul>");
    // Act
    applyTextReveal(root, 4);
    // Assert
    expect(root.querySelectorAll(`[${UNREVEALED_ATTRIBUTE}]`)).toHaveLength(0);
  });

  it("hides a textless element until the cut reaches it", () => {
    // Arrange
    const root = rooted("<p>ab</p><hr><p>cd</p>");
    // Act
    applyTextReveal(root, 1);
    // Assert
    expect(root.querySelector("hr")?.hasAttribute(UNREVEALED_ATTRIBUTE)).toBe(true);
  });

  it("shows a textless element once the cut reaches it", () => {
    // Arrange
    const root = rooted("<p>ab</p><hr><p>cd</p>");
    // Act
    applyTextReveal(root, 2);
    // Assert
    expect(root.querySelector("hr")?.hasAttribute(UNREVEALED_ATTRIBUTE)).toBe(false);
  });

  it("never splits a surrogate pair", () => {
    // Arrange: "a😀" is three code units, the emoji two of them.
    const root = rooted("<p>a😀</p>");
    // Act
    applyTextReveal(root, 2);
    // Assert
    expect(root.textContent).toBe("a");
  });

  it("grows a cut node back from its remembered full text", () => {
    // Arrange
    const root = rooted("<p>hello</p>");
    applyTextReveal(root, 1);
    // Act
    applyTextReveal(root, 4);
    // Assert
    expect(root.textContent).toBe("hell");
  });
});

describe("clearTextReveal", () => {
  it("restores every cut text node", () => {
    // Arrange
    const root = rooted("<p>hello <em>there</em></p>");
    applyTextReveal(root, 2);
    // Act
    clearTextReveal(root);
    // Assert
    expect(root.textContent).toBe("hello there");
  });

  it("shows every hidden element", () => {
    // Arrange
    const root = rooted("<ul><li>one</li><li>two</li></ul>");
    applyTextReveal(root, 1);
    // Act
    clearTextReveal(root);
    // Assert
    expect(root.querySelectorAll(`[${UNREVEALED_ATTRIBUTE}]`)).toHaveLength(0);
  });
});

describe("fullTextOf", () => {
  it("counts a cut node at its full text", () => {
    // Arrange
    const root = rooted("<p>hello <strong>world</strong></p>");
    applyTextReveal(root, 3);
    // Act
    const text = fullTextOf(root);
    // Assert
    expect(text).toBe("hello world");
  });
});
