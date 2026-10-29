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
  revisedWindow,
  visibleIn,
  wholeTextOf,
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

describe("wholeTextOf", () => {
  it("answers a truncated node's whole text", () => {
    // Arrange
    const root = document.createElement("p");
    root.textContent = "hello world";
    applyTextReveal(root, 3);

    // Act
    const whole = wholeTextOf(root.firstChild as Text);

    // Assert
    expect([whole, (root.firstChild as Text).data]).toEqual(["hello world", "hel"]);
  });

  it("answers an untouched node's own text", () => {
    // Arrange
    const node = document.createTextNode("plain");

    // Act / Assert
    expect(wholeTextOf(node)).toBe("plain");
  });
});

describe("applyTextReveal with a kept tail", () => {
  it.each([
    { name: "shows the head and the tail, hiding the middle", shown: 3, tailFrom: 8, want: "helrld" },
    { name: "shows the whole once the head reaches the tail", shown: 8, tailFrom: 8, want: "hello world" },
    { name: "shows only the tail at zero", shown: 0, tailFrom: 6, want: "world" },
  ])("$name", ({ shown, tailFrom, want }) => {
    // Arrange
    const root = rooted("<p>hello <strong>world</strong></p>");
    // Act
    applyTextReveal(root, shown, tailFrom);
    // Assert
    expect(root.textContent).toBe(want);
  });

  it("keeps an element standing in the tail revealed", () => {
    // Arrange
    const root = rooted("<ul><li>one</li><li>two</li><li>six</li></ul>");
    // Act
    applyTextReveal(root, 2, 6);
    // Assert
    const items = Array.from(root.querySelectorAll("li"));
    expect(items.map((li) => li.hasAttribute(UNREVEALED_ATTRIBUTE))).toEqual([false, true, false]);
  });

  it("restores the hidden middle whole when cleared", () => {
    // Arrange
    const root = rooted("<p>hello world</p>");
    applyTextReveal(root, 2, 9);
    // Act
    clearTextReveal(root);
    // Assert
    expect(root.textContent).toBe("hello world");
  });
});

describe("visibleIn", () => {
  it("never splits a surrogate pair at the tail's cut", () => {
    // Arrange: "a😀b" — the emoji is two code units at 1 and 2.
    const full = "a\u{1F600}b";
    // Act: a tail cut between the pair's halves.
    const visible = visibleIn(full, 0, { shown: 0, tailFrom: 2 });
    // Assert: the pair is shown whole.
    expect(visible).toBe("\u{1F600}b");
  });

  it("shows the head alone when there is no tail", () => {
    // Arrange, Act, Assert
    expect(visibleIn("hello", 0, { shown: 2, tailFrom: Number.POSITIVE_INFINITY })).toBe("he");
  });
});

describe("revisedWindow", () => {
  it("types out only a branch inserted in the middle", () => {
    // Arrange
    const before = "1 root\n2 last";
    const after = "1 root\n1.1 added\n2 last";
    // Act
    const window = revisedWindow(before, after, before.length);
    // Assert: the shared head and the shared tail stay shown; the branch types out.
    expect(after.slice(window.shown, window.tailFrom)).toBe("1.1 added\n");
  });

  it("types out an appended ending after the whole shown text, keeping no tail", () => {
    // Arrange
    const before = "hello";
    const after = "hello world";
    // Act
    const window = revisedWindow(before, after, before.length);
    // Assert
    expect(window).toEqual({ shown: 5, tailFrom: 11 });
  });

  it("has nothing to type for the same text restated", () => {
    // Arrange, Act
    const window = revisedWindow("same", "same", 4);
    // Assert
    expect(window.shown >= window.tailFrom).toBe(true);
  });

  it("resumes where an unfinished type-out stood, keeping no tail", () => {
    // Arrange: the earlier draw had shown 3 of 11 characters.
    const window = revisedWindow("hello world", "hello world", 3);
    // Assert
    expect(window).toEqual({ shown: 3, tailFrom: Number.POSITIVE_INFINITY });
  });

  it("never ends the shared head inside a surrogate pair", () => {
    // Arrange: two emoji sharing a high surrogate, differing in the low one.
    const before = "x\u{1F600}";
    const after = "x\u{1F601}";
    // Act
    const window = revisedWindow(before, after, before.length);
    // Assert
    expect(window.shown).toBe(1);
  });
});
