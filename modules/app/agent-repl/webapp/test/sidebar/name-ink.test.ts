// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { NAME_INK_CLASS, NAME_INK_MOTIONS, NAME_INK_TEXT_ATTRIBUTE, drawNameInk } from "../../src/sidebar/name-ink.js";
import { REVIVING_CLASS } from "../../src/sidebar/reviving.js";

/** A drawn name LABEL reading TEXT and wearing CLASSES, as row.ts draws one. */
function label(text: string, ...classes: string[]): HTMLElement {
  const el = document.createElement("span");
  el.className = ["name", ...classes].join(" ");
  el.textContent = text;
  return el;
}

/** The ink boxes drawn in LABEL. */
function inksOf(el: HTMLElement): Element[] {
  return [...el.children].filter((kid) => kid.classList.contains(NAME_INK_CLASS));
}

describe("drawNameInk", () => {
  it.each([
    { name: "gives a breathing name its ink box", classes: ["breathes"] },
    { name: "gives a reviving name its ink box", classes: [REVIVING_CLASS] },
    { name: "gives a name that breathes and revives one ink box", classes: ["breathes", REVIVING_CLASS] },
  ])("$name", ({ classes }) => {
    // Arrange
    const el = label("alpha", ...classes);

    // Act
    drawNameInk(el, "alpha");

    // Assert
    expect(inksOf(el)).toHaveLength(1);
  });

  it.each([
    { name: "draws no box on a still name", classes: [] },
    { name: "draws no box on a tinted name that does not breathe", classes: ["tinted"] },
    { name: "draws no box on a fading name that does not breathe", classes: ["fading"] },
  ])("$name", ({ classes }) => {
    // Arrange
    const el = label("alpha", ...classes);

    // Act
    drawNameInk(el, "alpha");

    // Assert
    expect(el.children).toHaveLength(0);
  });

  it("draws the copies' text from the name", () => {
    // Arrange
    const el = label("alpha", "breathes");

    // Act
    drawNameInk(el, "alpha");

    // Assert
    expect(inksOf(el)[0]?.getAttribute(NAME_INK_TEXT_ATTRIBUTE)).toBe("alpha");
  });

  it("hides the copies from the accessibility tree", () => {
    // Arrange
    const el = label("alpha", "breathes");

    // Act
    drawNameInk(el, "alpha");

    // Assert
    expect(inksOf(el)[0]?.getAttribute("aria-hidden")).toBe("true");
  });

  // Search (search.ts) reads the name's textContent; the copies are
  // generated text, so the name still reads once.
  it("leaves the name's text the name alone", () => {
    // Arrange
    const el = label("alpha", "breathes", REVIVING_CLASS);

    // Act
    drawNameInk(el, "alpha");

    // Assert
    expect(el.textContent).toBe("alpha");
  });

  it("keys on exactly the breath and the reviving shimmer", () => {
    // Arrange / Act / Assert
    expect(NAME_INK_MOTIONS).toEqual(["breathes", REVIVING_CLASS]);
  });
});
