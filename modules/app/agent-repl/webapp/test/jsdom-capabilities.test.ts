// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { installJsdomCapabilities } from "./jsdom-capabilities.js";

describe("the jsdom capabilities", () => {
  it.each([
    { name: "ResizeObserver", present: (): boolean => typeof globalThis.ResizeObserver === "function" },
    { name: "IntersectionObserver", present: (): boolean => typeof globalThis.IntersectionObserver === "function" },
    { name: "Range.getClientRects", present: (): boolean => typeof Range.prototype.getClientRects === "function" },
  ])("installs $name", ({ present }) => {
    // Arrange / Act
    installJsdomCapabilities();

    // Assert
    expect(present()).toBe(true);
  });

  it("answers a pseudo-element's style as running no animation, though its element runs one", () => {
    // Arrange
    installJsdomCapabilities();
    const el = document.createElement("span");
    el.style.animationName = "spin";
    document.body.append(el);

    // Act
    const read = [getComputedStyle(el).animationName, getComputedStyle(el, "::before").animationName];

    // Assert
    expect(read).toEqual(["spin", ""]);
    el.remove();
  });

  it("replaces a computed style a suite left standing with the substitution", () => {
    // Arrange: a suite's own getComputedStyle, as tree-layout.ts installs, left standing.
    installJsdomCapabilities();
    const substitution: unknown = Reflect.get(window, "getComputedStyle");
    const real = substitution as typeof window.getComputedStyle;
    window.getComputedStyle = (element: Element): CSSStyleDeclaration => real(element);
    const el = document.createElement("span");
    el.style.animationName = "spin";
    document.body.append(el);

    // Act
    installJsdomCapabilities();
    const read = getComputedStyle(el, "::before").animationName;

    // Assert: the substitution itself stands again, answering no pseudo style.
    expect([Reflect.get(window, "getComputedStyle") === substitution, read]).toEqual([true, ""]);
    el.remove();
  });

  it("stacks nothing over what files leave standing, however many files run", () => {
    // Arrange: 20000 files each leaving a wrapper over whatever stood, past
    // the depth that overflowed the stack when installs wrapped what stood.
    installJsdomCapabilities();
    const el = document.createElement("span");
    document.body.append(el);
    for (let file = 0; file < 20000; file += 1) {
      const beneath = window.getComputedStyle;
      window.getComputedStyle = (element: Element, pseudo?: string | null): CSSStyleDeclaration => beneath(element, pseudo);
      installJsdomCapabilities();
    }

    // Act: one read goes through whatever the installs left.
    const read = (): string => getComputedStyle(el).display;

    // Assert: a read of one frame's depth, no matter how many files ran.
    expect(read).not.toThrow();
    el.remove();
  });

  it("wraps the window's computed style once, however often it is installed", () => {
    // Arrange
    installJsdomCapabilities();
    const first: unknown = Reflect.get(window, "getComputedStyle");

    // Act
    installJsdomCapabilities();

    // Assert
    expect(Reflect.get(window, "getComputedStyle")).toBe(first);
  });

  it("answers no rects for a text range, since jsdom lays nothing out", () => {
    // Arrange
    installJsdomCapabilities();
    const range = document.createRange();
    range.selectNodeContents(document.createTextNode("hello"));

    // Act
    const rects = Array.from(range.getClientRects());

    // Assert
    expect(rects).toEqual([]);
  });
});
