// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { installJsdomCapabilities } from "./jsdom-capabilities.js";

const here = path.dirname(fileURLToPath(import.meta.url));

/** Every jsdom project's setup file. */
const SETUP_FILES = ["setup.ts", "webapp-layer/setup.ts"];

/** The single-capability installers only the shared installer may call. */
const SINGLE_INSTALLERS = [
  "installResizeObserver",
  "installIntersectionObserver",
  "installRangeClientRects",
  "installPseudoElementStyle",
];

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

  it("wraps a computed style a suite installed over the substitution", () => {
    // Arrange: a suite's own getComputedStyle, as tree-layout.ts installs.
    installJsdomCapabilities();
    const real: unknown = Reflect.get(window, "getComputedStyle");
    const suiteOwn = (element: Element): CSSStyleDeclaration => (real as typeof window.getComputedStyle)(element);
    window.getComputedStyle = suiteOwn;
    const el = document.createElement("span");
    el.style.animationName = "spin";
    document.body.append(el);

    // Act
    installJsdomCapabilities();
    const read = getComputedStyle(el, "::before").animationName;

    // Assert
    expect(read).toBe("");
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

  it.each(SETUP_FILES)("is what %s installs", (file) => {
    // Arrange
    const source = readFileSync(path.join(here, file), "utf8");

    // Act
    const called = SINGLE_INSTALLERS.filter((name) => source.includes(name));

    // Assert
    expect(source).toContain("installJsdomCapabilities();");
    expect(called).toEqual([]);
  });
});
