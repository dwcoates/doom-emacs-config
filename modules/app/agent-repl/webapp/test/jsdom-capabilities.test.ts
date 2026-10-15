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
  "installGetAnimations",
];

describe("the jsdom capabilities", () => {
  it.each([
    { name: "ResizeObserver", present: (): boolean => typeof globalThis.ResizeObserver === "function" },
    { name: "IntersectionObserver", present: (): boolean => typeof globalThis.IntersectionObserver === "function" },
    { name: "Range.getClientRects", present: (): boolean => typeof Range.prototype.getClientRects === "function" },
    { name: "Element.getAnimations", present: (): boolean => typeof Element.prototype.getAnimations === "function" },
  ])("installs $name", ({ present }) => {
    // Arrange / Act
    installJsdomCapabilities();

    // Assert
    expect(present()).toBe(true);
  });

  it("answers no animations for an element, since jsdom runs none", () => {
    // Arrange
    installJsdomCapabilities();

    // Act
    const animations = document.createElement("nav").getAnimations({ subtree: true });

    // Assert
    expect(animations).toEqual([]);
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
