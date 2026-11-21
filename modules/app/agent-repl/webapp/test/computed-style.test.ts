// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { filesUnder } from "./files-under.js";
import { describe, expect, it } from "vitest";
import { overrideComputedStyle } from "./computed-style.js";

const padded = (top: string) => ({ scrollPaddingTop: top }) as CSSStyleDeclaration;

describe("overrideComputedStyle", () => {
  it("answers the override for its element and the engine's style for every other", () => {
    // Arrange
    const a = document.createElement("div");
    const other = document.createElement("div");
    document.body.append(a, other);

    // Act
    overrideComputedStyle(a, padded("3px"));

    // Assert
    expect(window.getComputedStyle(a).scrollPaddingTop).toBe("3px");
    expect(window.getComputedStyle(other)).not.toBe(window.getComputedStyle(a));
  });

  it("answers every override a test makes, without recursing", () => {
    // Arrange
    const a = document.createElement("div");
    const b = document.createElement("div");

    // Act
    overrideComputedStyle(a, padded("1px"));
    overrideComputedStyle(b, padded("2px"));

    // Assert
    expect([window.getComputedStyle(a).scrollPaddingTop, window.getComputedStyle(b).scrollPaddingTop]).toEqual(["1px", "2px"]);
  });

  it("leaves a test free to override an element it keeps", () => {
    // Arrange / Act
    overrideComputedStyle(document.body, padded("9px"));

    // Assert
    expect(window.getComputedStyle(document.body).scrollPaddingTop).toBe("9px");
  });

  it("hands the next test the engine's style", () => {
    // Assert
    expect(window.getComputedStyle(document.body).scrollPaddingTop).not.toBe("9px");
  });
});

describe("every test that overrides some elements' computed style", () => {
  it("does it through overrideComputedStyle, never a spy of its own over the real method", () => {
    const here = fileURLToPath(import.meta.url);
    const handRolled = filesUnder(join(process.cwd(), "test"), [".test.ts", ".test.tsx"]).filter(
      (path) => path !== here && readFileSync(path, "utf8").includes("window.getComputedStyle.bind(window)"),
    );

    expect(handRolled).toEqual([]);
  });
});
