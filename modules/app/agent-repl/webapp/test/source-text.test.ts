/**
 * THE SHARED COMMENT STRIPPERS (`test/source-text.ts`). The proof every scan
 * uses them is `rules/source-text.rules.test.ts`.
 */
import { describe, expect, it } from "vitest";
import { codeOf, functionBody, withoutBlockComments } from "./source-text.js";

describe("withoutBlockComments", () => {
  it.each([
    { name: "removes a block comment", text: "a /* note */ b", want: "a  b" },
    { name: "removes a block comment spanning lines", text: "a /* one\ntwo */ b", want: "a  b" },
    { name: "removes each of several block comments", text: "/* x */a/* y */", want: "a" },
    { name: "keeps a line comment", text: "a // note", want: "a // note" },
  ])("$name", ({ text, want }) => {
    // Arrange / Act
    const stripped = withoutBlockComments(text);
    // Assert
    expect(stripped).toBe(want);
  });
});

describe("codeOf", () => {
  it.each([
    { name: "removes a block comment", source: "f(); /* note */", want: "f(); " },
    { name: "removes a whole-line comment", source: "f();\n  // note\ng();", want: "f();\n\ng();" },
    { name: "keeps a trailing slash pair, as in a URL", source: 'u("https://x"); // note', want: 'u("https://x"); // note' },
  ])("$name", ({ source, want }) => {
    // Arrange / Act
    const code = codeOf(source);
    // Assert
    expect(code).toBe(want);
  });
});

describe("functionBody", () => {
  it.each([
    { name: "cuts a function from its keyword to its closing brace", name_: "f", want: "function f() {\n  g();" },
    { name: "cuts an exported function the same way", name_: "h", want: "function h(x: number) {\n  return x;" },
  ])("$name", ({ name_, want }) => {
    // Arrange
    const code = "function f() {\n  g();\n}\nexport function h(x: number) {\n  return x;\n}\n";
    // Act
    const body = functionBody(code, name_);
    // Assert
    expect(body).toBe(want);
  });

  it("finds no function whose name merely opens another's", () => {
    // Arrange
    const code = "function fg() {\n}\n";
    // Act + Assert
    expect(() => functionBody(code, "f")).toThrow("the code declares no function f");
  });

  it("refuses a function with no closing brace at the start of a line", () => {
    // Arrange
    const code = "function f() { g(); }";
    // Act + Assert
    expect(() => functionBody(code, "f")).toThrow("the function f has no closing brace at the start of a line");
  });
});
