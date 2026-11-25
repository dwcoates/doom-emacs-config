/**
 * THE STYLESHEET TEXT PARSERS (`test/css-text.ts`), over CSS each case hands
 * them. Their use on the real stylesheet is `rules/stylesheet-text.test.ts`.
 */
import { describe, expect, it } from "vitest";
import { isOwnRule, keyframesOf, mediaBlocksOf, rulesOf } from "./css-text.js";

describe("rulesOf", () => {
  it.each([
    {
      name: "reads each rule's selectors and declarations in source order",
      css: ".a { color: red; } .b { color: blue; }",
      want: [
        { selectors: [".a"], declarations: " color: red; " },
        { selectors: [".b"], declarations: " color: blue; " },
      ],
    },
    {
      name: "splits a selector list into its trimmed selectors",
      css: ".a ,\n .b > .c { display: none; }",
      want: [{ selectors: [".a", ".b > .c"], declarations: " display: none; " }],
    },
    {
      name: "never credits a rule with a selector its preceding comment names",
      css: "/* see .bubble-subfeed { } */ .strip { display: flex; }",
      want: [{ selectors: [".strip"], declarations: " display: flex; " }],
    },
    {
      name: "reads no rules from a sheet of comments alone",
      css: "/* .a { color: red; } */",
      want: [],
    },
  ])("$name", ({ css, want }) => {
    // Arrange / Act
    const rules = rulesOf(css);
    // Assert
    expect(rules).toEqual(want);
  });
});

describe("isOwnRule", () => {
  it.each([
    { name: "is a rule whose selector list is the selector alone", css: ".a { color: red; }", want: true },
    { name: "is not a rule that shares its declarations with another selector", css: ".a, .b { color: red; }", want: false },
    { name: "is not a rule for a selector that only begins with it", css: ".a:hover { color: red; }", want: false },
    { name: "is not another selector's own rule", css: ".b { color: red; }", want: false },
  ])("$name", ({ css, want }) => {
    // Arrange
    const [rule] = rulesOf(css);

    // Act
    const own = isOwnRule(rule, ".a");

    // Assert
    expect(own).toBe(want);
  });
});

describe("mediaBlocksOf", () => {
  it.each([
    {
      name: "reads a block's body between its own braces, nested rules whole",
      css: "@media (q) { .a { x: 1; } .b { y: 2; } } .c { z: 3; }",
      want: [" .a { x: 1; } .b { y: 2; } "],
    },
    {
      name: "reads every block of the query in source order",
      css: "@media (q) { .a { x: 1; } } .m { } @media (q) { .b { y: 2; } }",
      want: [" .a { x: 1; } ", " .b { y: 2; } "],
    },
    { name: "skips a block of another query", css: "@media (other) { .a { x: 1; } }", want: [] },
  ])("$name", ({ css, want }) => {
    // Arrange / Act
    const bodies = mediaBlocksOf("q", css).map((block) => block.body);

    // Assert
    expect(bodies).toEqual(want);
  });

  it("spans a block from its @media to just past its closing brace", () => {
    // Arrange
    const css = ".x { } @media (q) { .a { x: 1; } } .y { }";

    // Act
    const [block] = mediaBlocksOf("q", css);

    // Assert
    expect(css.slice(block?.start, block?.end)).toBe("@media (q) { .a { x: 1; } }");
  });

  it("fails loudly on a block that never closes", () => {
    // Arrange / Act / Assert
    expect(() => mediaBlocksOf("q", "@media (q) { .a { x: 1; }")).toThrow(/@media \(q\) block never closes/);
  });
});

describe("keyframesOf", () => {
  it.each([
    { what: "a block of several stops", css: "@keyframes a {\n  0% { opacity: 0; }\n  100% { opacity: 1; }\n}\n", want: "\n  0% { opacity: 0; }\n  100% { opacity: 1; }" },
    { what: "a block of a single stop", css: "@keyframes a {\n  50% { opacity: 0; }\n}\n", want: "\n  50% { opacity: 0; }" },
    { what: "the named block among others", css: "@keyframes b {\n  to { color: red; }\n}\n@keyframes a {\n  to { color: blue; }\n}\n", want: "\n  to { color: blue; }" },
  ])("answers the body of $what", ({ css, want }) => {
    // Arrange / Act
    const body = keyframesOf("a", css);
    // Assert
    expect(body).toBe(want);
  });

  it("does not answer a block whose name merely starts with the asked name", () => {
    // Arrange
    const css = "@keyframes ab {\n  to { color: red; }\n}\n";
    // Act / Assert
    expect(() => keyframesOf("a", css)).toThrow("the stylesheet has no @keyframes a");
  });

  it("fails loudly when the block never closes", () => {
    // Arrange
    const css = "@keyframes a {\n  to { color: red; }";
    // Act / Assert
    expect(() => keyframesOf("a", css)).toThrow("the stylesheet's @keyframes a never closes");
  });
});
