/**
 * THE SHARED STYLESHEET-RULE READERS (`test/stylesheet.ts`).
 *
 * Every test that reads a rule off `src/styles.css` by text goes through
 * `rulesOf` or `selectorOf`, so a comment that names a selector can never be
 * credited to the rule after it. `source-text.test.ts` pins that sharing.
 */
import { describe, expect, it } from "vitest";
import { rulesOf, selectorOf } from "./stylesheet.js";

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

describe("selectorOf", () => {
  it("answers the selector the pattern's first group captures from the real stylesheet", () => {
    // Arrange / Act
    const selector = selectorOf(/([^{}]*\.merge-strip)\s*\{/, "merge strip");
    // Assert
    expect(selector).toBe(".merge-strip");
  });

  it("fails loudly naming what it looked for when nothing matches", () => {
    // Arrange / Act / Assert
    expect(() => selectorOf(/(\.no-such-class-anywhere)\s*\{/, "phantom rule")).toThrow(
      "the stylesheet has no phantom rule",
    );
  });
});
