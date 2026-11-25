import { describe, expect, it } from "vitest";
import { escapeRegExp } from "../src/regexp.js";

describe("escapeRegExp", () => {
  const cases: Array<{ name: string; text: string }> = [
    { name: "a plain word", text: "alpha" },
    { name: "a dot", text: "a.b" },
    { name: "parentheses", text: "main (doom)" },
    { name: "every special character at once", text: ".*+?^${}()|[]\\" },
    { name: "an empty string", text: "" },
  ];
  for (const tc of cases) {
    it(`matches ${tc.name} literally and nothing else`, () => {
      // ARRANGE
      const pattern = new RegExp(`^${escapeRegExp(tc.text)}$`);
      // ACT + ASSERT
      expect(pattern.test(tc.text)).toBe(true);
      expect(pattern.test(`${tc.text}x`)).toBe(false);
    });
  }
});
