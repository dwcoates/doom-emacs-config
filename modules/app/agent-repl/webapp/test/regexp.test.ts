import { readdirSync, readFileSync, statSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { escapeRegExp } from "../src/regexp.js";
import { codeOf } from "./source-text.js";

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

  it("is the one escaper in the webapp's source", () => {
    // ARRANGE
    const files: string[] = [];
    const walk = (dir: string): void => {
      for (const entry of readdirSync(dir)) {
        const path = join(dir, entry);
        if (statSync(path).isDirectory()) walk(path);
        else if (path.endsWith(".ts")) files.push(path);
      }
    };
    walk(join(process.cwd(), "src"));
    // ACT
    const escapers = files.filter((file) => codeOf(readFileSync(file, "utf8")).includes("/[.*+?^${}()|[\\]\\\\]/g"));
    // ASSERT
    expect(escapers.map((file) => file.slice(process.cwd().length + 1))).toEqual(["src/regexp.ts"]);
  });
});
