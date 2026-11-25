import { describe, expect, it } from "vitest";
import { countNoun, pluralNoun } from "../src/count-noun.js";

describe("pluralNoun", () => {
  it.each([
    { name: "keeps the singular for exactly one", n: 1, want: "commit" },
    { name: "adds an s for zero", n: 0, want: "commits" },
    { name: "adds an s for more than one", n: 2, want: "commits" },
  ])("$name", ({ n, want }) => {
    expect(pluralNoun(n, "commit")).toBe(want);
  });
});

describe("countNoun", () => {
  it.each([
    { name: "one in the singular", n: 1, noun: "merge", want: "1 merge" },
    { name: "zero in the plural", n: 0, noun: "merge", want: "0 merges" },
    { name: "many in the plural", n: 12, noun: "merge", want: "12 merges" },
    { name: "a several-word noun on its last word", n: 2, noun: "starting task", want: "2 starting tasks" },
  ])("says $name", ({ n, noun, want }) => {
    expect(countNoun(n, noun)).toBe(want);
  });
});
