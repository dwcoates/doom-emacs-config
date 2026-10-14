import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { countNoun, pluralNoun } from "../src/count-noun.js";
import { filesUnder } from "./files-under.js";

const here = path.dirname(fileURLToPath(import.meta.url));

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

describe("every count is worded through count-noun", () => {
  it("hand-rolls the singular-or-s ternary in no src file", () => {
    const src = path.join(here, "..", "src");
    const handRolled = /=== 1 \? (?:"" : "s"|" ?(\w+)" : " ?\1s")/;
    const offenders = filesUnder(src)
      .filter((file) => handRolled.test(readFileSync(file, "utf8")))
      .map((file) => path.relative(src, file));
    expect(offenders).toEqual([]);
  });
});
