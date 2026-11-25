/** escapeRegExp is the one escaper in the webapp's source. */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { codeOf } from "../test/source-text.js";
import { filesUnder } from "./files-under.js";

describe("escapeRegExp", () => {
  it("is the one escaper in the webapp's source", () => {
    // ARRANGE
    const files = filesUnder(join(process.cwd(), "src"));
    // ACT
    const escapers = files.filter((file) => codeOf(readFileSync(file, "utf8")).includes("/[.*+?^${}()|[\\]\\\\]/g"));
    // ASSERT
    expect(escapers.map((file) => file.slice(process.cwd().length + 1))).toEqual(["src/regexp.ts"]);
  });
});
