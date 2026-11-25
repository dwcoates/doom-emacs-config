/** Every count is worded through src/count-noun.ts: no source hand-rolls the singular-or-s ternary. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { filesUnder } from "./files-under.js";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("every count is worded through count-noun", () => {
  it("hand-rolls the singular-or-s ternary in no src file", () => {
    const src = path.join(here, "..", "src");
    const handRolled = /=== 1n? \? (?:"" : "s"|" ?(\w+)" : " ?\1s"|"[^"]*\b1 \w+" : `[^`]*\$\{[^}]*\} \w+s`)/;
    const offenders = filesUnder(src)
      .filter((file) => handRolled.test(readFileSync(file, "utf8")))
      .map((file) => path.relative(src, file));
    expect(offenders).toEqual([]);
  });
});
