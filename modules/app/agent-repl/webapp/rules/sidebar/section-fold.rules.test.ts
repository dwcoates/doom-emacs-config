/**
 * THE SIDEBAR'S FOLDS ARE MADE AND ASKED THROUGH `section-fold` ALONE: the
 * sidebar's other sources read for a hand-spelled fold control or ask.
 */
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { codeOf } from "../../test/source-text.js";

describe("the sidebar's folds", () => {
  it("make their fold control and ask their fold only through section-fold", () => {
    // A fold control or a fold ask spelled by hand could fold one way here and another there.
    const dir = join(__dirname, "..", "..", "src", "sidebar");
    const offenders = readdirSync(dir)
      .filter((name) => name.endsWith(".ts") && name !== "section-fold.ts")
      .filter((name) => {
        const code = codeOf(readFileSync(join(dir, name), "utf8"));
        return code.includes(`"data-section-fold", ""`) || code.includes(`case: "foldSection"`);
      });
    expect(offenders).toEqual([]);
  });
});
