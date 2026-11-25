/**
 * EVERY TITLE SITE SHARES THE ONE FOLD (src/feed/title-fold.ts): a rule over
 * the sources that draw a title line.
 */
import { readFileSync } from "node:fs";
import { join, relative } from "node:path";
import { describe, expect, it } from "vitest";
import { codeUnder, filesUnder } from "../files-under.js";

describe("every title site shares the one fold", () => {
  const src = join(process.cwd(), "src");

  /** The card modules that draw a title line, and the one call each makes. */
  const TITLE_SITES = [
    "feed/cards/shell.ts",
    "feed/cards/tool-call.ts",
    "feed/cards/skill.ts",
    "feed/cards/hook.ts",
    "feed/rows/subagent.ts",
  ];

  it.each(TITLE_SITES)("%s folds its title through foldTitle", (site) => {
    // Arrange / Act
    const text = readFileSync(join(src, site), "utf8");

    // Assert
    expect(text).toMatch(/\bfoldTitle\(/);
  });

  it("no source clamps lines by hand", () => {
    // Arrange / Act — an inline clamp is a title cap rolled outside the fold.
    const clamping = filesUnder(src)
      .filter((path) => /line-?clamp|lineClamp/i.test(readFileSync(path, "utf8")))
      .map((path) => relative(src, path));

    // Assert
    expect(clamping).toEqual([]);
  });

  it("no source but the fold's own writes the title-fold class", () => {
    // Arrange / Act — the class literal in CODE anywhere but its declaration
    // and the toggle's list means a site marking a title without the measurer.
    // Comments are prose about the fold, so they are read past.
    const owners = new Set(["feed/bubble-more.ts", "feed/title-fold.ts", "expand.ts"]);
    const marking = [...codeUnder(src)]
      .filter(([path, code]) => !owners.has(path) && /["'`.]title-fold/.test(code))
      .map(([path]) => path);

    // Assert
    expect(marking).toEqual([]);
  });
});
