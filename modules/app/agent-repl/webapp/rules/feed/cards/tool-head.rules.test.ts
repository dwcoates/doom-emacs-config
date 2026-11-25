/**
 * Every head and tool card draws through the shared tool head builders
 * (src/feed/cards/tool-head.ts): the head clock through asHeadClock, a clock
 * it may not have through drawStatedToolCardClock, the head through
 * drawToolCardHead, the badge through drawToolCardBadge, and no module builds
 * a tool head, name or clock by hand.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { filesUnder } from "../../files-under.js";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../../src");

/**
 * Every module drawing a head's elapsed clock: the tool card head and each
 * bubble head. A module here that draws a reserved elapsed clock not wrapped
 * in `asHeadClock` has a head clock the stylesheet leaves where it was drawn.
 */
const HEAD_CLOCK_SITES = ["feed/cards/tool-head.ts", "feed/cards/shell.ts", "feed/rows/subagent.ts", "feed/merge/merge.ts"] as const;

describe("every head marks its clock through asHeadClock", () => {
  it.each(HEAD_CLOCK_SITES)("wraps every reserved elapsed clock %s draws", (file) => {
    // Arrange
    const source = readFileSync(path.join(SRC, file), "utf8");

    // Act
    const drawn = [...source.matchAll(/reserved(?:Live|Settled)ElapsedClock\(/g)].length;
    const marked = [...source.matchAll(/asHeadClock\(\s*reserved(?:Live|Settled)ElapsedClock\(/g)].length;
    const viaCardClock = /head\.append\(asHeadClock\(clock\)\)/.test(source) && /drawStatedToolCardClock\(/.test(source);

    // Assert
    expect(file === "feed/cards/tool-head.ts" ? viaCardClock : drawn > 0 && marked === drawn).toBe(true);
  });
});

describe("drawStatedToolCardClock", () => {
  it("is the one way a card draws a clock it may not have", () => {
    // A card checking for a stated clock by hand drifts from the shared step.
    // The tool group alone draws through drawToolCardClock, from a member clock
    // it already knows is stated.
    const users = filesUnder(SRC)
      .filter((file) => file !== path.join(SRC, "feed/cards/tool-head.ts"))
      .filter((file) => /drawToolCardClock\(/.test(readFileSync(file, "utf8")));
    expect(users.map((file) => path.relative(SRC, file))).toEqual(["feed/tool-group.ts"]);
  });
});

/** The five purple-titled tool cards, by the module that draws each. */
const TOOL_CARDS = [
  "feed/cards/tool-call.ts",
  "feed/cards/skill.ts",
  "feed/cards/hook.ts",
  "feed/asks/permission.ts",
  "feed/asks/question.ts",
] as const;

describe("every tool card heads through the one builder", () => {
  it.each(TOOL_CARDS)("heads %s through drawToolCardHead", (file) => {
    expect(readFileSync(path.join(SRC, file), "utf8")).toContain("drawToolCardHead({");
  });

  it("leaves only the bubbles' head row building a bare head", () => {
    // Bubbles are their own category and lay out their own head inside the
    // shared row; any other module building a bare head row is a hand-rolled
    // tool card head.
    const own = path.join(SRC, "feed/cards/tool-head.ts");
    const users = filesUnder(SRC)
      .filter((file) => file !== own)
      .filter((file) => /drawToolHead\(/.test(readFileSync(file, "utf8")));
    expect(users.map((file) => path.relative(SRC, file))).toEqual(["feed/bubble.ts"]);
  });

  it("leaves no module building a tool head or tool name by hand", () => {
    // A hand-rolled head drifts from the shared one; this fails it by name.
    const own = path.join(SRC, "feed/cards/tool-head.ts");
    const offenders = filesUnder(SRC)
      .filter((file) => file !== own)
      .filter((file) => /className\s*=\s*["'`]tool-(head|name|clock)\b/.test(readFileSync(file, "utf8")));
    expect(offenders.map((file) => path.relative(SRC, file))).toEqual([]);
  });
});

describe("every tool card badges through the one builder", () => {
  it.each(TOOL_CARDS)("builds no badge by hand in %s", (file) => {
    // An exit code is content, not the head's badge, and keeps its own chip.
    const source = readFileSync(path.join(SRC, file), "utf8");
    const handRolled = [...source.matchAll(/className\s*=\s*[^;]*["'`]badge\b[^;]*;/g)]
      .map((m) => m[0])
      .filter((assignment) => !/exit|q-chip/.test(assignment));
    expect(handRolled).toEqual([]);
  });
});
