/**
 * The merge tests tab's rules: its one placement of a suite's counts
 * (src/feed/merge/tests-tab.ts), and the stylesheet's suite colors, run tally
 * and reused-suite source (src/styles.css).
 */
import { describe, expect, it } from "vitest";
import TESTS_TAB from "../../../src/feed/merge/tests-tab.ts?raw";
import STYLESHEET from "../../../src/styles.css?raw";
import { rulesOf } from "../../stylesheet-text.js";

describe("the stylesheet's suite colors", () => {
  /** The declarations of the rule whose selector is exactly SELECTOR. */
  function declarationsOf(selector: string): string {
    return rulesOf(STYLESHEET)
      .filter((rule) => rule.selectors.includes(selector))
      .map((rule) => rule.declarations)
      .join(";");
  }

  it.each([
    [".merge-suite-glyph.is-passing", "var(--suite-running)"],
    [".merge-suite-glyph.is-failing", "var(--err)"],
    [".merge-suite-glyph.is-unreported", "var(--suite-running)"],
    [".merge-suite-glyph.is-succeeded", "var(--ok)"],
    [".merge-suite-glyph.is-failed", "var(--err)"],
    [".merge-suite-counts .is-passed", "var(--ok)"],
    [".merge-suite-counts .is-failed", "var(--err)"],
  ])("colors %s with %s", (selector, color) => {
    expect(declarationsOf(selector)).toContain(`color: ${color}`);
  });

  it("paints no running suite dot green", () => {
    const green = rulesOf(STYLESHEET).filter(
      (rule) =>
        rule.selectors.some((one) => one === ".merge-suite-glyph.is-unreported" || one === ".merge-suite-glyph.is-passing") &&
        rule.declarations.includes("--ok"),
    );
    expect(green).toEqual([]);
  });

  it("paints no suite dot purple", () => {
    const purple = rulesOf(STYLESHEET).filter(
      (rule) => rule.selectors.some((one) => one.startsWith(".merge-suite-glyph")) && rule.declarations.includes("--blocked"),
    );
    expect(purple).toEqual([]);
  });
});

describe("the stylesheet's run tally", () => {
  const declarationsOf = (selector: string) =>
    rulesOf(STYLESHEET)
      .filter((rule) => rule.selectors.includes(selector))
      .map((rule) => rule.declarations)
      .join(";");

  it("draws the tally in the run's badge at the duration's font size", () => {
    expect([declarationsOf(".merge-tab > .merge-run-tally"), declarationsOf(".merge-tab-duration")].map((d) =>
      d.match(/font-size: [^;]+/)?.[0],
    )).toEqual(["font-size: 0.85em", "font-size: 0.85em"]);
  });

  it("no longer pushes the tally to the run strip's right end", () => {
    expect(declarationsOf(".merge-strip > .merge-run-tally")).toBe("");
  });
});

describe("placeFeedMergeTestCounts", () => {
  it("is the one placement of counts in tests-tab", () => {
    expect(TESTS_TAB.match(/:scope > \.merge-suite-counts/g)).toHaveLength(1);
  });
});

describe("a reused suite names the earlier run that passed its inputs", () => {
  it("styles the source quieter than the name", () => {
    const rules = rulesOf(STYLESHEET).filter((rule) => rule.selectors.includes(".merge-suite-reused"));
    expect(rules.map((rule) => rule.declarations)).toEqual([expect.stringContaining("var(--muted)")]);
  });
});
