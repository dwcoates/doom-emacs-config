/**
 * THE STYLESHEET'S SELECTABILITY CONTRACT.
 *
 * Owner ruling, 2026-09-14: "ensure that everything in the webapp (all text)
 * can be copied to clipboard; I need to be able to do this to easily relay
 * errors." A reader relays an error by dragging across it and pressing Cmd-C,
 * and the one thing in this codebase that can take that away wholesale is a
 * `user-select: none` rule. Two of them used to: the workspaces rail's blanket
 * rule and the thinking fold's summary.
 *
 * So the rule is inverted here: `user-select: none` is FORBIDDEN unless its
 * selector is named below with a reason. The allowlist admits pure controls
 * whose glyph is a state, never a sentence — a chevron, a fold triangle, a
 * disclosure marker. Adding a selector to it is a decision someone makes
 * deliberately, in one line, rather than a rule that quietly lands in a 5000
 * line file.
 */
import { describe, expect, it } from "vitest";
import stylesheet from "../src/styles.css?raw";

/**
 * Selectors permitted to suppress selection, each with the one reason that
 * earns it: the element's text is a control's state, not information.
 */
const ALLOWED: ReadonlyMap<string, string> = new Map([
  ["#ws-sidebar .row .chev", "the row's expand chevron: a '▸' glyph, no information in it"],
  ["#ws-sidebar [data-section-fold]", "the section fold triangle: '▸'/'▾' is the fold's state"],
  ["#ws-sidebar .repo-head .tri", "the repo header's fold triangle, the same glyph and the same state"],
  [".thinking summary::marker", "the disclosure marker only; the summary's TEXT stays user-select: text"],
]);

interface Rule {
  selectors: string[];
  declarations: string;
}

/** Every `selector { declarations }` pair in the file, comments stripped. */
function rulesOf(css: string): Rule[] {
  const withoutComments = css.replace(/\/\*[\s\S]*?\*\//g, "");
  const rules: Rule[] = [];
  const pattern = /([^{}]+)\{([^{}]*)\}/g;
  let match = pattern.exec(withoutComments);
  while (match !== null) {
    rules.push({
      selectors: (match[1] ?? "").split(",").map((one) => one.trim()).filter((one) => one !== ""),
      declarations: match[2] ?? "",
    });
    match = pattern.exec(withoutComments);
  }
  return rules;
}

/** The selectors on which any `user-select`/`-webkit-user-select` is `none`. */
function selectorsSuppressingSelection(css: string): string[] {
  const suppressing: string[] = [];
  for (const rule of rulesOf(css)) {
    if (!/(?:^|[\s;])-?(?:webkit-)?user-select\s*:\s*none/.test(rule.declarations)) continue;
    suppressing.push(...rule.selectors);
  }
  return suppressing;
}

describe("the stylesheet's selectability contract", () => {
  it("suppresses selection only on allowlisted pure controls", () => {
    // Arrange / Act
    const suppressing = selectorsSuppressingSelection(stylesheet);

    // Assert
    const unexplained = suppressing.filter((selector) => !ALLOWED.has(selector));
    expect(unexplained).toEqual([]);
  });

  it("leaves the thinking fold's summary text selectable", () => {
    // Arrange / Act
    const summaryRule = rulesOf(stylesheet.replace(/\/\*[\s\S]*?\*\//g, "")).find((rule) =>
      rule.selectors.includes(".thinking summary"),
    );

    // Assert
    expect(summaryRule?.declarations).toMatch(/user-select\s*:\s*text/);
  });

  it("lets the topbar's objective line take pointer events, so it can be dragged over", () => {
    // Arrange / Act
    const taskSummary = rulesOf(stylesheet).find((rule) => rule.selectors.includes("#task-summary"));

    // Assert
    expect(taskSummary?.declarations).not.toMatch(/pointer-events\s*:\s*none/);
  });

  it("keeps every allowlisted selector in use, so the allowlist cannot rot", () => {
    // Arrange / Act
    const suppressing = new Set(selectorsSuppressingSelection(stylesheet));

    // Assert
    expect([...ALLOWED.keys()].filter((selector) => !suppressing.has(selector))).toEqual([]);
  });
});
