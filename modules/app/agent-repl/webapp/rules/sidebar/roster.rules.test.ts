/**
 * THE ROSTER'S STYLESHEET RULES AND ITS ONE-WAY DRAWING, read off the
 * stylesheet and src/sidebar/roster.ts: the section counts, the Recently
 * Merged group's paint, the rail row paddings, and the one helper, shape and
 * box the group sections are drawn through.
 */
import { describe, expect, it } from "vitest";
import css from "../../src/styles.css?raw";
import rosterSource from "../../src/sidebar/roster.ts?raw";

describe("a section's folded count", () => {
  it("draws no section count inside a badge", () => {
    expect(css).not.toMatch(/\.sb-count[^{]*\{[^}]*(border|padding)/);
  });
});

describe("the Recently Merged section's header", () => {
  it("draws its today count through the one helper the groups draw theirs with", () => {
    // A section header formatting its own summary could drift from the groups'.
    expect(rosterSource.match(/appendChild\(drawMergedTodayCount\(/g)?.length).toBe(2);
  });
});

describe("recently merged rows", () => {
  it("paints a Recently Merged group's repository title purple and leaves its flanking rules alone", () => {
    expect(css).toContain("#ws-sidebar .merged-group-head > .sb-label { color: var(--blocked); }");
    expect(css).toMatch(/\.merged-group-head::after \{[^}]*background: var\(--border\);/);
  });

  it("styles a merged name with the repository headers' colour variable", () => {
    expect(css).toContain("#ws-sidebar .row .name.band-name { --name-rest: var(--repo-head-expanded); color: var(--repo-head-expanded); }");
  });

  it("draws no gutter bar on a row merged today", () => {
    expect(css).not.toContain("merged-today");
  });

  it("declares the one rail row vertical padding", () => {
    expect(css).toContain("#ws-sidebar { --row-pad-y: 0.14rem; }");
  });

  it("pads every rail row with the shared vertical padding", () => {
    expect(css).toContain("  padding: var(--row-pad-y) var(--row-pad-right) var(--row-pad-y) calc(0.9rem + var(--row-indent));");
  });

  it("declares the one rail row right padding", () => {
    expect(css).toContain("#ws-sidebar { --row-pad-right: 0.7rem; }");
  });

  it("builds a merged row's height out of the shared vertical padding", () => {
    expect(css).toContain("--merged-row-h: calc(var(--row-pad-y) * 2 + var(--merged-row-line-h));");
  });

  it("restates no row vertical padding of its own", () => {
    expect(css).not.toContain("--merged-row-pad-y");
  });
});

describe("drawGroupedSection", () => {
  it("is the one shape both group sections are drawn through", () => {
    // A section drawing its groups by hand could fold or leave itself out differently.
    expect([rosterSource.match(/return drawGroupedSection\(/g)?.length, rosterSource.match(/leftOut: \{ when: \(\) => groups\.length === 0/g)?.length]).toEqual([2, 1]);
  });
});

describe("drawRepositoryGroup", () => {
  it("is the one box both sections' groups are drawn in", () => {
    // A group box made by hand could miss its section or repository, which its fold is keyed by.
    expect([rosterSource.match(/drawRepositoryGroup\("(merge-queue|recently-merged)"/g)?.length, rosterSource.match(/className = MERGED_GROUP_CLASS/g)?.length]).toEqual([2, 1]);
  });
});
