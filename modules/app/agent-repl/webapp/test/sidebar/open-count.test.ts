// @vitest-environment jsdom
// Every workspace count in the Open section. See src/sidebar/open-count.ts.
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { RosterOpenCountSchema, RosterOpenRowTabSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { drawRosterOpenCount, openCountText, tabUnfolded } from "../../src/sidebar/open-count.js";
import { tabNumber } from "../../src/sidebar/tab-numbers.js";
import { drawWorkspaceRoster } from "../../src/sidebar/roster.js";
import { OPEN_COUNT_CLASS } from "../../src/sidebar/roster.js";
import { CHILD_COUNT_CLASS } from "../../src/sidebar/child-count.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { repoSection, roster, row, sidebarContext } from "./harness.js";

const NUMBERED = create(RosterOpenRowTabSchema, { tab: { case: "numbered", value: { number: 4 } } });
const FOLDED = create(RosterOpenRowTabSchema, { tab: { case: "folded", value: {} } });

describe("openCountText", () => {
  it("spells the unfolded rows over every row", () => {
    expect(openCountText({ workspaces: 7, unfolded: 3 })).toBe("(3/7)");
  });
});

describe("drawRosterOpenCount", () => {
  it("answers the daemon's count", () => {
    expect(drawRosterOpenCount(create(RosterOpenCountSchema, { workspaces: 4, unfolded: 4 }), "c")).toEqual({ workspaces: 4, unfolded: 4 });
  });
  it("refuses more unfolded rows than rows", () => {
    expect(() => drawRosterOpenCount(create(RosterOpenCountSchema, { workspaces: 1, unfolded: 2 }), "c")).toThrow(MalformedView);
  });
});

describe("tabUnfolded", () => {
  it("is true for a numbered tab", () => {
    expect(tabUnfolded(NUMBERED, "t")).toBe(true);
  });
  it("is false for a folded tab", () => {
    expect(tabUnfolded(FOLDED, "t")).toBe(false);
  });
  it("refuses a missing tab", () => {
    expect(() => tabUnfolded(undefined, "t")).toThrow(MalformedView);
  });
  it("refuses a tab with no arm", () => {
    expect(() => tabUnfolded(create(RosterOpenRowTabSchema, {}), "t")).toThrow(MalformedView);
  });
  it("reads the tab through tabNumber, the one reading the tab numbers share", () => {
    expect([NUMBERED, FOLDED].map((tab) => [tabUnfolded(tab, "t"), tabNumber(tab, "t")])).toEqual([
      [true, 4],
      [false, undefined],
    ]);
  });
});

describe("every count in the Open section", () => {
  it("is spelled unfolded over total", () => {
    // Arrange: the Open heading's count, a repository's, a subsection's and a row's child count.
    const kids = Array.from({ length: 6 }, (_, i) => row({ id: `kid-${i}` }));
    const wire = roster({
      repos: [
        repoSection({
          id: "repo-1",
          rows: [row({ id: "p", children: kids })],
          count: 9,
          unfolded: 8,
          subsections: [{ id: "sub-1", rows: [row({ id: "s-1" }), row({ id: "s-2" })], collapsed: true, unfolded: 0 }],
        }),
      ],
    });

    // Act
    const drawn = drawWorkspaceRoster(wire, sidebarContext());

    // Assert
    expect([
      drawn.querySelector(`.${OPEN_COUNT_CLASS}`)?.textContent,
      drawn.querySelector(".repo-section > .repo-head [data-section-count]")?.textContent,
      drawn.querySelector(".sb-subsection .sb-count")?.textContent,
      drawn.querySelector(`.${CHILD_COUNT_CLASS}`)?.textContent,
    ]).toEqual(["(8/9)", "(8/9)", "(0/2)", "(6/6)"]);
  });
});
