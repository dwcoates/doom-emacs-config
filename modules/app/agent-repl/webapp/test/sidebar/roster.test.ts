// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  UpdateSidebarViewResponseSchema,
  type UpdateSidebarViewRequest,
  type UpdateSidebarViewResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import {
  RosterLabelSchema,
  RosterRepoDivergenceSchema,
  RosterSectionHeaderSchema,
  RosterRepoSectionSchema,
  WorkspaceRosterSchema,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { SetWorkspaceHiddenResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_hidden_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { STAND_IN_ATTRIBUTE } from "../../src/sidebar/row.js";
import { createSidebarView } from "../../src/sidebar/view.js";
import type { FailureSink } from "../../src/failure/sink.js";
import { repoFoldKey } from "../../src/sidebar/context.js";
import {
  MERGED_DRAWN_AT_ONCE,
  MERGED_AT_ATTRIBUTE,
  createRosterDrawer,
  disposeWorkspaceRoster,
  drawFoldHeader,
  drawHeadedSection,
  drawRosterRepoDivergence,
  drawRosterSectionFold,
  drawSectionLabel,
  drawWorkspaceRoster,
  MERGED_TODAY_COUNT_CLASS,
  mergedTodayCommitsText,
  OPEN_COUNT_CLASS,
  OPEN_SECTION_CLASS,
  SECTION_LABEL_CLASS,
  todaysMerges,
} from "../../src/sidebar/roster.js";
import { cascadedValue, inRail } from "../stylesheet.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import {
  appContext,
  fakeTicker,
  hiddenSection,
  manualReveal,
  mergedBand,
  mergedGroup,
  mergedSection,
  NOW,
  openSection,
  repoSection,
  roster,
  row,
  sidebarContext,
  startOfToday,
  startOfTomorrow,
} from "./harness.js";
import { recordingFailures } from "../failure-sink.js";

// Imported at collection, not inside a test, so the transform never counts
// against a test's timeout.
import css from "../../src/styles.css?raw";

const VIEW_OK = create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } });
const VIEW_UNKNOWN_REPOSITORY = create(UpdateSidebarViewResponseSchema, {
  result: { case: "error", value: { cause: { case: "unknownRepository", value: {} } } },
});
/** One settled merge, so the merged band draws. */
const MERGED_ROW = row({ id: "ws-9", closed: true, status: { case: "merged", value: {} } });
/** One hidden workspace, so the hidden section draws. */
const HIDDEN_ROW = row({ id: "ws-8" });

async function click(control: Element): Promise<void> {
  (control as HTMLElement).dispatchEvent(new MouseEvent("click", { bubbles: true }));
  await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
}

/** The rail's one pane: the open section, then the hidden and merged bands. */
function pane(drawn: HTMLElement): HTMLElement {
  return drawn.querySelector(".sb-pane") as HTMLElement;
}

describe("drawRosterSectionFold", () => {
  it.each([
    ["expanded", false],
    ["collapsed", true],
  ] as const)("reads the %s arm as folded=%s", (arm, folded) => {
    expect(drawRosterSectionFold({ case: arm, value: {} }, "fold")).toBe(folded);
  });

  it("refuses an unset fold as a malformed view", () => {
    expect(() => drawRosterSectionFold({ case: undefined }, "fold")).toThrow(MalformedView);
  });
});

describe("the sections", () => {
  it("draws one section per repository, in the resolver's order", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "b" }), repoSection({ id: "a" })] }),
      sidebarContext(),
    );
    const keys = [...pane(drawn).querySelectorAll(".repo.repo-section")].map(
      (el) => el.getAttribute("data-section"),
    );
    expect(keys).toEqual([repoFoldKey("b"), repoFoldKey("a")]);
  });

  it("draws the repository's label", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", label: "doom" })] }),
      sidebarContext(),
    );
    expect(pane(drawn).querySelector(".repo-section .sb-label")?.textContent).toBe("doom");
  });

  // A JUST-REGISTERED REPOSITORY HAS NO ROWS. RegisterRepository (SPC j .)
  // mints a repository with no workspace under it, and the section IS the only
  // evidence the registration landed -- so it draws, header and all, rather
  // than being skipped as an empty band the way recently-merged is.
  it("draws a repository section with no rows at all", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-new", label: "just-registered", rows: [] })] }),
      sidebarContext(),
    );
    const section = pane(drawn).querySelector(
      ".repo-section",
    ) as HTMLElement;
    expect(section).not.toBeNull();
    expect(section.hidden).toBe(false);
  });

  it("draws the label of a repository section with no rows", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-new", label: "just-registered", rows: [] })] }),
      sidebarContext(),
    );
    expect(pane(drawn).querySelector(".repo-section .sb-label")?.textContent).toBe(
      "just-registered",
    );
  });

  it("offers each section a fold toggle", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1" })] }),
      sidebarContext(),
    );
    expect(pane(drawn).querySelector(".repo-section [data-section-fold]")).not.toBeNull();
  });

  it.each([
    [false, false],
    [true, true],
  ])("draws a repository section collapsed=%s folded=%s, as the daemon says", (collapsed, folded) => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", collapsed })] }),
      sidebarContext(),
    );
    expect(pane(drawn).querySelector(".open-section .repo")?.classList.contains("folded")).toBe(folded);
  });

  it.each([
    [false, "var(--repo-head-expanded)"],
    [true, "var(--repo-head-collapsed)"],
  ])("colors a repository header collapsed=%s with %s", (collapsed, want) => {
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1", collapsed })] }), sidebarContext());
    inRail(drawn, (rail) => {
      const header = rail.querySelector(".repo-section > .repo-head") as HTMLElement;
      expect(cascadedValue(header, "color")).toBe(want);
    });
  });

  it("sets a repository's fold triangle a step smaller than the band headers'", () => {
    // Arrange
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })] }), sidebarContext());
    // Act
    const sizes = inRail(drawn, (rail) => [
      cascadedValue(rail.querySelector(".repo-section > .repo-head .tri") as HTMLElement, "font-size"),
      cascadedValue(rail.querySelector(".open-section > .repo-head .tri") as HTMLElement, "font-size"),
    ]);
    // Assert
    expect(sizes).toEqual(["0.98rem", "1.05rem"]);
  });

  it("sets a repository's name a step smaller than the band headers, its count scaling with it", () => {
    // Arrange
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1", collapsed: true })] }), sidebarContext());
    // Act
    const sizes = inRail(drawn, (rail) => {
      const header = rail.querySelector(".repo-section > .repo-head") as HTMLElement;
      return [
        cascadedValue(header, "font-size"),
        cascadedValue(header.querySelector(".sb-count") as HTMLElement, "font-size"),
        cascadedValue(rail.querySelector(".open-section > .repo-head") as HTMLElement, "font-size"),
      ];
    });
    // Assert
    expect(sizes).toEqual(["0.7rem", "0.85em", "0.75rem"]);
  });

  it("marks a repository section so its header takes its fold's grey", () => {
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })] }), sidebarContext());
    expect(pane(drawn).querySelector(".open-section .repo")?.classList.contains("repo-section")).toBe(true);
  });

});

/** A daemon that records every UpdateSidebarView and answers ANSWER. */
function viewDaemon(answer: UpdateSidebarViewResponse = VIEW_OK): {
  asked: UpdateSidebarViewRequest[];
  sc: ReturnType<typeof sidebarContext>;
} {
  const asked: UpdateSidebarViewRequest[] = [];
  const sc = sidebarContext(
    appContext({
      updateSidebarView: (request) => {
        asked.push(request);
        return answer;
      },
    }),
  );
  return { asked, sc };
}

/** The section and fold one recorded fold change named. */
function foldAsked(request: UpdateSidebarViewRequest): { section: string | undefined; id: string; fold: string | undefined } {
  if (request.change.case !== "foldSection") return { section: request.change.case, id: "", fold: undefined };
  const section = request.change.value.section;
  const id = section.case === "repository" ? section.value.id : "";
  return { section: section.case, id, fold: request.change.value.fold.case };
}

const FOLDING_SECTIONS = [
  {
    kind: "repository",
    roster: (collapsed: boolean) => roster({ repos: [repoSection({ id: "repo-1", collapsed })] }),
    selector: ".repo-section",
    id: "repo-1",
  },
  {
    kind: "recentlyMerged",
    roster: (collapsed: boolean) => roster({ merged: mergedSection([MERGED_ROW], 1, collapsed) }),
    selector: ".merged-section",
    id: "",
  },
  {
    kind: "hidden",
    roster: (collapsed: boolean) => roster({ hidden: hiddenSection([HIDDEN_ROW], 1, collapsed) }),
    selector: ".hidden-section",
    id: "",
  },
  {
    kind: "open",
    roster: (collapsed: boolean) => roster({ open: openSection(collapsed), repos: [repoSection({ id: "repo-1" })] }),
    selector: ".open-section",
    id: "",
  },
] as const;

describe("folding a section is the daemon's view, shared by every page", () => {
  it.each(FOLDING_SECTIONS.flatMap((s) => [
    { ...s, collapsed: false, want: "collapse" },
    { ...s, collapsed: true, want: "expand" },
  ]))("asks for the other fold of a $kind section (collapsed=$collapsed asks $want)", async ({ roster: make, selector, kind, id, collapsed, want }) => {
    const { asked, sc } = viewDaemon();
    const drawn = drawWorkspaceRoster(make(collapsed), sc);
    await click(drawn.querySelector(`${selector} [data-section-fold]`) as Element);
    expect(asked.map(foldAsked)).toEqual([{ section: kind, id, fold: want }]);
  });

  it.each(FOLDING_SECTIONS)("folds a $kind section on this page at once, before any push", async ({ roster: make, selector }) => {
    const { sc } = viewDaemon();
    const drawn = drawWorkspaceRoster(make(false), sc);
    const section = drawn.querySelector(selector) as HTMLElement;
    await click(section.querySelector("[data-section-fold]") as Element);
    expect(section.classList.contains("folded")).toBe(true);
  });

  it("keeps the asked fold over a push still carrying the old one", async () => {
    const { sc } = viewDaemon();
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })] }), sc);
    await click(drawn.querySelector(".repo-section [data-section-fold]") as Element);
    const redrawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })] }), sc);
    expect(redrawn.querySelector(".repo-section")?.classList.contains("folded")).toBe(true);
  });

  it("follows the wire again once a push has carried the asked fold", async () => {
    const { sc } = viewDaemon();
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })] }), sc);
    await click(drawn.querySelector(".repo-section [data-section-fold]") as Element);
    drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1", collapsed: true })] }), sc);
    const later = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })] }), sc);
    expect(later.querySelector(".repo-section")?.classList.contains("folded")).toBe(false);
  });

  it("draws a fold another page asked for, from the push alone", () => {
    const sc = sidebarContext();
    drawWorkspaceRoster(roster({ merged: mergedSection([MERGED_ROW], 1, true) }), sc);
    const pushed = drawWorkspaceRoster(roster({ merged: mergedSection([MERGED_ROW], 1, false) }), sc);
    expect(pane(pushed).querySelector(".merged-section")?.classList.contains("folded")).toBe(false);
  });

  it("draws the daemon's refusal of a fold beside the toggle", async () => {
    const { sc } = viewDaemon(VIEW_UNKNOWN_REPOSITORY);
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })] }), sc);
    await click(drawn.querySelector(".repo-section [data-section-fold]") as Element);
    expect(pane(drawn).textContent).toContain("the daemon no longer has that repository");
  });

  it("puts a refused fold back as the wire drew it", async () => {
    const { sc } = viewDaemon(VIEW_UNKNOWN_REPOSITORY);
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })] }), sc);
    const section = drawn.querySelector(".repo-section") as HTMLElement;
    await click(section.querySelector("[data-section-fold]") as Element);
    expect(section.classList.contains("folded")).toBe(false);
  });

  it("words an unknown repository from the view verb's own table", async () => {
    const { sc } = viewDaemon(
      create(UpdateSidebarViewResponseSchema, {
        result: { case: "error", value: { cause: { case: "unknownRepository", value: {} } } },
      }),
    );
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })] }), sc);
    await click(drawn.querySelector(".repo-section [data-section-fold]") as Element);
    expect(pane(drawn).textContent).toContain("the daemon no longer has that repository");
  });

});

describe("closed workspaces never appear in a live grouping", () => {
  it("drops a closed row from a repository section", () => {
    const drawn = drawWorkspaceRoster(
      roster({
        repos: [
          repoSection({
            id: "repo-1",
            rows: [row({ id: "ws-live" }), row({ id: "ws-closed", closed: true })],
          }),
        ],
      }),
      sidebarContext(),
    );
    expect(pane(drawn).querySelector("[data-roster-row='ws-closed']")).toBeNull();
    expect(pane(drawn).querySelector("[data-roster-row='ws-live']")).not.toBeNull();
  });

  it("drops a killed row, whose lifecycle arm is inactive", () => {
    const drawn = drawWorkspaceRoster(
      roster({
        repos: [
          repoSection({
            id: "repo-1",
            rows: [row({ id: "ws-killed", closed: true, status: { case: "inactive", value: {} } })],
          }),
        ],
      }),
      sidebarContext(),
    );
    expect(pane(drawn).querySelector("[data-roster-row='ws-killed']")).toBeNull();
  });

  it("draws no '?' glyph once the closed rows are gone", () => {
    const drawn = drawWorkspaceRoster(
      roster({
        repos: [
          repoSection({
            id: "repo-1",
            rows: [row({ id: "ws-killed", closed: true, status: { case: "inactive", value: {} } })],
          }),
        ],
      }),
      sidebarContext(),
    );
    const glyphs = [...pane(drawn).querySelectorAll(".st")].map((el) => el.textContent);
    expect(glyphs).not.toContain("?");
  });

  it("hoists a killed parent's live child up in its place", () => {
    const drawn = drawWorkspaceRoster(
      roster({
        repos: [
          repoSection({
            id: "repo-1",
            rows: [row({ id: "ws-killed", closed: true, children: [row({ id: "ws-child" })] })],
          }),
        ],
      }),
      sidebarContext(),
    );
    expect(pane(drawn).querySelector("[data-roster-row='ws-killed']")).toBeNull();
    expect(pane(drawn).querySelector("[data-roster-row='ws-child']")).not.toBeNull();
  });
});

describe("drawHeadedSection", () => {
  /** A headed section's wire: a header, rows and a fold. */
  function wire(init: { collapsed?: boolean; rows?: ReturnType<typeof row>[] } = {}) {
    return repoSection({ id: "repo-1", label: "Head", rows: init.rows, collapsed: init.collapsed });
  }
  const SHAPE = { foldKey: "k:1", className: "kind-section", target: { case: "recentlyMerged", value: {} } } as const;

  it("draws the box with its kind's class and fold key", () => {
    const section = drawHeadedSection(wire(), sidebarContext(), "p", SHAPE);
    expect([section.classList.contains("repo"), section.classList.contains("kind-section"), section.getAttribute("data-section")]).toEqual([
      true,
      true,
      "k:1",
    ]);
  });

  it.each([
    [false, false],
    [true, true],
  ])("folds the box collapsed=%s folded=%s, as the wire says", (collapsed, folded) => {
    const section = drawHeadedSection(wire({ collapsed }), sidebarContext(), "p", SHAPE);
    expect(section.classList.contains("folded")).toBe(folded);
  });

  it("draws the header's label and folded count", () => {
    const section = drawHeadedSection(wire({ rows: [row({ id: "ws-1" })] }), sidebarContext(), "p", SHAPE);
    expect([section.querySelector(".sb-label")?.textContent, section.querySelector(".sb-count")?.textContent]).toEqual(["Head", "(1)"]);
  });

  it("drops a closed row from its rows", () => {
    const section = drawHeadedSection(wire({ rows: [row({ id: "ws-closed", closed: true })] }), sidebarContext(), "p", SHAPE);
    expect(section.querySelector("[data-roster-row='ws-closed']")).toBeNull();
  });

  it("hands the header and the box to the decoration, after the count", () => {
    let handed: [HTMLElement, HTMLElement] | undefined;
    const section = drawHeadedSection(wire(), sidebarContext(), "p", {
      ...SHAPE,
      decorateHeader: (header, box) => {
        handed = [header, box];
        header.appendChild(document.createElement("i"));
      },
    });
    expect([handed?.[0] === section.querySelector(".repo-head"), handed?.[1] === section, section.querySelector(".repo-head")?.lastElementChild?.tagName]).toEqual([
      true,
      true,
      "I",
    ]);
  });

  it("asks the daemon for the other fold of its target", async () => {
    const { asked, sc } = viewDaemon();
    const section = drawHeadedSection(wire(), sc, "p", SHAPE);
    await click(section.querySelector("[data-section-fold]") as Element);
    expect(asked.map(foldAsked)).toEqual([{ section: "recentlyMerged", id: "", fold: "collapse" }]);
  });

  it("refuses a section with no header as a malformed view", () => {
    const bare = wire();
    bare.header = undefined;
    expect(() => drawHeadedSection(bare, sidebarContext(), "p", SHAPE)).toThrow(MalformedView);
  });

  it("refuses a section with no rows as a malformed view", () => {
    const bare = wire();
    bare.rows = undefined;
    expect(() => drawHeadedSection(bare, sidebarContext(), "p", SHAPE)).toThrow(MalformedView);
  });

  it("keeps a closed row when told not to drop closed rows", () => {
    const section = drawHeadedSection(wire({ rows: [row({ id: "ws-closed", closed: true })] }), sidebarContext(), "p", {
      ...SHAPE,
      hideClosed: false,
    });
    expect(section.querySelector("[data-roster-row='ws-closed']")).not.toBeNull();
  });

  it("draws its rows in the place it is given", async () => {
    const shown: string[] = [];
    const sc = sidebarContext(
      appContext({
        setWorkspaceHidden: (request) => {
          shown.push(request.workspace?.id ?? "");
          return create(SetWorkspaceHiddenResponseSchema, { result: { case: "success", value: {} } });
        },
      }),
    );
    const section = drawHeadedSection(wire({ rows: [row({ id: "ws-1" })] }), sc, "p", { ...SHAPE, place: "hidden" });
    await click(section.querySelector("[data-select]") as Element);
    expect(shown).toEqual(["ws-1"]);
  });

  it("leaves the box out, marked, when its left-out test says so", () => {
    const section = drawHeadedSection(wire(), sidebarContext(), "p", {
      ...SHAPE,
      leftOut: { when: () => true, attribute: "data-test-empty" },
    });
    expect([section.hidden, section.getAttribute("data-test-empty"), section.querySelector(".repo-head")]).toEqual([
      true,
      "true",
      null,
    ]);
  });

  it("draws the box whole when its left-out test says no", () => {
    const section = drawHeadedSection(wire(), sidebarContext(), "p", {
      ...SHAPE,
      leftOut: { when: () => false, attribute: "data-test-empty" },
    });
    expect([section.hidden, section.querySelector(".repo-head") !== null]).toEqual([false, true]);
  });

  it("still refuses a malformed header in a left-out box", () => {
    const bare = wire();
    bare.header = create(RosterSectionHeaderSchema, { label: { text: "Head" } });
    expect(() =>
      drawHeadedSection(bare, sidebarContext(), "p", { ...SHAPE, leftOut: { when: () => true, attribute: "data-test-empty" } }),
    ).toThrow(MalformedView);
  });

  // EVERY FOLDING SECTION IS ONE SHAPE: each draws its box through
  // drawHeadedSection, so each carries the same header and fold control. A
  // hand-rolled section that drew its own header would carry neither here.
  it.each([
    ["repository", ".repo-section", roster({ repos: [repoSection({ id: "repo-1" })] })],
    ["hidden", ".hidden-section", roster({ hidden: hiddenSection([HIDDEN_ROW]) })],
    ["merged", ".merged-section", roster({ merged: mergedSection([MERGED_ROW]) })],
  ] as const)("draws the %s section in the one headed shape", (_kind, selector, made) => {
    const drawn = drawWorkspaceRoster(made, sidebarContext());
    const section = drawn.querySelector(selector) as HTMLElement;
    expect([
      section.querySelector(":scope > .repo-head > [data-section-fold]") !== null,
      section.querySelector(":scope > .repo-head > [data-section-count]") !== null,
      section.querySelector(":scope > .rows") !== null,
    ]).toEqual([true, true, true]);
  });

});

describe("recently merged", () => {
  /** A roster carrying one landed merge -- the band only draws with rows. */
  function landed(): ReturnType<typeof roster> {
    return roster({
      merged: mergedSection([row({ id: "ws-9", status: { case: "merged", value: {} } })]),
    });
  }

  it("keeps a merged row, which is closed on the wire, in the band", () => {
    const drawn = drawWorkspaceRoster(
      roster({
        merged: mergedSection([
          row({ id: "ws-9", closed: true, status: { case: "merged", value: {} } }),
        ]),
      }),
      sidebarContext(),
    );
    expect(
      pane(drawn).querySelector(".merged-section [data-roster-row='ws-9']"),
    ).not.toBeNull();
  });

  it("is drawn once, in the rail's one pane", () => {
    const drawn = drawWorkspaceRoster(landed(), sidebarContext());
    expect(drawn.querySelectorAll(".merged-section:not([hidden])").length).toBe(1);
  });

  it("starts folded, because settled history should not spend rail height", () => {
    const drawn = drawWorkspaceRoster(landed(), sidebarContext());
    expect(
      pane(drawn).querySelector(".merged-section")?.classList.contains("folded"),
    ).toBe(true);
  });

  it("points its folded triangle at the rows it is hiding", () => {
    const drawn = drawWorkspaceRoster(landed(), sidebarContext());
    expect(
      pane(drawn).querySelector(".merged-section [data-section-fold]")?.textContent,
    ).toBe("\u25b8");
  });

  it("lets the glyph state the fold, with no second turn from the stylesheet", () => {
    // The glyph is written by `paintTriangle`; a CSS rotation on top of it
    // turned the folded \u25b8 a further quarter turn and drew \u25b2, which
    // reads as "collapse me" on a section already collapsed.
    inRail(drawWorkspaceRoster(landed(), sidebarContext()), (rail) => {
      const triangle = rail.querySelector(
        ".merged-section.folded [data-section-fold]",
      ) as HTMLElement;
      expect(cascadedValue(triangle, "transform")).toBe("none");
    });
  });

  it("draws unfolded when the daemon says every page has it unfolded", () => {
    const drawn = drawWorkspaceRoster(roster({ merged: mergedSection([MERGED_ROW], 1, false) }), sidebarContext());
    expect(pane(drawn).querySelector(".merged-section")?.classList.contains("folded")).toBe(false);
  });

  it("draws its rows", () => {
    const drawn = drawWorkspaceRoster(landed(), sidebarContext());
    expect(
      pane(drawn).querySelector(".merged-section [data-roster-row='ws-9']"),
    ).not.toBeNull();
  });

  it("draws its header label", () => {
    const drawn = drawWorkspaceRoster(landed(), sidebarContext());
    expect(
      pane(drawn).querySelector(".merged-section .sb-label")?.textContent,
    ).toBe("Recently Merged");
  });

  it("draws no heading when no merge has landed", () => {
    const drawn = drawWorkspaceRoster(roster(), sidebarContext());
    expect(pane(drawn).querySelector(".merged-section .sb-label")).toBeNull();
  });

  it("keeps the empty band out of the rail entirely", () => {
    const drawn = drawWorkspaceRoster(roster(), sidebarContext());
    expect(
      (pane(drawn).querySelector(".merged-section") as HTMLElement).hidden,
    ).toBe(true);
  });
});

describe("hidden workspaces", () => {
  it("draws the hidden row in the rail's one hidden section", () => {
    const drawn = drawWorkspaceRoster(roster({ hidden: hiddenSection([HIDDEN_ROW]) }), sidebarContext());
    expect(drawn.querySelectorAll(".hidden-section [data-roster-row='ws-8']").length).toBe(1);
  });

  it("draws the section above the recently-merged band", () => {
    const drawn = drawWorkspaceRoster(
      roster({ hidden: hiddenSection([HIDDEN_ROW]), merged: mergedSection([MERGED_ROW]) }),
      sidebarContext(),
    );
    const bands = [...pane(drawn).querySelectorAll(".hidden-section, .merged-section")];
    expect(bands.map((band) => band.classList.contains("hidden-section"))).toEqual([true, false]);
  });

  it("draws the daemon's heading and count", () => {
    const drawn = drawWorkspaceRoster(roster({ hidden: hiddenSection([HIDDEN_ROW], 1) }), sidebarContext());
    const section = pane(drawn).querySelector(".hidden-section") as HTMLElement;
    expect([section.querySelector(".sb-label")?.textContent, section.querySelector(".sb-count")?.textContent]).toEqual([
      "Hidden",
      "(1)",
    ]);
  });

  it("keeps the section out of the rail when nothing is hidden", () => {
    const drawn = drawWorkspaceRoster(roster(), sidebarContext());
    const section = pane(drawn).querySelector(".hidden-section") as HTMLElement;
    expect([section.hidden, section.querySelector(".sb-label")]).toEqual([true, null]);
  });

  it("keeps the section out of the rail when its only row is closed", () => {
    const drawn = drawWorkspaceRoster(
      roster({ hidden: hiddenSection([row({ id: "ws-gone", closed: true })], 0) }),
      sidebarContext(),
    );
    expect((pane(drawn).querySelector(".hidden-section") as HTMLElement).hidden).toBe(true);
  });

  it("drops a closed row and keeps the open one", () => {
    const drawn = drawWorkspaceRoster(
      roster({ hidden: hiddenSection([row({ id: "ws-gone", closed: true }), HIDDEN_ROW], 1) }),
      sidebarContext(),
    );
    const section = pane(drawn).querySelector(".hidden-section") as HTMLElement;
    expect([...section.querySelectorAll("[data-roster-row]")].map((r) => r.getAttribute("data-roster-row"))).toEqual([
      "ws-8",
    ]);
  });

  it("refuses a roster with no hidden section as a malformed view", () => {
    const bare = create(WorkspaceRosterSchema, {
      repository: { sections: [] },
      recentlyMerged: mergedSection(),
    });
    expect(() => drawWorkspaceRoster(bare, sidebarContext())).toThrow(MalformedView);
  });

  it("refuses an empty hidden section with no fold as a malformed view", () => {
    const section = hiddenSection();
    section.fold = { case: undefined };
    expect(() => drawWorkspaceRoster(roster({ hidden: section }), sidebarContext())).toThrow(MalformedView);
  });
});

describe("the create controls", () => {
  it("offers a new workspace per repository section", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1" })] }),
      sidebarContext(),
    );
    expect(pane(drawn).querySelector(".repo-section > .repo-head .sb-add")).not.toBeNull();
  });

  it("opens the create form under that section's header", async () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1" })] }),
      sidebarContext(),
    );
    await click(pane(drawn).querySelector(".repo-section > .repo-head .sb-add") as Element);
    expect(
      pane(drawn).querySelector(".repo-section > .repo-head")?.nextElementSibling?.hasAttribute(
        "data-create-form",
      ),
    ).toBe(true);
  });

  it("does not fold the section when the create control is clicked", async () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1" })] }),
      sidebarContext(),
    );
    const section = pane(drawn).querySelector(".repo-section") as HTMLElement;
    await click(section.querySelector(".sb-add") as Element);
    expect(section.classList.contains("folded")).toBe(false);
  });

});

describe("a malformed roster", () => {
  it("refuses one with no repository view", () => {
    const malformed = create(WorkspaceRosterSchema, {
      recentlyMerged: mergedSection(),
    });
    expect(() => drawWorkspaceRoster(malformed, sidebarContext())).toThrow(MalformedView);
  });

  it("refuses one with no recently-merged section", () => {
    const malformed = create(WorkspaceRosterSchema, {
      repository: { sections: [] },
    });
    expect(() => drawWorkspaceRoster(malformed, sidebarContext())).toThrow(MalformedView);
  });

  it("refuses a current workspace with no identity", () => {
    const malformed = create(WorkspaceRosterSchema, {
      repository: { sections: [] },
      recentlyMerged: mergedSection(),
      current: {},
    });
    expect(() => drawWorkspaceRoster(malformed, sidebarContext())).toThrow(MalformedView);
  });

  it("refuses a repository section with no key", () => {
    const malformed = create(RosterRepoSectionSchema, {
      header: { label: { text: "doom" } },
      rows: { rows: [] },
      fold: { case: "expanded", value: {} },
    });
    expect(() =>
      drawWorkspaceRoster(roster({ repos: [malformed] }), sidebarContext()),
    ).toThrow(MalformedView);
  });

  it("refuses a repository section with no fold arm", () => {
    const malformed = create(RosterRepoSectionSchema, {
      key: { repository: { id: "repo-1", dir: "/repo" } },
      header: { label: { text: "doom" } },
      rows: { rows: [] },
    });
    expect(() =>
      drawWorkspaceRoster(roster({ repos: [malformed] }), sidebarContext()),
    ).toThrow(MalformedView);
  });

  it("refuses a repository section with no header", () => {
    const malformed = create(RosterRepoSectionSchema, {
      key: { repository: { id: "repo-1", dir: "/repo" } },
      rows: { rows: [] },
      fold: { case: "expanded", value: {} },
    });
    expect(() =>
      drawWorkspaceRoster(roster({ repos: [malformed] }), sidebarContext()),
    ).toThrow(MalformedView);
  });

  it("refuses a repository section with no rows box", () => {
    const malformed = create(RosterRepoSectionSchema, {
      key: { repository: { id: "repo-1", dir: "/repo" } },
      header: { label: { text: "doom" } },
      fold: { case: "expanded", value: {} },
    });
    expect(() =>
      drawWorkspaceRoster(roster({ repos: [malformed] }), sidebarContext()),
    ).toThrow(MalformedView);
  });

  it("refuses a section header with no label", () => {
    const malformed = create(RosterRepoSectionSchema, {
      key: { repository: { id: "repo-1", dir: "/repo" } },
      header: {},
      rows: { rows: [] },
      fold: { case: "expanded", value: {} },
    });
    expect(() =>
      drawWorkspaceRoster(roster({ repos: [malformed] }), sidebarContext()),
    ).toThrow(MalformedView);
  });
});

describe("a recently-merged section with no rows box", () => {
  it("is a malformed view, never an empty band", () => {
    const malformed = create(WorkspaceRosterSchema, {
      repository: { sections: [] },
      recentlyMerged: { header: { label: { text: "Recently Merged" } } },
    });
    expect(() => drawWorkspaceRoster(malformed, sidebarContext())).toThrow(MalformedView);
  });
});

describe("the bands' header color", () => {
  it.each([
    ["hidden", ".hidden-section", roster({ hidden: hiddenSection([HIDDEN_ROW]) })],
    ["merged", ".merged-section", roster({ merged: mergedSection([MERGED_ROW]) })],
  ] as const)("draws the %s band's header in the merged band's purple", (_kind, selector, made) => {
    // Arrange
    const drawn = drawWorkspaceRoster(made, sidebarContext());
    const head = pane(drawn).querySelector(`${selector} > .repo-head`) as HTMLElement;

    // Act
    const color = inRail(drawn, () => cascadedValue(head, "color"));

    // Assert
    expect(color).toBe("var(--blocked)");
  });

  it("leaves a hidden row's name out of the header's purple", () => {
    // Arrange
    const drawn = drawWorkspaceRoster(roster({ hidden: hiddenSection([HIDDEN_ROW]) }), sidebarContext());
    const name = pane(drawn).querySelector(".hidden-section [data-roster-row='ws-8'] .name") as HTMLElement;

    // Act
    const color = inRail(drawn, () => cascadedValue(name, "color"));

    // Assert
    expect(color).not.toBe("var(--blocked)");
  });
});

describe("a section's folded count", () => {
  const countOf = (drawn: HTMLElement, selector: string): HTMLElement =>
    pane(drawn).querySelector(`${selector} .sb-count`) as HTMLElement;

  it("draws the daemon's count as (N) in a repository header", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", count: 7, collapsed: true })] }),
      sidebarContext(),
    );
    expect(countOf(drawn, ".repo-section").textContent).toBe("(7)");
  });

  it("draws the count the daemon resolved, not the rows it can see", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", rows: [], count: 4, collapsed: true })] }),
      sidebarContext(),
    );
    expect(countOf(drawn, ".repo-section").textContent).toBe("(4)");
  });

  it("places the count between the label and the add control", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", count: 2, collapsed: true })] }),
      sidebarContext(),
    );
    const head = pane(drawn).querySelector(".repo-section > .repo-head") as HTMLElement;
    const parts = [...head.children].map((el) =>
      el.classList.contains("sb-label") ? "label" : el.classList.contains("sb-count") ? "count" : el.classList.contains("sb-add") ? "add" : "other",
    );
    expect(parts.slice(parts.indexOf("label"), parts.indexOf("label") + 3)).toEqual(["label", "count", "add"]);
  });

  it("shows the count while a repository section is folded", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", count: 2, collapsed: true })] }),
      sidebarContext(),
    );
    expect(inRail(drawn, () => cascadedValue(countOf(drawn, ".repo-section"), "display"))).toBe("inline");
  });

  it("shows the count while a repository section is unfolded", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", count: 2, collapsed: false })] }),
      sidebarContext(),
    );
    expect(inRail(drawn, () => cascadedValue(countOf(drawn, ".repo-section"), "display"))).toBe("inline");
  });

  it("shows the count while the hidden workspaces section is unfolded", () => {
    const drawn = drawWorkspaceRoster(roster({ hidden: hiddenSection([HIDDEN_ROW], 1, false) }), sidebarContext());
    expect(inRail(drawn, () => cascadedValue(countOf(drawn, ".hidden-section"), "display"))).toBe("inline");
  });

  it("shows the count while the hidden workspaces section is folded", () => {
    const drawn = drawWorkspaceRoster(roster({ hidden: hiddenSection([HIDDEN_ROW], 1, true) }), sidebarContext());
    expect(inRail(drawn, () => cascadedValue(countOf(drawn, ".hidden-section"), "display"))).toBe("inline");
  });

  it.each([
    { name: "size", property: "font-size" },
    { name: "color", property: "color" },
    { name: "border", property: "border-top-style" },
    { name: "padding", property: "padding-left" },
  ])("draws the hidden workspaces section's count in Recently Merged's today count's $name", ({ property }) => {
    const drawn = drawWorkspaceRoster(
      roster({
        hidden: hiddenSection([HIDDEN_ROW], 1, false),
        merged: mergedSection([row({ id: "ws-9", status: { case: "merged", value: {} } })], 1, false),
      }),
      sidebarContext(),
    );
    const today = pane(drawn).querySelector(".merged-section .sb-today-count") as HTMLElement;
    const [hidden, merged] = inRail(drawn, () => [
      cascadedValue(countOf(drawn, ".hidden-section"), property),
      cascadedValue(today, property),
    ]);
    expect(hidden).toBe(merged);
  });

  it("draws the hidden workspaces section's count as every row it holds", () => {
    const rows = [row({ id: "ws-h1" }), row({ id: "ws-h2" }), row({ id: "ws-h3" })];
    const drawn = drawWorkspaceRoster(roster({ hidden: hiddenSection(rows, 3, false) }), sidebarContext());
    expect([countOf(drawn, ".hidden-section").textContent, pane(drawn).querySelectorAll(".hidden-section .row").length]).toEqual(["(3)", 3]);
  });

  it("draws Recently Merged's count beside its label", () => {
    const drawn = drawWorkspaceRoster(
      roster({ merged: mergedSection([row({ id: "ws-9", status: { case: "merged", value: {} } })], 3) }),
      sidebarContext(),
    );
    const head = pane(drawn).querySelector(".merged-section > .repo-head") as HTMLElement;
    expect([head.querySelector(".sb-label")?.textContent, head.querySelector(".sb-count")?.textContent]).toEqual([
      "Recently Merged",
      "(3)",
    ]);
  });

  it("shows Recently Merged's count while it is folded, which is its default", () => {
    const drawn = drawWorkspaceRoster(
      roster({ merged: mergedSection([row({ id: "ws-9", status: { case: "merged", value: {} } })], 3) }),
      sidebarContext(),
    );
    expect(inRail(drawn, () => cascadedValue(countOf(drawn, ".merged-section"), "display"))).toBe("inline");
  });

  it("draws the count a step smaller than the label", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", count: 2, collapsed: true })] }),
      sidebarContext(),
    );
    expect(inRail(drawn, () => cascadedValue(countOf(drawn, ".repo-section"), "font-size"))).toBe("0.85em");
  });

  it.each([
    { name: "a folded repository section's", selector: ".repo-section .sb-count", r: () => roster({ repos: [repoSection({ id: "repo-1", count: 2, collapsed: true })] }) },
    { name: "an unfolded repository section's", selector: ".repo-section .sb-count", r: () => roster({ repos: [repoSection({ id: "repo-1", count: 2, collapsed: false })] }) },
    { name: "Hidden Workspaces'", selector: ".hidden-section .sb-count", r: () => roster({ hidden: hiddenSection([HIDDEN_ROW], 1, false) }) },
    { name: "a folded Recently Merged's", selector: ".merged-section .sb-count", r: () => roster({ merged: mergedSection([MERGED_ROW], 1, true) }) },
    { name: "an unfolded Recently Merged's today", selector: ".merged-section .sb-today-count", r: () => roster({ merged: mergedSection([MERGED_ROW], 1, false) }) },
  ])("draws $name count in grey a step smaller than its label", ({ selector, r }) => {
    const drawn = drawWorkspaceRoster(r(), sidebarContext());
    const got = inRail(drawn, () => {
      const count = pane(drawn).querySelector(selector) as HTMLElement;
      return [cascadedValue(count, "color"), cascadedValue(count, "font-size")];
    });
    expect(got).toEqual(["var(--muted)", "0.85em"]);
  });

  it("draws no section count inside a badge", () => {
    expect(css).not.toMatch(/\.sb-count[^{]*\{[^}]*(border|padding)/);
  });

  it("refuses a header whose count is unset", () => {
    const section = repoSection({ id: "repo-1" });
    section.header!.count = undefined;
    expect(() => drawWorkspaceRoster(roster({ repos: [section] }), sidebarContext())).toThrow(MalformedView);
  });

});

describe("the open Recently Merged band's today count", () => {
  const mergedAt = (id: string, atMs: number) =>
    row({ id, closed: true, status: { case: "merged", value: {} }, when: { case: "merged", value: { atMs: BigInt(atMs) } } });
  const drawBand = (rows = [mergedAt("ws-1", NOW)], collapsed = false, sc = sidebarContext()): HTMLElement =>
    drawWorkspaceRoster(roster({ merged: mergedSection(rows, rows.length, collapsed) }), sc);
  const todayCount = (drawn: HTMLElement): HTMLElement =>
    pane(drawn).querySelector(`.merged-section > .repo-head .${MERGED_TODAY_COUNT_CLASS}`) as HTMLElement;

  it.each([
    { name: "every merge that landed today", rows: () => [mergedAt("ws-1", NOW), mergedAt("ws-2", startOfToday())], want: "(2 today)" },
    { name: "none of yesterday's merges", rows: () => [mergedAt("ws-1", NOW), mergedAt("ws-2", startOfToday() - 1)], want: "(1 today)" },
    { name: "zero when nothing merged today", rows: () => [mergedAt("ws-1", startOfToday() - 1)], want: "(0 today)" },
  ])("counts $name", ({ rows, want }) => {
    // Act
    const drawn = drawBand(rows());
    // Assert
    expect(todayCount(drawn).textContent).toBe(want);
  });

  it("counts the merges the fit hides, not only the ones it shows", () => {
    // Arrange
    const drawn = drawBand([mergedAt("ws-1", NOW), mergedAt("ws-2", NOW), mergedAt("ws-3", NOW)]);
    const rows = [...pane(drawn).querySelectorAll<HTMLElement>(".merged-section .merged-group > .rows > *")];
    // Act
    rows.slice(1).forEach((r) => (r.hidden = true));
    // Assert
    expect(todayCount(drawn).textContent).toBe("(3 today)");
  });

  it("sits right after the folded count in the band's header", () => {
    expect(todayCount(drawBand()).previousElementSibling?.hasAttribute("data-section-count")).toBe(true);
  });

  it("drops a merge once the shared ticker crosses midnight", () => {
    // Arrange
    const ticker = fakeTicker();
    const drawn = drawBand([mergedAt("ws-1", NOW)], false, sidebarContext(appContext({}, ticker)));
    // Act
    ticker.tick(startOfTomorrow());
    // Assert
    expect(todayCount(drawn).textContent).toBe("(0 today)");
  });

  it("shows while the band is unfolded", () => {
    const drawn = drawBand([mergedAt("ws-1", NOW)], false);
    expect(inRail(drawn, () => cascadedValue(todayCount(drawn), "display"))).not.toBe("none");
  });

  it("hides while the band is folded, where the folded count shows", () => {
    const drawn = drawBand([mergedAt("ws-1", NOW)], true);
    expect(inRail(drawn, () => cascadedValue(todayCount(drawn), "display"))).toBe("none");
  });

  it("draws grey, the size of the folded count", () => {
    const drawn = drawBand();
    expect(inRail(drawn, () => [cascadedValue(todayCount(drawn), "color"), cascadedValue(todayCount(drawn), "font-size")])).toEqual([
      "var(--muted)",
      "0.85em",
    ]);
  });

  it("is drawn on no other section's header", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" })] })], hidden: hiddenSection([HIDDEN_ROW]) }),
      sidebarContext(),
    );
    expect(drawn.querySelector(`.${MERGED_TODAY_COUNT_CLASS}`)).toBeNull();
  });

  it("refuses a merged row whose when-column is unset", () => {
    const bad = mergedAt("ws-1", NOW);
    bad.when = undefined;
    expect(() => drawBand([bad])).toThrow(MalformedView);
  });

  it("refuses a merged row whose when-column is on another arm than merged", () => {
    const bad = row({ id: "ws-1", closed: true, when: { case: "turnRunning", value: { startedAtMs: BigInt(NOW) } } });
    expect(() => drawBand([bad])).toThrow(MalformedView);
  });

  it("leaves a collapsed repository's merges out of the count", () => {
    // Arrange — beta's merge landed today, but beta is collapsed.
    const drawn = drawWorkspaceRoster(
      roster({
        merged: mergedBand(
          [
            mergedGroup({ id: "alpha", rows: [mergedAt("ws-1", NOW)] }),
            mergedGroup({ id: "beta", rows: [mergedAt("ws-2", NOW)], repositoryCollapsed: true }),
          ],
          1,
          false,
        ),
      }),
      sidebarContext(),
    );
    // Assert
    expect(todayCount(drawn).textContent).toBe("(1 today)");
  });
});

describe("mergedTodayCommitsText", () => {
  it.each([
    { name: "no merge as zero commits", today: [], want: "0 commits merged today" },
    { name: "one commit in the singular", today: [{ atMs: NOW, commits: 1 }], want: "1 commit merged today" },
    { name: "every merge's commits summed", today: [{ atMs: NOW, commits: 3 }, { atMs: NOW, commits: 4 }], want: "7 commits merged today" },
    { name: "a zero count as a real count", today: [{ atMs: NOW, commits: 0 }], want: "0 commits merged today" },
    {
      name: "one uncounted merge apart from the sum",
      today: [{ atMs: NOW, commits: 2 }, { atMs: NOW, commits: undefined }],
      want: "2 commits merged today (1 merge not counted)",
    },
    {
      name: "several uncounted merges in the plural",
      today: [{ atMs: NOW, commits: undefined }, { atMs: NOW, commits: undefined }],
      want: "0 commits merged today (2 merges not counted)",
    },
  ])("words $name", ({ today, want }) => {
    expect(mergedTodayCommitsText(today)).toBe(want);
  });
});

describe("todaysMerges", () => {
  it.each([
    { name: "keeps a merge that landed at the start of today", atMs: () => startOfToday(), kept: 1 },
    { name: "drops a merge that landed yesterday", atMs: () => startOfToday() - 1, kept: 0 },
  ])("$name", ({ atMs, kept }) => {
    expect(todaysMerges([{ atMs: atMs(), commits: 1 }], NOW)).toHaveLength(kept);
  });
});

describe("the Recently Merged band header's commit hover", () => {
  const merged = (id: string, atMs: number, commits?: number) =>
    row({
      id,
      closed: true,
      status: { case: "merged", value: {} },
      when: { case: "merged", value: { atMs: BigInt(atMs) } },
      detail: commits === undefined ? {} : { commits: { count: commits } },
    });
  const header = (drawn: HTMLElement): HTMLElement =>
    pane(drawn).querySelector(".merged-section > .repo-head") as HTMLElement;
  const todayCount = (drawn: HTMLElement): string | null | undefined =>
    header(drawn).querySelector(`.${MERGED_TODAY_COUNT_CLASS}`)?.textContent;
  const drawBand = (rows: ReturnType<typeof row>[], sc = sidebarContext()): HTMLElement =>
    drawWorkspaceRoster(roster({ merged: mergedSection(rows, rows.length, false) }), sc);

  it("sums the commits of only the merges that landed today", () => {
    // Act
    const drawn = drawBand([merged("ws-1", NOW, 3), merged("ws-2", startOfToday(), 5), merged("ws-3", startOfToday() - 1, 100)]);
    // Assert
    expect(header(drawn).title).toBe("8 commits merged today");
  });

  it("names a merge the daemon sent no count for", () => {
    const drawn = drawBand([merged("ws-1", NOW, 3), merged("ws-2", NOW)]);
    expect(header(drawn).title).toBe("3 commits merged today (1 merge not counted)");
  });

  it("leaves out a collapsed repository's merges, as the today count does", () => {
    // Arrange
    const band = mergedBand(
      [
        mergedGroup({ id: "repo-1", rows: [merged("ws-1", NOW, 2)] }),
        mergedGroup({ id: "repo-2", rows: [merged("ws-2", NOW, 40)], repositoryCollapsed: true }),
      ],
      1,
      false,
    );
    // Act
    const drawn = drawWorkspaceRoster(roster({ merged: band }), sidebarContext());
    // Assert
    expect([header(drawn).title, todayCount(drawn)]).toEqual(["2 commits merged today", "(1 today)"]);
  });

  it("drops a merge at midnight on the same tick as the today count", () => {
    // Arrange
    const ticker = fakeTicker();
    const drawn = drawBand([merged("ws-1", NOW, 3)], sidebarContext(appContext({}, ticker)));
    // Act
    ticker.tick(startOfTomorrow());
    // Assert
    expect([header(drawn).title, todayCount(drawn)]).toEqual(["0 commits merged today", "(0 today)"]);
  });
});

describe("drawSectionLabel", () => {
  it("draws the label's text in the section label span", () => {
    const label = drawSectionLabel(create(RosterLabelSchema, { text: "alpha" }), "p");
    expect([label.tagName, label.className, label.textContent]).toEqual(["SPAN", SECTION_LABEL_CLASS, "alpha"]);
  });

  it("is the one label every section header and merged group header draws", () => {
    // Arrange — a repository section, the merged band, and its group.
    const drawn = drawWorkspaceRoster(
      roster({
        repos: [repoSection({ id: "repo-1", label: "alpha" })],
        merged: mergedBand([mergedGroup({ id: "repo-1", label: "alpha", rows: [row({ id: "ws-m", closed: true })] })], 1, false),
      }),
      sidebarContext(),
    );
    const repository = pane(drawn);
    const heads = [
      ...repository.querySelectorAll(".repo-section > .repo-head, .merged-section > .repo-head, .merged-group-head"),
    ];
    // Assert — each header holds exactly one label span, and nothing else draws its text.
    expect(heads.map((h) => h.querySelectorAll(`:scope > .${SECTION_LABEL_CLASS}`).length)).toEqual([1, 1, 1]);
  });
});

describe("recently merged repository groups", () => {
  const merge = (id: string) => row({ id, closed: true, status: { case: "merged", value: {} } });
  const groupsIn = (drawn: HTMLElement): HTMLElement[] => [
    ...pane(drawn).querySelectorAll<HTMLElement>(".merged-section > .rows > .merged-group"),
  ];
  const twoGroups = (betaCollapsed = false) =>
    mergedBand(
      [
        mergedGroup({ id: "alpha", label: "alpha", rows: [merge("ws-a")] }),
        mergedGroup({ id: "beta", label: "beta", rows: [merge("ws-b")], repositoryCollapsed: betaCollapsed }),
      ],
      undefined,
      false,
    );

  it("draws one group per wire group, in wire order, under its repository's label", () => {
    const drawn = drawWorkspaceRoster(roster({ merged: twoGroups() }), sidebarContext());
    expect(groupsIn(drawn).map((g) => [g.getAttribute("data-repository"), g.querySelector(".merged-group-head")?.textContent])).toEqual([
      ["alpha", "alpha"],
      ["beta", "beta"],
    ]);
  });

  it("draws a group's rows inside the group", () => {
    const drawn = drawWorkspaceRoster(roster({ merged: twoGroups() }), sidebarContext());
    expect(groupsIn(drawn).map((g) => [...g.querySelectorAll(":scope > .rows > .ws")].length)).toEqual([1, 1]);
  });

  it("draws no fold toggle on a group's header", () => {
    const drawn = drawWorkspaceRoster(roster({ merged: twoGroups() }), sidebarContext());
    expect(groupsIn(drawn).map((g) => g.querySelector("[data-section-fold]"))).toEqual([null, null]);
  });

  it("states each row's merged instant for the fit", () => {
    const drawn = drawWorkspaceRoster(
      roster({ merged: mergedBand([mergedGroup({ id: "alpha", rows: [row({ id: "ws-a", closed: true, when: { case: "merged", value: { atMs: 42n } } })] })], 1, false) }),
      sidebarContext(),
    );
    expect(groupsIn(drawn)[0].querySelector(":scope > .rows > .ws")?.getAttribute("data-merged-at")).toBe("42");
  });

  it("leaves a collapsed repository's group undrawn", () => {
    const drawn = drawWorkspaceRoster(roster({ merged: twoGroups(true) }), sidebarContext());
    expect(groupsIn(drawn).map((g) => g.hidden)).toEqual([false, true]);
  });

  it("leaves the band out when every merge is a collapsed repository's", () => {
    const drawn = drawWorkspaceRoster(
      roster({ merged: mergedBand([mergedGroup({ id: "alpha", rows: [merge("ws-a")], repositoryCollapsed: true })], 0, false) }),
      sidebarContext(),
    );
    expect(pane(drawn).querySelector(".merged-section")?.getAttribute("data-merged-empty")).toBe("true");
  });

  it("takes a repository's group out at once when the repository section is folded", async () => {
    const { sc } = viewDaemon();
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "beta" })], merged: twoGroups() }),
      sc,
    );
    await click(pane(drawn).querySelector(".repo-section [data-section-fold]") as Element);
    expect(groupsIn(drawn).map((g) => g.hidden)).toEqual([false, true]);
  });

  it("puts a repository's group back at once when the repository section is unfolded", async () => {
    const { sc } = viewDaemon();
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "beta", collapsed: true })], merged: twoGroups(true) }),
      sc,
    );
    await click(pane(drawn).querySelector(".repo-section [data-section-fold]") as Element);
    expect(groupsIn(drawn).map((g) => g.hidden)).toEqual([false, false]);
  });

  it("refuses a group whose drawn arm is unset", () => {
    const bad = mergedGroup({ id: "alpha", rows: [merge("ws-a")] });
    bad.drawn = { case: undefined };
    expect(() => drawWorkspaceRoster(roster({ merged: mergedBand([bad], 1, false) }), sidebarContext())).toThrow(MalformedView);
  });
});

describe("recently merged rows", () => {
  const drawnMerged = (): HTMLElement =>
    drawWorkspaceRoster(
      roster({ merged: mergedSection([row({ id: "ws-9", status: { case: "merged", value: {} } })]) }),
      sidebarContext(),
    );
  const mergedRow = (drawn: HTMLElement): HTMLElement =>
    pane(drawn).querySelector(".merged-section [data-roster-row='ws-9']") as HTMLElement;

  it("draws no status dot", () => {
    expect(mergedRow(drawnMerged()).querySelector(".st")).toBeNull();
  });

  it("draws the name in the repository names' colour, not the viewed grey", () => {
    const name = mergedRow(drawnMerged()).querySelector(".name");
    expect([name?.classList.contains("band-name"), name?.classList.contains("viewed")]).toEqual([true, false]);
  });

  it("styles a merged name with the repository headers' colour variable", () => {
    expect(css).toContain("#ws-sidebar .row .name.band-name { color: var(--repo-head-expanded); }");
  });

  it("draws a row merged today with the off-white gutter bar", () => {
    expect(css).toContain("#ws-sidebar .row.merged-today {\n  border-left: var(--row-gutter-w) solid var(--merged-today-gutter);");
  });

  it("declares the one rail gutter width every row bar draws in", () => {
    expect(css).toContain("#ws-sidebar { --row-gutter-w: 3px; }");
  });

  it("declares the one rail row vertical padding", () => {
    expect(css).toContain("#ws-sidebar { --row-pad-y: 0.14rem; }");
  });

  it("pads every rail row with the shared vertical padding", () => {
    expect(css).toContain("  padding: var(--row-pad-y) 0.7rem var(--row-pad-y) 0.9rem;");
  });

  it("builds a merged row's height out of the shared vertical padding", () => {
    expect(css).toContain("--merged-row-h: calc(var(--row-pad-y) * 2 + var(--merged-row-line-h));");
  });

  it("restates no row vertical padding of its own", () => {
    expect(css).not.toContain("--merged-row-pad-y");
  });

  it("draws the selection's bar and the merge-today bar at the shared width", () => {
    const bar = (selector: string): string | undefined =>
      css.split(`${selector} {\n`)[1]?.split("}")[0];
    expect([bar("#ws-sidebar .ws.current > .row"), bar("#ws-sidebar .row.merged-today")].map((b) =>
      b?.includes("border-left: var(--row-gutter-w) solid") && b.includes("padding-left: calc(0.9rem - var(--row-gutter-w))"),
    )).toEqual([true, true]);
  });

  it("no longer colours a name merged today", () => {
    expect(css).not.toContain(".name.band-name.merged-today");
  });

  it("keeps the merged arm on the row, the hook contract's", () => {
    expect(mergedRow(drawnMerged()).getAttribute("data-arm")).toBe("merged");
  });

  it("still draws a status dot on a row in the normal list", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" })] })] }),
      sidebarContext(),
    );
    expect(pane(drawn).querySelector(".repo-section [data-roster-row='ws-1'] .st")).not.toBeNull();
  });
});

describe("the repository label's divergence hover", () => {
  function labelTitle(divergence: Parameters<typeof repoSection>[0]["divergence"]): string {
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1", divergence })] }), sidebarContext());
    const label = pane(drawn).querySelector<HTMLElement>(".repo-section .sb-label");
    if (label === null) throw new Error("no repository label drawn");
    return label.title;
  }

  it("says the daemon has not measured yet while the measure is pending", () => {
    expect(labelTitle({ state: { case: "pending", value: {} } })).toBe("ahead/behind not measured yet");
  });

  it("draws the counts against the upstream once counted", () => {
    expect(
      labelTitle({
        state: { case: "counted", value: { branch: "master", upstream: "origin/master", ahead: 2, behind: 7 } },
      }),
    ).toBe("master: 2 ahead, 7 behind origin/master");
  });

  it("says the branch tracks nothing when it has no upstream", () => {
    expect(labelTitle({ state: { case: "noUpstream", value: { branch: "main" } } })).toBe("main tracks no upstream");
  });

  it("draws the failure's reason when the measure failed", () => {
    expect(labelTitle({ state: { case: "failed", value: { reason: "fetching origin failed: offline" } } })).toBe(
      "ahead/behind unknown: fetching origin failed: offline",
    );
  });

  it("refuses an unset measurement arm as a malformed view", () => {
    expect(() => drawRosterRepoDivergence(create(RosterRepoDivergenceSchema, {}), "divergence")).toThrow(MalformedView);
  });

  it("refuses a repository section with no divergence as a malformed view", () => {
    const section = create(RosterRepoSectionSchema, {
      key: { repository: { id: "repo-1", dir: "/repo/repo-1" } },
      header: { label: { text: "repo-1" }, count: { workspaces: 0 } },
      rows: { rows: [] },
      fold: { case: "expanded", value: {} },
    });
    const wire = roster({ repos: [] });
    wire.repository = { $typeName: "frontend.v1.RosterRepositoryView", sections: [section] };
    expect(() => drawWorkspaceRoster(wire, sidebarContext())).toThrow(MalformedView);
  });
});

describe("drawFoldHeader", () => {
  const draw = (): { header: HTMLElement; section: HTMLElement; calls: HTMLElement[] } => {
    const section = document.createElement("div");
    const label = document.createElement("span");
    label.textContent = "alpha";
    const calls: HTMLElement[] = [];
    const header = drawFoldHeader(label, section, (_section, control) => calls.push(control));
    return { header, section, calls };
  };

  it("draws the fold triangle, then the label, in a repo-head", () => {
    // Arrange, Act
    const { header } = draw();
    // Assert
    expect([header.className, ...[...header.children].map((c) => c.className || c.textContent)]).toEqual([
      "repo-head",
      "tri",
      "alpha",
    ]);
  });

  it("asks for the other fold from a click anywhere on the header", () => {
    // Arrange
    const { header, calls } = draw();
    // Act
    header.click();
    // Assert
    expect(calls).toEqual([header]);
  });

  it("asks once, from the triangle, when the triangle is clicked", () => {
    // Arrange
    const { header, calls } = draw();
    const triangle = header.querySelector<HTMLElement>("[data-section-fold]")!;
    // Act
    triangle.click();
    // Assert
    expect(calls).toEqual([triangle]);
  });

  it("is the header every headed section draws", () => {
    // Arrange, Act
    const drawn = drawWorkspaceRoster(
      roster({
        repos: [repoSection({ id: "repo-1" })],
        hidden: hiddenSection([HIDDEN_ROW]),
        merged: mergedSection([row({ id: "ws-m", closed: true })], 1, false),
      }),
      sidebarContext(),
    );
    // Assert: each section's header is a repo-head opening on the triangle then the label.
    const heads = [...pane(drawn).querySelectorAll<HTMLElement>(".repo > .repo-head")];
    expect(heads.length).toBeGreaterThanOrEqual(3);
    for (const head of heads) {
      expect([...head.children].slice(0, 2).map((c) => c.className)).toEqual(["tri", SECTION_LABEL_CLASS]);
    }
  });
});

describe("the open workspaces section", () => {
  const openIn = (drawn: HTMLElement): HTMLElement =>
    pane(drawn).querySelector<HTMLElement>(`:scope > .${OPEN_SECTION_CLASS}`)!;
  const openCount = (drawn: HTMLElement): HTMLElement =>
    openIn(drawn).querySelector<HTMLElement>(`:scope > .repo-head > .${OPEN_COUNT_CLASS}`)!;
  const MANY = (): ReturnType<typeof roster> =>
    roster({
      repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" }), row({ id: "ws-2" })] })],
      openCount: 3,
    });

  it("is the pane's first box, holding the repository sections", () => {
    // Arrange, Act
    const drawn = drawWorkspaceRoster(MANY(), sidebarContext());
    // Assert
    const section = openIn(drawn);
    expect([pane(drawn).firstElementChild === section, section.lastElementChild?.className]).toEqual([true, "sb-sections"]);
  });

  it("draws the daemon's label in its header", () => {
    const drawn = drawWorkspaceRoster(MANY(), sidebarContext());
    expect(openIn(drawn).querySelector(`:scope > .repo-head > .${SECTION_LABEL_CLASS}`)?.textContent).toBe("Open");
  });

  it("draws the daemon's count beside the label", () => {
    const drawn = drawWorkspaceRoster(MANY(), sidebarContext());
    expect(openCount(drawn).textContent).toBe("(3)");
  });

  it.each([false, true])("shows its count with the section folded=%s", (collapsed) => {
    const drawn = drawWorkspaceRoster(roster({ open: openSection(collapsed) }), sidebarContext());
    expect(inRail(drawn, () => cascadedValue(openCount(drawn), "display"))).not.toBe("none");
  });

  it("draws its count in the Recently Merged today count's grey and size", () => {
    const drawn = drawWorkspaceRoster(roster(), sidebarContext());
    expect(inRail(drawn, () => [cascadedValue(openCount(drawn), "color"), cascadedValue(openCount(drawn), "font-size")])).toEqual([
      "var(--muted)",
      "0.85em",
    ]);
  });

  it("draws its header in the Hidden and Recently Merged headers' purple", () => {
    const drawn = drawWorkspaceRoster(roster(), sidebarContext());
    const header = openIn(drawn).querySelector<HTMLElement>(":scope > .repo-head")!;
    expect(inRail(drawn, () => cascadedValue(header, "color"))).toBe("var(--blocked)");
  });

  it.each([
    [false, "block"],
    [true, "none"],
  ])("folded=%s shows the repository sections as %s", (collapsed, want) => {
    const drawn = drawWorkspaceRoster(roster({ open: openSection(collapsed), repos: [repoSection({ id: "repo-1" })] }), sidebarContext());
    const sections = openIn(drawn).querySelector<HTMLElement>(":scope > .sb-sections")!;
    expect(inRail(drawn, () => cascadedValue(sections, "display") ?? "block")).toBe(want);
  });

  it("leaves the repository sections' own folds alone when it folds", async () => {
    // Arrange
    const { sc } = viewDaemon();
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })] }), sc);
    // Act
    await click(openIn(drawn).querySelector("[data-section-fold]") as Element);
    // Assert
    const repo = drawn.querySelector<HTMLElement>(".repo-section")!;
    expect([repo.classList.contains("folded"), repo.querySelector("[data-section-fold]")?.getAttribute("data-folded")]).toEqual([
      false,
      "false",
    ]);
  });

  it("refuses a roster without it as a malformed view", () => {
    const bare = roster();
    bare.open = undefined;
    expect(() => drawWorkspaceRoster(bare, sidebarContext())).toThrow(MalformedView);
  });

  it("refuses a repository view without its open count", () => {
    const bare = roster();
    bare.repository!.openCount = undefined;
    expect(() => drawWorkspaceRoster(bare, sidebarContext())).toThrow(MalformedView);
  });

  it("refuses a section with no fold arm", () => {
    const bare = roster();
    bare.open!.fold = { case: undefined };
    expect(() => drawWorkspaceRoster(bare, sidebarContext())).toThrow(MalformedView);
  });
});

describe("the roster drawer: a push redraws only what it changed", () => {
  /** A repository section whose one row counts a running turn, so it ticks. */
  const ticking = (id: string, label = id): ReturnType<typeof repoSection> =>
    repoSection({ id, label, rows: [row({ id: `ws-${id}`, when: { case: "turnRunning", value: { startedAtMs: BigInt(NOW) } } })] });

  /** The element of repository ID's section. */
  const repoElement = (drawn: HTMLElement, id: string): Element | null =>
    drawn.querySelector(`[data-roster-row='ws-${id}']`)?.closest(".sb-sections > *") ?? null;

  it("keeps a repository section the push leaves unchanged", () => {
    // Arrange
    const sc = sidebarContext();
    const drawer = createRosterDrawer(sc);
    const first = drawer.draw(roster({ repos: [ticking("a"), ticking("b")] }));
    const before = repoElement(first, "a");

    // Act
    const second = drawer.draw(roster({ repos: [ticking("a"), ticking("b", "b renamed")] }));

    // Assert
    expect(repoElement(second, "a")).toBe(before);
  });

  it("redraws only the two rows a switch moves the highlight between, keeping their section", () => {
    // Arrange
    const drawer = createRosterDrawer(sidebarContext());
    const at = (ids: string, current: string) =>
      roster({ repos: [repoSection({ id: "a", rows: ids.split("").map((id) => row({ id, current: id === current })) })] });
    const first = drawer.draw(at("xyz", "x"));
    const section = first.querySelector(".repo-section");
    const [x, y, z] = ["x", "y", "z"].map((id) => first.querySelector(`[data-roster-row='${id}']`));

    // Act
    const second = drawer.draw(at("xyz", "y"));

    // Assert
    const now = (id: string) => second.querySelector(`[data-roster-row='${id}']`);
    expect([second.querySelector(".repo-section") === section, now("x") === x, now("y") === y, now("z") === z]).toEqual([
      true,
      false,
      false,
      true,
    ]);
    expect(second.querySelector("[data-current]")?.getAttribute("data-roster-row")).toBe("y");
  });

  it("keeps both repositories' sections when a switch crosses between them", () => {
    // Arrange
    const drawer = createRosterDrawer(sidebarContext());
    const at = (current: string) =>
      roster({
        repos: [
          repoSection({ id: "a", rows: [row({ id: "x", current: current === "x" })] }),
          repoSection({ id: "b", rows: [row({ id: "y", current: current === "y" })] }),
        ],
      });
    const first = drawer.draw(at("x"));
    const sections = [...first.querySelectorAll(".repo-section")];

    // Act
    const second = drawer.draw(at("y"));

    // Assert
    expect([...second.querySelectorAll(".repo-section")]).toEqual(sections);
    expect(second.querySelector("[data-current]")?.getAttribute("data-roster-row")).toBe("y");
  });

  it("redraws a section whose rows changed order", () => {
    // Arrange
    const drawer = createRosterDrawer(sidebarContext());
    const at = (ids: string[]) => roster({ repos: [repoSection({ id: "a", rows: ids.map((id) => row({ id })) })] });
    const section = drawer.draw(at(["x", "y"])).querySelector(".repo-section");

    // Act
    const after = drawer.draw(at(["y", "x"])).querySelector(".repo-section");

    // Assert
    expect([after === section, [...after!.querySelectorAll("[data-roster-row]")].map((el) => el.getAttribute("data-roster-row"))]).toEqual([
      false,
      ["y", "x"],
    ]);
  });

  it("redraws a repository section the push changed", () => {
    // Arrange
    const sc = sidebarContext();
    const drawer = createRosterDrawer(sc);
    const before = repoElement(drawer.draw(roster({ repos: [ticking("a")] })), "a");

    // Act
    const after = repoElement(drawer.draw(roster({ repos: [ticking("a", "a renamed")] })), "a");

    // Assert
    expect([after === before, after?.textContent?.includes("a renamed")]).toEqual([false, true]);
  });

  it("tears down a changed section's clocks with its old drawing", () => {
    // Arrange
    const ticker = fakeTicker();
    const sc = sidebarContext(appContext(undefined, ticker));
    const drawer = createRosterDrawer(sc);
    drawer.draw(roster({ repos: [ticking("a")] }));
    const drawnOnce = ticker.subscribers();

    // Act
    drawer.draw(roster({ repos: [ticking("a", "a renamed")] }));

    // Assert
    expect(ticker.subscribers()).toBe(drawnOnce);
  });

  it("tears down a section the push no longer carries", () => {
    // Arrange
    const ticker = fakeTicker();
    const sc = sidebarContext(appContext(undefined, ticker));
    const drawer = createRosterDrawer(sc);
    drawer.draw(roster({ repos: [ticking("a")] }));
    const withA = ticker.subscribers();
    drawer.draw(roster({ repos: [ticking("a"), ticking("b")] }));

    // Act
    const drawn = drawer.draw(roster({ repos: [ticking("a")] }));

    // Assert
    expect([ticker.subscribers(), drawn.querySelector("[data-roster-row='ws-b']")]).toEqual([withA, null]);
  });

  it("places kept sections in the push's order", () => {
    // Arrange
    const drawer = createRosterDrawer(sidebarContext());
    drawer.draw(roster({ repos: [ticking("a"), ticking("b")] }));

    // Act
    const drawn = drawer.draw(roster({ repos: [ticking("b"), ticking("a")] }));

    // Assert
    expect([...drawn.querySelectorAll("[data-roster-row]")].map((el) => el.getAttribute("data-roster-row"))).toEqual([
      "ws-b",
      "ws-a",
    ]);
  });

  it("keeps the repository sections when only the open section's heading changes", () => {
    // Arrange
    const drawer = createRosterDrawer(sidebarContext());
    const before = repoElement(drawer.draw(roster({ repos: [ticking("a")], openCount: 1 })), "a");

    // Act
    const drawn = drawer.draw(roster({ repos: [ticking("a")], openCount: 2 }));

    // Assert
    expect([repoElement(drawn, "a"), drawn.querySelector(`.${OPEN_COUNT_CLASS}`)?.textContent]).toEqual([before, "(2)"]);
  });

  it("keeps the recently-merged band the push leaves unchanged", () => {
    // Arrange
    const band = mergedSection([row({ id: "ws-m", when: { case: "merged", value: { atMs: BigInt(NOW) } } })], 1, false);
    const drawer = createRosterDrawer(sidebarContext());
    const before = drawer.draw(roster({ merged: band })).querySelector("[data-roster-row='ws-m']");

    // Act
    const after = drawer.draw(roster({ merged: band, repos: [ticking("a")] })).querySelector("[data-roster-row='ws-m']");

    // Assert
    expect(after).toBe(before);
  });

  it("keeps the hidden band the push leaves unchanged", () => {
    // Arrange
    const band = hiddenSection([row({ id: "ws-h" })]);
    const drawer = createRosterDrawer(sidebarContext());
    const before = drawer.draw(roster({ hidden: band })).querySelector("[data-roster-row='ws-h']");

    // Act
    const after = drawer.draw(roster({ hidden: band, repos: [ticking("a")] })).querySelector("[data-roster-row='ws-h']");

    // Assert
    expect(after).toBe(before);
  });

  it("keeps a kept section's attention marker alive across the pass", () => {
    // Arrange
    const sc = sidebarContext();
    const attention = sc.attention;
    const drawer = createRosterDrawer(sc);
    const marked = repoSection({ id: "a", rows: [row({ id: "ws-a", attention: true })] });
    attention.beginPass();
    drawer.draw(roster({ repos: [marked] }));
    attention.endPass();

    // Act
    attention.beginPass();
    drawer.draw(roster({ repos: [marked, ticking("b")] }));
    attention.endPass();

    // Assert
    expect(attention.stateOf("ws-a")).not.toBeNull();
  });

  it("refuses a push naming one repository twice as a malformed view", () => {
    // Arrange
    const drawer = createRosterDrawer(sidebarContext());

    // Act
    const draw = (): HTMLElement => drawer.draw(roster({ repos: [ticking("a"), ticking("a", "again")] }));

    // Assert
    expect(draw).toThrow(MalformedView);
  });

  it("tears down every section when the rail is disposed", () => {
    // Arrange
    const ticker = fakeTicker();
    const sc = sidebarContext(appContext(undefined, ticker));
    const drawer = createRosterDrawer(sc);
    drawer.draw(roster({ repos: [ticking("a"), ticking("b")] }));

    // Act
    drawer.dispose();

    // Assert
    expect(ticker.subscribers()).toBe(0);
  });

  it("gives each rail its own drawer, disposed with it", () => {
    // Arrange
    const ticker = fakeTicker();
    const sc = sidebarContext(appContext(undefined, ticker));
    const before = repoElement(drawWorkspaceRoster(roster({ repos: [ticking("a")] }), sc), "a");

    // Act
    disposeWorkspaceRoster(sc);
    const after = repoElement(drawWorkspaceRoster(roster({ repos: [ticking("a")] }), sc), "a");

    // Assert
    expect([after === before, ticker.subscribers()]).toEqual([false, 1]);
  });
});

describe("the recently-merged band: rows out of view are not drawn", () => {
  /** N merged rows, ws-0 first. */
  const merges = (n: number, from = 0): ReturnType<typeof row>[] =>
    Array.from({ length: n }, (_, i) => row({ id: `ws-${from + i}` }));

  /** Draw a band of GROUPS through a hand-run reveal. */
  function drawBand(groups: ReturnType<typeof mergedGroup>[], failures?: FailureSink) {
    const reveal = manualReveal();
    const ctx = appContext();
    const sc = sidebarContext(
      failures === undefined ? ctx : { ...ctx, failures },
      undefined,
      createSidebarView(),
      new Set(),
      reveal,
    );
    const drawer = createRosterDrawer(sc);
    const drawn = drawer.draw(roster({ merged: mergedBand(groups, undefined, false) }));
    return { drawn, reveal, drawer };
  }

  const standIns = (drawn: HTMLElement): string[] =>
    [...drawn.querySelectorAll(`[${STAND_IN_ATTRIBUTE}]`)].map((el) => el.getAttribute("data-roster-row") ?? "");

  it("draws the band's first rows with the band", () => {
    // Act
    const { drawn } = drawBand([mergedGroup({ id: "r", rows: merges(MERGED_DRAWN_AT_ONCE + 3) })]);

    // Assert
    expect(drawn.querySelector(`[data-roster-row='ws-${MERGED_DRAWN_AT_ONCE - 1}']`)?.hasAttribute(STAND_IN_ATTRIBUTE)).toBe(false);
  });

  it("draws a stand-in for every row after the first ones", () => {
    // Act
    const { drawn } = drawBand([mergedGroup({ id: "r", rows: merges(MERGED_DRAWN_AT_ONCE + 3) })]);

    // Assert
    expect(standIns(drawn)).toEqual([0, 1, 2].map((i) => `ws-${MERGED_DRAWN_AT_ONCE + i}`));
  });

  it("counts the rows drawn at once across the band's groups", () => {
    // Act
    const { drawn } = drawBand([
      mergedGroup({ id: "r1", rows: merges(MERGED_DRAWN_AT_ONCE - 1) }),
      mergedGroup({ id: "r2", rows: merges(3, 100) }),
    ]);

    // Assert
    expect(standIns(drawn)).toEqual(["ws-101", "ws-102"]);
  });

  it("gives a stand-in the row's name on a line, as the fit and the search read it", () => {
    // Act
    const { drawn } = drawBand([mergedGroup({ id: "r", rows: merges(MERGED_DRAWN_AT_ONCE + 1) })]);

    // Assert
    const standIn = drawn.querySelector(`[${STAND_IN_ATTRIBUTE}]`);
    expect(standIn?.querySelector(":scope > .row > .name")?.textContent).toBe(`ws-${MERGED_DRAWN_AT_ONCE}`);
  });

  it("stamps a stand-in with the row's merged instant", () => {
    // Act
    const { drawn } = drawBand([mergedGroup({ id: "r", rows: merges(MERGED_DRAWN_AT_ONCE + 1) })]);

    // Assert
    expect(drawn.querySelector(`[${STAND_IN_ATTRIBUTE}]`)?.getAttribute(MERGED_AT_ATTRIBUTE)).toBe(
      String(1_000_000_000 - MERGED_DRAWN_AT_ONCE * 60_000),
    );
  });

  it("draws the row in its stand-in's place once it comes into view", () => {
    // Arrange
    const { drawn, reveal } = drawBand([mergedGroup({ id: "r", rows: merges(MERGED_DRAWN_AT_ONCE + 1) })]);
    const [standIn] = reveal.waiting();

    // Act
    reveal.show(standIn);

    // Assert
    const shown = drawn.querySelector(`[data-roster-row='ws-${MERGED_DRAWN_AT_ONCE}']`);
    expect([shown?.hasAttribute(STAND_IN_ATTRIBUTE), shown?.getAttribute(MERGED_AT_ATTRIBUTE), standIn.parentElement !== null]).toEqual([
      false,
      String(1_000_000_000 - MERGED_DRAWN_AT_ONCE * 60_000),
      false,
    ]);
  });

  it("stops waiting on a stand-in whose band is drawn anew", () => {
    // Arrange
    const { reveal, drawer } = drawBand([mergedGroup({ id: "r", rows: merges(MERGED_DRAWN_AT_ONCE + 1) })]);

    // Act
    drawer.draw(roster({ merged: mergedBand([mergedGroup({ id: "r", rows: merges(MERGED_DRAWN_AT_ONCE) })], undefined, false) }));

    // Assert
    expect(reveal.waiting()).toEqual([]);
  });

  it("files a row it finds malformed when it comes into view, leaving the stand-in", () => {
    // Arrange
    const failures = recordingFailures();
    const bad = row({ id: "ws-bad" });
    bad.status = { case: undefined };
    const { reveal } = drawBand([mergedGroup({ id: "r", rows: [...merges(MERGED_DRAWN_AT_ONCE), bad] })], failures);
    const [standIn] = reveal.waiting();

    // Act
    reveal.show(standIn);

    // Assert
    expect([failures.cases, standIn.parentElement !== null]).toEqual([["frameUndecodable"], true]);
  });

  it("logs a row it finds malformed when it comes into view", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const bad = row({ id: "ws-bad" });
    bad.status = { case: undefined };
    const { reveal } = drawBand([mergedGroup({ id: "r", rows: [...merges(MERGED_DRAWN_AT_ONCE), bad] })]);

    // Act
    reveal.show(reveal.waiting()[0]);

    // Assert
    const record = await forwardedRecord(capture, "sidebar.row.reveal-undecodable");
    expect([record.level.case, record.context?.path]).toEqual([
      "error",
      `WorkspaceRoster.recently_merged.groups[0].rows.rows[${MERGED_DRAWN_AT_ONCE}].status`,
    ]);
  });
});


describe("the open rows' tab numbers", () => {
  /** The number drawn on workspace ID's row, or null when its slot is empty. */
  const numberOf = (drawn: HTMLElement, id: string): string | null =>
    drawn.querySelector(`[data-roster-row='${id}'] > .row > .sb-num`)?.getAttribute("data-tab-number") ?? null;

  it("numbers each open row by its drawn tab, across repositories", () => {
    // Arrange
    const u = roster({
      repos: [repoSection({ id: "a", rows: [row({ id: "x" })] }), repoSection({ id: "b", rows: [row({ id: "y" })] })],
      tabs: [{ id: "y" }, { id: "x" }],
    });

    // Act
    const drawn = createRosterDrawer(sidebarContext()).draw(u);

    // Assert
    expect([numberOf(drawn, "x"), numberOf(drawn, "y")]).toEqual(["2", "1"]);
  });

  it("numbers a child row by its own tab", () => {
    // Arrange
    const u = roster({
      repos: [repoSection({ id: "a", rows: [row({ id: "p", children: [row({ id: "c" })] })] })],
      tabs: [{ id: "c" }, { id: "p" }],
    });

    // Act
    const drawn = createRosterDrawer(sidebarContext()).draw(u);

    // Assert
    expect([numberOf(drawn, "p"), numberOf(drawn, "c")]).toEqual(["2", "1"]);
  });

  it("renumbers a kept section's rows when a tab elsewhere shifts them", () => {
    // Arrange
    const drawer = createRosterDrawer(sidebarContext());
    const repos = [repoSection({ id: "a", rows: [row({ id: "x" })] }), repoSection({ id: "b", rows: [row({ id: "y" })] })];
    const first = drawer.draw(roster({ repos, tabs: [{ id: "y" }, { id: "x" }] }));
    const kept = first.querySelector("[data-roster-row='x']");

    // Act
    const second = drawer.draw(roster({ repos, tabs: [{ id: "y", drawn: "withheldHidden" }, { id: "x" }] }));

    // Assert
    expect([second.querySelector("[data-roster-row='x']") === kept, numberOf(second, "x")]).toEqual([true, "1"]);
  });

  it("leaves the slot of a row with no drawn tab empty", () => {
    // Arrange
    const u = roster({ repos: [repoSection({ id: "a", rows: [row({ id: "x" })] })], tabs: [] });

    // Act
    const drawn = createRosterDrawer(sidebarContext()).draw(u);

    // Assert
    expect([numberOf(drawn, "x"), drawn.querySelector("[data-roster-row='x'] .sb-num")?.textContent]).toEqual([null, ""]);
  });

  it("refuses a roster with no tab bar as a malformed view", () => {
    // Arrange
    const u = roster({ repos: [repoSection({ id: "a", rows: [row({ id: "x" })] })] });
    u.tabBar = undefined;

    // Act
    const draw = (): unknown => createRosterDrawer(sidebarContext()).draw(u);

    // Assert
    expect(draw).toThrow(/WorkspaceRoster\.tab_bar/);
  });
});

describe("the tab number's stylesheet", () => {
  it.each([
    ["color", "var(--tab-number-color)"],
    ["font-size", "0.62rem"],
    ["margin-left", "-0.4rem"],
    ["margin-right", "0.1rem"],
    ["text-align", "right"],
    ["width", "var(--tab-number-w)"],
  ])("draws the number with %s %s", (property, want) => {
    // Arrange
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "a", rows: [row({ id: "x" })] })], tabs: [{ id: "x" }] }),
      sidebarContext(),
    );

    // Act
    const value = inRail(drawn, (rail) => cascadedValue(rail.querySelector(".sb-num")!, property));

    // Assert
    expect(value).toBe(want);
  });

  it("names the number's grey once, on the rail", () => {
    // Arrange
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "a", rows: [row({ id: "x" })] })], tabs: [{ id: "x" }] }),
      sidebarContext(),
    );

    // Act
    const value = inRail(drawn, (rail) => cascadedValue(rail, "--tab-number-color"));

    // Assert
    expect(value.replace(/\s+/g, "")).toBe("color-mix(insrgb,var(--muted)75%,black)");
  });
});
