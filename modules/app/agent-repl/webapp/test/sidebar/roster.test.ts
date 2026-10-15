// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { UpdateTaskResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_task_pb";
import {
  UpdateSidebarViewResponseSchema,
  type UpdateSidebarViewRequest,
  type UpdateSidebarViewResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import {
  RosterLabelSchema,
  RosterMaturityKeySchema,
  RosterRepoDivergenceSchema,
  RosterSectionHeaderSchema,
  RosterRepoSectionSchema,
  RosterTaskSectionSchema,
  WorkspaceRosterSchema,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { SetWorkspaceHiddenResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_hidden_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { repoFoldKey } from "../../src/sidebar/context.js";
import {
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
import {
  appContext,
  fakeTicker,
  hiddenSection,
  maturitySection,
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
  taskSection,
} from "./harness.js";

const UPDATE_OK = create(UpdateTaskResponseSchema, { result: { case: "success", value: {} } });
const VIEW_OK = create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } });
const VIEW_UNKNOWN_TASK = create(UpdateSidebarViewResponseSchema, {
  result: { case: "error", value: { cause: { case: "unknownTask", value: {} } } },
});
/** One settled merge, so the merged band draws. */
const MERGED_ROW = row({ id: "ws-9", closed: true, status: { case: "merged", value: {} } });
/** One hidden workspace, so the hidden section draws. */
const HIDDEN_ROW = row({ id: "ws-8" });

async function click(control: Element): Promise<void> {
  (control as HTMLElement).dispatchEvent(new MouseEvent("click", { bubbles: true }));
  await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
}

function pane(drawn: HTMLElement, grouping: string): HTMLElement {
  return drawn.querySelector(`[data-grouping='${grouping}']`) as HTMLElement;
}

/** The task SECTION's header, which owns the rename controls; the pane's own
 *  new-task form carries a `[data-task-title]` of its own. */
function taskHead(drawn: HTMLElement): HTMLElement {
  return pane(drawn, "task").querySelector(".task-head") as HTMLElement;
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

describe("the groupings", () => {
  it("draws every one, because every one arrives resolved", () => {
    const drawn = drawWorkspaceRoster(roster(), sidebarContext());
    expect([...drawn.querySelectorAll("[data-grouping]")].map((p) => p.getAttribute("data-grouping"))).toEqual([
      "repository",
      "task",
      "maturity",
    ]);
  });

  it("shows the maturity grouping when the daemon says every page shows it", () => {
    const drawn = drawWorkspaceRoster(roster({ shown: "maturity" }), sidebarContext());
    expect([pane(drawn, "repository").hidden, pane(drawn, "task").hidden, pane(drawn, "maturity").hidden]).toEqual([
      true,
      true,
      false,
    ]);
  });

  it("shows the repository grouping by default", () => {
    const drawn = drawWorkspaceRoster(roster(), sidebarContext());
    expect(pane(drawn, "repository").hidden).toBe(false);
  });

  it("hides the grouping the user is not looking at, rather than dropping it", () => {
    const drawn = drawWorkspaceRoster(roster(), sidebarContext());
    expect(pane(drawn, "task").hidden).toBe(true);
  });

  it("shows the task grouping when the daemon says every page shows it", () => {
    const drawn = drawWorkspaceRoster(roster({ shown: "task" }), sidebarContext());
    expect([pane(drawn, "task").hidden, pane(drawn, "repository").hidden]).toEqual([false, true]);
  });

  it("draws a workspace in BOTH groupings when both place it", () => {
    const drawn = drawWorkspaceRoster(
      roster({
        repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" })] })],
        tasks: [taskSection({ id: "task-1", rows: [row({ id: "ws-1" })] })],
      }),
      sidebarContext(),
    );
    expect(drawn.querySelectorAll("[data-roster-row='ws-1']").length).toBe(2);
  });
});

describe("the sections", () => {
  it("draws one section per repository, in the resolver's order", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "b" }), repoSection({ id: "a" })] }),
      sidebarContext(),
    );
    const keys = [...pane(drawn, "repository").querySelectorAll(".repo.repo-section")].map(
      (el) => el.getAttribute("data-section"),
    );
    expect(keys).toEqual([repoFoldKey("b"), repoFoldKey("a")]);
  });

  it("draws the repository's label", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", label: "doom" })] }),
      sidebarContext(),
    );
    expect(pane(drawn, "repository").querySelector(".repo-section .sb-label")?.textContent).toBe("doom");
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
    const section = pane(drawn, "repository").querySelector(
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
    expect(pane(drawn, "repository").querySelector(".repo-section .sb-label")?.textContent).toBe(
      "just-registered",
    );
  });

  it("draws one section per task", () => {
    const drawn = drawWorkspaceRoster(
      roster({ tasks: [taskSection({ id: "task-1" }), taskSection({ id: "task-2" })] }),
      sidebarContext(),
    );
    expect(pane(drawn, "task").querySelectorAll(".task-section").length).toBe(2);
  });

  it("offers each section a fold toggle", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1" })] }),
      sidebarContext(),
    );
    expect(pane(drawn, "repository").querySelector(".repo-section [data-section-fold]")).not.toBeNull();
  });

  it.each([
    [false, false],
    [true, true],
  ])("draws a repository section collapsed=%s folded=%s, as the daemon says", (collapsed, folded) => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", collapsed })] }),
      sidebarContext(),
    );
    expect(pane(drawn, "repository").querySelector(".open-section .repo")?.classList.contains("folded")).toBe(folded);
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

  it("marks a repository section so its header takes its fold's grey", () => {
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })] }), sidebarContext());
    expect(pane(drawn, "repository").querySelector(".open-section .repo")?.classList.contains("repo-section")).toBe(true);
  });

  it.each([
    [false, false],
    [true, true],
  ])("draws a task section collapsed=%s folded=%s, as the daemon says", (collapsed, folded) => {
    const drawn = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1", collapsed })] }), sidebarContext());
    expect(pane(drawn, "task").querySelector(".task-section")?.classList.contains("folded")).toBe(folded);
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
  const id =
    section.case === "repository" || section.case === "task"
      ? section.value.id
      : section.case === "maturity"
        ? (section.value.stage.case ?? "")
        : "";
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
    kind: "task",
    roster: (collapsed: boolean) => roster({ tasks: [taskSection({ id: "task-1", collapsed })] }),
    selector: ".task-section",
    id: "task-1",
  },
  {
    kind: "maturity",
    roster: (collapsed: boolean) => roster({ maturity: [maturitySection({ stage: "inProgress", collapsed })] }),
    selector: ".maturity-section",
    id: "inProgress",
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

  it("folds every copy of the merged band, one per grouping", async () => {
    const { sc } = viewDaemon();
    const drawn = drawWorkspaceRoster(roster({ merged: mergedSection([MERGED_ROW], 1, false) }), sc);
    await click(pane(drawn, "repository").querySelector(".merged-section [data-section-fold]") as Element);
    expect([...drawn.querySelectorAll(".merged-section")].map((m) => m.classList.contains("folded"))).toEqual([
      true,
      true,
      true,
    ]);
  });

  it("keeps the asked fold over a push still carrying the old one", async () => {
    const { sc } = viewDaemon();
    const drawn = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1" })] }), sc);
    await click(drawn.querySelector(".task-section [data-section-fold]") as Element);
    const redrawn = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1" })] }), sc);
    expect(redrawn.querySelector(".task-section")?.classList.contains("folded")).toBe(true);
  });

  it("follows the wire again once a push has carried the asked fold", async () => {
    const { sc } = viewDaemon();
    const drawn = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1" })] }), sc);
    await click(drawn.querySelector(".task-section [data-section-fold]") as Element);
    drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1", collapsed: true })] }), sc);
    const later = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1" })] }), sc);
    expect(later.querySelector(".task-section")?.classList.contains("folded")).toBe(false);
  });

  it("draws a fold another page asked for, from the push alone", () => {
    const sc = sidebarContext();
    drawWorkspaceRoster(roster({ merged: mergedSection([MERGED_ROW], 1, true) }), sc);
    const pushed = drawWorkspaceRoster(roster({ merged: mergedSection([MERGED_ROW], 1, false) }), sc);
    expect(pane(pushed, "repository").querySelector(".merged-section")?.classList.contains("folded")).toBe(false);
  });

  it("draws the daemon's refusal of a fold beside the toggle", async () => {
    const { sc } = viewDaemon(VIEW_UNKNOWN_TASK);
    const drawn = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1" })] }), sc);
    await click(drawn.querySelector(".task-section [data-section-fold]") as Element);
    expect(pane(drawn, "task").textContent).toContain("the daemon no longer has that task");
  });

  it("puts a refused fold back as the wire drew it", async () => {
    const { sc } = viewDaemon(VIEW_UNKNOWN_TASK);
    const drawn = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1" })] }), sc);
    const section = drawn.querySelector(".task-section") as HTMLElement;
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
    expect(pane(drawn, "repository").textContent).toContain("the daemon no longer has that repository");
  });

  it("refuses an unset task fold as a malformed view", () => {
    const section = create(RosterTaskSectionSchema, {
      key: { taskId: "task-1" },
      header: { label: { text: "t" }, done: { done: false } },
      rows: { rows: [] },
    });
    expect(() => drawWorkspaceRoster(roster({ tasks: [section] }), sidebarContext())).toThrow(MalformedView);
  });
});

describe("the grouping shown is the daemon's view, shared by every page", () => {
  it("refuses an unset grouping as a malformed view", () => {
    const bare = create(WorkspaceRosterSchema, {
      repository: { sections: [] },
      task: { sections: [] },
      recentlyMerged: mergedSection(),
    });
    expect(() => drawWorkspaceRoster(bare, sidebarContext())).toThrow(MalformedView);
  });

  it("draws the grouping another page chose, from the push alone", () => {
    const sc = sidebarContext();
    drawWorkspaceRoster(roster(), sc);
    const pushed = drawWorkspaceRoster(roster({ shown: "task" }), sc);
    expect(pane(pushed, "task").hidden).toBe(false);
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
    expect(pane(drawn, "repository").querySelector("[data-roster-row='ws-closed']")).toBeNull();
    expect(pane(drawn, "repository").querySelector("[data-roster-row='ws-live']")).not.toBeNull();
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
    expect(pane(drawn, "repository").querySelector("[data-roster-row='ws-killed']")).toBeNull();
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
    const glyphs = [...pane(drawn, "repository").querySelectorAll(".st")].map((el) => el.textContent);
    expect(glyphs).not.toContain("?");
  });

  it("drops a closed row from a task section", () => {
    const drawn = drawWorkspaceRoster(
      roster({
        tasks: [taskSection({ id: "task-1", rows: [row({ id: "ws-closed", closed: true })] })],
      }),
      sidebarContext(),
    );
    expect(pane(drawn, "task").querySelector("[data-roster-row='ws-closed']")).toBeNull();
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
    expect(pane(drawn, "repository").querySelector("[data-roster-row='ws-killed']")).toBeNull();
    expect(pane(drawn, "repository").querySelector("[data-roster-row='ws-child']")).not.toBeNull();
  });
});

describe("drawHeadedSection", () => {
  /** A headed section's wire: a header, rows and a fold. */
  function wire(init: { collapsed?: boolean; rows?: ReturnType<typeof row>[] } = {}) {
    return maturitySection({ stage: "backlog", label: "Head", rows: init.rows, collapsed: init.collapsed });
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

  // EVERY FOLDING SECTION BUT A TASK'S IS ONE SHAPE: each draws its box through
  // drawHeadedSection, so each carries the same header and fold control. A
  // hand-rolled section that drew its own header would carry neither here.
  it.each([
    ["repository", ".repo-section", roster({ repos: [repoSection({ id: "repo-1" })] })],
    ["maturity", ".maturity-section", roster({ maturity: [maturitySection({ stage: "backlog" })] })],
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

  it("is the one shape the repository and maturity sections share", () => {
    /** A section's element tree, by tag and class, without the repository's create control. */
    const outline = (section: Element | null): string[] =>
      [...(section?.querySelectorAll(":scope > *, :scope > .repo-head > *") ?? [])]
        .filter((el) => !el.classList.contains("sb-add"))
        .map((el) => `${el.tagName}.${[...el.classList].join(".")}`);
    const drawn = drawWorkspaceRoster(
      roster({
        repos: [repoSection({ id: "repo-1", label: "x" })],
        maturity: [maturitySection({ stage: "backlog", label: "x" })],
      }),
      sidebarContext(),
    );
    expect(outline(drawn.querySelector(".maturity-section"))).toEqual(outline(drawn.querySelector(".repo-section")));
  });
});

describe("the maturity grouping", () => {
  const STAGES = [
    maturitySection({ stage: "backlog", label: "Backlog", rows: [row({ id: "ws-new" })] }),
    maturitySection({ stage: "inProgress", label: "In Progress", rows: [row({ id: "ws-busy" })] }),
    maturitySection({ stage: "merging", label: "Merging" }),
  ];

  it("draws one section per stage, in the resolver's order", () => {
    const drawn = drawWorkspaceRoster(roster({ maturity: STAGES }), sidebarContext());
    const stages = [...pane(drawn, "maturity").querySelectorAll(".maturity-section")].map((s) =>
      s.getAttribute("data-maturity-stage"),
    );
    expect(stages).toEqual(["backlog", "inProgress", "merging"]);
  });

  it("draws each stage's label", () => {
    const drawn = drawWorkspaceRoster(roster({ maturity: STAGES }), sidebarContext());
    const labels = [...pane(drawn, "maturity").querySelectorAll(".maturity-section .sb-label")].map((l) => l.textContent);
    expect(labels).toEqual(["Backlog", "In Progress", "Merging"]);
  });

  it("draws a workspace under the stage the daemon filed it in", () => {
    const drawn = drawWorkspaceRoster(roster({ maturity: STAGES }), sidebarContext());
    const stage = pane(drawn, "maturity").querySelector("[data-roster-row='ws-busy']")?.closest(".maturity-section");
    expect(stage?.getAttribute("data-maturity-stage")).toBe("inProgress");
  });

  it("draws a stage with no rows", () => {
    const drawn = drawWorkspaceRoster(roster({ maturity: STAGES }), sidebarContext());
    expect(pane(drawn, "maturity").querySelector("[data-maturity-stage='merging']")).not.toBeNull();
  });

  it("drops a closed row from a stage", () => {
    const drawn = drawWorkspaceRoster(
      roster({ maturity: [maturitySection({ stage: "inProgress", rows: [row({ id: "ws-closed", closed: true })] })] }),
      sidebarContext(),
    );
    expect(pane(drawn, "maturity").querySelector("[data-roster-row='ws-closed']")).toBeNull();
  });

  it("carries its own copy of the recently-merged band", () => {
    const drawn = drawWorkspaceRoster(roster({ merged: mergedSection([MERGED_ROW], 1) }), sidebarContext());
    expect(pane(drawn, "maturity").querySelector(".merged-section [data-roster-row='ws-9']")).not.toBeNull();
  });

  it.each([
    [false, false],
    [true, true],
  ])("draws a stage collapsed=%s folded=%s, as the daemon says", (collapsed, folded) => {
    const drawn = drawWorkspaceRoster(
      roster({ maturity: [maturitySection({ stage: "backlog", collapsed })] }),
      sidebarContext(),
    );
    expect(pane(drawn, "maturity").querySelector(".maturity-section")?.classList.contains("folded")).toBe(folded);
  });

  it("refuses a stage key with no stage arm as a malformed view", () => {
    const section = maturitySection({ stage: "backlog" });
    section.key = create(RosterMaturityKeySchema, { stage: {} });
    expect(() => drawWorkspaceRoster(roster({ maturity: [section] }), sidebarContext())).toThrow(MalformedView);
  });

  it("refuses a stage key with no stage as a malformed view", () => {
    const section = maturitySection({ stage: "backlog" });
    section.key = create(RosterMaturityKeySchema, {});
    expect(() => drawWorkspaceRoster(roster({ maturity: [section] }), sidebarContext())).toThrow(MalformedView);
  });

  it("refuses a roster with no maturity view as a malformed view", () => {
    const bare = create(WorkspaceRosterSchema, {
      repository: { sections: [] },
      task: { sections: [] },
      recentlyMerged: mergedSection(),
      shown: { case: "shownRepository", value: {} },
    });
    expect(() => drawWorkspaceRoster(bare, sidebarContext())).toThrow(MalformedView);
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
      pane(drawn, "repository").querySelector(".merged-section [data-roster-row='ws-9']"),
    ).not.toBeNull();
  });

  it("appears under EVERY grouping", () => {
    const drawn = drawWorkspaceRoster(landed(), sidebarContext());
    expect(drawn.querySelectorAll(".merged-section:not([hidden])").length).toBe(3);
  });

  it("starts folded, because settled history should not spend rail height", () => {
    const drawn = drawWorkspaceRoster(landed(), sidebarContext());
    expect(
      pane(drawn, "repository").querySelector(".merged-section")?.classList.contains("folded"),
    ).toBe(true);
  });

  it("points its folded triangle at the rows it is hiding", () => {
    const drawn = drawWorkspaceRoster(landed(), sidebarContext());
    expect(
      pane(drawn, "repository").querySelector(".merged-section [data-section-fold]")?.textContent,
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
    expect(pane(drawn, "repository").querySelector(".merged-section")?.classList.contains("folded")).toBe(false);
  });

  it("draws its rows", () => {
    const drawn = drawWorkspaceRoster(landed(), sidebarContext());
    expect(
      pane(drawn, "repository").querySelector(".merged-section [data-roster-row='ws-9']"),
    ).not.toBeNull();
  });

  it("draws its header label", () => {
    const drawn = drawWorkspaceRoster(landed(), sidebarContext());
    expect(
      pane(drawn, "repository").querySelector(".merged-section .sb-label")?.textContent,
    ).toBe("Recently Merged");
  });

  it("draws no heading when no merge has landed", () => {
    const drawn = drawWorkspaceRoster(roster(), sidebarContext());
    expect(pane(drawn, "repository").querySelector(".merged-section .sb-label")).toBeNull();
  });

  it("keeps the empty band out of the rail entirely", () => {
    const drawn = drawWorkspaceRoster(roster(), sidebarContext());
    expect(
      (pane(drawn, "repository").querySelector(".merged-section") as HTMLElement).hidden,
    ).toBe(true);
  });
});

describe("hidden workspaces", () => {
  it("draws the hidden row under every grouping", () => {
    const drawn = drawWorkspaceRoster(roster({ hidden: hiddenSection([HIDDEN_ROW]) }), sidebarContext());
    expect(
      ["repository", "task", "maturity"].map(
        (grouping) => pane(drawn, grouping).querySelector(".hidden-section [data-roster-row='ws-8']") !== null,
      ),
    ).toEqual([true, true, true]);
  });

  it("draws the section above the recently-merged band", () => {
    const drawn = drawWorkspaceRoster(
      roster({ hidden: hiddenSection([HIDDEN_ROW]), merged: mergedSection([MERGED_ROW]) }),
      sidebarContext(),
    );
    const bands = [...pane(drawn, "repository").querySelectorAll(".hidden-section, .merged-section")];
    expect(bands.map((band) => band.classList.contains("hidden-section"))).toEqual([true, false]);
  });

  it("draws the daemon's heading and count", () => {
    const drawn = drawWorkspaceRoster(roster({ hidden: hiddenSection([HIDDEN_ROW], 1) }), sidebarContext());
    const section = pane(drawn, "repository").querySelector(".hidden-section") as HTMLElement;
    expect([section.querySelector(".sb-label")?.textContent, section.querySelector(".sb-count")?.textContent]).toEqual([
      "Hidden",
      "(1)",
    ]);
  });

  it("keeps the section out of the rail when nothing is hidden", () => {
    const drawn = drawWorkspaceRoster(roster(), sidebarContext());
    const section = pane(drawn, "repository").querySelector(".hidden-section") as HTMLElement;
    expect([section.hidden, section.querySelector(".sb-label")]).toEqual([true, null]);
  });

  it("keeps the section out of the rail when its only row is closed", () => {
    const drawn = drawWorkspaceRoster(
      roster({ hidden: hiddenSection([row({ id: "ws-gone", closed: true })], 0) }),
      sidebarContext(),
    );
    expect((pane(drawn, "repository").querySelector(".hidden-section") as HTMLElement).hidden).toBe(true);
  });

  it("drops a closed row and keeps the open one", () => {
    const drawn = drawWorkspaceRoster(
      roster({ hidden: hiddenSection([row({ id: "ws-gone", closed: true }), HIDDEN_ROW], 1) }),
      sidebarContext(),
    );
    const section = pane(drawn, "repository").querySelector(".hidden-section") as HTMLElement;
    expect([...section.querySelectorAll("[data-roster-row]")].map((r) => r.getAttribute("data-roster-row"))).toEqual([
      "ws-8",
    ]);
  });

  it("refuses a roster with no hidden section as a malformed view", () => {
    const bare = create(WorkspaceRosterSchema, {
      repository: { sections: [] },
      task: { sections: [] },
      maturity: { sections: [] },
      recentlyMerged: mergedSection(),
      shown: { case: "shownRepository", value: {} },
    });
    expect(() => drawWorkspaceRoster(bare, sidebarContext())).toThrow(MalformedView);
  });

  it("refuses an empty hidden section with no fold as a malformed view", () => {
    const section = hiddenSection();
    section.fold = { case: undefined };
    expect(() => drawWorkspaceRoster(roster({ hidden: section }), sidebarContext())).toThrow(MalformedView);
  });
});

describe("the task section's header", () => {
  it("draws the done check from the wire", () => {
    const drawn = drawWorkspaceRoster(
      roster({ tasks: [taskSection({ id: "task-1", done: true })] }),
      sidebarContext(appContext()),
    );
    expect(pane(drawn, "task").querySelector("[data-task-status]")?.getAttribute("data-task-status")).toBe(
      "done",
    );
  });

  it("completes an open task through the set_done arm", async () => {
    const arms: string[] = [];
    const sc = sidebarContext(
      appContext({
        updateTask: (request) => {
          arms.push(request.change.case ?? "unset");
          return UPDATE_OK;
        },
      })
    );
    const drawn = drawWorkspaceRoster(
      roster({ tasks: [taskSection({ id: "task-1", done: false })] }),
      sc,
    );
    await click(pane(drawn, "task").querySelector("[data-task-status]") as Element);
    expect(arms).toEqual(["setDone"]);
  });

  it("reopens a done task through the set_open arm", async () => {
    const arms: string[] = [];
    const sc = sidebarContext(
      appContext({
        updateTask: (request) => {
          arms.push(request.change.case ?? "unset");
          return UPDATE_OK;
        },
      })
    );
    const drawn = drawWorkspaceRoster(
      roster({ tasks: [taskSection({ id: "task-1", done: true })] }),
      sc,
    );
    await click(pane(drawn, "task").querySelector("[data-task-status]") as Element);
    expect(arms).toEqual(["setOpen"]);
  });

  it("echoes the task's id on the change", async () => {
    let id = "";
    const sc = sidebarContext(
      appContext({
        updateTask: (request) => {
          id = request.task?.id ?? "";
          return UPDATE_OK;
        },
      })
    );
    const drawn = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1" })] }), sc);
    await click(pane(drawn, "task").querySelector("[data-task-status]") as Element);
    expect(id).toBe("task-1");
  });

  it("does not fold the section when the check is clicked", async () => {
    const sc = sidebarContext(appContext({ updateTask: () => UPDATE_OK }));
    const drawn = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1" })] }), sc);
    const section = pane(drawn, "task").querySelector(".task-section") as HTMLElement;
    await click(section.querySelector("[data-task-status]") as Element);
    expect(section.classList.contains("folded")).toBe(false);
  });

  it("offers a rename on the label", () => {
    const drawn = drawWorkspaceRoster(
      roster({ tasks: [taskSection({ id: "task-1" })] }),
      sidebarContext(appContext()),
    );
    expect(
      pane(drawn, "task").querySelector("[data-task-rename]")?.getAttribute("data-task-rename"),
    ).toBe("task-1");
  });

  it("holds the current title in the rename field", () => {
    const drawn = drawWorkspaceRoster(
      roster({ tasks: [taskSection({ id: "task-1", label: "ship" })] }),
      sidebarContext(appContext()),
    );
    expect(
      (taskHead(drawn).querySelector("[data-task-rename]") as HTMLInputElement).value,
    ).toBe("ship");
  });

  it("retitles through the set_title arm", async () => {
    let title = "";
    const sc = sidebarContext(
      appContext({
        updateTask: (request) => {
          title = request.change.case === "setTitle" ? request.change.value.title : "";
          return UPDATE_OK;
        },
      })
    );
    const drawn = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1" })] }), sc);
    (taskHead(drawn).querySelector("[data-task-rename]") as HTMLInputElement).value = "renamed";
    await click(taskHead(drawn).querySelector("[data-task-change='setTitle']") as Element);
    expect(title).toBe("renamed");
  });

  it("sends no blank retitle, because the contract forbids one", async () => {
    let calls = 0;
    const sc = sidebarContext(
      appContext({
        updateTask: () => {
          calls += 1;
          return UPDATE_OK;
        },
      })
    );
    const drawn = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1" })] }), sc);
    (taskHead(drawn).querySelector("[data-task-rename]") as HTMLInputElement).value = "   ";
    await click(taskHead(drawn).querySelector("[data-task-change='setTitle']") as Element);
    expect(calls).toBe(0);
  });

  it("says an UpdateTask refusal at the control that made the call", async () => {
    const sc = sidebarContext(
      appContext({
        updateTask: () =>
          create(UpdateTaskResponseSchema, {
            result: { case: "error", value: { cause: { case: "unknownTask", value: {} } } },
          }),
      })
    );
    const drawn = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1" })] }), sc);
    await click(taskHead(drawn).querySelector("[data-task-status]") as Element);
    expect(taskHead(drawn).querySelector(".refusal[data-arm]")?.getAttribute("data-arm")).toBe(
      "unknownTask",
    );
  });

  it("words that refusal from the task verbs' own table", async () => {
    const sc = sidebarContext(
      appContext({
        updateTask: () =>
          create(UpdateTaskResponseSchema, {
            result: { case: "error", value: { cause: { case: "noChange", value: {} } } },
          }),
      })
    );
    const drawn = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1" })] }), sc);
    await click(taskHead(drawn).querySelector("[data-task-status]") as Element);
    expect(taskHead(drawn).querySelector(".refusal")?.textContent).toContain("exactly as it is");
  });
});

describe("the create controls", () => {
  it("offers a new workspace per repository section", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1" })] }),
      sidebarContext(),
    );
    expect(pane(drawn, "repository").querySelector(".repo-section > .repo-head .sb-add")).not.toBeNull();
  });

  it("opens the create form under that section's header", async () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1" })] }),
      sidebarContext(),
    );
    await click(pane(drawn, "repository").querySelector(".repo-section > .repo-head .sb-add") as Element);
    expect(
      pane(drawn, "repository").querySelector(".repo-section > .repo-head")?.nextElementSibling?.hasAttribute(
        "data-create-form",
      ),
    ).toBe(true);
  });

  it("does not fold the section when the create control is clicked", async () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1" })] }),
      sidebarContext(),
    );
    const section = pane(drawn, "repository").querySelector(".repo-section") as HTMLElement;
    await click(section.querySelector(".sb-add") as Element);
    expect(section.classList.contains("folded")).toBe(false);
  });

  it("offers the new-task control at the head of the task grouping", () => {
    const drawn = drawWorkspaceRoster(roster(), sidebarContext());
    expect(pane(drawn, "task").querySelector("[data-task-create]")).not.toBeNull();
  });
});

describe("the assign menu's choices", () => {
  it("are refreshed from the task view on every push", () => {
    const sc = sidebarContext();
    drawWorkspaceRoster(
      roster({ tasks: [taskSection({ id: "task-1", label: "ship" })] }),
      sc,
    );
    expect(sc.tasks).toEqual([{ id: "task-1", label: "ship" }]);
  });

  it("hold nothing over from a push that no longer carries the task", () => {
    const sc = sidebarContext();
    drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1" })] }), sc);
    drawWorkspaceRoster(roster(), sc);
    expect(sc.tasks).toEqual([]);
  });
});

describe("a malformed roster", () => {
  it("refuses one with no repository view", () => {
    const malformed = create(WorkspaceRosterSchema, {
      task: { sections: [] },
      recentlyMerged: mergedSection(),
    });
    expect(() => drawWorkspaceRoster(malformed, sidebarContext())).toThrow(MalformedView);
  });

  it("refuses one with no task view", () => {
    const malformed = create(WorkspaceRosterSchema, {
      repository: { sections: [] },
      recentlyMerged: mergedSection(),
    });
    expect(() => drawWorkspaceRoster(malformed, sidebarContext())).toThrow(MalformedView);
  });

  it("refuses one with no recently-merged section", () => {
    const malformed = create(WorkspaceRosterSchema, {
      repository: { sections: [] },
      task: { sections: [] },
    });
    expect(() => drawWorkspaceRoster(malformed, sidebarContext())).toThrow(MalformedView);
  });

  it("refuses a current workspace with no identity", () => {
    const malformed = create(WorkspaceRosterSchema, {
      repository: { sections: [] },
      task: { sections: [] },
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

  it("refuses a task section with no done check", () => {
    const malformed = create(RosterTaskSectionSchema, {
      key: { taskId: "task-1" },
      header: { label: { text: "ship" } },
      rows: { rows: [] },
    });
    expect(() =>
      drawWorkspaceRoster(roster({ tasks: [malformed] }), sidebarContext()),
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

describe("a task section's verb menu", () => {
  /** A roster in the task grouping, drawn over IMPL. */
  function drawn(impl = {}): HTMLElement {
    const sc = sidebarContext(appContext(impl));
    return drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1", label: "ship" })] }), sc);
  }

  it("stays folded until the actions control is clicked", () => {
    expect((taskHead(drawn()).querySelector(".sb-menu") as HTMLElement).hidden).toBe(true);
  });

  it("opens when the actions control is clicked", async () => {
    const head = taskHead(drawn());
    await click(head.querySelector(".sb-more") as Element);
    expect((head.querySelector(".sb-menu") as HTMLElement).hidden).toBe(false);
  });

  it("folds away again on a second click of the actions control", async () => {
    const head = taskHead(drawn());
    await click(head.querySelector(".sb-more") as Element);
    await click(head.querySelector(".sb-more") as Element);
    expect((head.querySelector(".sb-menu") as HTMLElement).hidden).toBe(true);
  });

  it("retitles on Enter in the rename field, without reaching for the button", async () => {
    let title = "";
    const head = taskHead(
      drawn({
        updateTask: (request: { change: { case?: string; value?: { title: string } } }) => {
          title = request.change.case === "setTitle" ? (request.change.value?.title ?? "") : "";
          return UPDATE_OK;
        },
      }),
    );
    const input = head.querySelector("[data-task-rename]") as HTMLInputElement;
    input.value = "renamed by keyboard";
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter", bubbles: true }));
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
    expect(title).toBe("renamed by keyboard");
  });

  it("ignores a key that is not Enter in the rename field", async () => {
    let calls = 0;
    const head = taskHead(
      drawn({
        updateTask: () => {
          calls += 1;
          return UPDATE_OK;
        },
      }),
    );
    const input = head.querySelector("[data-task-rename]") as HTMLInputElement;
    input.value = "half typed";
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "a", bubbles: true }));
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
    expect(calls).toBe(0);
  });

  it("marks a task done through the set_done arm", async () => {
    let arm = "";
    const head = taskHead(
      drawn({
        updateTask: (request: { change: { case?: string } }) => {
          arm = request.change.case ?? "";
          return UPDATE_OK;
        },
      }),
    );
    await click(head.querySelector("[data-task-change='setDone']") as Element);
    expect(arm).toBe("setDone");
  });

  it("reopens a task through the set_open arm", async () => {
    let arm = "";
    const head = taskHead(
      drawn({
        updateTask: (request: { change: { case?: string } }) => {
          arm = request.change.case ?? "";
          return UPDATE_OK;
        },
      }),
    );
    await click(head.querySelector("[data-task-change='setOpen']") as Element);
    expect(arm).toBe("setOpen");
  });

  it("words a menu refusal from the task verbs' own table, at that control", async () => {
    const head = taskHead(
      drawn({
        updateTask: () =>
          create(UpdateTaskResponseSchema, {
            result: { case: "error", value: { cause: { case: "noChange", value: {} } } },
          }),
      }),
    );
    const button = head.querySelector("[data-task-change='setDone']") as HTMLElement;
    await click(button);
    expect(button.nextElementSibling?.textContent).toContain("exactly as it is");
  });
});

describe("a recently-merged section with no rows box", () => {
  it("is a malformed view, never an empty band", () => {
    const malformed = create(WorkspaceRosterSchema, {
      repository: { sections: [] },
      task: { sections: [] },
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
    const head = pane(drawn, "repository").querySelector(`${selector} > .repo-head`) as HTMLElement;

    // Act
    const color = inRail(drawn, () => cascadedValue(head, "color"));

    // Assert
    expect(color).toBe("var(--info-agents)");
  });

  it("leaves a hidden row's name out of the header's purple", () => {
    // Arrange
    const drawn = drawWorkspaceRoster(roster({ hidden: hiddenSection([HIDDEN_ROW]) }), sidebarContext());
    const name = pane(drawn, "repository").querySelector(".hidden-section [data-roster-row='ws-8'] .name") as HTMLElement;

    // Act
    const color = inRail(drawn, () => cascadedValue(name, "color"));

    // Assert
    expect(color).not.toBe("var(--info-agents)");
  });
});

describe("a section's folded count", () => {
  const countOf = (drawn: HTMLElement, selector: string): HTMLElement =>
    pane(drawn, "repository").querySelector(`${selector} .sb-count`) as HTMLElement;

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
    const head = pane(drawn, "repository").querySelector(".repo-section > .repo-head") as HTMLElement;
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

  it("hides the count while a repository section is unfolded", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", count: 2, collapsed: false })] }),
      sidebarContext(),
    );
    expect(inRail(drawn, () => cascadedValue(countOf(drawn, ".repo-section"), "display"))).toBe("none");
  });

  it("draws Recently Merged's count beside its label", () => {
    const drawn = drawWorkspaceRoster(
      roster({ merged: mergedSection([row({ id: "ws-9", status: { case: "merged", value: {} } })], 3) }),
      sidebarContext(),
    );
    const head = pane(drawn, "repository").querySelector(".merged-section > .repo-head") as HTMLElement;
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

  it("draws the count in the label's own color", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", count: 2, collapsed: true })] }),
      sidebarContext(),
    );
    expect(
      inRail(drawn, () => [
        cascadedValue(countOf(drawn, ".repo-section"), "color"),
        cascadedValue(countOf(drawn, ".repo-section").previousElementSibling!, "color"),
      ]),
    ).toEqual(["var(--repo-head-collapsed)", "var(--repo-head-collapsed)"]);
  });

  it("refuses a header whose count is unset", () => {
    const section = repoSection({ id: "repo-1" });
    section.header!.count = undefined;
    expect(() => drawWorkspaceRoster(roster({ repos: [section] }), sidebarContext())).toThrow(MalformedView);
  });

  it("draws no count on a task section header", () => {
    const drawn = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "t-1" })] }), sidebarContext());
    expect(pane(drawn, "task").querySelector(".task-head .sb-count")).toBeNull();
  });
});

describe("the open Recently Merged band's today count", () => {
  const mergedAt = (id: string, atMs: number) =>
    row({ id, closed: true, status: { case: "merged", value: {} }, when: { case: "merged", value: { atMs: BigInt(atMs) } } });
  const drawBand = (rows = [mergedAt("ws-1", NOW)], collapsed = false, sc = sidebarContext()): HTMLElement =>
    drawWorkspaceRoster(roster({ merged: mergedSection(rows, rows.length, collapsed) }), sc);
  const todayCount = (drawn: HTMLElement): HTMLElement =>
    pane(drawn, "repository").querySelector(`.merged-section > .repo-head .${MERGED_TODAY_COUNT_CLASS}`) as HTMLElement;

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
    const rows = [...pane(drawn, "repository").querySelectorAll<HTMLElement>(".merged-section .merged-group > .rows > *")];
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
    pane(drawn, "repository").querySelector(".merged-section > .repo-head") as HTMLElement;
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
    const repository = pane(drawn, "repository");
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
    ...pane(drawn, "repository").querySelectorAll<HTMLElement>(".merged-section > .rows > .merged-group"),
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
    expect(pane(drawn, "repository").querySelector(".merged-section")?.getAttribute("data-merged-empty")).toBe("true");
  });

  it("takes a repository's group out at once when the repository section is folded", async () => {
    const { sc } = viewDaemon();
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "beta" })], merged: twoGroups() }),
      sc,
    );
    await click(pane(drawn, "repository").querySelector(".repo-section [data-section-fold]") as Element);
    expect(groupsIn(drawn).map((g) => g.hidden)).toEqual([false, true]);
  });

  it("puts a repository's group back at once when the repository section is unfolded", async () => {
    const { sc } = viewDaemon();
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "beta", collapsed: true })], merged: twoGroups(true) }),
      sc,
    );
    await click(pane(drawn, "repository").querySelector(".repo-section [data-section-fold]") as Element);
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
    pane(drawn, "repository").querySelector(".merged-section [data-roster-row='ws-9']") as HTMLElement;

  it("draws no status dot", () => {
    expect(mergedRow(drawnMerged()).querySelector(".st")).toBeNull();
  });

  it("draws the name in the repository names' colour, not the viewed grey", () => {
    const name = mergedRow(drawnMerged()).querySelector(".name");
    expect([name?.classList.contains("band-name"), name?.classList.contains("viewed")]).toEqual([true, false]);
  });

  it("styles a merged name with the repository headers' colour variable", async () => {
    const css = (await import("../../src/styles.css?raw")).default;
    expect(css).toContain("#ws-sidebar .row .name.band-name { color: var(--repo-head-expanded); }");
  });

  it("draws a row merged today with the off-white gutter bar", async () => {
    const css = (await import("../../src/styles.css?raw")).default;
    expect(css).toContain("#ws-sidebar .row.merged-today {\n  border-left: var(--row-gutter-w) solid var(--merged-today-gutter);");
  });

  it("declares the one rail gutter width every row bar draws in", async () => {
    const css = (await import("../../src/styles.css?raw")).default;
    expect(css).toContain("#ws-sidebar { --row-gutter-w: 3px; }");
  });

  it("draws the selection's bar and the merge-today bar at the shared width", async () => {
    const css = (await import("../../src/styles.css?raw")).default;
    const bar = (selector: string): string | undefined =>
      css.split(`${selector} {\n`)[1]?.split("}")[0];
    expect([bar("#ws-sidebar .ws.current > .row"), bar("#ws-sidebar .row.merged-today")].map((b) =>
      b?.includes("border-left: var(--row-gutter-w) solid") && b.includes("padding-left: calc(0.9rem - var(--row-gutter-w))"),
    )).toEqual([true, true]);
  });

  it("no longer colours a name merged today", async () => {
    const css = (await import("../../src/styles.css?raw")).default;
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
    expect(pane(drawn, "repository").querySelector(".repo-section [data-roster-row='ws-1'] .st")).not.toBeNull();
  });
});

describe("the repository label's divergence hover", () => {
  function labelTitle(divergence: Parameters<typeof repoSection>[0]["divergence"]): string {
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1", divergence })] }), sidebarContext());
    const label = pane(drawn, "repository").querySelector<HTMLElement>(".repo-section .sb-label");
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
    const heads = [...pane(drawn, "repository").querySelectorAll<HTMLElement>(".repo > .repo-head")];
    expect(heads.length).toBeGreaterThanOrEqual(3);
    for (const head of heads) {
      expect([...head.children].slice(0, 2).map((c) => c.className)).toEqual(["tri", SECTION_LABEL_CLASS]);
    }
  });
});

describe("the open workspaces section", () => {
  const openIn = (drawn: HTMLElement, grouping: "repository" | "task" | "maturity"): HTMLElement =>
    pane(drawn, grouping).querySelector<HTMLElement>(`:scope > .${OPEN_SECTION_CLASS}`)!;
  const openCount = (drawn: HTMLElement, grouping: "repository" | "task" | "maturity" = "repository"): HTMLElement =>
    openIn(drawn, grouping).querySelector<HTMLElement>(`:scope > .repo-head > .${OPEN_COUNT_CLASS}`)!;
  const MANY = (): ReturnType<typeof roster> =>
    roster({
      repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" }), row({ id: "ws-2" })] })],
      tasks: [taskSection({ id: "task-1", rows: [row({ id: "ws-1" })] })],
      maturity: [maturitySection({ stage: "inProgress", rows: [row({ id: "ws-1" }), row({ id: "ws-2" })] })],
      openCounts: { repository: 3, task: 1, maturity: 2 },
    });

  it("is the first box of every pane, holding that grouping's sections", () => {
    // Arrange, Act
    const drawn = drawWorkspaceRoster(MANY(), sidebarContext());
    // Assert
    for (const grouping of ["repository", "task", "maturity"] as const) {
      const section = openIn(drawn, grouping);
      expect([pane(drawn, grouping).firstElementChild === section, section.lastElementChild?.className]).toEqual([true, "sb-sections"]);
    }
  });

  it("draws the daemon's label in its header", () => {
    const drawn = drawWorkspaceRoster(MANY(), sidebarContext());
    expect(openIn(drawn, "repository").querySelector(`:scope > .repo-head > .${SECTION_LABEL_CLASS}`)?.textContent).toBe("Open");
  });

  it("draws each grouping's own count beside the label", () => {
    const drawn = drawWorkspaceRoster(MANY(), sidebarContext());
    expect((["repository", "task", "maturity"] as const).map((g) => openCount(drawn, g).textContent)).toEqual(["(3)", "(1)", "(2)"]);
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
    const header = openIn(drawn, "repository").querySelector<HTMLElement>(":scope > .repo-head")!;
    expect(inRail(drawn, () => cascadedValue(header, "color"))).toBe("var(--info-agents)");
  });

  it.each([
    [false, "block"],
    [true, "none"],
  ])("folded=%s shows the grouping's sections as %s", (collapsed, want) => {
    const drawn = drawWorkspaceRoster(roster({ open: openSection(collapsed), repos: [repoSection({ id: "repo-1" })] }), sidebarContext());
    const sections = openIn(drawn, "repository").querySelector<HTMLElement>(":scope > .sb-sections")!;
    expect(inRail(drawn, () => cascadedValue(sections, "display") ?? "block")).toBe(want);
  });

  it("folds every pane's copy at once", async () => {
    // Arrange
    const { sc } = viewDaemon();
    const drawn = drawWorkspaceRoster(roster(), sc);
    // Act
    await click(openIn(drawn, "repository").querySelector("[data-section-fold]") as Element);
    // Assert
    expect([...drawn.querySelectorAll(`.${OPEN_SECTION_CLASS}`)].map((o) => o.classList.contains("folded"))).toEqual([true, true, true]);
  });

  it("leaves the repository sections' own folds alone when it folds", async () => {
    // Arrange
    const { sc } = viewDaemon();
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })] }), sc);
    // Act
    await click(openIn(drawn, "repository").querySelector("[data-section-fold]") as Element);
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

  it.each(["repository", "task", "maturity"] as const)("refuses a %s view without its open count", (grouping) => {
    const bare = roster();
    bare[grouping]!.openCount = undefined;
    expect(() => drawWorkspaceRoster(bare, sidebarContext())).toThrow(MalformedView);
  });

  it("refuses a section with no fold arm", () => {
    const bare = roster();
    bare.open!.fold = { case: undefined };
    expect(() => drawWorkspaceRoster(bare, sidebarContext())).toThrow(MalformedView);
  });
});
