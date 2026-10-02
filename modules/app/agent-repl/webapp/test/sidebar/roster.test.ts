// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { UpdateTaskResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_task_pb";
import { FoldRepositoryResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_fold_repository_pb";
import {
  RosterRepoSectionSchema,
  RosterTaskSectionSchema,
  WorkspaceRosterSchema,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { MERGED_FOLD_KEY, repoFoldKey, taskFoldKey } from "../../src/sidebar/context.js";
import { drawWorkspaceRoster } from "../../src/sidebar/roster.js";
import { cascadedValue, installStylesheet } from "../stylesheet.js";
import {
  appContext,
  memoryPrefs,
  mergedSection,
  repoSection,
  roster,
  row,
  sidebarContext,
  taskSection,
} from "./harness.js";

const UPDATE_OK = create(UpdateTaskResponseSchema, { result: { case: "success", value: {} } });

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

describe("the two groupings", () => {
  it("draws both, because both arrive resolved", () => {
    const drawn = drawWorkspaceRoster(roster(), sidebarContext());
    expect(drawn.querySelectorAll("[data-grouping]").length).toBe(2);
  });

  it("shows the repository grouping by default", () => {
    const drawn = drawWorkspaceRoster(roster(), sidebarContext());
    expect(pane(drawn, "repository").hidden).toBe(false);
  });

  it("hides the grouping the user is not looking at, rather than dropping it", () => {
    const drawn = drawWorkspaceRoster(roster(), sidebarContext());
    expect(pane(drawn, "task").hidden).toBe(true);
  });

  it("honors a remembered preference for the task grouping", () => {
    const sc = sidebarContext(appContext(), memoryPrefs({ grouping: "task" }));
    const drawn = drawWorkspaceRoster(roster(), sc);
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
    const keys = [...pane(drawn, "repository").querySelectorAll(".repo:not(.merged-section)")].map(
      (el) => el.getAttribute("data-section"),
    );
    expect(keys).toEqual([repoFoldKey("b"), repoFoldKey("a")]);
  });

  it("draws the repository's label", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", label: "doom" })] }),
      sidebarContext(),
    );
    expect(pane(drawn, "repository").querySelector(".sb-label")?.textContent).toBe("doom");
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
      ".repo:not(.merged-section)",
    ) as HTMLElement;
    expect(section).not.toBeNull();
    expect(section.hidden).toBe(false);
  });

  it("draws the label of a repository section with no rows", () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-new", label: "just-registered", rows: [] })] }),
      sidebarContext(),
    );
    expect(pane(drawn, "repository").querySelector(".sb-label")?.textContent).toBe(
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
    expect(pane(drawn, "repository").querySelector("[data-section-fold]")).not.toBeNull();
  });

  it.each([
    [false, false],
    [true, true],
  ])("draws a repository section collapsed=%s folded=%s, as the daemon says", (collapsed, folded) => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", collapsed })] }),
      sidebarContext(),
    );
    expect(pane(drawn, "repository").querySelector(".repo")?.classList.contains("folded")).toBe(folded);
  });

  it.each([
    [false, "collapse"],
    [true, "expand"],
  ])("asks the daemon for the other fold on the toggle (collapsed=%s asks %s)", async (collapsed, want) => {
    const asked: Array<{ repository: string | undefined; fold: string | undefined }> = [];
    const sc = sidebarContext(
      appContext({
        foldRepository: (request) => {
          asked.push({ repository: request.repository?.id, fold: request.fold.case });
          return create(FoldRepositoryResponseSchema, { result: { case: "success", value: {} } });
        },
      }),
    );
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1", collapsed })] }), sc);
    await click(pane(drawn, "repository").querySelector("[data-section-fold]") as Element);
    expect(asked).toEqual([{ repository: "repo-1", fold: want }]);
  });

  it("leaves the section as the daemon drew it until the push that answers the toggle", async () => {
    const sc = sidebarContext(
      appContext({
        foldRepository: () => create(FoldRepositoryResponseSchema, { result: { case: "success", value: {} } }),
      }),
    );
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })] }), sc);
    const section = pane(drawn, "repository").querySelector(".repo") as HTMLElement;
    await click(section.querySelector("[data-section-fold]") as Element);
    expect(section.classList.contains("folded")).toBe(false);
  });

  it("draws the daemon's refusal of a fold beside the toggle", async () => {
    const sc = sidebarContext(
      appContext({
        foldRepository: () =>
          create(FoldRepositoryResponseSchema, {
            result: { case: "error", value: { cause: { case: "unknownRepository", value: {} } } },
          }),
      }),
    );
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })] }), sc);
    await click(pane(drawn, "repository").querySelector("[data-section-fold]") as Element);
    expect(pane(drawn, "repository").textContent).toContain("the daemon no longer has that repository");
  });

  it("never folds a repository section from a remembered local fold", () => {
    const prefs = memoryPrefs({ folded: { [repoFoldKey("repo-1")]: true } });
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1" })] }),
      sidebarContext(appContext(), prefs),
    );
    expect(pane(drawn, "repository").querySelector(".repo")?.classList.contains("folded")).toBe(false);
  });

  it.each([
    [false, "var(--repo-head-expanded)"],
    [true, "var(--repo-head-collapsed)"],
  ])("colors a repository header collapsed=%s with %s", (collapsed, want) => {
    const teardown = installStylesheet();
    try {
      const rail = document.createElement("div");
      rail.id = "ws-sidebar";
      rail.appendChild(drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1", collapsed })] }), sidebarContext()));
      document.body.appendChild(rail);
      const header = rail.querySelector(".repo-section > .repo-head") as HTMLElement;
      expect(cascadedValue(header, "color")).toBe(want);
      rail.remove();
    } finally {
      teardown();
    }
  });

  it("marks a repository section so its header takes its fold's grey", () => {
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })] }), sidebarContext());
    expect(pane(drawn, "repository").querySelector(".repo")?.classList.contains("repo-section")).toBe(true);
  });

  it("remembers a task's fold under the task's id, not its title", async () => {
    const prefs = memoryPrefs({ grouping: "task" });
    const drawn = drawWorkspaceRoster(
      roster({ tasks: [taskSection({ id: "task-1", label: "ship" })] }),
      sidebarContext(appContext(), prefs),
    );
    await click(pane(drawn, "task").querySelector("[data-section-fold]") as Element);
    expect(prefs.state.folded[taskFoldKey("task-1")]).toBe(true);
  });

  it("draws a remembered task fold folded on the next push", () => {
    const prefs = memoryPrefs({ grouping: "task", folded: { [taskFoldKey("task-1")]: true } });
    const drawn = drawWorkspaceRoster(
      roster({ tasks: [taskSection({ id: "task-1" })] }),
      sidebarContext(appContext(), prefs),
    );
    expect(pane(drawn, "task").querySelector(".repo")?.classList.contains("folded")).toBe(true);
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

  it("appears under BOTH groupings", () => {
    const drawn = drawWorkspaceRoster(landed(), sidebarContext());
    expect(drawn.querySelectorAll(".merged-section:not([hidden])").length).toBe(2);
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
    const teardown = installStylesheet();
    try {
      const rail = document.createElement("div");
      rail.id = "ws-sidebar";
      rail.appendChild(drawWorkspaceRoster(landed(), sidebarContext()));
      document.body.appendChild(rail);
      const triangle = rail.querySelector(
        ".merged-section.folded [data-section-fold]",
      ) as HTMLElement;
      expect(cascadedValue(triangle, "transform")).toBe("none");
      rail.remove();
    } finally {
      teardown();
    }
  });

  it("remembers its fold under the one fixed key", async () => {
    const prefs = memoryPrefs();
    const drawn = drawWorkspaceRoster(landed(), sidebarContext(appContext(), prefs));
    const section = pane(drawn, "repository").querySelector(".merged-section") as HTMLElement;
    await click(section.querySelector("[data-section-fold]") as Element);
    expect(prefs.state.folded[MERGED_FOLD_KEY]).toBe(false);
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

describe("the task section's header", () => {
  it("draws the done check from the wire", () => {
    const drawn = drawWorkspaceRoster(
      roster({ tasks: [taskSection({ id: "task-1", done: true })] }),
      sidebarContext(appContext(), memoryPrefs({ grouping: "task" })),
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
      }),
      memoryPrefs({ grouping: "task" }),
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
      }),
      memoryPrefs({ grouping: "task" }),
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
      }),
      memoryPrefs({ grouping: "task" }),
    );
    const drawn = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1" })] }), sc);
    await click(pane(drawn, "task").querySelector("[data-task-status]") as Element);
    expect(id).toBe("task-1");
  });

  it("does not fold the section when the check is clicked", async () => {
    const sc = sidebarContext(appContext({ updateTask: () => UPDATE_OK }), memoryPrefs({ grouping: "task" }));
    const drawn = drawWorkspaceRoster(roster({ tasks: [taskSection({ id: "task-1" })] }), sc);
    const section = pane(drawn, "task").querySelector(".task-section") as HTMLElement;
    await click(section.querySelector("[data-task-status]") as Element);
    expect(section.classList.contains("folded")).toBe(false);
  });

  it("offers a rename on the label", () => {
    const drawn = drawWorkspaceRoster(
      roster({ tasks: [taskSection({ id: "task-1" })] }),
      sidebarContext(appContext(), memoryPrefs({ grouping: "task" })),
    );
    expect(
      pane(drawn, "task").querySelector("[data-task-rename]")?.getAttribute("data-task-rename"),
    ).toBe("task-1");
  });

  it("holds the current title in the rename field", () => {
    const drawn = drawWorkspaceRoster(
      roster({ tasks: [taskSection({ id: "task-1", label: "ship" })] }),
      sidebarContext(appContext(), memoryPrefs({ grouping: "task" })),
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
      }),
      memoryPrefs({ grouping: "task" }),
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
      }),
      memoryPrefs({ grouping: "task" }),
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
      }),
      memoryPrefs({ grouping: "task" }),
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
      }),
      memoryPrefs({ grouping: "task" }),
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
    expect(pane(drawn, "repository").querySelector(".repo-head .sb-add")).not.toBeNull();
  });

  it("opens the create form under that section's header", async () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1" })] }),
      sidebarContext(),
    );
    await click(pane(drawn, "repository").querySelector(".repo-head .sb-add") as Element);
    expect(
      pane(drawn, "repository").querySelector(".repo-head")?.nextElementSibling?.hasAttribute(
        "data-create-form",
      ),
    ).toBe(true);
  });

  it("does not fold the section when the create control is clicked", async () => {
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1" })] }),
      sidebarContext(),
    );
    const section = pane(drawn, "repository").querySelector(".repo") as HTMLElement;
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
    const sc = sidebarContext(appContext(impl), memoryPrefs({ grouping: "task" }));
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

describe("a section's folded count", () => {
  /** The rail with the real stylesheet, around one drawn roster. */
  function inRail<T>(drawn: HTMLElement, check: (rail: HTMLElement) => T): T {
    const teardown = installStylesheet();
    const rail = document.createElement("div");
    rail.id = "ws-sidebar";
    rail.appendChild(drawn);
    document.body.appendChild(rail);
    try {
      return check(rail);
    } finally {
      rail.remove();
      teardown();
    }
  }

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

  it("draws the name in the viewed grey", () => {
    expect(mergedRow(drawnMerged()).querySelector(".name")?.classList.contains("viewed")).toBe(true);
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
