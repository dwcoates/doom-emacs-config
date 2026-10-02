/**
 * SIDEBAR — the roster, its two groupings, and every workspace verb.
 *
 * The roster is the ONE global stream, and both groupings arrive resolved as
 * siblings: which one renders is a webview-local preference, not wire state,
 * and so are a task section's folds. A REPOSITORY section's fold is the
 * daemon's (FoldRepository), because the Emacs tab bar hides a collapsed
 * repository's workspaces off the same pushed arm. That split is what most of
 * this file asserts.
 *
 * The blink cadence gets its own section because it is specified exactly once,
 * on `RosterRowAttention` — two blinks at 500 ms on/off, then steady — and
 * divergence from the Emacs tab bar is a defect. Fake timers make that
 * assertable rather than a matter of watching it.
 */
import type { MergeWorkspaceRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_merge_workspace_pb";
import { afterEach, describe, expect, it, vi } from "vitest";

import { RosterRowSchema, RosterRowWhenSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";

import { bootColdOnce, startHarness, type Harness } from "./harness";
import { PREFS_KEY } from "../../src/sidebar/sidebar";
import {
  assertVocabCoversArms,
  isColoredMergeArm,
  RENDER_COLORS,
  mergeGlyph,
  rosterStatusColor,
} from "./vocab";
import {
  ROSTER_MERGE_ARMS,
  ROSTER_STATUS_ARMS,
  WORKSPACE_ID,
  assertCoversOneof,
  roster,
  rosterRow,
} from "./fixtures";

let harness: Harness;

bootColdOnce();

afterEach(async () => {
  await harness?.stop();
});

/** Boot with one roster already scripted. */
async function withRoster(init: Parameters<typeof roster>[0]): Promise<Harness> {
  harness = await startHarness({ arrange: (fake) => fake.setRoster(roster(init)) });
  return harness;
}

describe("arm coverage", () => {
  it("covers every roster status arm", () => {
    assertCoversOneof(RosterRowSchema, "status", [...ROSTER_STATUS_ARMS]);
  });

  it("gives every status arm a color in the vocabulary", () => {
    assertVocabCoversArms(RENDER_COLORS.roster_status, [...ROSTER_STATUS_ARMS], "roster_status");
  });

  it("covers every when-column arm", () => {
    assertCoversOneof(RosterRowWhenSchema, "shown", [
      "active",
      "created",
      "merged",
    ]);
  });

  it("gives every merge arm a glyph, and a color only where the vocabulary declares one", () => {
    // Assert: the merge pipeline spends no lifecycle color except on the arms
    // colored_merge_arms declares (owner ruling, 2026-09-28).
    for (const arm of ROSTER_MERGE_ARMS) {
      expect(mergeGlyph(arm)).not.toBe("");
      if (!isColoredMergeArm(arm)) expect(rosterStatusColor(arm)).toBe("none");
    }
  });

  it.each([
    ["mergeFailed", "turquoise"],
    ["merging", "purple"],
  ])("paints the colored merge arm %s %s", (arm, color) => {
    expect(rosterStatusColor(arm)).toBe(color);
  });
});

describe("the groupings", () => {
  it("carries both groupings from one push", async () => {
    // Arrange / Act: both arrive resolved as siblings on the same view.
    await withRoster({});
    // Assert
    expect(harness.$$("[data-grouping]").map((el) => el.dataset.grouping).sort()).toEqual([
      "repository",
      "task",
    ]);
  });

  it("renders the repository grouping by default", async () => {
    // Arrange / Act
    await withRoster({});
    // Assert
    expect(harness.$('[data-grouping="repository"]')?.hidden).toBe(false);
  });

  it("hides the grouping the preference does not pick", async () => {
    // Arrange / Act
    await withRoster({});
    // Assert
    expect(harness.$('[data-grouping="task"]')?.hidden).toBe(true);
  });

  it("switches which grouping renders on the local preference", async () => {
    // Arrange
    await withRoster({});
    // Act
    await harness.click('[data-grouping-pick="task"]');
    // Assert
    expect(harness.$('[data-grouping="task"]')?.hidden).toBe(false);
  });

  it("makes no rpc call to switch grouping", async () => {
    // Arrange
    await withRoster({});
    harness.fake.clearCalls();
    // Act
    await harness.click('[data-grouping-pick="task"]');
    // Assert: grouping is webview-local, never wire state.
    expect(harness.fake.log()).toEqual([]);
  });

  it("draws the repository section header verbatim", async () => {
    // Arrange / Act
    await withRoster({});
    // Assert
    expect(harness.$('[data-grouping="repository"]')?.textContent).toContain("doom");
  });

  it("draws the task section header verbatim", async () => {
    // Arrange
    await withRoster({});
    // Act
    await harness.click('[data-grouping-pick="task"]');
    // Assert
    expect(harness.$('[data-grouping="task"]')?.textContent).toContain("the overhaul");
  });
});

/** A task section's fold triangle; task folds are the webview's own. */
const TASK_FOLD = '[data-grouping="task"] [data-section-fold]';

/** A repository section's fold triangle; repository folds are the daemon's. */
const REPOSITORY_FOLD = '[data-grouping="repository"] [data-section-fold]';

describe("task folds", () => {
  it("folds a task section on click", async () => {
    // Arrange
    await withRoster({});
    await harness.click('[data-grouping-pick="task"]');
    // Act
    await harness.click(TASK_FOLD);
    // Assert
    expect(harness.$(TASK_FOLD)?.dataset.folded).toBe("true");
  });

  it("makes no rpc call to fold a task section", async () => {
    // Arrange
    await withRoster({});
    await harness.click('[data-grouping-pick="task"]');
    harness.fake.clearCalls();
    // Act
    await harness.click(TASK_FOLD);
    // Assert
    expect(harness.fake.log()).toEqual([]);
  });

  it("keeps a task fold across a re-push", async () => {
    // Arrange
    await withRoster({});
    await harness.click('[data-grouping-pick="task"]');
    await harness.click(TASK_FOLD);
    // Act: R2 — the user's toggle wins after the first draw.
    harness.fake.setRoster(roster({}));
    await harness.settle();
    // Assert
    expect(harness.$(TASK_FOLD)?.dataset.folded).toBe("true");
  });
});

/** The one repository section, as the daemon pushes it collapsed. */
const COLLAPSED_REPOSITORY = [{ repositoryId: "repo-1", label: "doom", rows: [rosterRow()], collapsed: true }];

describe("repository folds", () => {
  it("asks the daemon to collapse an expanded repository on click", async () => {
    // Arrange
    await withRoster({});
    harness.fake.clearCalls();
    // Act
    await harness.click(REPOSITORY_FOLD);
    // Assert
    const [request] = harness.fake.calls<{ fold: { case?: string } }>("foldRepository");
    expect(request?.fold.case).toBe("collapse");
  });

  it("asks the daemon to expand a collapsed repository on click", async () => {
    // Arrange
    await withRoster({ repositorySections: COLLAPSED_REPOSITORY });
    harness.fake.clearCalls();
    // Act
    await harness.click(REPOSITORY_FOLD);
    // Assert
    const [request] = harness.fake.calls<{ fold: { case?: string } }>("foldRepository");
    expect(request?.fold.case).toBe("expand");
  });

  it("never folds the section itself ahead of the daemon's push", async () => {
    // Arrange
    await withRoster({});
    // Act
    await harness.click(REPOSITORY_FOLD);
    // Assert
    expect(harness.$(REPOSITORY_FOLD)?.dataset.folded).toBe("false");
  });

  it("draws the fold the daemon pushes", async () => {
    // Arrange
    await withRoster({});
    // Act
    harness.fake.setRoster(roster({ repositorySections: COLLAPSED_REPOSITORY }));
    await harness.settle();
    // Assert
    expect(harness.$(REPOSITORY_FOLD)?.dataset.folded).toBe("true");
  });
});

describe.each(ROSTER_STATUS_ARMS)("a %s row", (status) => {
  it("carries its status arm", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ status })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"]`)?.dataset.arm).toBe(status);
  });

  it("paints the color the vocabulary assigns", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ status })] });
    // Assert
    const expected = rosterStatusColor(status);
    const drawn = harness.$(`[data-roster-row="${WORKSPACE_ID}"]`)?.className ?? "";
    expect(drawn).toContain(`tone-${expected}`);
  });
});

describe.each(ROSTER_MERGE_ARMS)("the %s merge arm", (status) => {
  it("draws the glyph the vocabulary names", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ status })] });
    // Assert
    expect(
      harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-glyph]`)?.dataset.glyph,
    ).toBe(mergeGlyph(status));
  });
});

describe("row decoration", () => {
  it("draws the served name verbatim", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ name: "the suite" })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"]`)?.textContent).toContain("the suite");
  });

  it("marks the current row", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ current: true })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"]`)?.dataset.current).toBe("true");
  });

  it("draws the priority badge's label verbatim", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ priority: "P1" })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-priority]`)?.textContent).toBe("P1");
  });

  it("draws no priority badge when none is served", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({})] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-priority]`)).toBeNull();
  });

  it("draws the detail branch verbatim", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ detail: true })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"]`)?.textContent).toContain(
      "overhaul/webapp-integration-suite",
    );
  });

  it("omits the detail lines when the row carries none", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ detail: false })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-detail]`)).toBeNull();
  });

  // A CLOSED ROW NEVER APPEARS IN THE RAIL (owner ruling, 2026-09-14). The
  // daemon still EMITS it — Emacs reconciles its tab set from the `closed`
  // flag — so the omission is this renderer's, applied where a live section's
  // rows are laid out. The recently-merged band is the deliberate exception and
  // has its own tests above.
  it("drops a closed row from the rail rather than receding it", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ closed: true })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"]`)).toBeNull();
  });

  it("hoists a closed row's live child into its place", async () => {
    // Arrange / Act: a live workspace cut from a killed parent's branch is
    // still live, and keeps its row one level up rather than vanishing.
    await withRoster({
      rows: [rosterRow({ closed: true, children: [rosterRow({ id: "ws-child" })] })],
    });
    // Assert
    expect(harness.$('[data-roster-row="ws-child"]')).not.toBeNull();
  });

  it("does not mark an open row closed", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ closed: false })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"]`)?.dataset.closed).toBeUndefined();
  });

  it("nests a child row inside its parent", async () => {
    // Arrange / Act
    await withRoster({
      rows: [rosterRow({ id: "ws-parent", children: [rosterRow({ id: "ws-child" })] })],
    });
    // Assert
    expect(
      harness.$('[data-roster-row="ws-parent"]')?.contains(harness.$('[data-roster-row="ws-child"]')),
    ).toBe(true);
  });

  it("draws the recently merged section", async () => {
    // Arrange / Act
    await withRoster({});
    // Assert
    expect(harness.$('[data-component="sidebar"]')?.textContent).toContain("recently merged");
  });

  it("draws a merged row in the merged section", async () => {
    // Arrange / Act
    await withRoster({});
    // Assert
    expect(harness.$('[data-roster-row="ws-merged"]')).not.toBeNull();
  });
});

describe("the when column", () => {
  it("draws a relative age from the active arm", async () => {
    // Arrange / Act: `active` is what this build's daemon sends — when the
    // workspace LAST DID REAL WORK, not when it was last looked at.
    await withRoster({ rows: [rosterRow({ when: "active", whenAtMs: 0n })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-when]`)?.dataset.when).toBe(
      "active",
    );
  });

  it("draws the active arm as a bare age, with no prefix", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ when: "active", whenAtMs: 0n })] });
    // Assert: only `merged` earns a word in front of its age.
    expect(
      harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-when]`)?.textContent,
    ).not.toContain("merged");
  });

  it("draws a relative age from the created arm", async () => {
    // Arrange / Act: the fallback for a workspace that has never taken a turn,
    // so the column is never blank for one that genuinely has a time to show.
    await withRoster({ rows: [rosterRow({ when: "created", whenAtMs: 0n })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-when]`)?.dataset.when).toBe(
      "created",
    );
  });

  it("draws the created arm as a bare age, with no prefix", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ when: "created", whenAtMs: 0n })] });
    // Assert
    expect(
      harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-when]`)?.textContent,
    ).not.toContain("created");
  });

  it("draws a relative age from the merged arm", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ when: "merged", whenAtMs: 0n })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-when]`)?.dataset.when).toBe("merged");
  });

  it("ages an activity figure as time passes", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ when: "active", whenAtMs: 0n })] });
    const before = harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-when]`)?.textContent;
    // Act
    await harness.tick(120_000);
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-when]`)?.textContent).not.toBe(before);
  });
});

describe("the attention blink cadence", () => {
  /** Whether the marker currently reads as lit. */
  const lit = (): boolean =>
    harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-attention]`)?.dataset.blink === "on";

  it("draws the attention marker when the marker is present", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ attention: true })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-attention]`)).not.toBeNull();
  });

  it("draws no attention marker when it is absent", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ attention: false })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-attention]`)).toBeNull();
  });

  it("starts lit", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ attention: true })] });
    // Assert
    expect(lit()).toBe(true);
  });

  it("goes dark after the first 500 ms", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ attention: true })] });
    // Act
    await harness.tick(500);
    // Assert
    expect(lit()).toBe(false);
  });

  it("lights again for the second blink", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ attention: true })] });
    // Act
    await harness.tick(1_000);
    // Assert
    expect(lit()).toBe(true);
  });

  it("goes dark again ending the second blink", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ attention: true })] });
    // Act
    await harness.tick(1_500);
    // Assert
    expect(lit()).toBe(false);
  });

  it("settles steady after exactly two blinks", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ attention: true })] });
    // Act
    await harness.tick(2_000);
    // Assert
    expect(lit()).toBe(true);
  });

  it("stays steady rather than blinking a third time", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ attention: true })] });
    await harness.tick(2_000);
    // Act
    await harness.tick(500);
    // Assert
    expect(lit()).toBe(true);
  });
});

describe("selecting a workspace", () => {
  it("calls SelectWorkspace on a row click", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ id: "ws-other" })] });
    // Act
    await harness.click('[data-roster-row="ws-other"] [data-select]');
    // Assert
    expect(harness.fake.calls("selectWorkspace")).toHaveLength(1);
  });

  it("echoes the row's own WorkspaceRef", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ id: "ws-other" })] });
    // Act
    await harness.click('[data-roster-row="ws-other"] [data-select]');
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("selectWorkspace");
    expect(request.workspace?.id).toBe("ws-other");
  });

  it("makes no other call for a cross-workspace click", async () => {
    // Arrange: R8 — SelectWorkspace and nothing else.
    await withRoster({ rows: [rosterRow({ id: "ws-other" })] });
    harness.fake.clearCalls();
    // Act
    await harness.click('[data-roster-row="ws-other"] [data-select]');
    // Assert
    expect(harness.fake.log().map((c) => c.rpc)).toEqual(["selectWorkspace"]);
  });
});

/** Each workspace verb, with the rpc it must call. */
const WORKSPACE_VERBS = [
  { verb: "open", rpc: "openWorkspace" as const },
  { verb: "close", rpc: "closeWorkspace" as const },
  { verb: "kill", rpc: "killWorkspace" as const },
  { verb: "nuke", rpc: "nukeWorkspace" as const },
  { verb: "merge", rpc: "mergeWorkspace" as const },
  { verb: "restart", rpc: "restartWorkspace" as const },
  { verb: "priority", rpc: "setWorkspacePriority" as const },
];

describe.each(WORKSPACE_VERBS)("the $verb verb", ({ verb, rpc }) => {
  it("calls its own rpc", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ id: "ws-target" })] });
    // Act
    await harness.click(`[data-roster-row="ws-target"] [data-verb="${verb}"]`);
    // Assert
    expect(harness.fake.calls(rpc)).toHaveLength(1);
  });

  it("echoes the row's own WorkspaceRef", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ id: "ws-target" })] });
    // Act
    await harness.click(`[data-roster-row="ws-target"] [data-verb="${verb}"]`);
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>(rpc);
    expect(request.workspace?.id).toBe("ws-target");
  });
});

describe("the merge verb's source", () => {
  it("asks to merge the row's own branch, closing it once it lands", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ id: "ws-target" })] });
    // Act
    await harness.click('[data-roster-row="ws-target"] [data-verb="merge"]');
    // Assert
    const [request] = harness.fake.calls<MergeWorkspaceRequest>("mergeWorkspace");
    const source = request.source?.source;
    expect(source?.case === "ownBranch" ? { keepOpen: source.value.keepOpen } : source?.case).toEqual({
      keepOpen: false,
    });
  });
});

describe("the task verbs", () => {
  it("calls CreateTask with the typed title", async () => {
    // Arrange
    await withRoster({});
    const input = harness.$("[data-task-title]") as HTMLInputElement;
    input.value = "a new task";
    input.dispatchEvent(new Event("input", { bubbles: true }));
    // Act
    await harness.click("[data-task-create]");
    // Assert
    const [request] = harness.fake.calls<{ title: string }>("createTask");
    expect(request.title).toBe("a new task");
  });

  it("calls UpdateTask with the set_title change", async () => {
    // Arrange
    await withRoster({});
    const input = harness.$('[data-task-rename="task-1"]') as HTMLInputElement;
    input.value = "renamed";
    input.dispatchEvent(new Event("input", { bubbles: true }));
    // Act
    await harness.click('[data-task-change="setTitle"]');
    // Assert
    const [request] = harness.fake.calls<{ change: { case?: string } }>("updateTask");
    expect(request.change.case).toBe("setTitle");
  });

  it("calls UpdateTask with the set_done change", async () => {
    // Arrange
    await withRoster({});
    // Act
    await harness.click('[data-task-change="setDone"]');
    // Assert
    const [request] = harness.fake.calls<{ change: { case?: string } }>("updateTask");
    expect(request.change.case).toBe("setDone");
  });

  it("calls UpdateTask with the set_open change", async () => {
    // Arrange
    await withRoster({});
    // Act
    await harness.click('[data-task-change="setOpen"]');
    // Assert
    const [request] = harness.fake.calls<{ change: { case?: string } }>("updateTask");
    expect(request.change.case).toBe("setOpen");
  });

  it("echoes the task's own TaskRef on an update", async () => {
    // Arrange
    await withRoster({});
    // Act
    await harness.click('[data-task-change="setDone"]');
    // Assert
    const [request] = harness.fake.calls<{ task?: { id: string } }>("updateTask");
    expect(request.task?.id).toBe("task-1");
  });

  it("calls AssignWorkspaceTask with a task when one is picked", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ id: "ws-assign" })] });
    // Act
    await harness.click('[data-roster-row="ws-assign"] [data-assign-task="task-1"]');
    // Assert
    const [request] = harness.fake.calls<{ task?: { id: string } }>("assignWorkspaceTask");
    expect(request.task?.id).toBe("task-1");
  });

  it("calls AssignWorkspaceTask with no task to unassign", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ id: "ws-assign" })] });
    // Act
    await harness.click('[data-roster-row="ws-assign"] [data-assign-task=""]');
    // Assert
    const [request] = harness.fake.calls<{ task?: { id: string } }>("assignWorkspaceTask");
    expect(request.task).toBeUndefined();
  });

  it("echoes the workspace on an assignment", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ id: "ws-assign" })] });
    // Act
    await harness.click('[data-roster-row="ws-assign"] [data-assign-task="task-1"]');
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("assignWorkspaceTask");
    expect(request.workspace?.id).toBe("ws-assign");
  });
});

describe("row omission", () => {
  it("drops a row the next push omits rather than waiting for a deletion event", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ id: "ws-a" }), rosterRow({ id: "ws-b" })] });
    // Act
    harness.fake.setRoster(roster({ rows: [rosterRow({ id: "ws-a" })] }));
    await harness.settle();
    // Assert
    expect(harness.$('[data-roster-row="ws-b"]')).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// THE RESTART IS ONE IMMEDIATE MODE
//
// `RestartWorkspace` — one rpc, one mode, one menu item.
// ---------------------------------------------------------------------------

describe("the restart", () => {
  it("calls RestartWorkspace", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ id: "ws-target" })] });
    // Act
    await harness.click('[data-roster-row="ws-target"] [data-verb="restart"]');
    // Assert
    expect(harness.fake.calls("restartWorkspace")).toHaveLength(1);
  });

  it("echoes the row's own WorkspaceRef", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ id: "ws-target" })] });
    // Act
    await harness.click('[data-roster-row="ws-target"] [data-verb="restart"]');
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("restartWorkspace");
    expect(request.workspace?.id).toBe("ws-target");
  });
});

// ---------------------------------------------------------------------------
// R14: THE RAIL'S PREFERENCES SURVIVE A RELOAD (audit 1, item 16)
//
// "Nothing is persisted client-side except webview-local preferences
// (grouping, folds, panel selection) in `localStorage` behind try/catch." A
// fresh mount with a seeded store is what a reload looks like; a store that
// throws costs the memory and nothing else.
// ---------------------------------------------------------------------------

describe("the remembered preferences", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  /** What the rail has written to its own key. */
  const stored = (): Record<string, unknown> =>
    JSON.parse(window.localStorage.getItem(PREFS_KEY) ?? "{}") as Record<string, unknown>;

  it("stores the grouping the reader picked", async () => {
    // Arrange
    await withRoster({});
    // Act
    await harness.click('[data-grouping-pick="task"]');
    // Assert
    expect(stored().grouping).toBe("task");
  });

  it("renders the stored grouping on a fresh mount", async () => {
    // Arrange: what a reload looks like.
    window.localStorage.setItem(PREFS_KEY, JSON.stringify({ grouping: "task" }));
    // Act
    await withRoster({});
    // Assert
    expect(harness.$('[data-grouping="task"]')?.hidden).toBe(false);
  });

  it("stores the task fold the reader closed", async () => {
    // Arrange
    await withRoster({});
    await harness.click('[data-grouping-pick="task"]');
    // Act
    await harness.click(TASK_FOLD);
    // Assert
    expect(Object.values(stored().folded ?? {})).toContain(true);
  });

  it("draws a stored task fold closed on a fresh mount", async () => {
    // Arrange
    await withRoster({});
    await harness.click('[data-grouping-pick="task"]');
    await harness.click(TASK_FOLD);
    const folded = stored().folded;
    await harness.stop();
    window.localStorage.setItem(PREFS_KEY, JSON.stringify({ grouping: "task", folded }));
    // Act
    await withRoster({});
    // Assert
    expect(harness.$(TASK_FOLD)?.dataset.folded).toBe("true");
  });

  it("takes the default grouping when nothing is stored", async () => {
    // Arrange / Act
    await withRoster({});
    // Assert
    expect(harness.$('[data-grouping="repository"]')?.hidden).toBe(false);
  });

  it("takes the default grouping when the stored value is not JSON", async () => {
    // Arrange
    window.localStorage.setItem(PREFS_KEY, "{not json");
    // Act
    await withRoster({});
    // Assert
    expect(harness.$('[data-grouping="repository"]')?.hidden).toBe(false);
  });

  it("still draws the rail when reading storage throws", async () => {
    // Arrange
    vi.spyOn(window.localStorage, "getItem").mockImplementation(() => {
      throw new Error("site data is disabled");
    });
    // Act
    await withRoster({});
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"]`)).not.toBeNull();
  });

  it("still switches grouping when writing storage throws", async () => {
    // Arrange
    vi.spyOn(window.localStorage, "setItem").mockImplementation(() => {
      throw new Error("site data is disabled");
    });
    await withRoster({});
    // Act
    await harness.click('[data-grouping-pick="task"]');
    // Assert: the memory is lost, the rail is not.
    expect(harness.$('[data-grouping="task"]')?.hidden).toBe(false);
  });

  it("still folds a task section when writing storage throws", async () => {
    // Arrange
    vi.spyOn(window.localStorage, "setItem").mockImplementation(() => {
      throw new Error("site data is disabled");
    });
    await withRoster({});
    await harness.click('[data-grouping-pick="task"]');
    // Act
    await harness.click(TASK_FOLD);
    // Assert
    expect(harness.$(TASK_FOLD)?.dataset.folded).toBe("true");
  });
});

/**
 * THE WIRE'S ORDER IS THE ORDER. The daemon resolved the roster's ordering —
 * priority, recency, whatever it weighed — so the client sorts nothing. A
 * client that re-sorted alphabetically would look tidy and be wrong.
 */
describe("roster ordering", () => {
  it("draws the rows in the order the wire carried them", async () => {
    // Arrange / Act: deliberately neither alphabetical nor priority order.
    await withRoster({
      rows: [
        rosterRow({ id: "ws-zeta", name: "zeta" }),
        rosterRow({ id: "ws-alpha", name: "alpha", priority: "p1" }),
        rosterRow({ id: "ws-mid", name: "mid" }),
      ],
    });
    // Assert
    expect(
      harness
        .$$('[data-grouping="repository"] [data-roster-row]')
        .map((el) => el.dataset.rosterRow),
    ).toEqual(["ws-zeta", "ws-alpha", "ws-mid", "ws-merged"]);
  });

  it("draws the sections in the order the wire carried them", async () => {
    // Arrange / Act
    await withRoster({
      repositorySections: [
        { repositoryId: "repo-z", label: "zulu", rows: [rosterRow({ id: "ws-z" })] },
        { repositoryId: "repo-a", label: "alpha", rows: [rosterRow({ id: "ws-a" })] },
      ],
    });
    // Assert
    // The recently-merged section is the grouping's last, by construction.
    expect(harness.texts('[data-grouping="repository"] .repo-head .sb-label')).toEqual([
      "zulu",
      "alpha",
      "recently merged",
    ]);
  });

  it("keeps a later section's rows after an earlier section's", async () => {
    // Arrange / Act
    await withRoster({
      repositorySections: [
        { repositoryId: "repo-z", label: "zulu", rows: [rosterRow({ id: "ws-z" })] },
        { repositoryId: "repo-a", label: "alpha", rows: [rosterRow({ id: "ws-a" })] },
      ],
    });
    // Assert
    expect(
      harness
        .$$('[data-grouping="repository"] [data-roster-row]')
        .map((el) => el.dataset.rosterRow),
    ).toEqual(["ws-z", "ws-a", "ws-merged"]);
  });
});

/**
 * `WorkspaceRoster.current` IS NOT A JOIN KEY. The highlight is drawn from the
 * ROW's own `RosterRowCurrent.current`, so an unset roster-level pointer is a
 * legitimate state (nothing is current) rather than a malformed view or a
 * reason to compare ids client-side.
 */
describe("a roster with no current workspace", () => {
  it("highlights no row", async () => {
    // Arrange / Act
    await withRoster({ currentUnset: true, rows: [rosterRow({ id: "ws-1", current: false })] });
    // Assert
    expect(harness.$$('[data-roster-row][data-current="true"]')).toEqual([]);
  });

  it("draws the roster rather than refusing it", async () => {
    // Arrange / Act
    await withRoster({ currentUnset: true, rows: [rosterRow({ id: "ws-1", current: false })] });
    // Assert
    expect(harness.$('[data-roster-row="ws-1"]')).not.toBeNull();
  });

  it("reports no malformed view", async () => {
    // Arrange / Act
    await withRoster({ currentUnset: true, rows: [rosterRow({ id: "ws-1", current: false })] });
    // Assert
    expect(harness.failureArms()).toEqual([]);
  });

  it("still highlights the row whose own flag says it is current", async () => {
    // Arrange / Act: the row's flag is the fact, with no pointer above it.
    await withRoster({ currentUnset: true, rows: [rosterRow({ id: "ws-1", current: true })] });
    // Assert
    expect(harness.$('[data-roster-row="ws-1"]')?.dataset.current).toBe("true");
  });
});

describe("a task section's done check", () => {
  it("draws the open state the header carries", async () => {
    // Arrange
    await withRoster({ taskDone: false });
    // Act
    await harness.click('[data-grouping-pick="task"]');
    // Assert
    expect(harness.$('.task-head [data-task-status]')?.dataset.taskStatus).toBe("open");
  });

  it("draws the done state the header carries", async () => {
    // Arrange
    await withRoster({ taskDone: true });
    // Act
    await harness.click('[data-grouping-pick="task"]');
    // Assert
    expect(harness.$('.task-head [data-task-status]')?.dataset.taskStatus).toBe("done");
  });

  it("offers the reopen change on a done task", async () => {
    // Arrange
    await withRoster({ taskDone: true });
    // Act
    await harness.click('[data-grouping-pick="task"]');
    // Assert: the control flips what the check currently says.
    expect(harness.$('.task-head [data-task-status]')?.dataset.taskChange).toBe("setOpen");
  });

  it("offers the complete change on an open task", async () => {
    // Arrange
    await withRoster({ taskDone: false });
    // Act
    await harness.click('[data-grouping-pick="task"]');
    // Assert
    expect(harness.$('.task-head [data-task-status]')?.dataset.taskChange).toBe("setDone");
  });
});
