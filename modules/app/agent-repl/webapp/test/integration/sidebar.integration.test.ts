/**
 * SIDEBAR — the roster, its two groupings, and every workspace verb.
 *
 * The roster is the ONE global stream, and both groupings arrive resolved as
 * siblings: which one renders is a webview-local preference, not wire state,
 * and so are the folds. That split is what most of this file asserts.
 *
 * The blink cadence gets its own section because it is specified exactly once,
 * on `RosterRowAttention` — two blinks at 500 ms on/off, then steady — and
 * divergence from the Emacs tab bar is a defect. Fake timers make that
 * assertable rather than a matter of watching it.
 */
import { afterEach, describe, expect, it } from "vitest";

import { RosterRowSchema, RosterRowWhenSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";

import { startHarness, type Harness } from "./harness";
import { assertVocabCoversArms, RENDER_COLORS, mergeGlyph, rosterStatusColor } from "./vocab";
import {
  ROSTER_MERGE_ARMS,
  ROSTER_STATUS_ARMS,
  WORKSPACE_ID,
  assertCoversOneof,
  roster,
  rosterRow,
} from "./fixtures";

let harness: Harness;

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

  it("covers both when-column arms", () => {
    assertCoversOneof(RosterRowWhenSchema, "shown", ["lastSelected", "merged"]);
  });

  it("gives every merge arm a glyph rather than a color", () => {
    // Assert: the merge pipeline deliberately spends no lifecycle color.
    for (const arm of ROSTER_MERGE_ARMS) {
      expect(rosterStatusColor(arm)).toBe("none");
      expect(mergeGlyph(arm)).not.toBe("");
    }
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

describe("folds", () => {
  it("folds a section on click", async () => {
    // Arrange
    await withRoster({});
    // Act
    await harness.click("[data-section-fold]");
    // Assert
    expect(harness.$("[data-section-fold]")?.dataset.folded).toBe("true");
  });

  it("makes no rpc call to fold a section", async () => {
    // Arrange
    await withRoster({});
    harness.fake.clearCalls();
    // Act
    await harness.click("[data-section-fold]");
    // Assert
    expect(harness.fake.log()).toEqual([]);
  });

  it("keeps the fold across a re-push", async () => {
    // Arrange
    await withRoster({});
    await harness.click("[data-section-fold]");
    // Act: R2 — the user's toggle wins after the first draw.
    harness.fake.setRoster(roster({}));
    await harness.settle();
    // Assert
    expect(harness.$("[data-section-fold]")?.dataset.folded).toBe("true");
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

  it("recedes a closed row", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ closed: true })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"]`)?.dataset.closed).toBe("true");
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
  it("draws a relative age from the last-selected arm", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ when: "lastSelected", whenAtMs: 0n })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-when]`)?.dataset.when).toBe(
      "lastSelected",
    );
  });

  it("draws a relative age from the merged arm", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ when: "merged", whenAtMs: 0n })] });
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-when]`)?.dataset.when).toBe("merged");
  });

  it("ages the figure as time passes", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ when: "lastSelected", whenAtMs: 0n })] });
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
