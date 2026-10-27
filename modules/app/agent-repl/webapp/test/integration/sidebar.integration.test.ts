/**
 * SIDEBAR — the roster and every workspace verb.
 *
 * The roster is the ONE global stream. Every section's fold and every row's
 * open detail are the DAEMON'S view state, carried on the push so the sidebar looks
 * the same in every workspace's page (owner rulings, 2026-10-06): a gesture
 * paints its page at once and asks UpdateSidebarView, and the push reconciles.
 *
 * The blink cadence gets its own section because it is specified exactly once,
 * on `RosterRowAttention` — two blinks at 500 ms on/off, then steady — and
 * divergence from the Emacs tab bar is a defect. Fake timers make that
 * assertable rather than a matter of watching it.
 */
import type { FinishWorkspaceRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_finish_workspace_pb";
import { afterEach, describe, expect, it, vi } from "vitest";

import { RosterRowSchema, RosterRowWhenSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";

import { bootColdOnce, startHarness, type Harness } from "./harness";
import { PREFS_KEY } from "../../src/sidebar/sidebar";
import type { UpdateSidebarViewRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
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
      "merged",
      "hidden",
      "syncMerging",
      "turnRunning",
      "noTurnRunning",
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
    // A failed merge is green, the workspace back with the user (owner
    // ruling, 2026-10-13); its glyph's red is an ink, not the arm's color.
    ["mergeFailed", "green"],
    ["merging", "purple"],
  ])("paints the colored merge arm %s %s", (arm, color) => {
    expect(rosterStatusColor(arm)).toBe(color);
  });
});

/** A repository section's fold triangle. */
const REPOSITORY_FOLD = '.sb-pane .repo-section [data-section-fold]';

/** The fold the one recorded UpdateSidebarView asked for, and of which section. */
function askedFold(): { section: string | undefined; fold: string | undefined } {
  const [request] = harness.fake.calls<UpdateSidebarViewRequest>("updateSidebarView");
  if (request?.change.case !== "foldSection") return { section: undefined, fold: undefined };
  return { section: request.change.value.section.case, fold: request.change.value.fold.case };
}

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
    expect(askedFold()).toEqual({ section: "repository", fold: "collapse" });
  });

  it("asks the daemon to expand a collapsed repository on click", async () => {
    // Arrange
    await withRoster({ repositorySections: COLLAPSED_REPOSITORY });
    harness.fake.clearCalls();
    // Act
    await harness.click(REPOSITORY_FOLD);
    // Assert
    expect(askedFold()).toEqual({ section: "repository", fold: "expand" });
  });

  it("folds the section at once, ahead of the daemon's push", async () => {
    // Arrange
    await withRoster({});
    // Act
    await harness.click(REPOSITORY_FOLD);
    // Assert
    expect(harness.$(REPOSITORY_FOLD)?.dataset.folded).toBe("true");
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

/** The open workspaces section's own fold triangle, in the repository grouping's copy. */
const OPEN_FOLD = '.sb-pane .open-section > .repo-head [data-section-fold]';

describe("the open workspaces section's fold", () => {
  it("starts unfolded, as the daemon's default is", async () => {
    // Arrange / Act
    await withRoster({});
    // Assert
    expect(harness.$(OPEN_FOLD)?.dataset.folded).toBe("false");
  });

  it("asks the daemon to fold it in every page", async () => {
    // Arrange
    await withRoster({});
    harness.fake.clearCalls();
    // Act
    await harness.click(OPEN_FOLD);
    // Assert
    expect(askedFold()).toEqual({ section: "open", fold: "collapse" });
  });
});

/** The merged band's fold triangle, in the repository grouping's copy. */
const MERGED_FOLD = '.sb-pane .merged-section [data-section-fold]';

describe("the recently-merged band's fold", () => {
  it("starts folded, as the daemon's default is", async () => {
    // Arrange / Act
    await withRoster({});
    // Assert
    expect(harness.$(MERGED_FOLD)?.dataset.folded).toBe("true");
  });

  it("asks the daemon to unfold it in every page", async () => {
    // Arrange
    await withRoster({});
    harness.fake.clearCalls();
    // Act
    await harness.click(MERGED_FOLD);
    // Assert
    expect(askedFold()).toEqual({ section: "recentlyMerged", fold: "expand" });
  });

  it("draws it unfolded when another page unfolded it", async () => {
    // Arrange
    await withRoster({});
    // Act
    harness.fake.setRoster(roster({ mergedCollapsed: false }));
    await harness.settle();
    // Assert
    expect(harness.$(MERGED_FOLD)?.dataset.folded).toBe("false");
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

  it("draws the recently deployed heading", async () => {
    // Arrange / Act
    await withRoster({});
    // Assert
    expect(harness.$('[data-component="sidebar"]')?.textContent).toContain("recently deployed");
  });

  it("draws a merged row in the merged section", async () => {
    // Arrange / Act
    await withRoster({});
    // Assert
    expect(harness.$('[data-roster-row="ws-merged"]')).not.toBeNull();
  });

  it("draws a merged row inside its repository's group", async () => {
    // Arrange / Act
    await withRoster({ mergedCollapsed: false });
    // Assert
    const group = harness.$('.sb-pane .merged-group [data-roster-row="ws-merged"]')?.closest(".merged-group");
    expect(group?.querySelector(".merged-group-head")?.textContent).toBe("doom");
  });

  it("leaves a collapsed repository's merges out of the band, the band still drawn", async () => {
    // Arrange / Act
    await withRoster({ mergedCollapsed: false, mergedRepositoryCollapsed: true });
    // Assert
    const band = harness.$(".sb-pane .merged-section") as HTMLElement | null;
    expect([band?.hidden, band?.querySelector<HTMLElement>(".merged-group")?.hidden]).toEqual([false, true]);
  });
});

describe("the when column", () => {
  const whenOf = (): HTMLElement | null => harness.$(`[data-roster-row="${WORKSPACE_ID}"] [data-when]`);

  it("draws 0s from the no_turn_running arm", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ when: "noTurnRunning" })] });
    // Assert
    expect([whenOf()?.dataset.when, whenOf()?.textContent]).toEqual(["noTurnRunning", "0s"]);
  });

  it("draws the running turn's elapsed time from the turn_running arm", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ when: "turnRunning", whenAtMs: 0n })] });
    // Assert
    expect(whenOf()?.dataset.when).toBe("turnRunning");
  });

  it("draws a relative age from the merged arm", async () => {
    // Arrange / Act
    await withRoster({ rows: [rosterRow({ when: "merged", whenAtMs: 0n })] });
    // Assert
    expect(whenOf()?.dataset.when).toBe("merged");
  });

  it("counts a running turn up as time passes", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ when: "turnRunning", whenAtMs: 0n })] });
    const before = whenOf()?.textContent;
    // Act
    await harness.tick(120_000);
    // Assert
    expect(whenOf()?.textContent).not.toBe(before);
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
  it("calls SelectInEditor on a row click", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ id: "ws-other" })] });
    // Act
    await harness.click('[data-roster-row="ws-other"] [data-select]');
    // Assert
    expect(harness.fake.calls("selectInEditor")).toHaveLength(1);
  });

  it("echoes the row's own WorkspaceRef", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ id: "ws-other" })] });
    // Act
    await harness.click('[data-roster-row="ws-other"] [data-select]');
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("selectInEditor");
    expect(request.workspace?.id).toBe("ws-other");
  });

  it("makes no other call for a cross-workspace click", async () => {
    // Arrange: R8 — SelectInEditor and nothing else.
    await withRoster({ rows: [rosterRow({ id: "ws-other" })] });
    harness.fake.clearCalls();
    // Act
    await harness.click('[data-roster-row="ws-other"] [data-select]');
    // Assert
    expect(harness.fake.log().map((c) => c.rpc)).toEqual(["selectInEditor"]);
  });
});

/** Each workspace verb, with the rpc it must call. */
const WORKSPACE_VERBS = [
  { verb: "open", rpc: "openWorkspace" as const },
  { verb: "close", rpc: "closeWorkspace" as const },
  { verb: "kill", rpc: "killWorkspace" as const },
  { verb: "nuke", rpc: "nukeWorkspace" as const },
  { verb: "finish", rpc: "finishWorkspace" as const },
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

describe("the finish verb's ask", () => {
  it("asks as the user at the back of the queue", async () => {
    // Arrange
    await withRoster({ rows: [rosterRow({ id: "ws-target" })] });
    // Act
    await harness.click('[data-roster-row="ws-target"] [data-verb="finish"]');
    // Assert
    const [request] = harness.fake.calls<FinishWorkspaceRequest>("finishWorkspace");
    expect(request.requester?.asker.case).toBe("user");
    expect(request.placement?.place.case).toBe("back");
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
// THE PREFERENCES A PAGE ONCE STORED ARE RETIRED
//
// The view is the daemon's now (owner rulings, 2026-10-06). A page that still
// carries what it once stored in `localStorage` draws the daemon's view and
// drops what it stored; nothing is migrated.
// ---------------------------------------------------------------------------

describe("the preferences a page once stored", () => {
  afterEach(() => {
    vi.restoreAllMocks();
    window.localStorage.removeItem(PREFS_KEY);
  });

  it("are dropped from storage", async () => {
    // Arrange
    window.localStorage.setItem(PREFS_KEY, JSON.stringify({ grouping: "task", folded: { "task:task-1": true } }));
    // Act
    await withRoster({});
    // Assert
    expect(window.localStorage.getItem(PREFS_KEY)).toBeNull();
  });

  it("still let the rail draw when reading storage throws", async () => {
    // Arrange
    vi.spyOn(window.localStorage, "getItem").mockImplementation(() => {
      throw new Error("site data is disabled");
    });
    // Act
    await withRoster({});
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"]`)).not.toBeNull();
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
        .$$('.sb-pane [data-roster-row]')
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
    // The open workspaces section heads the pane and the Deploys section
    // follows it, by construction (an empty Hidden section is left out).
    expect(harness.texts('.sb-pane .repo-head .sb-label')).toEqual([
      "Open",
      "zulu",
      "alpha",
      "deploys",
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
        .$$('.sb-pane [data-roster-row]')
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
