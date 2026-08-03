/**
 * The `workspaceRoster` frame contract: decode + loud validation of
 * agentshim.frontend.v1.WorkspaceRoster, and the completeness of the
 * status oneof's arm vocabulary against the sidebar's existing status set.
 *
 * Shape only — this wave carries no rendering. One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import {
  ROSTER_ROW_STATUS_CASES,
  ROSTER_ROW_STATUS_KEYWORD,
  decodeFrontendFrame,
} from "../src/frontend-proto.js";
import { WORKSPACE_STATUSES } from "../src/sidebar.js";

/** Wrap a plain object as a protojson string and decode it. */
function decode(obj: unknown): ReturnType<typeof decodeFrontendFrame> {
  return decodeFrontendFrame(JSON.stringify(obj));
}

/** A row's fields WITHOUT a status: no arm set, which is invalid on its own. */
const ARMLESS_ROW = {
  dir: "/worktrees/alpha",
  name: "alpha",
  current: true,
};

/**
 * A row carrying exactly the named status arm. Arms are payload-free, so the
 * arm's value is the empty message protojson spells `{}`.
 */
function row(status: string, overrides: Record<string, unknown> = {}): Record<string, unknown> {
  return { ...ARMLESS_ROW, [status]: {}, ...overrides };
}

const ROW = row("ready");

/** A minimal-but-valid roster in the repository grouping. */
function roster(overrides: Record<string, unknown> = {}): unknown {
  return {
    workspaceRoster: {
      revision: "7",
      repository: { sections: [{ repoKey: "repo", folded: false, rows: [ROW] }] },
      currentDir: "/worktrees/alpha",
      ...overrides,
    },
  };
}

/** Narrow a decoded frame to its roster payload, failing loudly otherwise. */
function rosterOf(obj: unknown) {
  const frame = decode(obj);
  if (frame.frame.case !== "workspaceRoster") {
    throw new Error(`expected workspaceRoster, got ${frame.frame.case}`);
  }
  return frame.frame.value;
}

describe("decodeFrontendFrame — workspaceRoster arm", () => {
  it("decodes the roster frame variant", () => {
    // Arrange + Act.
    const frame = decode(roster());

    // Assert.
    expect(frame.frame.case).toBe("workspaceRoster");
  });

  it("parses the int64 revision from its protojson string form", () => {
    // Arrange + Act.
    const value = rosterOf(roster({ revision: "9007" }));

    // Assert.
    expect(value.revision).toBe(9007);
  });

  it("carries the repository grouping as the set view arm", () => {
    // Arrange + Act.
    const value = rosterOf(roster());

    // Assert.
    expect(value.view.case).toBe("repository");
  });

  it("carries the task grouping as the set view arm", () => {
    // Arrange + Act.
    const value = rosterOf(
      roster({
        repository: undefined,
        task: { sections: [{ taskId: "t1", title: "Ship it", done: false, rows: [ROW] }] },
      }),
    );

    // Assert.
    expect(value.view.case).toBe("task");
  });

  it("rejects a roster that sets no view arm", () => {
    // Arrange + Act + Assert.
    expect(() => decode(roster({ repository: undefined }))).toThrow(/no view variant/);
  });

  it("rejects a roster that sets both view arms", () => {
    // Arrange + Act + Assert.
    expect(() => decode(roster({ task: { sections: [] } }))).toThrow(/both view arms/);
  });

  it("rejects a roster with an unrecognized field", () => {
    // Arrange + Act + Assert.
    expect(() => decode(roster({ bogus: 1 }))).toThrow(/WorkspaceRoster has unrecognized field/);
  });

  it("defaults an absent recently-merged section to an empty row list", () => {
    // Arrange + Act.
    const value = rosterOf(roster());

    // Assert.
    expect(value.recentlyMerged.rows).toEqual([]);
  });

  it("defaults an absent recently-merged fold to unfolded", () => {
    // Arrange + Act.
    const value = rosterOf(roster());

    // Assert.
    expect(value.recentlyMerged.folded).toBe(false);
  });

  it("defaults an absent recently-merged label to the empty string", () => {
    // Arrange + Act.
    const value = rosterOf(roster());

    // Assert.
    expect(value.recentlyMerged.label).toBe("");
  });

  it("decodes the recently-merged section's fold state", () => {
    // Arrange + Act.
    const value = rosterOf(roster({ recentlyMerged: { rows: [], folded: true } }));

    // Assert.
    expect(value.recentlyMerged.folded).toBe(true);
  });

  it("decodes the recently-merged section's display label", () => {
    // Arrange + Act.
    const value = rosterOf(roster({ recentlyMerged: { rows: [], label: "Recently Merged" } }));

    // Assert.
    expect(value.recentlyMerged.label).toBe("Recently Merged");
  });

  it("rejects a non-boolean recently-merged fold", () => {
    // Arrange + Act + Assert.
    expect(() => decode(roster({ recentlyMerged: { rows: [], folded: "yes" } }))).toThrow(
      /folded must be a boolean/,
    );
  });

  it("rejects a non-string recently-merged label", () => {
    // Arrange + Act + Assert.
    expect(() => decode(roster({ recentlyMerged: { rows: [], label: 7 } }))).toThrow(
      /label must be a string/,
    );
  });

  it("decodes a repo section's display label", () => {
    // Arrange + Act.
    const value = rosterOf(
      roster({ repository: { sections: [{ repoKey: "/p/.git", label: "p", rows: [ROW] }] } }),
    );

    // Assert.
    if (value.view.case !== "repository") throw new Error("wrong view arm");
    expect(value.view.value.sections[0]?.label).toBe("p");
  });

  it("defaults an absent repo section label to the empty string", () => {
    // Arrange + Act.
    const value = rosterOf(roster());

    // Assert.
    if (value.view.case !== "repository") throw new Error("wrong view arm");
    expect(value.view.value.sections[0]?.label).toBe("");
  });

  it("rejects a non-string repo section label", () => {
    // Arrange + Act + Assert.
    expect(() =>
      decode(roster({ repository: { sections: [{ repoKey: "r", label: 7, rows: [] }] } })),
    ).toThrow(/RosterRepoSection.label must be a string/);
  });

  it("decodes the recently-merged section shared by both views", () => {
    // Arrange + Act.
    const value = rosterOf(
      roster({
        recentlyMerged: {
          rows: [row("merged", { dir: "/worktrees/old" })],
        },
      }),
    );

    // Assert.
    expect(value.recentlyMerged.rows[0]?.status.case).toBe("merged");
  });

  it("defaults an absent nav dir to the empty string", () => {
    // Arrange + Act.
    const value = rosterOf(roster());

    // Assert.
    expect(value.navDir).toBe("");
  });

  it("keeps a folded section's rows in the model", () => {
    // Arrange + Act.
    const value = rosterOf(
      roster({ repository: { sections: [{ repoKey: "repo", folded: true, rows: [ROW] }] } }),
    );

    // Assert.
    if (value.view.case !== "repository") throw new Error("wrong view arm");
    expect(value.view.value.sections[0]?.rows).toHaveLength(1);
  });

  it("decodes a task section's done axis", () => {
    // Arrange + Act.
    const value = rosterOf(
      roster({
        repository: undefined,
        task: { sections: [{ taskId: "t1", title: "Ship it", done: true, rows: [] }] },
      }),
    );

    // Assert.
    if (value.view.case !== "task") throw new Error("wrong view arm");
    expect(value.view.value.sections[0]?.done).toBe(true);
  });

  it("decodes nested child rows recursively", () => {
    // Arrange + Act.
    const value = rosterOf(
      roster({
        repository: {
          sections: [
            {
              repoKey: "repo",
              folded: false,
              rows: [{ ...ROW, children: [row("ready", { dir: "/worktrees/child", current: false })] }],
            },
          ],
        },
      }),
    );

    // Assert.
    if (value.view.case !== "repository") throw new Error("wrong view arm");
    expect(value.view.value.sections[0]?.rows[0]?.children[0]?.dir).toBe("/worktrees/child");
  });

  it("rejects a row with an unrecognized field", () => {
    // Arrange + Act + Assert.
    expect(() =>
      decode(roster({ repository: { sections: [{ repoKey: "r", rows: [{ ...ROW, bogus: 1 }] }] } })),
    ).toThrow(/unrecognized field/);
  });
});

describe("RosterRow.status — the set arm is the status", () => {
  /** Wrap rows in a roster that sets the repository grouping. */
  function withRows(rows: unknown[]): unknown {
    return roster({ repository: { sections: [{ repoKey: "r", rows }] } });
  }

  it("decodes the set arm as the row's status", () => {
    // Arrange + Act.
    const value = rosterOf(withRows([row("vendorBlocked")]));

    // Assert.
    if (value.view.case !== "repository") throw new Error("wrong view arm");
    expect(value.view.value.sections[0]?.rows[0]?.status.case).toBe("vendorBlocked");
  });

  it("rejects a row that sets no status arm", () => {
    // Arrange + Act + Assert.
    expect(() => decode(withRows([ARMLESS_ROW]))).toThrow(/sets no status arm/);
  });

  it("rejects a row that sets more than one status arm", () => {
    // Arrange + Act + Assert.
    expect(() => decode(withRows([row("ready", { thinking: {} })]))).toThrow(
      /sets multiple status arms/,
    );
  });

  it("rejects a status arm outside the closed vocabulary", () => {
    // Arrange + Act + Assert.
    expect(() => decode(withRows([row("napping")]))).toThrow(/unrecognized field/);
  });

  it("rejects a status arm carrying a payload, since every arm is empty", () => {
    // Arrange + Act + Assert.
    expect(() => decode(withRows([row("ready", { ready: { since: 1 } })]))).toThrow(
      /unrecognized field/,
    );
  });
});

describe("RosterRow — the rail's display fields", () => {
  /** Decode a single row carrying OVERRIDES, and hand back the decoded row. */
  function rowOf(overrides: Record<string, unknown>) {
    const value = rosterOf(
      roster({ repository: { sections: [{ repoKey: "r", rows: [row("ready", overrides)] }] } }),
    );
    if (value.view.case !== "repository") throw new Error("wrong view arm");
    const decoded = value.view.value.sections[0]?.rows[0];
    if (decoded === undefined) throw new Error("no row decoded");
    return decoded;
  }

  it("parses the int64 last-viewed stamp from its protojson string form", () => {
    // Arrange + Act.
    const decoded = rowOf({ lastViewedAtMs: "1750000000000" });

    // Assert.
    expect(decoded.lastViewedAtMs).toBe(1750000000000);
  });

  it("defaults an absent last-viewed stamp to zero, meaning never viewed", () => {
    // Arrange + Act.
    const decoded = rowOf({});

    // Assert.
    expect(decoded.lastViewedAtMs).toBe(0);
  });

  it("rejects a last-viewed stamp that is neither number nor numeric string", () => {
    // Arrange + Act + Assert.
    expect(() => rowOf({ lastViewedAtMs: true })).toThrow(/lastViewedAtMs must be a number/);
  });

  it("parses the int64 merged stamp from its protojson string form", () => {
    // Arrange + Act.
    const decoded = rowOf({ mergedAtMs: "1750000001000" });

    // Assert.
    expect(decoded.mergedAtMs).toBe(1750000001000);
  });

  it("defaults an absent merged stamp to zero, meaning not merged", () => {
    // Arrange + Act.
    const decoded = rowOf({});

    // Assert.
    expect(decoded.mergedAtMs).toBe(0);
  });

  it("decodes the row's branch", () => {
    // Arrange + Act.
    const decoded = rowOf({ branch: "feat/alpha" });

    // Assert.
    expect(decoded.branch).toBe("feat/alpha");
  });

  it("defaults an absent branch to the empty string, meaning unknown", () => {
    // Arrange + Act.
    const decoded = rowOf({});

    // Assert.
    expect(decoded.branch).toBe("");
  });

  it("rejects a non-string branch", () => {
    // Arrange + Act + Assert.
    expect(() => rowOf({ branch: 7 })).toThrow(/branch must be a string/);
  });

  it("decodes the row's parent branch", () => {
    // Arrange + Act.
    const decoded = rowOf({ parentBranch: "master" });

    // Assert.
    expect(decoded.parentBranch).toBe("master");
  });

  it("defaults an absent parent branch to the empty string", () => {
    // Arrange + Act.
    const decoded = rowOf({});

    // Assert.
    expect(decoded.parentBranch).toBe("");
  });

  it("decodes the row's summary", () => {
    // Arrange + Act.
    const decoded = rowOf({ summary: "wiring the rail" });

    // Assert.
    expect(decoded.summary).toBe("wiring the rail");
  });

  it("defaults an absent summary to the empty string, meaning none", () => {
    // Arrange + Act.
    const decoded = rowOf({});

    // Assert.
    expect(decoded.summary).toBe("");
  });

  it("decodes the closed flag that recedes a row", () => {
    // Arrange + Act.
    const decoded = rowOf({ closed: true });

    // Assert.
    expect(decoded.closed).toBe(true);
  });

  it("defaults an absent closed flag to false", () => {
    // Arrange + Act.
    const decoded = rowOf({});

    // Assert.
    expect(decoded.closed).toBe(false);
  });

  it("rejects a non-boolean closed flag", () => {
    // Arrange + Act + Assert.
    expect(() => rowOf({ closed: "yes" })).toThrow(/closed must be a boolean/);
  });

  it("keeps closed orthogonal to the status oneof", () => {
    // Arrange + Act.
    const decoded = rowOf({ closed: true });

    // Assert.
    expect(decoded.status.case).toBe("ready");
  });

  it("carries the display fields down into nested child rows", () => {
    // Arrange + Act.
    const decoded = rowOf({
      children: [row("ready", { dir: "/worktrees/child", branch: "feat/child" })],
    });

    // Assert.
    expect(decoded.children[0]?.branch).toBe("feat/child");
  });
});

describe("RosterRow.status — vocabulary completeness", () => {
  it("covers every status in the sidebar's closed set", () => {
    // Arrange.
    const covered = new Set(ROSTER_ROW_STATUS_CASES.map((c) => ROSTER_ROW_STATUS_KEYWORD[c]));

    // Act.
    const missing = [...WORKSPACE_STATUSES].filter((s) => !covered.has(s));

    // Assert.
    expect(missing).toEqual([]);
  });

  it("adds no status the sidebar's closed set does not have", () => {
    // Arrange + Act.
    const extra = ROSTER_ROW_STATUS_CASES.map((c) => ROSTER_ROW_STATUS_KEYWORD[c]).filter(
      (s) => !WORKSPACE_STATUSES.has(s),
    );

    // Assert.
    expect(extra).toEqual([]);
  });

  it("maps every arm to a distinct keyword", () => {
    // Arrange + Act.
    const keywords = ROSTER_ROW_STATUS_CASES.map((c) => ROSTER_ROW_STATUS_KEYWORD[c]);

    // Assert.
    expect(new Set(keywords).size).toBe(keywords.length);
  });

  it("declares one arm per status the sidebar can carry", () => {
    // Arrange + Act + Assert.
    expect(ROSTER_ROW_STATUS_CASES).toHaveLength(WORKSPACE_STATUSES.size);
  });
});
