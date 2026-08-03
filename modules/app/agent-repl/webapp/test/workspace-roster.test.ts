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
