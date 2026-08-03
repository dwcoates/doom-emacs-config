/**
 * The `workspaceRoster` frame contract: decode + loud validation of
 * agentshim.frontend.v1.WorkspaceRoster, and the completeness of the
 * RosterRowStatus vocabulary against the sidebar's existing status set.
 *
 * Shape only — this wave carries no rendering. One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import {
  ROSTER_ROW_STATUS_KEYWORD,
  RosterRowStatus,
  decodeFrontendFrame,
} from "../src/frontend-proto.js";
import { WORKSPACE_STATUSES } from "../src/sidebar.js";

/** Wrap a plain object as a protojson string and decode it. */
function decode(obj: unknown): ReturnType<typeof decodeFrontendFrame> {
  return decodeFrontendFrame(JSON.stringify(obj));
}

const ROW = {
  dir: "/worktrees/alpha",
  name: "alpha",
  status: "ROSTER_ROW_STATUS_READY",
  current: true,
};

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
          rows: [{ ...ROW, dir: "/worktrees/old", status: "ROSTER_ROW_STATUS_MERGED" }],
        },
      }),
    );

    // Assert.
    expect(value.recentlyMerged.rows[0]?.status).toBe(RosterRowStatus.MERGED);
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
              rows: [{ ...ROW, children: [{ ...ROW, dir: "/worktrees/child", current: false }] }],
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

describe("RosterRowStatus — no silent fallback", () => {
  it("rejects an explicit UNSPECIFIED row status", () => {
    // Arrange + Act + Assert.
    expect(() =>
      decode(
        roster({
          repository: {
            sections: [
              { repoKey: "r", rows: [{ ...ROW, status: "ROSTER_ROW_STATUS_UNSPECIFIED" }] },
            ],
          },
        }),
      ),
    ).toThrow(/not a lifecycle/);
  });

  it("rejects an absent row status, which protojson cannot tell from UNSPECIFIED", () => {
    // Arrange.
    const { status: _status, ...noStatus } = ROW;

    // Act + Assert.
    expect(() =>
      decode(roster({ repository: { sections: [{ repoKey: "r", rows: [noStatus] }] } })),
    ).toThrow(/not a lifecycle/);
  });

  it("rejects a row status outside the closed vocabulary", () => {
    // Arrange + Act + Assert.
    expect(() =>
      decode(
        roster({
          repository: { sections: [{ repoKey: "r", rows: [{ ...ROW, status: "ROSTER_ROW_STATUS_NAPPING" }] }] },
        }),
      ),
    ).toThrow(/unknown enum value/);
  });
});

describe("RosterRowStatus — vocabulary completeness", () => {
  /** Every enum member except the invalid zero value. */
  const members = Object.values(RosterRowStatus).filter(
    (v): v is RosterRowStatus => typeof v === "number" && v !== RosterRowStatus.UNSPECIFIED,
  );

  it("covers every status in the sidebar's closed set", () => {
    // Arrange.
    const covered = new Set(members.map((m) => ROSTER_ROW_STATUS_KEYWORD[m]));

    // Act.
    const missing = [...WORKSPACE_STATUSES].filter((s) => !covered.has(s));

    // Assert.
    expect(missing).toEqual([]);
  });

  it("adds no status the sidebar's closed set does not have", () => {
    // Arrange + Act.
    const extra = members
      .map((m) => ROSTER_ROW_STATUS_KEYWORD[m])
      .filter((s) => !WORKSPACE_STATUSES.has(s));

    // Assert.
    expect(extra).toEqual([]);
  });

  it("maps every member to a distinct keyword", () => {
    // Arrange + Act.
    const keywords = members.map((m) => ROSTER_ROW_STATUS_KEYWORD[m]);

    // Assert.
    expect(new Set(keywords).size).toBe(keywords.length);
  });
});
