/**
 * merge-status — the one reading of `WorkspaceState.merge_status` that the
 * footer, the sidebar, and the composer's merge gate all draw from. One edge
 * per test (AAA).
 */
import { describe, expect, it } from "vitest";

import { mergeFacts, mergeStatusLogValue, shortSha } from "../src/merge-status.js";
import type { MergeStatus } from "../src/frontend-proto.js";

/** A merge status carrying PHASE, with the envelope fixed. */
function status(phase: MergeStatus["phase"]): MergeStatus {
  return { runId: "run-1", phaseStartedAtMs: 900, updatedAtMs: 1000, phase };
}

describe("mergeFacts — no merge", () => {
  it("reads a null status as no merge to render", () => {
    // Arrange / Act
    const facts = mergeFacts(null);
    // Assert
    expect(facts).toBeNull();
  });
});

describe("mergeFacts — enqueued", () => {
  const enqueued = status({ case: "enqueued", value: { position: 2, depth: 3 } });

  it("names the phase", () => {
    expect(mergeFacts(enqueued)?.word).toBe("merge queued");
  });

  it("counts the queue place, which no phase word can carry", () => {
    expect(mergeFacts(enqueued)?.count).toBe("2/3");
  });

  it("spells the queue place out for the hover line", () => {
    expect(mergeFacts(enqueued)?.countTitle).toBe(
      "this workspace is 2 of 3 waiting to merge into this repository",
    );
  });

  it("does NOT breathe: a queued merge waits on another workspace", () => {
    expect(mergeFacts(enqueued)?.breathing).toBe(false);
  });

  it("drops the count when the daemon reports no queue depth", () => {
    // Arrange / Act — "0/0" beside a phase word is noise, not a place.
    const facts = mergeFacts(status({ case: "enqueued", value: { position: 0, depth: 0 } }));
    // Assert
    expect(facts?.count).toBe("");
  });
});

describe("mergeFacts — before-action", () => {
  const before = status({ case: "beforeAction", value: { prompt: "run the linter" } });

  it("names the phase the daemon's pre-merge prompt runs in", () => {
    expect(mergeFacts(before)?.word).toBe("merge before-action");
  });

  it("shows the prompt as the activity", () => {
    expect(mergeFacts(before)?.activity).toBe("before-action · run the linter");
  });

  it("breathes: the daemon is driving the session", () => {
    expect(mergeFacts(before)?.breathing).toBe(true);
  });

  it("degrades a missing prompt to the bare label", () => {
    // Arrange / Act — a dangling separator would read as a truncated prompt.
    const facts = mergeFacts(status({ case: "beforeAction", value: { prompt: "" } }));
    // Assert
    expect(facts?.activity).toBe("before-action · running");
  });
});

describe("mergeFacts — cherry-picking", () => {
  const picking = status({
    case: "cherryPicking",
    value: { commitsTotal: 4, commitsLanded: 1, currentSha: "abc1234def", currentSubject: "fix it" },
  });

  it("names the phase", () => {
    expect(mergeFacts(picking)?.word).toBe("merging");
  });

  it("counts the commits landed against the run's total", () => {
    expect(mergeFacts(picking)?.count).toBe("1/4");
  });

  it("shows the commit in hand as the activity", () => {
    expect(mergeFacts(picking)?.activity).toBe("picking · abc1234 fix it");
  });

  it("spells the arithmetic out for the hover line", () => {
    expect(mergeFacts(picking)?.countTitle).toBe("1 of 4 commits landed on the merge target");
  });

  it("drops the activity when the daemon names no commit", () => {
    // Arrange / Act
    const facts = mergeFacts(
      status({
        case: "cherryPicking",
        value: { commitsTotal: 4, commitsLanded: 1, currentSha: "", currentSubject: "" },
      }),
    );
    // Assert
    expect(facts?.activity).toBe("");
  });

  it("drops the count before the run has a plan to count against", () => {
    // Arrange / Act
    const facts = mergeFacts(
      status({
        case: "cherryPicking",
        value: { commitsTotal: 0, commitsLanded: 0, currentSha: "", currentSubject: "" },
      }),
    );
    // Assert
    expect(facts?.count).toBe("");
  });
});

describe("mergeFacts — testing", () => {
  const testing = status({
    case: "testing",
    value: { commitsTotal: 4, commitsLanded: 4, currentSha: "def5678", currentSubject: "last one" },
  });

  it("reads apart from cherry-picking, which shares its payload", () => {
    expect(mergeFacts(testing)?.word).toBe("merge testing");
  });

  it("shows the commit under test as the activity", () => {
    expect(mergeFacts(testing)?.activity).toBe("testing · def5678 last one");
  });
});

describe("mergeFacts — conflict", () => {
  const conflict = status({
    case: "conflict",
    value: {
      conflictedSha: "bad1234",
      conflictedSubject: "touch the same file",
      commitsTotal: 4,
      commitsLanded: 2,
    },
  });

  it("names the phase", () => {
    expect(mergeFacts(conflict)?.word).toBe("merge conflict");
  });

  it("is loud: the run is parked on a human", () => {
    expect(mergeFacts(conflict)?.tone).toBe("error");
  });

  it("names the CONFLICTED commit, which the phase word never did", () => {
    expect(mergeFacts(conflict)?.activity).toBe("conflict · bad1234 touch the same file");
  });

  it("keeps the count of what landed before the conflict", () => {
    expect(mergeFacts(conflict)?.count).toBe("2/4");
  });
});

describe("mergeFacts — after-action", () => {
  const after = status({ case: "afterAction", value: { prompt: "close the workspace" } });

  it("names the phase the daemon's post-merge prompt runs in", () => {
    expect(mergeFacts(after)?.word).toBe("merge after-action");
  });

  it("shows the prompt as the activity", () => {
    expect(mergeFacts(after)?.activity).toBe("after-action · close the workspace");
  });
});

describe("mergeFacts — merged", () => {
  const merged = status({ case: "merged", value: { commitsTotal: 4, afterActionError: "" } });

  it("names the phase", () => {
    expect(mergeFacts(merged)?.word).toBe("merged");
  });

  it("is calm: the run settled", () => {
    expect(mergeFacts(merged)?.tone).toBe("ok");
  });

  it("counts what landed", () => {
    expect(mergeFacts(merged)?.count).toBe("4");
  });

  it("carries no note when the after-action succeeded", () => {
    expect(mergeFacts(merged)?.note).toBe("");
  });

  it("notes a failed after-action", () => {
    // Arrange / Act
    const facts = mergeFacts(
      status({ case: "merged", value: { commitsTotal: 4, afterActionError: "prompt timed out" } }),
    );
    // Assert
    expect(facts?.note).toBe("after-action failed: prompt timed out");
  });

  it("stays CALM despite a failed after-action, because the merge landed", () => {
    // Arrange / Act — painting this red would tell the user to undo work that
    // is already on the target.
    const facts = mergeFacts(
      status({ case: "merged", value: { commitsTotal: 4, afterActionError: "prompt timed out" } }),
    );
    // Assert
    expect(facts?.tone).toBe("ok");
  });

  it("drops the count when the daemon reports none", () => {
    const facts = mergeFacts(status({ case: "merged", value: { commitsTotal: 0, afterActionError: "" } }));
    expect(facts?.count).toBe("");
  });
});

describe("mergeFacts — failed", () => {
  const failed = status({
    case: "failed",
    value: {
      cause: "tests failed",
      commitsTotal: 4,
      commitsLanded: 3,
      failingSha: "fee1234",
      failingSubject: "break the build",
      failedJson:
        '{"cause":"tests failed","commitsTotal":4,"commitsLanded":3,"failingSha":"fee1234","failingSubject":"break the build"}',
    },
  });

  it("names the phase", () => {
    expect(mergeFacts(failed)?.word).toBe("merge failed");
  });

  it("surfaces the daemon's classified cause as the standing note", () => {
    expect(mergeFacts(failed)?.note).toBe("tests failed");
  });

  it("names the commit the run stopped on", () => {
    expect(mergeFacts(failed)?.activity).toBe("failed on · fee1234 break the build");
  });

  it("names the ABSENCE of a cause rather than hiding it", () => {
    // Arrange / Act — a run that stopped with no reason is not a run with no
    // note; the user still needs to know nothing explained it.
    const facts = mergeFacts(
      status({
        case: "failed",
        value: {
          cause: "",
          commitsTotal: 0,
          commitsLanded: 0,
          failingSha: "",
          failingSubject: "",
          failedJson: "{}",
        },
      }),
    );
    // Assert
    expect(facts?.note).toBe("the daemon reported no cause");
  });
});

describe("shortSha", () => {
  it("trims a full sha to the bytes a human reads", () => {
    expect(shortSha("abc1234def5678")).toBe("abc1234");
  });

  it("leaves an absent sha absent", () => {
    expect(shortSha("")).toBe("");
  });
});

describe("mergeStatusLogValue", () => {
  it("names the phase, the run, and the refresh stamp", () => {
    expect(
      mergeStatusLogValue(status({ case: "merged", value: { commitsTotal: 1, afterActionError: "" } })),
    ).toBe("merged/run-1@1000");
  });

  it("reads no merge as 'none'", () => {
    expect(mergeStatusLogValue(null)).toBe("none");
  });
});
