/**
 * merge-gate — the composer's gate while the merge coordinator owns this
 * workspace's session. One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";

import {
  MERGE_GATE_NOTICE,
  mergeGateBlockedLog,
  mergeGateNoticeHtml,
  mergeGateProgress,
  mergeGateSendTitle,
  submitBlocked,
} from "../src/merge-gate.js";
import type { MergeStatus } from "../src/state-adapter.js";

describe("submitBlocked: the lease decides, nothing else", () => {
  it("blocks submission while the merge holds the lease", () => {
    // Arrange / Act
    const got = submitBlocked(true);
    // Assert
    expect(got).toBe(true);
  });

  it("unblocks submission the moment the lease is released", () => {
    // Arrange / Act — the daemon clears the flag on the same revisioned
    // WorkspaceState that set it, so release needs no local unwinding.
    const got = submitBlocked(false);
    // Assert
    expect(got).toBe(false);
  });
});

describe("mergeGateNoticeHtml: the standing explanation", () => {
  it("carries the explanation while the lease is held", () => {
    // Arrange / Act
    const got = mergeGateNoticeHtml(true);
    // Assert
    expect(got).toContain(MERGE_GATE_NOTICE);
  });

  it("says the workspace is being merged, not merely that sending failed", () => {
    // Arrange / Act — the notice's whole job is the REASON; a bare "cannot
    // send" would leave the user re-trying a block that is not theirs to clear.
    const got = mergeGateNoticeHtml(true);
    // Assert
    expect(got).toContain("being merged");
  });

  it("says prompting resumes when the merge finishes", () => {
    // Arrange / Act
    const got = mergeGateNoticeHtml(true);
    // Assert
    expect(got).toContain("until it finishes");
  });

  it("renders EMPTY with no lease, which collapses the slot", () => {
    // Arrange / Act
    const got = mergeGateNoticeHtml(false);
    // Assert
    expect(got).toBe("");
  });
});

describe("mergeGateSendTitle: the disabled control names its own reason", () => {
  it("titles the send button with the explanation while blocked", () => {
    // Arrange / Act
    const got = mergeGateSendTitle(true);
    // Assert
    expect(got).toBe(MERGE_GATE_NOTICE);
  });

  it("carries no title once the lease is released", () => {
    // Arrange / Act
    const got = mergeGateSendTitle(false);
    // Assert
    expect(got).toBe("");
  });
});

describe("mergeGateBlockedLog: an attempted submit leaves evidence", () => {
  it("records the same explanation the user is shown", () => {
    // Arrange / Act — one wording for the screen and the log, so a support
    // reading cannot disagree with what the user saw.
    const got = mergeGateBlockedLog(12);
    // Assert
    expect(got).toContain(MERGE_GATE_NOTICE);
  });

  it("records the attempted prompt's length rather than its text", () => {
    // Arrange / Act — the draft is the user's content; its size is the fact a
    // diagnostic needs.
    const got = mergeGateBlockedLog(12);
    // Assert
    expect(got).toContain("prompt_length=12");
  });

  it("records that the draft was kept, so a blocked send is not a lost one", () => {
    // Arrange / Act
    const got = mergeGateBlockedLog(12);
    // Assert
    expect(got).toContain("draft retained");
  });

  it("names the merge as unreported when the daemon stamped no status", () => {
    // Arrange / Act — the absence is a fact worth recording; a missing field
    // would read as a merge nobody bothered to look up.
    const got = mergeGateBlockedLog(12);
    // Assert
    expect(got).toContain("merge=unreported");
  });
});

// --- what the merge holding the lease is actually doing ----------------------

describe("the gate's structured merge progress", () => {
  /** A merge status carrying PHASE, with the envelope fixed. */
  const merge = (phase: MergeStatus["phase"]): MergeStatus => ({
    runId: "run-1",
    phaseStartedAtMs: 900,
    updatedAtMs: 1000,
    phase,
  });

  const PICKING = merge({
    case: "cherryPicking",
    value: { commitsTotal: 4, commitsLanded: 1, currentSha: "abc1234", currentSubject: "fix it" },
  });

  it("says nothing when the daemon stamped no status", () => {
    // Arrange / Act
    const got = mergeGateProgress(null);
    // Assert
    expect(got).toBe("");
  });

  it("says how far the run has got", () => {
    // Arrange / Act — a merge waiting behind two workspaces and a merge on its
    // last commit used to read identically.
    const got = mergeGateProgress(PICKING);
    // Assert
    expect(got).toBe("merging · 1/4 · picking · abc1234 fix it");
  });

  it("says where in the queue a waiting merge sits", () => {
    // Arrange / Act
    const got = mergeGateProgress(merge({ case: "enqueued", value: { position: 2, depth: 3 } }));
    // Assert
    expect(got).toBe("merge queued · 2/3");
  });

  it("names the pre-merge prompt driving the session", () => {
    // Arrange / Act
    const got = mergeGateProgress(merge({ case: "beforeAction", value: { prompt: "lint" } }));
    // Assert
    expect(got).toContain("before-action · lint");
  });

  it("adds the progress to the standing notice", () => {
    // Arrange / Act
    const got = mergeGateNoticeHtml(true, PICKING);
    // Assert
    expect(got).toContain("merging · 1/4 · picking · abc1234 fix it");
  });

  it("keeps the fixed explanation beside the progress", () => {
    // Arrange / Act — the progress qualifies the block, it does not replace it.
    const got = mergeGateNoticeHtml(true, PICKING);
    // Assert
    expect(got).toContain(MERGE_GATE_NOTICE);
  });

  it("escapes the progress, which carries daemon text", () => {
    // Arrange
    const status = merge({
      case: "cherryPicking",
      value: { commitsTotal: 1, commitsLanded: 0, currentSha: "abc1234", currentSubject: "<img src=x>" },
    });
    // Act
    const got = mergeGateNoticeHtml(true, status);
    // Assert
    expect(got).not.toContain("<img");
  });

  it("still renders EMPTY with no lease, whatever the merge says", () => {
    // Arrange / Act — the lease is the gate; the status only describes it.
    const got = mergeGateNoticeHtml(false, PICKING);
    // Assert
    expect(got).toBe("");
  });

  it("parenthesizes the progress in the send button's title", () => {
    // Arrange / Act
    const got = mergeGateSendTitle(true, PICKING);
    // Assert
    expect(got).toBe(`${MERGE_GATE_NOTICE} (merging · 1/4 · picking · abc1234 fix it)`);
  });

  it("records the progress in the blocked-attempt log", () => {
    // Arrange / Act
    const got = mergeGateBlockedLog(12, PICKING);
    // Assert
    expect(got).toContain("merge=merging · 1/4 · picking · abc1234 fix it");
  });
});
