/**
 * merge-gate — the composer's gate while the merge coordinator owns this
 * workspace's session. One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";

import {
  MERGE_GATE_NOTICE,
  mergeGateBlockedLog,
  mergeGateNoticeHtml,
  mergeGateSendTitle,
  submitBlocked,
} from "../src/merge-gate.js";

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
});
