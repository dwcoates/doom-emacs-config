/**
 * hibernation — the revival gate and the composer block it enforces.
 *
 * The gate's whole job is to say WHY the session is asleep and to make the two
 * revival modes a deliberate choice, so most of these assert the cause arm
 * reaching the prose and the costlier mode never being reachable by default.
 * One edge per test.
 */
import { describe, expect, it } from "vitest";

import type { HibernationDetail } from "../src/frontend-proto.js";
import {
  HIBERNATED_BODY_CLASS,
  HIBERNATION_COMPOSER_NOTICE,
  REVIVAL_GATE_HEADING,
  REVIVE_COMPACT_ATTR,
  REVIVE_COMPACT_EXPLANATION,
  REVIVE_DIRECT_ATTR,
  hibernationBlocked,
  hibernationBlockedLog,
  hibernationCauseText,
  hibernationNoticeHtml,
  hibernationSendTitle,
  reviveDirectWarning,
  revivalGateHtml,
  revivePendingText,
} from "../src/hibernation.js";

const HOUR = 3_600_000;
const NOW = Date.parse("2024-05-01T12:00:00.000Z");

/** Asleep at the idle cutoff, the routine case. */
function idleCutoff(cutoffMs = HOUR): HibernationDetail {
  return { sinceMs: NOW - HOUR, cause: { case: "idleCutoff", value: { cutoffMs } } };
}

/** Asleep because the user asked for it. */
function forced(): HibernationDetail {
  return { sinceMs: NOW - HOUR, cause: { case: "forced", value: {} } };
}

/** Asleep because the cache went cold before a ping could fire. */
function cacheExpired(elapsedMs = 4 * HOUR, ttlMs = HOUR): HibernationDetail {
  return { sinceMs: NOW - HOUR, cause: { case: "cacheExpired", value: { elapsedMs, ttlMs } } };
}

describe("hibernationBlocked: the composer gate", () => {
  it("blocks a prompt while the daemon reports the session asleep", () => {
    // Arrange / Act / Assert — the daemon nacks a prompt on a hibernated
    // session, so a live composer would only spend a draft on a refusal.
    expect(hibernationBlocked(forced())).toBe(true);
  });

  it("does not block an awake session", () => {
    // Arrange / Act / Assert
    expect(hibernationBlocked(null)).toBe(false);
  });
});

describe("hibernationCauseText: the gate names WHY, per cause arm", () => {
  it("names the idle cutoff that tripped", () => {
    // Arrange / Act
    const got = hibernationCauseText(idleCutoff(HOUR));
    // Assert — carried on the wire so the gate never guesses daemon config.
    expect(got).toContain("1h");
  });

  it("says the idle case was the daemon reclaiming memory, not a fault", () => {
    // Arrange / Act / Assert
    expect(hibernationCauseText(idleCutoff())).toContain("reclaim its memory");
  });

  it("reports a forced sleep as the user's own act", () => {
    // Arrange / Act / Assert
    expect(hibernationCauseText(forced())).toBe("You put this session to sleep.");
  });

  it("names the elapsed idle time on the cache-expired cause", () => {
    // Arrange / Act
    const got = hibernationCauseText(cacheExpired(4 * HOUR, HOUR));
    // Assert
    expect(got).toContain("4h");
  });

  it("names the cache lifetime the elapsed time exceeded", () => {
    // Arrange — the gate says "N past M" only because the daemon carries both.
    const got = hibernationCauseText(cacheExpired(4 * HOUR, 2 * HOUR));
    // Act / Assert
    expect(got).toContain("2h cache lifetime");
  });

  it("explains that pinging a cold cache would have paid for nothing", () => {
    // Arrange / Act / Assert
    expect(hibernationCauseText(cacheExpired())).toContain("full re-ingest cost for nothing");
  });
});

describe("reviveDirectWarning: what resuming as-is costs", () => {
  it("warns that every later turn carries the full context", () => {
    // Arrange / Act / Assert
    expect(reviveDirectWarning(forced())).toContain("every turn from here on");
  });

  it("hardens the warning when the cache is already known to be gone", () => {
    // Arrange — on cacheExpired the re-ingest is certain, not merely likely,
    // so the mode that is usually the cheap one is not.
    const got = reviveDirectWarning(cacheExpired());
    // Act / Assert
    expect(got).toContain("already");
  });

  it("does not claim the cache is gone when it was a plain idle sleep", () => {
    // Arrange / Act / Assert
    expect(reviveDirectWarning(idleCutoff())).not.toContain("already");
  });
});

describe("revivalGateHtml: the blocking card", () => {
  it("renders nothing for an awake session", () => {
    // Arrange / Act / Assert — an awake session pays no layout for a gate.
    expect(revivalGateHtml(null)).toBe("");
  });

  it("leads with a heading that names the state without prose", () => {
    // Arrange / Act / Assert
    expect(revivalGateHtml(forced(), null, NOW)).toContain(REVIVAL_GATE_HEADING);
  });

  it("carries the idle-cutoff cause into the card", () => {
    // Arrange / Act / Assert
    expect(revivalGateHtml(idleCutoff(), null, NOW)).toContain("reclaim its memory");
  });

  it("carries the forced cause into the card", () => {
    // Arrange / Act / Assert
    expect(revivalGateHtml(forced(), null, NOW)).toContain("You put this session to sleep.");
  });

  it("carries the cache-expired cause into the card", () => {
    // Arrange / Act / Assert
    expect(revivalGateHtml(cacheExpired(), null, NOW)).toContain("prompt cache expired");
  });

  it("marks the card with its cause, so the styling can differ per reason", () => {
    // Arrange / Act / Assert
    expect(revivalGateHtml(cacheExpired(), null, NOW)).toContain("cause-cacheExpired");
  });

  it("offers the compact-first action", () => {
    // Arrange / Act / Assert
    expect(revivalGateHtml(forced(), null, NOW)).toContain(REVIVE_COMPACT_ATTR);
  });

  it("offers the resume-as-is action", () => {
    // Arrange / Act / Assert
    expect(revivalGateHtml(forced(), null, NOW)).toContain(REVIVE_DIRECT_ATTR);
  });

  it("offers exactly those two actions and no third", () => {
    // Arrange — no dismiss: the gate is a pure function of daemon state, so a
    // dismissed gate would reappear having taught the user the block is
    // optional. And a hibernated session has nothing to cancel back to.
    const buttons = revivalGateHtml(forced(), null, NOW).match(/<button/g) ?? [];
    // Act / Assert
    expect(buttons).toHaveLength(2);
  });

  it("explains what compacting first buys", () => {
    // Arrange / Act / Assert
    expect(revivalGateHtml(forced(), null, NOW)).toContain(REVIVE_COMPACT_EXPLANATION);
  });

  it("ages the sleep from the daemon's own since stamp", () => {
    // Arrange / Act
    const got = revivalGateHtml(forced(), null, NOW);
    // Assert
    expect(got).toContain("asleep for 1h");
  });

  it("omits the age when the daemon stamped no since time", () => {
    // Arrange — a zero would render as decades, which is fabricated, not
    // missing.
    const got = revivalGateHtml({ sinceMs: 0, cause: { case: "forced", value: {} } }, null, NOW);
    // Act / Assert
    expect(got).not.toContain("asleep for");
  });

  it("replaces the buttons with a pending line once compact-first is sent", () => {
    // Arrange / Act
    const got = revivalGateHtml(forced(), "compactFirst", NOW);
    // Assert — greying them would leave the other mode one click away while
    // the first decision is still in flight.
    expect(got).not.toContain("<button");
  });

  it("reports which decision is in flight", () => {
    // Arrange / Act / Assert
    expect(revivalGateHtml(forced(), "direct", NOW)).toContain(revivePendingText("direct"));
  });
});

describe("revivePendingText: the in-flight report", () => {
  it("says compaction is happening first on the compact path", () => {
    // Arrange / Act / Assert
    expect(revivePendingText("compactFirst")).toContain("compacting first");
  });

  it("says the full context is being carried on the direct path", () => {
    // Arrange / Act / Assert
    expect(revivePendingText("direct")).toContain("full context");
  });
});

describe("the composer's own notice", () => {
  it("points the user at the gate rather than restating the whole cause", () => {
    // Arrange / Act / Assert
    expect(hibernationNoticeHtml(forced())).toContain(HIBERNATION_COMPOSER_NOTICE);
  });

  it("collapses to nothing on an awake session", () => {
    // Arrange / Act / Assert
    expect(hibernationNoticeHtml(null)).toBe("");
  });

  it("titles the disabled send button with the same one explanation", () => {
    // Arrange — one string, so the notice and the tooltip cannot drift into
    // two accounts of the same block.
    expect(hibernationSendTitle(forced())).toBe(HIBERNATION_COMPOSER_NOTICE);
  });

  it("leaves the send button untitled while the session is awake", () => {
    // Arrange / Act / Assert
    expect(hibernationSendTitle(null)).toBe("");
  });
});

describe("hibernationBlockedLog: the record of a refused submit", () => {
  it("records the cause the block was made under", () => {
    // Arrange / Act / Assert
    expect(hibernationBlockedLog(42, cacheExpired())).toContain("cause=cacheExpired");
  });

  it("says the draft was kept, since a vanished prompt is the failure mode", () => {
    // Arrange / Act / Assert
    expect(hibernationBlockedLog(42, forced())).toContain("draft retained");
  });
});

describe("the click and body markers", () => {
  it("marks the document while the gate stands, so chrome can paint against it", () => {
    // Arrange / Act / Assert
    expect(HIBERNATED_BODY_CLASS).toBe("hibernated");
  });
});

describe("escaping", () => {
  it("escapes the cause prose rather than trusting it as markup", () => {
    // Arrange — every figure in the prose is daemon-supplied, so the whole
    // line goes through the escaper on principle.
    const got = revivalGateHtml(idleCutoff(HOUR), null, NOW);
    // Act / Assert
    expect(got).not.toContain("<script");
  });
});
