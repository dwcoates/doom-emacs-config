/**
 * hibernation — the revival gate and the composer block it enforces.
 *
 * The gate's whole job is to say WHY the session is asleep and to make the
 * revival decisions deliberate, so most of these assert the cause arm reaching
 * the prose, each offered decision reaching the socket as itself, and the
 * costlier mode never being reachable by default. One edge per test.
 */
import { describe, expect, it } from "vitest";

import type { HibernationDetail } from "../src/frontend-proto.js";
import type { AdapterEffect } from "../src/state-adapter.js";
import type { RevivalGateInput } from "../src/hibernation.js";
import {
  HIBERNATED_BODY_CLASS,
  HIBERNATION_COMPOSER_NOTICE,
  HIBERNATION_PROGRESS_CLASS,
  revivalGateSignature,
  revivalSinceText,
  REVIVAL_CONTEXT_UNKNOWN_TEXT,
  REVIVAL_GATE_HEADING,
  REVIVE_ATTR,
  REVIVE_CLEAR_EXPLANATION,
  REVIVE_COMPACT_EXPLANATION,
  REVIVE_COMPACT_PROMPTS_AND_RESPONSES_EXPLANATION,
  REVIVE_COMPACT_PROMPTS_EXPLANATION,
  REVIVE_COMPACT_RESPONSES_EXPLANATION,
  REVIVE_FAILED_TEXT,
  ReviveWatch,
  reviveDecisionFromAttr,
  reviveOptions,
  hibernateRefusedNotice,
  hibernationBlocked,
  hibernationBlockedLog,
  hibernationCauseText,
  hibernationNoticeHtml,
  hibernationSendTitle,
  reviveDirectWarning,
  revivalContextSizeText,
  revivalGateHtml,
  revivePendingText,
  reviveFailedLog,
  reviveRefusedLog,
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

/** A standing context size the daemon has reported, for the ordinary case. */
const CONTEXT_TOKENS = 128_500;

/**
 * The gate at a fixed clock and a known context size, so every test that is
 * about something ELSE says only the thing it is about.
 */
function gate(
  hibernation: HibernationDetail | null,
  overrides: Partial<Omit<RevivalGateInput, "hibernation">> = {},
): string {
  return revivalGateHtml({ hibernation, contextTokens: CONTEXT_TOKENS, now: NOW, ...overrides });
}

/**
 * One pushed `WorkspaceGateView` for a workspace, as an ingest-batch effect.
 *
 * The gate — not a session catalog entry — is what the watch rules on: a
 * snapshot carries several session views per workspace, in no authority order,
 * so a verdict taken from one could be taken from a retired session.
 */
function gateEffect(workspace: string, hibernation: HibernationDetail | null): AdapterEffect {
  return {
    kind: "fenced-view",
    value: {
      case: "workspaceGate",
      value: {
        workspace,
        fence: "f1",
        gate:
          hibernation === null
            ? { case: "open" }
            : { case: "hibernated", detail: hibernation },
      },
    },
  };
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

describe("revivalContextSizeText: how big the decision is", () => {
  it("states the session's input tokens, so the trade has a size", () => {
    // Arrange / Act / Assert — "the whole accumulated context" reads the same
    // at 12k and at 400k, and those two sessions have opposite right answers.
    expect(revivalContextSizeText(128_500)).toContain("128,500 input tokens");
  });

  it("counts cached and uncached input as one figure", () => {
    // Arrange / Act / Assert — a resumed turn re-presents the whole standing
    // prefix, and which bucket it bills to is a fact about the cache, not
    // about the size of the conversation.
    expect(revivalContextSizeText(128_500)).toContain("cached and uncached together");
  });

  it("says the tokens are re-ingested when the compaction is foregone", () => {
    // Arrange / Act / Assert — the figure is on the card to price that choice.
    expect(revivalContextSizeText(128_500)).toContain("re-ingests them");
  });

  it("reports an unmeasured session as unknown rather than as zero", () => {
    // Arrange / Act / Assert — a printed `0 input tokens` would recommend a
    // direct resume on exactly the evidence the gate does not have.
    expect(revivalContextSizeText(null)).toBe(REVIVAL_CONTEXT_UNKNOWN_TEXT);
  });

  it("never prints a token figure it was not given", () => {
    // Arrange / Act / Assert
    expect(revivalContextSizeText(null)).not.toContain("input tokens of context");
  });
});

describe("revivalGateHtml: the blocking card", () => {
  it("renders nothing for an awake session", () => {
    // Arrange / Act / Assert — an awake session pays no layout for a gate.
    expect(gate(null)).toBe("");
  });

  it("leads with a heading that names the state without prose", () => {
    // Arrange / Act / Assert
    expect(gate(forced())).toContain(REVIVAL_GATE_HEADING);
  });

  it("carries the idle-cutoff cause into the card", () => {
    // Arrange / Act / Assert
    expect(gate(idleCutoff())).toContain("reclaim its memory");
  });

  it("carries the forced cause into the card", () => {
    // Arrange / Act / Assert
    expect(gate(forced())).toContain("You put this session to sleep.");
  });

  it("carries the cache-expired cause into the card", () => {
    // Arrange / Act / Assert
    expect(gate(cacheExpired())).toContain("prompt cache expired");
  });

  it("marks the card with its cause, so the styling can differ per reason", () => {
    // Arrange / Act / Assert
    expect(gate(cacheExpired())).toContain("cause-cacheExpired");
  });

  it("states the session's token count on the card", () => {
    // Arrange / Act / Assert — the size of what a foregone compaction
    // re-ingests is the fact the whole decision turns on.
    expect(gate(forced())).toContain(revivalContextSizeText(CONTEXT_TOKENS));
  });

  it("gives the token count its own row, so it is not buried in the cause", () => {
    // Arrange / Act / Assert
    expect(gate(forced())).toContain('class="hibernation-context"');
  });

  it("says the size is unknown when the daemon reported no total", () => {
    // Arrange / Act / Assert — the row still renders: silence about the size
    // would read as a session that has none.
    expect(gate(forced(), { contextTokens: null })).toContain(REVIVAL_CONTEXT_UNKNOWN_TEXT);
  });

  it("drops the token count once the decision it priced has been taken", () => {
    // Arrange — the figure exists to price a decision the user is MAKING, and
    // once it is made the card collapses to the one line saying what is
    // running. The same standing figure stays on the topbar chip throughout.
    const got = gate(forced(), { pending: "direct" });
    // Act / Assert
    expect(got).not.toContain(revivalContextSizeText(CONTEXT_TOKENS));
  });

  it("offers the compact-everything action", () => {
    // Arrange / Act / Assert
    expect(gate(forced())).toContain(`${REVIVE_ATTR}="compactAll"`);
  });

  it("offers the responses-only compaction", () => {
    // Arrange / Act / Assert
    expect(gate(forced())).toContain(`${REVIVE_ATTR}="compactResponses"`);
  });

  it("offers the prompts-only compaction", () => {
    // Arrange / Act / Assert
    expect(gate(forced())).toContain(`${REVIVE_ATTR}="compactPrompts"`);
  });

  it("offers the prompts-and-responses compaction", () => {
    // Arrange / Act / Assert
    expect(gate(forced())).toContain(
      `${REVIVE_ATTR}="compactPromptsAndResponses"`,
    );
  });

  it("offers the resume-as-is action", () => {
    // Arrange / Act / Assert
    expect(gate(forced())).toContain(`${REVIVE_ATTR}="direct"`);
  });

  it("offers the clear action", () => {
    // Arrange / Act / Assert
    expect(gate(forced())).toContain(`${REVIVE_ATTR}="clear"`);
  });

  it("offers exactly the revival decisions and nothing else", () => {
    // Arrange — no dismiss: the gate is a pure function of daemon state, so a
    // dismissed gate would reappear having taught the user the block is
    // optional. And a hibernated session has nothing to cancel back to.
    const buttons = gate(forced()).match(/<button/g) ?? [];
    // Act / Assert
    expect(buttons).toHaveLength(reviveOptions(forced()).length);
  });

  it("explains that clearing discards the conversation", () => {
    // Arrange / Act / Assert — it sits one click from four options that all
    // keep something, so the sentence has to say that this one does not.
    expect(gate(forced())).toContain(REVIVE_CLEAR_EXPLANATION);
  });

  it("explains what a responses-only compaction keeps", () => {
    // Arrange / Act / Assert — the option is a decision about what is LOST, so
    // an unexplained button would be a blind one.
    expect(gate(forced())).toContain(REVIVE_COMPACT_RESPONSES_EXPLANATION);
  });

  it("explains what a prompts-only compaction keeps", () => {
    // Arrange / Act / Assert
    expect(gate(forced())).toContain(REVIVE_COMPACT_PROMPTS_EXPLANATION);
  });

  it("explains what a prompts-and-responses compaction keeps", () => {
    // Arrange / Act / Assert
    expect(gate(forced())).toContain(
      REVIVE_COMPACT_PROMPTS_AND_RESPONSES_EXPLANATION,
    );
  });

  it("explains what compacting first buys", () => {
    // Arrange / Act / Assert
    expect(gate(forced())).toContain(REVIVE_COMPACT_EXPLANATION);
  });

  it("ages the sleep from the daemon's own since stamp", () => {
    // Arrange / Act
    const got = gate(forced());
    // Assert
    expect(got).toContain("asleep for 1h");
  });

  it("omits the age when the daemon stamped no since time", () => {
    // Arrange — a zero would render as decades, which is fabricated, not
    // missing.
    const got = gate({ sinceMs: 0, cause: { case: "forced", value: {} } });
    // Act / Assert
    expect(got).not.toContain("asleep for");
  });

  it("replaces the buttons with a pending line once clear is sent", () => {
    // Arrange / Act
    const got = gate(forced(), { pending: "clear" });
    // Assert — the destructive option must not stay one click away while the
    // decision that produced it is still in flight.
    expect(got).not.toContain("<button");
  });

  it("gives clear its own button class, so it is not styled as the cheap path", () => {
    // Arrange / Act / Assert — the filled `hibernation-compact` style reads as
    // the recommended option, which the one unrecoverable choice must not.
    expect(gate(forced())).toContain('class="hibernation-clear"');
  });

  it("replaces the buttons with a pending line once compact-first is sent", () => {
    // Arrange / Act
    const got = gate(forced(), { pending: "compactAll" });
    // Assert — greying them would leave the other mode one click away while
    // the first decision is still in flight.
    expect(got).not.toContain("<button");
  });

  it("reports which decision is in flight", () => {
    // Arrange / Act / Assert
    expect(gate(forced(), { pending: "direct" })).toContain(revivePendingText("direct"));
  });
});

describe("revivePendingText: the in-flight report", () => {
  it("says the whole conversation is being compacted on the compact-all path", () => {
    // Arrange / Act / Assert
    expect(revivePendingText("compactAll")).toContain("whole conversation");
  });

  it("names the responses when only they are being compacted", () => {
    // Arrange / Act / Assert — a shared "compacting…" line would leave the user
    // unable to tell which decision the click actually sent.
    expect(revivePendingText("compactResponses")).toContain("responses");
  });

  it("names the prompts when only they are being compacted", () => {
    // Arrange / Act / Assert
    expect(revivePendingText("compactPrompts")).toContain("prompts");
  });

  it("names both when prompts and responses are being compacted", () => {
    // Arrange / Act / Assert
    expect(revivePendingText("compactPromptsAndResponses")).toContain("prompts and responses");
  });

  it("says the full context is being carried on the direct path", () => {
    // Arrange / Act / Assert
    expect(revivePendingText("direct")).toContain("full context");
  });

  it("says the conversation is being cleared on the clear path", () => {
    // Arrange / Act / Assert — a line borrowed from a compaction would report
    // a summary being made where nothing is being kept.
    expect(revivePendingText("clear")).toContain("clearing the conversation");
  });

  it("gives every decision a line of its own", () => {
    // Arrange — the pending line is all the user has while a decision is in
    // flight, so two decisions sharing one would make the click unverifiable.
    const lines = reviveOptions(forced()).map((o) => revivePendingText(o.decision));
    // Act / Assert
    expect(new Set(lines).size).toBe(lines.length);
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

describe("reviveRefusedLog: the record of a refused revival decision", () => {
  it("names the mode the daemon turned down", () => {
    // Arrange / Act / Assert — the two modes cost different things, so which
    // one was refused is part of the fact.
    expect(reviveRefusedLog("compactResponses", new Error("not hibernated"))).toContain(
      "compactResponses",
    );
  });

  it("says the session is still asleep, since the gate is coming back up", () => {
    // Arrange / Act / Assert
    expect(reviveRefusedLog("direct", new Error("not hibernated"))).toContain("still");
  });

  it("carries the rejection's own words", () => {
    // Arrange / Act / Assert
    expect(reviveRefusedLog("direct", new Error("merge lease held"))).toContain(
      "merge lease held",
    );
  });

  it("reads a non-Error rejection rather than dropping it", () => {
    // Arrange / Act / Assert — a thrown string is still the only account of
    // what happened.
    expect(reviveRefusedLog("direct", "socket not open")).toContain("socket not open");
  });
});

describe("hibernateRefusedNotice: the topbar line for a refused sleep", () => {
  it("says the session was not put to sleep", () => {
    // Arrange / Act / Assert — the button hides itself once a session sleeps,
    // so its absence must not be read as success.
    expect(hibernateRefusedNotice(new Error("a turn is in flight"))).toContain(
      "could not put this session to sleep",
    );
  });

  it("carries the daemon's reason beside it", () => {
    // Arrange / Act / Assert
    expect(hibernateRefusedNotice(new Error("a turn is in flight"))).toContain(
      "a turn is in flight",
    );
  });
});

describe("ReviveWatch: the exit an accepted-but-failed revival needs", () => {
  it("waits while no session view for the workspace has landed", () => {
    // Arrange — nothing has been said about the session yet, and a verdict now
    // would be a guess.
    const watch = new ReviveWatch();
    watch.arm("/w", "direct");
    // Act / Assert
    expect(watch.observe([]).kind).toBe("waiting");
  });

  it("reads a view that dropped the hibernation field as the revival landing", () => {
    // Arrange
    const watch = new ReviveWatch();
    watch.arm("/w", "compactAll");
    // Act
    const verdict = watch.observe([gateEffect("/w", null)]);
    // Assert
    expect(verdict.kind).toBe("revived");
  });

  it("reads a view that still carries hibernation as the bring-up having failed", () => {
    // Arrange — the ack meant "decision taken", not "session up"; this is the
    // state that used to leave the gate on "Waking the session…" forever.
    const watch = new ReviveWatch();
    watch.arm("/w", "direct");
    // Act
    const verdict = watch.observe([gateEffect("/w", forced())]);
    // Assert
    expect(verdict.kind).toBe("failed");
  });

  it("carries the mode the failed verdict was armed with", () => {
    // Arrange
    const watch = new ReviveWatch();
    watch.arm("/w", "compactAll");
    // Act
    const verdict = watch.observe([gateEffect("/w", forced())]);
    // Assert — the two modes cost different things, so the report names one.
    expect(verdict.kind === "failed" && verdict.mode).toBe("compactAll");
  });

  it("carries the detail the failing view reported, not the one it was armed on", () => {
    // Arrange — a bring-up that failed into a different cause is the daemon
    // saying something new happened.
    const watch = new ReviveWatch();
    watch.arm("/w", "direct");
    // Act
    const verdict = watch.observe([gateEffect("/w", cacheExpired())]);
    // Assert
    expect(verdict.kind === "failed" && verdict.hibernation.cause.case).toBe("cacheExpired");
  });

  it("rules once: a second batch after a verdict is ordinary state", () => {
    // Arrange — the question was answered; a later view is not a re-judgement.
    const watch = new ReviveWatch();
    watch.arm("/w", "direct");
    watch.observe([gateEffect("/w", forced())]);
    // Act / Assert
    expect(watch.observe([gateEffect("/w", forced())]).kind).toBe("waiting");
  });

  it("rules on the LAST view in a batch, as the store does", () => {
    // Arrange — ruling on an earlier one would judge the revival by a state
    // the store has already superseded.
    const watch = new ReviveWatch();
    watch.arm("/w", "direct");
    // Act
    const verdict = watch.observe([
      gateEffect("/w", forced()),
      gateEffect("/w", null),
    ]);
    // Assert
    expect(verdict.kind).toBe("revived");
  });

  it("ignores a view belonging to a different workspace", () => {
    // Arrange
    const watch = new ReviveWatch();
    watch.arm("/w", "direct");
    // Act / Assert
    expect(watch.observe([gateEffect("/other", null)]).kind).toBe("waiting");
  });

  it("settles on a pre-init view that names no workspace", () => {
    // Arrange — an empty workspace is silence about identity, not evidence of
    // a different session: this socket carries exactly one.
    const watch = new ReviveWatch();
    watch.arm("/w", "direct");
    // Act / Assert
    expect(watch.observe([gateEffect("", null)]).kind).toBe("revived");
  });

  it("rules on nothing until a decision is armed", () => {
    // Arrange — an ordinary view on a session nobody asked to wake.
    const watch = new ReviveWatch();
    // Act / Assert
    expect(watch.observe([gateEffect("/w", forced())]).kind).toBe("waiting");
  });

  it("drops the expectation on disarm, so a rejected ack is owed no verdict", () => {
    // Arrange
    const watch = new ReviveWatch();
    watch.arm("/w", "direct");
    // Act
    watch.disarm();
    // Assert
    expect(watch.observe([gateEffect("/w", forced())]).kind).toBe("waiting");
  });

  it("reports the armed decision while one is outstanding", () => {
    // Arrange
    const watch = new ReviveWatch();
    // Act
    watch.arm("/w", "compactAll");
    // Assert
    expect(watch.pending).toBe("compactAll");
  });
});

describe("reviveFailedLog: the record of an accepted revival that did not take", () => {
  it("names the mode that was accepted", () => {
    // Arrange / Act / Assert
    expect(reviveFailedLog("compactAll", forced())).toContain("compactAll");
  });

  it("carries the cause the failing view reported", () => {
    // Arrange / Act / Assert
    expect(reviveFailedLog("direct", cacheExpired())).toContain("cause=cacheExpired");
  });

  it("says the gate is restored, since that is the user-visible consequence", () => {
    // Arrange / Act / Assert
    expect(reviveFailedLog("direct", forced())).toContain("gate is restored");
  });
});

describe("the gate's failed-revival line", () => {
  it("renders the failure above the cause when one is supplied", () => {
    // Arrange / Act
    const got = gate(forced(), { failure: REVIVE_FAILED_TEXT });
    // Assert
    expect(got).toContain("hibernation-failed");
  });

  it("keeps the buttons on offer beside the failure, since choosing again is the exit", () => {
    // Arrange / Act
    const got = gate(forced(), { failure: REVIVE_FAILED_TEXT });
    // Assert
    expect(got).toContain(`${REVIVE_ATTR}="compactAll"`);
  });

  it("draws no failure line when there is nothing to report", () => {
    // Arrange / Act
    const got = gate(forced());
    // Assert
    expect(got).not.toContain("hibernation-failed");
  });

  it("escapes the failure line rather than trusting it as markup", () => {
    // Arrange / Act
    const got = gate(forced(), { failure: "<script>x</script>" });
    // Assert
    expect(got).not.toContain("<script");
  });
});

describe("the taken decision's collapsed card", () => {
  it("collapses the card to the progress line the moment a decision is accepted", () => {
    // Arrange / Act
    const got = gate(forced(), { pending: "compactAll" });
    // Assert — the popup asked a question that has now been answered, and a
    // compact-first revival keeps it answered for a whole compaction.
    expect(got).toContain(HIBERNATION_PROGRESS_CLASS);
    expect(got).not.toContain(REVIVAL_GATE_HEADING);
  });

  it("drops the cause prose from the collapsed card, since nothing is being decided", () => {
    // Arrange / Act
    const got = gate(forced(), { pending: "compactAll" });
    // Assert
    expect(got).not.toContain(hibernationCauseText(forced()));
  });

  it("keeps the waking copy, so the collapsed line still says what is running", () => {
    // Arrange / Act
    const got = gate(forced(), { pending: "compactAll" });
    // Assert
    expect(got).toContain(revivePendingText("compactAll"));
  });

  it("restores the FULL card when an accepted decision left the session asleep", () => {
    // Arrange — the failure path: the same verdict clears the pending mark and
    // sets the failure line.
    const got = gate(forced(), { pending: null, failure: REVIVE_FAILED_TEXT });
    // Assert — the choice is handed back, which is the only exit left.
    expect(got).toContain(REVIVAL_GATE_HEADING);
    expect(got).toContain(REVIVE_FAILED_TEXT);
    expect(got).toContain(`${REVIVE_ATTR}="compactAll"`);
  });

  it("draws nothing at all for an awake session, pending or not", () => {
    // Arrange / Act / Assert — the collapse is a presentation of the block,
    // never a reason to draw one where the daemon reports none.
    expect(gate(null, { pending: "direct" })).toBe("");
  });
});

describe("revivalGateSignature: what makes the card worth rebuilding", () => {
  const state = { hibernation: forced(), contextTokens: CONTEXT_TOKENS };

  it("is unchanged while only the clock moves", () => {
    // Arrange / Act / Assert — the age is reconciled as text; folding the
    // clock in would differ every frame and guard nothing, and a rebuild
    // between a mousedown and a mouseup swallows the click.
    expect(revivalGateSignature(state)).toBe(revivalGateSignature({ ...state }));
  });

  it("changes when the session wakes", () => {
    // Arrange / Act / Assert
    expect(revivalGateSignature({ ...state, hibernation: null })).not.toBe(
      revivalGateSignature(state),
    );
  });

  it("changes when a decision goes in flight", () => {
    // Arrange / Act / Assert — the frame that takes the buttons down.
    expect(revivalGateSignature({ ...state, pending: "compactAll" })).not.toBe(
      revivalGateSignature(state),
    );
  });

  it("changes when a failure line is raised", () => {
    // Arrange / Act / Assert — the frame that hands the choice back.
    expect(revivalGateSignature({ ...state, failure: REVIVE_FAILED_TEXT })).not.toBe(
      revivalGateSignature(state),
    );
  });

  it("changes when the context figure the decision is priced by moves", () => {
    // Arrange / Act / Assert
    expect(revivalGateSignature({ ...state, contextTokens: 1 })).not.toBe(
      revivalGateSignature(state),
    );
  });

  it("changes when the daemon reports a different cause for the sleep", () => {
    // Arrange / Act / Assert — the cause decides which option is right, so a
    // card that kept the old one would advise on stale news.
    expect(revivalGateSignature({ ...state, hibernation: cacheExpired() })).not.toBe(
      revivalGateSignature(state),
    );
  });
});

describe("revivalSinceText: the age written in place", () => {
  it("ages the sleep from the daemon's own since stamp", () => {
    // Arrange / Act / Assert
    expect(revivalSinceText(forced(), NOW)).toBe("asleep for 1h");
  });

  it("says nothing when the daemon stamped no since time", () => {
    // Arrange — a zero would render as decades, which is fabricated.
    expect(revivalSinceText({ sinceMs: 0, cause: { case: "forced", value: {} } }, NOW)).toBe("");
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
    const got = gate(idleCutoff(HOUR));
    // Act / Assert
    expect(got).not.toContain("<script");
  });
});

describe("reviveOptions: the offered answers", () => {
  it("offers every decision the wire can carry", () => {
    // Arrange — an option missing here is a decision the daemon accepts and
    // the user can never reach.
    const decisions = reviveOptions(forced()).map((o) => o.decision);
    // Act / Assert
    expect(decisions).toEqual([
      "compactAll",
      "compactPromptsAndResponses",
      "compactResponses",
      "compactPrompts",
      "direct",
      "clear",
    ]);
  });

  it("marks the two options that are not ordinary compactions", () => {
    // Arrange / Act
    const warned = reviveOptions(forced()).filter((o) => o.warn);
    // Assert — the compactions all pay their cost once and keep a summary.
    // Resume-as-is pays it on every later turn, and clear keeps nothing at
    // all; both are consequences a scoped compaction does not carry.
    expect(warned.map((o) => o.decision)).toEqual(["direct", "clear"]);
  });

  it("offers clear last, off the keep-more axis the other options sit on", () => {
    // Arrange — the four compactions and the direct resume answer "how much of
    // this is worth carrying". Clear answers "none of it", so it must not read
    // as the cheap end of that axis.
    const decisions = reviveOptions(forced()).map((o) => o.decision);
    // Act / Assert
    expect(decisions[decisions.length - 1]).toBe("clear");
  });

  it("says clear DISCARDS rather than describing it as a smaller compaction", () => {
    // Arrange / Act
    const clear = reviveOptions(forced()).find((o) => o.decision === "clear");
    // Assert
    expect(clear?.explanation).toContain("discard the conversation entirely");
  });

  it("strengthens the resume-as-is sentence when the cache is already gone", () => {
    // Arrange / Act
    const option = reviveOptions(cacheExpired()).find((o) => o.decision === "direct");
    // Assert
    expect(option?.explanation).toContain("already");
  });

  it("gives every option its own explanation", () => {
    // Arrange — a shared sentence would make two buttons indistinguishable in
    // the one respect that matters: what each one throws away.
    const explanations = reviveOptions(forced()).map((o) => o.explanation);
    // Act / Assert
    expect(new Set(explanations).size).toBe(explanations.length);
  });
});

describe("reviveDecisionFromAttr: reading a clicked decision", () => {
  it("reads back every decision the gate renders", () => {
    // Arrange
    for (const option of reviveOptions(forced())) {
      // Act / Assert — the render and the read are one vocabulary.
      expect(reviveDecisionFromAttr(option.decision)).toBe(option.decision);
    }
  });

  it("throws on an unrecognized decision rather than guessing one", () => {
    // Arrange / Act / Assert — defaulting to direct would resume at full
    // context a user who asked to compact; defaulting to a compaction would
    // discard a conversation nobody consented to lose.
    expect(() => reviveDecisionFromAttr("compactSomething")).toThrow(/unknown decision/);
  });

  it("throws on a missing attribute", () => {
    // Arrange / Act / Assert
    expect(() => reviveDecisionFromAttr(null)).toThrow(/unknown decision/);
  });
});
