// @vitest-environment jsdom
//
// The pure builders (phase, activity, cells, click classification) need no DOM;
// jsdom is here for ProgressFooter itself, whose slot paint and disclosure
// survival are asserted against a real mount.
import { describe, expect, it } from "vitest";

import { BREATH_SHADES, breathColor } from "../src/breathing.js";
import { CounterEntry } from "../src/counter-menu.js";
import {
  alreadyCompletePhaseViolation,
  Activity,
  FooterDisclosure,
  FooterInput,
  ProgressFooter,
  activityBody,
  activityDetail,
  countersHtml,
  errorRowHtml,
  expensiveTurnRowHtml,
  footerClickAction,
  footerHtml,
  hasLiveCounters,
  interruptChip,
  mergeNoteRowHtml,
  mergeStatusChip,
  phaseLabel,
  rateLimitAccent,
  rateLimitActivity,
  runningTool,
  sheetHtml,
  tokenCellHtml,
  toolElapsed,
} from "../src/progress-footer.js";
import type { ContextCostAlert } from "../src/frontend-proto.js";
import type { MergeStatus, ProgressInput } from "../src/state-adapter.js";
import { ConversationItem, SystemFailureCard, ToolItem } from "../src/store.js";

const NOW = Date.parse("2024-05-01T12:00:00.000Z");

/** Millisecond scales, for reset deadlines expressed as a wait from NOW. */
const MINUTE = 60_000;
const HOUR = 60 * MINUTE;
const DAY = 24 * HOUR;

/** A daemon-classified failure, defaulted to an addressable API failure. */
function failureCard(over: Partial<SystemFailureCard> = {}): SystemFailureCard {
  return {
    kind: "failure",
    errorClass: "API",
    errorType: "api.overloaded",
    message: "the API is overloaded",
    sourceDetail: "status=529",
    resolvedAtMs: 0,
    uuid: "failure:e9",
    detail: { kind: "none" },
    ...over,
  };
}

/** A resolved progress view, defaulted to a quiet idle session. */
function progress(over: Partial<ProgressInput> = {}): ProgressInput {
  return {
    workspace: "/w",
    sessionId: "s1",
    turnStartedAtMs: 0,
    thinkingTokens: 0,
    inputTokens: 0,
    ttftMs: 0,
    compacting: null,
    retrying: null,
    authenticating: null,
    hook: null,
    blocked: null,
    interrupt: null,
    rateLimited: null,
    rateLimitedWeekly: null,
    failure: null,
    expensiveTurn: null,
    pendingPermissions: 0,
    queueDepth: 0,
    liveTaskCount: 0,
    ...over,
  };
}

/** A footer input, defaulted to the quiet idle session with no rosters. */
function input(over: Partial<FooterInput> = {}): FooterInput {
  return {
    progress: progress(),
    renderState: "idle",
    mergeStatus: null,
    agents: [],
    tasks: [],
    items: [],
    timerLabel: "0:24",
    ...over,
  };
}

/** The footer's phase cell markup, or "" when the strip renders no phase. */
function phaseCell(html: string): string {
  return html.match(/<div class="pfooter-cell pfooter-phase[^]*?<\/div>/)?.[0] ?? "";
}

/** A running tool call at the feed tail. */
function tool(over: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
    toolUseId: "tu1",
    toolName: "Bash",
    messageId: "m1",
    ts: new Date(NOW - 12_000).toISOString(),
    inputJson: "",
    inputDone: true,
    ...over,
  } as ToolItem;
}

/** The prompt bubble a submit produces, which bounds the running-tool scan. */
function userTurn(): ConversationItem {
  return {
    kind: "user-turn",
    requestId: "r1",
    ts: "",
    content: [{ type: "text", text: "next question" }],
  } as ConversationItem;
}

/** An active roster entry. */
function counterEntry(over: Partial<CounterEntry> = {}): CounterEntry {
  return { id: "a1", summary: "verify", detail: "", status: "running", nested: false, ...over };
}

const CLOSED: FooterDisclosure = { agentsOpen: false, tasksOpen: false, expanded: false };

/** A click target standing in for a DOM element, matched by selector list. */
function target(selectors: Record<string, Record<string, string>>) {
  return {
    closest(sel: string) {
      const attrs = selectors[sel];
      if (attrs === undefined) return null;
      return { getAttribute: (n: string) => attrs[n] ?? null };
    },
  };
}

// --- the phase mirror --------------------------------------------------------

describe("phaseLabel: the SSM's verdict as the footer's anchor", () => {
  it("spins while thinking", () => {
    // Arrange / Act
    const got = phaseLabel("thinking");
    // Assert
    expect(got).toEqual({ word: "thinking", tone: "thinking", breathing: true });
  });

  it("says clearing while the context is being discarded", () => {
    // Arrange / Act
    const got = phaseLabel("clearing");
    // Assert — thinking's tone and spin: the same claim, a different word.
    expect(got).toEqual({ word: "clearing", tone: "thinking", breathing: true });
  });

  it("says compacting while the context is being summarized", () => {
    // Arrange / Act
    const got = phaseLabel("compacting");
    // Assert
    expect(got).toEqual({ word: "compacting", tone: "thinking", breathing: true });
  });

  it("does not spin once the turn is done", () => {
    // Arrange / Act
    const got = phaseLabel("done");
    // Assert
    expect(got.breathing).toBe(false);
  });

  it("names a workspace whose route broke severed", () => {
    // Arrange / Act
    const got = phaseLabel("severed");
    // Assert
    expect(got.word).toBe("severed");
  });

  it("does NOT spin while severed", () => {
    // Arrange / Act — `starting` spins because a bring-up is really under way;
    // severed is the opposite claim, and a spinner would say work is happening.
    const got = phaseLabel("severed");
    // Assert
    expect(got.breathing).toBe(false);
  });

  it("takes the blue tone while severed", () => {
    // Arrange / Act — the same blue every broken-route state wears.
    const got = phaseLabel("severed");
    // Assert
    expect(got.tone).toBe("error");
  });

  it("names a workspace asleep on purpose hibernated", () => {
    // Arrange / Act
    const got = phaseLabel("hibernated");
    // Assert
    expect(got.word).toBe("hibernated");
  });

  it("does NOT spin while hibernated", () => {
    // Arrange / Act — nothing is wired and nothing is coming until the next
    // prompt pays a bring-up, so a spinner would say work is happening.
    const got = phaseLabel("hibernated");
    // Assert
    expect(got.breathing).toBe(false);
  });

  it("takes a BENIGN tone while hibernated, never the error tone", () => {
    // Arrange / Act — this is the correction the split exists for. This state
    // inherited `dormant`'s error tone, which put an error-shaped word in the
    // footer of a workspace whose only sin was being asleep on purpose. Nothing
    // here needs acting on.
    const got = phaseLabel("hibernated");
    // Assert
    expect(got.tone).toBe("hibernated");
    expect(got.tone).not.toBe("error");
  });

  it("still spins while starting, which severed is the opposite of", () => {
    // Arrange / Act — the contrast the two blues exist to draw.
    const got = phaseLabel("init");
    // Assert
    expect(got.breathing).toBe(true);
  });

  it("takes the alarm tone for a dead session", () => {
    // Arrange / Act
    const got = phaseLabel("dead");
    // Assert
    expect(got.tone).toBe("error");
  });

  it("keeps a merge quiet, since the sidebar leads on it", () => {
    // Arrange / Act
    const got = phaseLabel("merge_queued");
    // Assert
    expect(got.tone).toBe("muted");
  });

  it("names a running merge as merging", () => {
    // Arrange / Act — the footer says what the merge is doing in realtime, not
    // only once it has queued or finished.
    const got = phaseLabel("merging");
    // Assert
    expect(got.word).toBe("merging");
  });

  it("breathes while merging, because work really is happening", () => {
    // Arrange / Act
    const got = phaseLabel("merging");
    // Assert
    expect(got.breathing).toBe(true);
  });

  it("names the first instant of a merge attempt as merge enqueuing", () => {
    // Arrange / Act — the window between the command arriving and the merge
    // being durably queued, which nothing described before it.
    const got = phaseLabel("merge_enqueuing");
    // Assert
    expect(got.word).toBe("merge enqueuing");
  });

  it("breathes while enqueuing, because the daemon really is working", () => {
    // Arrange / Act — resolving the geometry and writing the durable entry is
    // work happening, unlike a queued merge's wait.
    const got = phaseLabel("merge_enqueuing");
    // Assert
    expect(got.breathing).toBe(true);
  });

  it("keeps enqueuing quiet, like every other merge phase the sidebar leads on", () => {
    // Arrange / Act
    const got = phaseLabel("merge_enqueuing");
    // Assert
    expect(got.tone).toBe("muted");
  });

  it("distinguishes enqueuing from the queue place that follows it", () => {
    // Arrange / Act — the two are consecutive phases of one pipeline, and a
    // shared word would hide the transition entirely.
    const enqueuing = phaseLabel("merge_enqueuing");
    const queued = phaseLabel("merge_queued");
    // Assert
    expect(enqueuing.word).not.toBe(queued.word);
  });

  it("names idle_async as monitoring", () => {
    // Arrange / Act — the yellow no-turn-but-live-async signal.
    const got = phaseLabel("idle_async");
    // Assert
    expect(got.word).toBe("monitoring");
  });

  // --- the five-color remap ---------------------------------------------

  it("takes the yellow async tone while detached work runs", () => {
    // Arrange / Act
    const got = phaseLabel("idle_async");
    // Assert — the same yellow the tab-bar and the sidebar dot wear.
    expect(got.tone).toBe("async");
  });

  it("names a ready session ready", () => {
    // Arrange / Act
    const got = phaseLabel("ready");
    // Assert
    expect(got.word).toBe("ready");
  });

  it("takes the green tone for a ready session", () => {
    // Arrange / Act — the route works and the agent is available.
    const got = phaseLabel("ready");
    // Assert
    expect(got.tone).toBe("ok");
  });

  it("takes the green tone for an idle session too", () => {
    // Arrange / Act — idle is the same claim as ready, not a lesser one.
    const got = phaseLabel("idle");
    // Assert
    expect(got.tone).toBe("ok");
  });

  it("names a vendor-blocked session blocked", () => {
    // Arrange / Act
    const got = phaseLabel("vendor_blocked");
    // Assert
    expect(got.word).toBe("blocked");
  });

  it("takes the purple tone when the vendor has stopped the session", () => {
    // Arrange / Act
    const got = phaseLabel("vendor_blocked");
    // Assert
    expect(got.tone).toBe("blocked");
  });

  it("does NOT spin while blocked, since nothing is in progress", () => {
    // Arrange / Act — every other breathing phase is work happening, so
    // animating this one would say the opposite of what it means.
    const got = phaseLabel("vendor_blocked");
    // Assert
    expect(got.breathing).toBe(false);
  });

  it("takes the compromised-route tone during bring-up", () => {
    // Arrange / Act — init is blue, like every other broken-route state.
    const got = phaseLabel("init");
    // Assert
    expect(got.tone).toBe("error");
  });

  it("still spins during bring-up, which really is in progress", () => {
    // Arrange / Act
    const got = phaseLabel("init");
    // Assert
    expect(got.breathing).toBe(true);
  });

  it("keeps a pending permission out of the alarm tones", () => {
    // Arrange / Act — the agent is ready for the user to look, not broken.
    const got = phaseLabel("permission");
    // Assert
    expect(got.tone).not.toBe("error");
  });
});

// --- the activity detail cell ------------------------------------------------

describe("activityDetail: exactly one account of the dead air", () => {
  it("is empty when nothing is live", () => {
    // Arrange / Act
    const got = activityDetail(input(), NOW);
    // Assert
    expect(got).toBeNull();
  });

  it("shows the running tool with its elapsed", () => {
    // Arrange
    const i = input({ items: [tool({ progressElapsedS: 12 })] });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert
    expect(got.text).toBe("Bash · 12s");
  });

  it("colors a running tool with the tool-title accent", () => {
    // Arrange
    const i = input({ items: [tool({ progressElapsedS: 12 })] });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert
    expect(got.tone).toBe("tool");
  });

  // AMENDED: this used to assert `compacting…` superseding the tool. The
  // compaction now has its own PHASE WORD, so the detail cell no longer speaks
  // for it — repeating the phase in the cell beside it spent the one activity
  // slot restating the word to its left and hid the tool that was the only
  // thing the cell could add.
  it("leaves a running tool in the cell while a compaction holds the phase word", () => {
    // Arrange
    const i = input({
      progress: progress({ compacting: { sinceMs: NOW - 3_000, detail: "compacting" } }),
      items: [tool({ progressElapsedS: 12 })],
    });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert
    expect(got.text).toBe("Bash · 12s");
  });

  it("still lets a retry speak while a compaction holds the phase word", () => {
    // Arrange — a retry is the agent STALLED, which the phase word does not
    // say, so it remains the cell's account of the dead air.
    const i = input({
      progress: progress({
        compacting: { sinceMs: NOW, detail: "compacting" },
        retrying: { sinceMs: NOW, detail: "attempt 3/10 · overloaded" },
      }),
    });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert
    expect(got.text).toBe("retrying · attempt 3/10 · overloaded");
  });

  it("shows nothing in the cell when a compaction is the only thing live", () => {
    // Arrange — the phase word already says `compacting`; a rung here would be
    // the same fact twice.
    const i = input({
      progress: progress({ compacting: { sinceMs: NOW - 3_000, detail: "compacting" } }),
    });
    // Act
    const got = activityDetail(i, NOW);
    // Assert
    expect(got).toBeNull();
  });

  it("lets a rate limit supersede a retry", () => {
    // Arrange
    const i = input({
      progress: progress({
        retrying: { sinceMs: NOW, detail: "attempt 3/10" },
        rateLimited: {
          active: true,
          resetsAt: Math.floor(NOW / 1000),
          utilization: 0.91,
          status: "allowed_warning",
        },
      }),
    });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert — the rung's wording is the session-usage line (user-directed
    // rewording); the assertion here is only about which rung WON.
    expect(got.text).toContain("Session usage");
  });

  it("leaves a quiet allowance out of the cell, so a running tool keeps it", () => {
    // Arrange — the vendor reported the session at 12% and called it allowed,
    // which is a figure worth carrying and NOT worth displacing the tool for.
    const i = input({
      items: [tool({ toolName: "Bash" })],
      progress: progress({
        rateLimited: { active: false, resetsAt: 0, utilization: 0.12, status: "allowed" },
      }),
    });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert
    expect(got.text).toContain("Bash");
  });

  it("lets a newsworthy weekly allowance claim the cell on its own", () => {
    // Arrange — only the WEEKLY window is active, and it must still speak.
    const i = input({
      items: [tool({ toolName: "Bash" })],
      progress: progress({
        rateLimitedWeekly: { active: true, resetsAt: 0, utilization: 0.91, status: "allowed_warning" },
      }),
    });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert
    expect(got.text).toBe("Weekly usage: 91%");
  });

  it("lets an auth prompt supersede everything, since it blocks on the reader", () => {
    // Arrange
    const i = input({
      progress: progress({
        authenticating: { sinceMs: NOW, detail: "paste your code" },
        rateLimited: { active: true, resetsAt: 0, utilization: 0, status: "limited" },
      }),
    });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert
    expect(got.text).toBe("auth · paste your code");
  });

  it("says the session is parked on the reader", () => {
    // Arrange
    const i = input({ progress: progress({ blocked: { sinceMs: NOW, detail: "waiting on you" } }) });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert
    expect(got.text).toBe("waiting on you");
  });

  it("lets a parked session supersede a retry", () => {
    // Arrange — nothing will happen until the reader acts, whatever the agent
    // was doing before it stopped.
    const i = input({
      progress: progress({
        blocked: { sinceMs: NOW, detail: "waiting on you" },
        retrying: { sinceMs: NOW, detail: "attempt 3/10" },
      }),
    });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert
    expect(got.text).toBe("waiting on you");
  });

  it("lets an auth prompt supersede a parked session", () => {
    // Arrange — auth is the same statement with a specific remedy attached.
    const i = input({
      progress: progress({
        authenticating: { sinceMs: NOW, detail: "paste your code" },
        blocked: { sinceMs: NOW, detail: "waiting on you" },
      }),
    });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert
    expect(got.text).toBe("auth · paste your code");
  });

  it("names the running hook", () => {
    // Arrange
    const i = input({ progress: progress({ hook: { sinceMs: NOW, detail: "PreToolUse:Bash" } }) });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert
    expect(got.text).toBe("hook · PreToolUse:Bash");
  });

  it("carries the rate limit's reset deadline as a countdown, not a clock reading", () => {
    // Arrange — a deadline three hours and twenty minutes out.
    const resetsAt = Math.floor((NOW + 3 * HOUR + 20 * MINUTE) / 1000);
    const i = input({
      progress: progress({
        rateLimited: { active: true, resetsAt, utilization: 0, status: "limited" },
      }),
    });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert — a time of day says nothing about how long the wait is.
    expect(got.text).toBe("Session reset: 3h 20m");
  });
});

// --- the rate-limit rung -----------------------------------------------------

describe("rateLimitActivity: both allowances, in words the reader acts on", () => {
  /** One allowance's window, defaulted to a warning deep into it. */
  function limit(over: Partial<NonNullable<ProgressInput["rateLimited"]>> = {}) {
    return {
      active: true,
      resetsAt: Math.floor(NOW / 1000),
      utilization: 0.8,
      status: "allowed_warning",
      ...over,
    };
  }

  it("names one allowance's two facts, separated by the interpunct", () => {
    // Arrange
    const r = limit({ resetsAt: Math.floor((NOW + 2 * HOUR) / 1000) });
    // Act
    const got = rateLimitActivity(progress({ rateLimited: r }), NOW);
    // Assert
    expect(got.text).toBe("Session usage: 80% • Session reset: 2h");
  });

  it("names the two allowances apart, separated by the pipe", () => {
    // Arrange — the reported bug: a barely touched session beside a spent week.
    const p = progress({
      rateLimited: { active: false, resetsAt: Math.floor((NOW + 90 * MINUTE) / 1000), utilization: 0.12, status: "allowed" },
      rateLimitedWeekly: { active: true, resetsAt: Math.floor((NOW + 2 * DAY) / 1000), utilization: 0.91, status: "allowed_warning" },
    });
    // Act
    const got = rateLimitActivity(p, NOW);
    // Assert
    expect(got.text).toBe(
      "Session usage: 12% • Session reset: 1h 30m  |  Weekly usage: 91% • Weekly reset: 2d",
    );
  });

  it("gives each allowance its OWN accent, since the two can read very differently", () => {
    // Arrange
    const p = progress({
      rateLimited: { active: false, resetsAt: 0, utilization: 0.12, status: "allowed" },
      rateLimitedWeekly: { active: true, resetsAt: 0, utilization: 0.91, status: "allowed_warning" },
    });
    // Act
    const got = rateLimitActivity(p, NOW);
    // Assert
    expect(got.html).toBe(
      `Session usage: <span class="pfooter-rl-ok">12%</span>  |  ` +
        `Weekly usage: <span class="pfooter-rl-limit">91%</span>`,
    );
  });

  it("omits an allowance the vendor never reported", () => {
    // Arrange — only the weekly window has ever arrived.
    const p = progress({ rateLimitedWeekly: limit({ resetsAt: 0 }) });
    // Act
    const got = rateLimitActivity(p, NOW);
    // Assert
    expect(got.text).toBe("Weekly usage: 80%");
  });

  it("drops the reset segment when the vendor reported no reset time", () => {
    // Arrange
    const p = progress({ rateLimited: limit({ resetsAt: 0 }) });
    // Act
    const got = rateLimitActivity(p, NOW);
    // Assert
    expect(got.text).toBe("Session usage: 80%");
  });

  it("drops the usage segment when the vendor reported no utilization", () => {
    // Arrange
    const p = progress({
      rateLimited: limit({ utilization: 0, resetsAt: Math.floor((NOW + 45 * MINUTE) / 1000) }),
    });
    // Act
    const got = rateLimitActivity(p, NOW);
    // Assert
    expect(got.text).toBe("Session reset: 45m");
  });

  it("falls back to a worded line when the vendor reported neither figure", () => {
    // Arrange
    const p = progress({ rateLimited: limit({ resetsAt: 0, utilization: 0 }) });
    // Act
    const got = rateLimitActivity(p, NOW);
    // Assert — never an empty cell: the window being open is itself the news.
    expect(got.text).toBe("Rate limit reported");
  });

  it("keeps the refusal's red on the worded fallback, so it is not silent chrome", () => {
    // Arrange — no figures at all, but the session is stopped dead.
    const p = progress({ rateLimited: limit({ resetsAt: 0, utilization: 0, status: "rejected" }) });
    // Act
    const got = rateLimitActivity(p, NOW);
    // Assert — a green line here would say the opposite of what it means.
    expect(got.html).toContain("pfooter-rl-limit");
  });

  it("takes the LOUDER allowance's accent for the worded fallback", () => {
    // Arrange — a quiet session beside a refused week; the week is the news.
    const p = progress({
      rateLimited: { active: false, resetsAt: 0, utilization: 0, status: "allowed" },
      rateLimitedWeekly: { active: true, resetsAt: 0, utilization: 0, status: "rejected" },
    });
    // Act
    const got = rateLimitActivity(p, NOW);
    // Assert
    expect(got.html).toContain("pfooter-rl-limit");
  });

  it("colors a 50% allowance green", () => {
    // Arrange — the top of the green band, which is inclusive.
    const r = limit({ utilization: 0.5 });
    // Act
    const got = rateLimitAccent(r);
    // Assert
    expect(got).toBe("pfooter-rl-ok");
  });

  it("colors a 51% allowance yellow", () => {
    // Arrange
    const r = limit({ utilization: 0.51 });
    // Act
    const got = rateLimitAccent(r);
    // Assert
    expect(got).toBe("pfooter-rl-warn");
  });

  it("colors a 75% allowance yellow", () => {
    // Arrange — the top of the yellow band, which is inclusive.
    const r = limit({ utilization: 0.75 });
    // Act
    const got = rateLimitAccent(r);
    // Assert
    expect(got).toBe("pfooter-rl-warn");
  });

  it("colors a 76% allowance orange", () => {
    // Arrange
    const r = limit({ utilization: 0.76 });
    // Act
    const got = rateLimitAccent(r);
    // Assert
    expect(got).toBe("pfooter-rl-high");
  });

  it("colors an 89% allowance orange", () => {
    // Arrange — the top of the orange band, which stops short of 90%.
    const r = limit({ utilization: 0.89 });
    // Act
    const got = rateLimitAccent(r);
    // Assert
    expect(got).toBe("pfooter-rl-high");
  });

  it("colors a 90% allowance red", () => {
    // Arrange — the ONE inclusive floor: 90% exactly is spent, not merely low.
    const r = limit({ utilization: 0.9 });
    // Act
    const got = rateLimitAccent(r);
    // Assert
    expect(got).toBe("pfooter-rl-limit");
  });

  it("colors an outright rejection red however little of the allowance is spent", () => {
    // Arrange — being capped is red by definition.
    const r = limit({ utilization: 0.1, status: "rejected" });
    // Act
    const got = rateLimitAccent(r);
    // Assert
    expect(got).toBe("pfooter-rl-limit");
  });

  it("colors a status-less report red, since that shape only ever meant a refusal", () => {
    // Arrange
    const r = limit({ utilization: 0.1, status: "" });
    // Act
    const got = rateLimitAccent(r);
    // Assert
    expect(got).toBe("pfooter-rl-limit");
  });

  it("puts the accent on the figures, not on the labels", () => {
    // Arrange
    const p = progress({ rateLimited: limit({ resetsAt: 0, utilization: 0.8 }) });
    // Act
    const got = rateLimitActivity(p, NOW);
    // Assert
    expect(got.html).toBe(`Session usage: <span class="pfooter-rl-high">80%</span>`);
  });

  it("leaves the cell's own tone muted, so the figures are what the eye lands on", () => {
    // Arrange
    const p = progress({ rateLimited: limit() });
    // Act
    const got = rateLimitActivity(p, NOW);
    // Assert
    expect(got.tone).toBe("muted");
  });

  it("carries the same wording into the expansion sheet", () => {
    // Arrange
    const i = input({ progress: progress({ rateLimited: limit({ resetsAt: 0 }) }) });
    // Act
    const got = sheetHtml(i, NOW);
    // Assert
    expect(got).toContain(`Session usage: <span class="pfooter-rl-high">80%</span>`);
  });

  it("puts a QUIET allowance in the sheet, which the strip's rung declines to show", () => {
    // Arrange — nothing is newsworthy, so the strip stays on the running tool.
    const i = input({
      progress: progress({
        rateLimitedWeekly: { active: false, resetsAt: 0, utilization: 0.3, status: "allowed" },
      }),
    });
    // Act
    const got = sheetHtml(i, NOW);
    // Assert
    expect(got).toContain(`Weekly usage: <span class="pfooter-rl-ok">30%</span>`);
  });

  it("leaves an unrelated activity rung's plain-text rendering untouched", () => {
    // Arrange — the hook rung ships no markup, so the cell escapes its text.
    const i = input({ progress: progress({ hook: { sinceMs: NOW, detail: "PreToolUse:<Bash>" } }) });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert
    expect(activityBody(got)).toBe("hook · PreToolUse:&lt;Bash&gt;");
  });
});

// --- the running tool --------------------------------------------------------

describe("runningTool: which call the detail cell is about", () => {
  it("takes the newest unsettled call", () => {
    // Arrange
    const older = tool({ toolUseId: "old", toolName: "Read" });
    const newer = tool({ toolUseId: "new", toolName: "Bash" });
    // Act
    const got = runningTool([older, newer]);
    // Assert
    expect(got?.toolUseId).toBe("new");
  });

  it("skips a settled call", () => {
    // Arrange
    const settled = tool({ result: { isError: false, content: "ok" } });
    // Act
    const got = runningTool([settled]);
    // Assert
    expect(got).toBeNull();
  });

  it("skips a result-only item still awaiting its tool_use pair", () => {
    // Arrange — the store's half-merged shape carries an empty name.
    const orphan = tool({ toolName: "" });
    // Act
    const got = runningTool([orphan]);
    // Assert
    expect(got).toBeNull();
  });

  it("ignores non-tool items", () => {
    // Arrange
    const items: ConversationItem[] = [
      {
        kind: "user-turn",
        requestId: "r1",
        ts: "",
        content: [{ type: "text", text: "hi" }],
      } as ConversationItem,
    ];
    // Act
    const got = runningTool(items);
    // Assert
    expect(got).toBeNull();
  });

  it("drops a call the newest user turn has already scrolled past", () => {
    // Arrange — an interrupted turn's call never settled, then a prompt was sent.
    const orphan = tool({ toolUseId: "orphan", toolName: "Bash" });
    // Act
    const got = runningTool([orphan, userTurn()]);
    // Assert
    expect(got).toBeNull();
  });

  it("takes a call the newest user turn precedes", () => {
    // Arrange — the same feed, with this turn's own call appended after the prompt.
    const orphan = tool({ toolUseId: "orphan", toolName: "Bash" });
    const live = tool({ toolUseId: "live", toolName: "Read" });
    // Act
    const got = runningTool([orphan, userTurn(), live]);
    // Assert
    expect(got?.toolUseId).toBe("live");
  });
});

describe("toolElapsed: whose clock the tool cell reads", () => {
  it("prefers the daemon's heartbeat clock", () => {
    // Arrange — the heartbeat says 5s, this tab's own reading would say 12s.
    const t = tool({ progressElapsedS: 5 });
    // Act
    const got = toolElapsed(t, NOW);
    // Assert
    expect(got).toBe("5s");
  });

  it("falls back to this tab's reading before any heartbeat", () => {
    // Arrange
    const t = tool();
    // Act
    const got = toolElapsed(t, NOW);
    // Assert
    expect(got).toBe("12s");
  });
});

// --- the token cell ----------------------------------------------------------

describe("tokenCellHtml: the turn's input tokens, and never its output", () => {
  it("shows the turn's cumulative input figure", () => {
    // Arrange / Act
    const got = tokenCellHtml(progress({ inputTokens: 41_200 }));
    // Assert — the shared pill formatter's magnitude, not a footer-local one.
    expect(got).toContain("41k in");
  });

  it("shows the thinking ticker beside it while reasoning", () => {
    // Arrange / Act
    const got = tokenCellHtml(progress({ inputTokens: 41_200, thinkingTokens: 1_400 }));
    // Assert
    expect(got).toContain("1.4k thought");
  });

  it("reads the idle dash when the turn has spent nothing yet", () => {
    // Arrange / Act — the turn's landed figure lives on its response bubble
    // now, so between turns the cell reports nothing rather than a stale sum.
    const got = tokenCellHtml(progress());
    // Assert
    expect(got).toBe('<span class="info-tokens">--</span>');
  });

  it("drops the dash the moment a figure exists to show", () => {
    // Arrange / Act — a real figure displaces the placeholder outright.
    const got = tokenCellHtml(progress({ inputTokens: 41_200 }));
    // Assert
    expect(got).not.toContain("--");
  });

  it("drops the dash while only the thinking ticker is running", () => {
    // Arrange / Act — reasoning before the first request lands is activity,
    // not idleness, so the cell shows the ticker alone.
    const got = tokenCellHtml(progress({ thinkingTokens: 1_400 }));
    // Assert
    expect(got).toBe('<span class="pfooter-thinking-tokens">1.4k thought</span>');
  });

  it("arms the counter at zero the moment the daemon acks the prompt", () => {
    // Arrange — the ack's publish stamps the turn's start before any usage.
    const p = progress({ turnStartedAtMs: 1_700_000_000_000, inputTokens: 0 });
    // Act
    const got = tokenCellHtml(p);
    // Assert
    expect(got).toBe('<span class="info-tokens">0 in</span>');
  });

  it("shows the real figure once usage lands on the armed turn", () => {
    // Arrange
    const p = progress({ turnStartedAtMs: 1_700_000_000_000, inputTokens: 41_200 });
    // Act
    const got = tokenCellHtml(p);
    // Assert — the arming zero is displaced, never added to.
    expect(got).toBe('<span class="info-tokens">41k in</span>');
  });

  it("keeps the idle dash when no turn is in flight", () => {
    // Arrange — an unstamped turn start is the daemon saying nothing is running.
    const p = progress({ turnStartedAtMs: 0, inputTokens: 0 });
    // Act
    const got = tokenCellHtml(p);
    // Assert
    expect(got).toBe('<span class="info-tokens">--</span>');
  });

  it("composes the arming zero beside a running thinking ticker", () => {
    // Arrange
    const p = progress({ turnStartedAtMs: 1_700_000_000_000, thinkingTokens: 1_400 });
    // Act
    const got = tokenCellHtml(p);
    // Assert
    expect(got).toBe(
      '<span class="info-tokens">0 in</span> ' +
        '<span class="pfooter-thinking-tokens">1.4k thought</span>',
    );
  });
});

// --- the counters cluster ----------------------------------------------------

describe("countersHtml: the relocated rosters and the badges", () => {
  it("renders the subagent roster through the shared counter facade", () => {
    // Arrange
    const i = input({ agents: [counterEntry()] });
    // Act
    const got = countersHtml(i, CLOSED);
    // Assert — the facade's own chip markup, not a footer-local reimplementation.
    expect(got).toContain("data-agents-toggle");
  });

  it("renders the task roster through the same facade", () => {
    // Arrange
    const i = input({ tasks: [counterEntry({ id: "t1" })] });
    // Act
    const got = countersHtml(i, CLOSED);
    // Assert
    expect(got).toContain("data-tasks-toggle");
  });

  it("shows the queue depth as a badge", () => {
    // Arrange
    const i = input({ progress: progress({ queueDepth: 3 }) });
    // Act
    const got = countersHtml(i, CLOSED);
    // Assert
    expect(got).toContain("3 queued");
  });

  it("shows waiting permissions as a badge", () => {
    // Arrange
    const i = input({ progress: progress({ pendingPermissions: 1 }) });
    // Act
    const got = countersHtml(i, CLOSED);
    // Assert
    expect(got).toContain("1 perm");
  });

  it("hides every counter at zero", () => {
    // Arrange / Act — a badge over nothing is chrome with no news.
    const got = countersHtml(input(), CLOSED);
    // Assert
    expect(got).toBe("");
  });

  it("drops a roster whose entries have all settled", () => {
    // Arrange
    const i = input({ agents: [counterEntry({ status: "done" })] });
    // Act
    const got = countersHtml(i, CLOSED);
    // Assert
    expect(got).toBe("");
  });
});

describe("hasLiveCounters", () => {
  it("is false when every roster entry has settled", () => {
    // Arrange
    const i = input({ agents: [counterEntry({ status: "done" })] });
    // Act / Assert
    expect(hasLiveCounters(i)).toBe(false);
  });

  it("is true while one roster entry still runs", () => {
    // Arrange
    const i = input({ tasks: [counterEntry({ status: "starting" })] });
    // Act / Assert
    expect(hasLiveCounters(i)).toBe(true);
  });
});

// --- the error row -----------------------------------------------------------

describe("errorRowHtml: the persistent error line", () => {
  it("renders nothing when no error stands", () => {
    // Arrange / Act
    const got = errorRowHtml(progress());
    // Assert
    expect(got).toBe("");
  });

  it("carries the daemon's classified message", () => {
    // Arrange / Act
    const got = errorRowHtml(
      progress({ failure: failureCard({ message: "overloaded (529) after 10 attempts" }) }),
    );
    // Assert
    expect(got).toContain("overloaded (529) after 10 attempts");
  });

  it("addresses the card it names, so the row can scroll to it", () => {
    // Arrange / Act
    const got = errorRowHtml(progress({ failure: failureCard({ uuid: "failure:e9" }) }));
    // Assert
    expect(got).toContain('data-pfooter-error-uuid="failure:e9"');
  });

  it("is not clickable when the failure names no card", () => {
    // Arrange / Act — a turn-end error carries no ApiErrorLine of its own.
    const got = errorRowHtml(progress({ failure: failureCard({ uuid: "" }) }));
    // Assert
    expect(got).not.toContain("addressable");
  });

  it("takes the API class's color rather than a red of its own", () => {
    // Arrange / Act — the footer must not contradict the card it points at.
    const got = errorRowHtml(progress({ failure: failureCard({ errorClass: "API" }) }));
    // Assert
    expect(got).toContain("failure-api");
  });

  it("takes the INTERNAL class's color for a machinery failure", () => {
    // Arrange / Act
    const got = errorRowHtml(progress({ failure: failureCard({ errorClass: "INTERNAL" }) }));
    // Assert
    expect(got).toContain("failure-internal");
  });

  it("escapes the message", () => {
    // Arrange / Act
    const got = errorRowHtml(progress({ failure: failureCard({ message: "<img src=x>" }) }));
    // Assert
    expect(got).not.toContain("<img");
  });
});

// --- the expansion sheet -----------------------------------------------------

describe("sheetHtml: the detail the thin strip drops", () => {
  it("says so when there is nothing else in flight", () => {
    // Arrange / Act
    const got = sheetHtml(input(), NOW);
    // Assert
    expect(got).toContain("nothing else in flight");
  });

  it("gives each open window a row with its age", () => {
    // Arrange
    const i = input({ progress: progress({ hook: { sinceMs: NOW - 5_000, detail: "PreToolUse" } }) });
    // Act
    const got = sheetHtml(i, NOW);
    // Assert
    expect(got).toContain("hook (5s) — PreToolUse");
  });

  it("gives the blocked window its own sheet row", () => {
    // Arrange
    const i = input({
      progress: progress({ blocked: { sinceMs: NOW - 9_000, detail: "waiting on you" } }),
    });
    // Act
    const got = sheetHtml(i, NOW);
    // Assert
    expect(got).toContain("blocked (9s) — waiting on you");
  });

  it("reports the live task count", () => {
    // Arrange
    const i = input({ progress: progress({ liveTaskCount: 2 }) });
    // Act
    const got = sheetHtml(i, NOW);
    // Assert
    expect(got).toContain("2 live tasks");
  });

  it("omits an unfed ttft rather than printing a zero", () => {
    // Arrange / Act — no producer relays first-token latency yet.
    const got = sheetHtml(input(), NOW);
    // Assert
    expect(got).not.toContain("first token");
  });
});

// --- the whole dock ----------------------------------------------------------

describe("footerHtml: the phase comes from the workspace state (F5)", () => {
  it("shows the phase the WORKSPACE STATE resolved", () => {
    // Arrange / Act — the footer's phase source is the same message the tab
    // bar and the sidebar dot read, so the three cannot disagree.
    const got = footerHtml(input({ renderState: "thinking" }), CLOSED, NOW);
    // Assert
    expect(got).toContain("thinking");
  });

  it("shows ready for a fully backfilled workspace with no prompt yet", () => {
    // Arrange / Act — THE regression this closes. The footer used to read a
    // copy of the phase kept on ProgressView, seeded INIT and refreshed only
    // on the progress resolver's own triggers, so a freshly opened workspace
    // read "starting" against an already-green tab until the first prompt.
    const got = footerHtml(input({ renderState: "ready" }), CLOSED, NOW);
    // Assert
    expect(got).toContain("ready");
    expect(got).not.toContain("starting");
  });

  it("carries the phase word alone in the phase cell", () => {
    // Arrange / Act — an impaired route is the case that used to hang a grey
    // session-status word beside the colored phase.
    const got = footerHtml(input({ renderState: "degraded" }), CLOSED, NOW);
    // Assert
    expect(phaseCell(got)).toBe('<div class="pfooter-cell pfooter-phase error">degraded</div>');
  });

  it("names no phase at all before a state has been resolved", () => {
    // Arrange / Act — naming one anyway is exactly the fabrication the stale
    // mirror committed.
    const got = footerHtml(input({ renderState: null }), CLOSED, NOW);
    // Assert
    expect(got).not.toContain("pfooter-phase");
  });

  it("takes no phase from the progress view, which no longer carries one", () => {
    // Arrange / Act — the ProgressInput has no `state` field to read; the
    // footer's only phase source is renderState.
    const got = footerHtml(input({ renderState: "vendor_blocked" }), CLOSED, NOW);
    // Assert
    expect(got).toContain("blocked");
  });
});

describe("footerHtml: the breathing phase word", () => {
  it("wraps a working phase's word so it can breathe", () => {
    // Arrange / Act — the word IS the liveness signal now.
    const got = footerHtml(input({ renderState: "thinking" }), CLOSED, NOW);
    // Assert
    expect(got).toContain('class="pfooter-breath"');
  });

  it("leaves a resting phase's word bare", () => {
    // Arrange / Act — nothing is working, so nothing should move.
    const got = footerHtml(input({ renderState: "ready" }), CLOSED, NOW);
    // Assert
    expect(got).not.toContain("pfooter-breath");
  });

  it("renders no rotating arc beside the word any more", () => {
    // Arrange / Act — the breath replaced the spinner outright.
    const got = footerHtml(input({ renderState: "thinking" }), CLOSED, NOW);
    // Assert
    expect(got).not.toContain("pfooter-spin");
  });

  it("paints the working word in the ramp stop the tick is standing on", () => {
    // Arrange
    const breath = { shade: 3, elapsedMs: 0 };
    // Act
    const got = footerHtml(input({ renderState: "thinking" }), CLOSED, NOW, breath);
    // Assert
    expect(got).toContain(`color:${breathColor(3)}`);
  });

  it("seeks the fresh element into the cycle with a negative delay", () => {
    // Arrange — the footer rewrites its markup every chrome frame, so without
    // this the breath would restart on each one.
    const breath = { shade: 0, elapsedMs: 1234 };
    // Act
    const got = footerHtml(input({ renderState: "thinking" }), CLOSED, NOW, breath);
    // Assert
    expect(got).toContain("animation-delay:-1234ms");
  });

  it("rounds a fractional elapsed to whole milliseconds", () => {
    // Arrange — a raw float would emit an unparseable delay.
    const breath = { shade: 0, elapsedMs: 1234.56 };
    // Act
    const got = footerHtml(input({ renderState: "thinking" }), CLOSED, NOW, breath);
    // Assert
    expect(got).toContain("animation-delay:-1235ms");
  });

  it("ends the phase cell at the breathing word", () => {
    // Arrange / Act — nothing trails the working word inside its cell, so the
    // cell can never pair the breathing phase with a second, still word.
    const got = footerHtml(input({ renderState: "thinking" }), CLOSED, NOW, {
      shade: 0,
      elapsedMs: 0,
    });
    // Assert
    expect(phaseCell(got)).toMatch(/thinking<\/span><\/div>$/);
  });
});

// --- the interrupt chip (I1) -------------------------------------------------

describe("alreadyCompletePhaseViolation: footer/state invariant", () => {
  it.each(["thinking", "clearing", "compacting", "permission"] as const)(
    "flags already_complete beside %s",
    (state) => {
      expect(alreadyCompletePhaseViolation(state, "already_complete")).toBe(state);
    },
  );

  it.each(["ready", "idle", "done", "vendor_blocked"] as const)(
    "accepts already_complete beside settled phase %s",
    (state) => {
      expect(alreadyCompletePhaseViolation(state, "already_complete")).toBeNull();
    },
  );

  it("does not flag an active phase for another interrupt outcome", () => {
    expect(alreadyCompletePhaseViolation("thinking", "interrupted")).toBeNull();
  });
});

// REWRITTEN off the retired flat merge_queue_position / merge_queue_depth pair.
// Those figures reach the footer only through MergeStatus's `enqueued` arm now,
// so every case below drives the same guarantee through the structured input.
describe("footerHtml: the merge queue's place beside the phase", () => {
  /** An enqueued merge status at a given place on its repository's queue. */
  const enqueued = (position: number, depth: number): MergeStatus => ({
    runId: "run-queued",
    phaseStartedAtMs: NOW - 5_000,
    updatedAtMs: NOW,
    phase: { case: "enqueued", value: { position, depth } },
  });

  it("renders the queue place while the workspace is queued", () => {
    // Arrange
    const built = input({ renderState: "merge_queued", mergeStatus: enqueued(2, 3) });
    // Act
    const got = footerHtml(built, CLOSED, NOW);
    // Assert
    expect(got).toContain(">2/3</div>");
  });

  it("renders the queue place in its own cell, not inside the phase cell", () => {
    // Arrange — the phase cell's geometry is fixed to the closed phase
    // vocabulary; the queue's two figures are unbounded.
    const built = input({ renderState: "merge_queued", mergeStatus: enqueued(2, 3) });
    // Act
    const got = footerHtml(built, CLOSED, NOW);
    // Assert
    expect(got).toContain("pfooter-merge-queue");
  });

  it("still names the phase beside the place", () => {
    // Arrange
    const built = input({ renderState: "merge_queued", mergeStatus: enqueued(2, 3) });
    // Act
    const got = footerHtml(built, CLOSED, NOW);
    // Assert
    expect(got).toContain("merge queued");
  });

  it("names the enqueuing phase with no queue cell beside it", () => {
    // Arrange — the transition the pipeline renders: the phase word alone
    // while enqueuing, the word plus the place once queued.
    const built = input({ renderState: "merge_enqueuing", mergeStatus: null });
    // Act
    const got = footerHtml(built, CLOSED, NOW);
    // Assert
    expect(got).toContain("merge enqueuing");
    expect(got).not.toContain("pfooter-merge-queue");
  });

  it("renders NO queue cell when the workspace is not enqueued", () => {
    // Arrange
    const built = input({ renderState: "thinking", mergeStatus: null });
    // Act
    const got = footerHtml(built, CLOSED, NOW);
    // Assert
    expect(got).not.toContain("pfooter-merge-queue");
  });

  it("renders NO queue cell for an enqueued arm carrying no depth", () => {
    // Arrange — depth 0 is the run saying it has no queue place to report; a
    // `0/0` chip would be noise beside every phase.
    const built = input({ renderState: "merge_queued", mergeStatus: enqueued(0, 0) });
    // Act
    const got = footerHtml(built, CLOSED, NOW);
    // Assert
    expect(got).not.toContain("pfooter-merge-queue");
  });

  it("names a running merge in the phase cell", () => {
    // Arrange
    const built = input({ renderState: "merging", mergeStatus: null });
    // Act
    const got = footerHtml(built, CLOSED, NOW);
    // Assert
    expect(got).toContain("merging");
  });
});

describe("interruptChip: the outcome of the stop the user asked for", () => {
  it("says interrupted when the stop was delivered to a live turn", () => {
    // Arrange / Act
    const got = interruptChip(progress({ interrupt: { sinceMs: NOW, outcome: "interrupted" } }));
    // Assert
    expect(got?.text).toBe("interrupted");
  });

  it("keeps the delivered stop in the calm tone, not an alarm one", () => {
    // Arrange / Act — a concluded turn the user asked for is not a fault.
    const got = interruptChip(progress({ interrupt: { sinceMs: NOW, outcome: "interrupted" } }));
    // Assert
    expect(got?.tone).toBe("ok");
  });

  it("says the turn was already over for ALREADY_COMPLETE", () => {
    // Arrange / Act — the word must not repeat `interrupted`: nothing was
    // stopped, because there was nothing left to stop.
    const got = interruptChip(progress({ interrupt: { sinceMs: NOW, outcome: "already_complete" } }));
    // Assert
    expect(got?.text).toBe("already finished");
  });

  it("treats ALREADY_COMPLETE as a success rather than an error", () => {
    // Arrange / Act — the user asked for the turn to be over and it already
    // is; painting that red is exactly the misread the outcome enum ends.
    const got = interruptChip(progress({ interrupt: { sinceMs: NOW, outcome: "already_complete" } }));
    // Assert
    expect(got?.tone).toBe("ok");
  });

  it("says the stop failed for FAILED", () => {
    // Arrange / Act
    const got = interruptChip(progress({ interrupt: { sinceMs: NOW, outcome: "failed" } }));
    // Assert
    expect(got?.text).toBe("stop failed");
  });

  it("gives FAILED the error tone, the only outcome that reads as a failure", () => {
    // Arrange / Act
    const got = interruptChip(progress({ interrupt: { sinceMs: NOW, outcome: "failed" } }));
    // Assert
    expect(got?.tone).toBe("error");
  });

  it("shows nothing while no interrupt window is open", () => {
    // Arrange / Act
    const got = interruptChip(progress());
    // Assert
    expect(got).toBeNull();
  });
});

describe("footerHtml: the V4 segmented dock", () => {
  it("renders the interrupt chip beside the phase while the window is open", () => {
    // Arrange
    const i = input({
      renderState: "interrupted",
      progress: progress({ interrupt: { sinceMs: NOW, outcome: "interrupted" } }),
    });
    // Act
    const got = footerHtml(i, CLOSED, NOW);
    // Assert
    expect(got).toContain(`<div class="pfooter-cell pfooter-interrupt ok"`);
  });

  it("drops the chip when the frame arrives with the window inactive", () => {
    // Arrange / Act — the daemon cleared it; the webapp holds nothing of its
    // own that could keep it on screen.
    const got = footerHtml(input({ progress: progress({ interrupt: null }) }), CLOSED, NOW);
    // Assert
    expect(got).not.toContain("pfooter-interrupt");
  });

  it("leaves the activity cell to live work rather than the interrupt", () => {
    // Arrange — the chip qualifies the phase; the grow cell stays the home of
    // what is happening NOW.
    const i = input({ progress: progress({ interrupt: { sinceMs: NOW, outcome: "failed" } }) });
    // Act
    const got = footerHtml(i, CLOSED, NOW);
    // Assert — the cell carries the grabber notch (its absolute-positioning
    // host) and nothing else: no interrupt text leaked into it.
    expect(got).toContain(
      `<div class="pfooter-cell pfooter-grow muted">` +
        `<div class="pfooter-grab" aria-hidden="true"></div></div>`,
    );
  });


  it("renders nothing before the daemon has resolved anything", () => {
    // Arrange / Act
    const got = footerHtml(input({ progress: null }), CLOSED, NOW);
    // Assert
    expect(got).toBe("");
  });

  it("wears the grabber notch", () => {
    // Arrange / Act
    const got = footerHtml(input(), CLOSED, NOW);
    // Assert
    expect(got).toContain("pfooter-grab");
  });

  it("hangs the notch inside the grow cell, not above the strip", () => {
    // Arrange / Act — the grow cell is the notch's positioning host, so the
    // notch centers on the middle section like the activity text does.
    const got = footerHtml(input(), CLOSED, NOW);
    // Assert
    expect(got).toMatch(/class="pfooter-cell pfooter-grow[^"]*">[^<]*<div class="pfooter-grab"/);
  });

  it("marks the one span the clock tick repaints", () => {
    // Arrange / Act
    const got = footerHtml(input(), CLOSED, NOW);
    // Assert
    expect(got).toContain("data-task-timer");
  });

  it("bakes the current clock reading in while a turn runs", () => {
    // Arrange
    const i = input({ progress: progress({ turnStartedAtMs: NOW - 24_000 }), timerLabel: "24s" });
    // Act
    const got = footerHtml(i, CLOSED, NOW);
    // Assert
    expect(got).toContain(">24s<");
  });

  it("shows the idle label off-turn rather than dropping the clock cell", () => {
    // Arrange / Act — the cell owns the tick's target span.
    const got = footerHtml(input(), CLOSED, NOW);
    // Assert
    expect(got).toContain(">--<");
  });

  it("withholds the sheet while collapsed", () => {
    // Arrange / Act
    const got = footerHtml(input(), CLOSED, NOW);
    // Assert
    expect(got).not.toContain("pfooter-sheet");
  });

  it("shows the sheet when expanded", () => {
    // Arrange / Act
    const got = footerHtml(input(), { ...CLOSED, expanded: true }, NOW);
    // Assert
    expect(got).toContain("pfooter-sheet");
  });

  it("reports its expansion to assistive tech", () => {
    // Arrange / Act
    const got = footerHtml(input(), { ...CLOSED, expanded: true }, NOW);
    // Assert
    expect(got).toContain('aria-expanded="true"');
  });

  it("dashes the token cell when the turn has spent nothing", () => {
    // Arrange / Act — off-turn the cell reads the idle dash rather than
    // vanishing, so the strip's geometry does not jump at a turn boundary.
    const got = footerHtml(input(), CLOSED, NOW);
    // Assert
    expect(got).toContain('<div class="pfooter-cell pfooter-tokens"><span class="info-tokens">--</span></div>');
  });

  it("escapes the activity detail", () => {
    // Arrange
    const i = input({ progress: progress({ hook: { sinceMs: NOW, detail: "<img src=x>" } }) });
    // Act
    const got = footerHtml(i, CLOSED, NOW);
    // Assert
    expect(got).not.toContain("<img");
  });
});

// --- click delegation --------------------------------------------------------

describe("footerClickAction", () => {
  it("flips the agents overlay from its chip", () => {
    // Arrange / Act
    const got = footerClickAction(target({ "[data-agents-toggle]": {} }));
    // Assert
    expect(got).toEqual({ kind: "toggle-menu", menu: "agents" });
  });

  it("flips the tasks overlay from its chip", () => {
    // Arrange / Act
    const got = footerClickAction(target({ "[data-tasks-toggle]": {} }));
    // Assert
    expect(got).toEqual({ kind: "toggle-menu", menu: "tasks" });
  });

  it("reveals the bubble a roster row names", () => {
    // Arrange / Act
    const got = footerClickAction(target({ ".agent-row": { "data-agent-id": "tu9" } }));
    // Assert
    expect(got).toEqual({ kind: "reveal-agent", agentId: "tu9" });
  });

  it("reveals the bubble a task row names", () => {
    // Arrange / Act
    const got = footerClickAction(target({ ".task-row": { "data-task-id": "t9" } }));
    // Assert
    expect(got).toEqual({ kind: "reveal-task", taskId: "t9" });
  });

  it("scrolls the feed to the item the error row names", () => {
    // Arrange / Act
    const got = footerClickAction(
      target({ "[data-pfooter-error-uuid]": { "data-pfooter-error-uuid": "e9" } }),
    );
    // Assert
    expect(got).toEqual({ kind: "reveal-error", uuid: "e9" });
  });

  it("expands the dock from a click on a bare cell", () => {
    // Arrange / Act
    const got = footerClickAction(target({ "[data-pfooter-strip]": {} }));
    // Assert
    expect(got).toEqual({ kind: "toggle-expand" });
  });

  it("lets a roster chip inside the strip win over the expansion toggle", () => {
    // Arrange — the chip lives INSIDE the clickable strip.
    const got = footerClickAction(
      target({ "[data-agents-toggle]": {}, "[data-pfooter-strip]": {} }),
    );
    // Assert
    expect(got).toEqual({ kind: "toggle-menu", menu: "agents" });
  });

  it("is null for a click on nothing actionable", () => {
    // Arrange / Act
    const got = footerClickAction(target({}));
    // Assert
    expect(got).toBeNull();
  });
});

// --- the DOM owner -----------------------------------------------------------

describe("ProgressFooter", () => {
  it("writes the dock into its slot", () => {
    // Arrange
    const el = document.createElement("div");
    const footer = new ProgressFooter(el, () => NOW);
    // Act
    footer.render(input());
    // Assert
    expect(el.querySelector(".pfooter")).not.toBeNull();
  });

  it("collapses its slot before the daemon has resolved anything", () => {
    // Arrange
    const el = document.createElement("div");
    const footer = new ProgressFooter(el, () => NOW);
    // Act
    footer.render(input({ progress: null }));
    // Assert
    expect(el.innerHTML).toBe("");
  });

  it("repaints ONLY the clock span on a tick", () => {
    // Arrange
    const el = document.createElement("div");
    const footer = new ProgressFooter(el, () => NOW);
    footer.render(input({ progress: progress({ turnStartedAtMs: NOW - 1_000 }) }));
    const before = el.querySelector(".pfooter-phase")?.outerHTML;
    // Act
    footer.paintTurnTimer("1m 5s");
    // Assert — the tick is a paint, never a re-render.
    expect(el.querySelector("[data-task-timer]")?.textContent).toBe("1m 5s");
    expect(el.querySelector(".pfooter-phase")?.outerHTML).toBe(before);
  });

  it("advances the breath's color when a new progress view arrives", () => {
    // Arrange — a distinct ProgressInput object is exactly "the daemon sent
    // another one", which is the tick the color channel reports.
    const el = document.createElement("div");
    const footer = new ProgressFooter(el, () => NOW);
    footer.render(input({ renderState: "thinking", progress: progress() }));
    // Act
    footer.render(input({ renderState: "thinking", progress: progress() }));
    // Assert
    const style = el.querySelector(".pfooter-breath")?.getAttribute("style") ?? "";
    expect(style).toContain(`color:${breathColor(1)}`);
  });

  it("holds the breath's color across a re-render of the same view", () => {
    // Arrange — the footer renders on the chrome cadence, so most renders
    // carry no new arrival and must not step the ramp.
    const el = document.createElement("div");
    const footer = new ProgressFooter(el, () => NOW);
    const view = progress();
    footer.render(input({ renderState: "thinking", progress: view }));
    // Act
    footer.render(input({ renderState: "thinking", progress: view }));
    // Assert
    const style = el.querySelector(".pfooter-breath")?.getAttribute("style") ?? "";
    expect(style).toContain(`color:${breathColor(0)}`);
  });

  it("wraps the ramp rather than running out of shades", () => {
    // Arrange — a long turn delivers far more views than the ramp has stops.
    const el = document.createElement("div");
    const footer = new ProgressFooter(el, () => NOW);
    // Act — one adoption plus a full lap.
    for (let i = 0; i <= BREATH_SHADES; i += 1) {
      footer.render(input({ renderState: "thinking", progress: progress() }));
    }
    // Assert
    const style = el.querySelector(".pfooter-breath")?.getAttribute("style") ?? "";
    expect(style).toContain(`color:${breathColor(0)}`);
  });

  it("does NOT reset the breath's size cycle when a new view arrives", () => {
    // Arrange — THE requirement the epoch exists for: the word carries on to
    // its already-planned next size instead of snapping back to the start.
    const el = document.createElement("div");
    let now = NOW;
    const footer = new ProgressFooter(el, () => now);
    footer.render(input({ renderState: "thinking", progress: progress() }));
    // Act — a second view lands most of a breath later.
    now = NOW + 1_700;
    footer.render(input({ renderState: "thinking", progress: progress() }));
    // Assert — the delay seeks the fresh element to where the cycle already was.
    const style = el.querySelector(".pfooter-breath")?.getAttribute("style") ?? "";
    expect(style).toContain("animation-delay:-1700ms");
  });

  it("leaves no chip behind when the next frame carries the window closed", () => {
    // Arrange — the daemon clears the window on the next turn, and the webapp
    // keeps no bookkeeping that could outlive the frame it was told in.
    const el = document.createElement("div");
    const footer = new ProgressFooter(el, () => NOW);
    footer.render(input({ progress: progress({ interrupt: { sinceMs: NOW, outcome: "failed" } }) }));
    // Act
    footer.render(input({ progress: progress({ interrupt: null }) }));
    // Assert
    expect(el.querySelector(".pfooter-interrupt")).toBeNull();
  });

  it("opens one overlay at a time", () => {
    // Arrange
    const footer = new ProgressFooter(document.createElement("div"), () => NOW);
    footer.setMenu("agents");
    // Act
    footer.setMenu("tasks");
    // Assert
    expect(footer.disclosure()).toMatchObject({ agentsOpen: false, tasksOpen: true });
  });

  it("closes every overlay on a click-away", () => {
    // Arrange
    const footer = new ProgressFooter(document.createElement("div"), () => NOW);
    footer.setMenu("agents");
    // Act
    footer.closeMenus();
    // Assert
    expect(footer.disclosure().agentsOpen).toBe(false);
  });

  it("keeps the sheet open across a menu close", () => {
    // Arrange — the two disclosures are independent.
    const footer = new ProgressFooter(document.createElement("div"), () => NOW);
    footer.toggleExpanded();
    // Act
    footer.closeMenus();
    // Assert
    expect(footer.disclosure().expanded).toBe(true);
  });

  it("survives a re-render with an overlay open", () => {
    // Arrange
    const el = document.createElement("div");
    const footer = new ProgressFooter(el, () => NOW);
    footer.setMenu("agents");
    // Act
    footer.render(input({ agents: [counterEntry()] }));
    // Assert — disclosure is renderer-owned, so the overlay outlives the frame.
    expect(el.querySelector(".agents-overlay")).not.toBeNull();
  });
});

// --- the structured merge status (WorkspaceState.merge_status) ---------------

describe("the footer's structured merge status", () => {
  /** A merge status carrying PHASE, with the envelope fixed. */
  const merge = (phase: MergeStatus["phase"]): MergeStatus => ({
    runId: "run-1",
    phaseStartedAtMs: NOW - 5_000,
    updatedAtMs: NOW,
    phase,
  });

  const PICKING = merge({
    case: "cherryPicking",
    value: { commitsTotal: 4, commitsLanded: 1, currentSha: "abc1234", currentSubject: "fix it" },
  });

  it("names the before-action phase, which no render state can", () => {
    // Arrange — the daemon runs the pre-merge prompt inside `merging`, so
    // without the override the footer says "merging" while nothing is merging.
    const built = input({
      renderState: "merging",
      mergeStatus: merge({ case: "beforeAction", value: { prompt: "run the linter" } }),
    });
    // Act
    const got = footerHtml(built, CLOSED, NOW);
    // Assert
    expect(got).toContain("merge before-action");
  });

  it("names the after-action phase", () => {
    // Arrange
    const built = input({
      renderState: "merging",
      mergeStatus: merge({ case: "afterAction", value: { prompt: "close the workspace" } }),
    });
    // Act
    const got = footerHtml(built, CLOSED, NOW);
    // Assert
    expect(got).toContain("merge after-action");
  });

  it("shows the before-action prompt as the activity", () => {
    // Arrange — it is what the daemon is making the session do under a lease
    // the user cannot prompt through.
    const built = input({
      renderState: "merging",
      mergeStatus: merge({ case: "beforeAction", value: { prompt: "run the linter" } }),
    });
    // Act
    const got = activityDetail(built, NOW);
    // Assert
    expect(got?.text).toBe("before-action · run the linter");
  });

  it("shows the after-action prompt as the activity", () => {
    // Arrange
    const built = input({
      renderState: "merging",
      mergeStatus: merge({ case: "afterAction", value: { prompt: "close the workspace" } }),
    });
    // Act
    const got = activityDetail(built, NOW);
    // Assert
    expect(got?.text).toBe("after-action · close the workspace");
  });

  it("counts the commits landed while cherry-picking", () => {
    // Arrange / Act
    const got = mergeStatusChip(PICKING);
    // Assert
    expect(got?.text).toBe("1/4");
  });

  it("shows the commit in hand as the activity while cherry-picking", () => {
    // Arrange
    const built = input({ renderState: "merging", mergeStatus: PICKING });
    // Act
    const got = activityDetail(built, NOW);
    // Assert
    expect(got?.text).toBe("picking · abc1234 fix it");
  });

  it("shows the commit under test as the activity while testing", () => {
    // Arrange
    const built = input({
      renderState: "merging",
      mergeStatus: merge({
        case: "testing",
        value: { commitsTotal: 4, commitsLanded: 4, currentSha: "def5678", currentSubject: "last" },
      }),
    });
    // Act
    const got = activityDetail(built, NOW);
    // Assert
    expect(got?.text).toBe("testing · def5678 last");
  });

  it("names the conflicted commit, which the phase word never did", () => {
    // Arrange
    const built = input({
      renderState: "merge_conflict",
      mergeStatus: merge({
        case: "conflict",
        value: {
          conflictedSha: "bad1234",
          conflictedSubject: "same file",
          commitsTotal: 4,
          commitsLanded: 2,
        },
      }),
    });
    // Act
    const got = footerHtml(built, CLOSED, NOW);
    // Assert
    expect(got).toContain("conflict · bad1234 same file");
  });

  it("outranks the running tool for the activity cell", () => {
    // Arrange — a tool call under a merge is the merge prompt's OWN work, so
    // naming the tool spends the slot on a detail of an unnamed thing.
    const built = input({
      renderState: "merging",
      mergeStatus: PICKING,
      items: [tool({ ts: new Date(NOW - 4_000).toISOString() })],
    });
    // Act
    const got = activityDetail(built, NOW);
    // Assert
    expect(got?.text).toBe("picking · abc1234 fix it");
  });

  it("yields the activity cell to an auth prompt, which stops the session dead", () => {
    // Arrange
    const built = input({
      renderState: "merging",
      mergeStatus: PICKING,
      progress: progress({ authenticating: { sinceMs: NOW, detail: "device code" } }),
    });
    // Act
    const got = activityDetail(built, NOW);
    // Assert
    expect(got?.text).toBe("auth · device code");
  });

  it("shows the failure cause as a standing note", () => {
    // Arrange
    const status = merge({
      case: "failed",
      value: {
        cause: "tests failed",
        commitsTotal: 4,
        commitsLanded: 3,
        failingSha: "fee1234",
        failingSubject: "break it",
        failedJson:
          '{"cause":"tests failed","commitsTotal":4,"commitsLanded":3,"failingSha":"fee1234","failingSubject":"break it"}',
      },
    });
    // Act
    const got = mergeNoteRowHtml(status);
    // Assert
    expect(got).toContain("tests failed");
  });

  it("notes a landed merge whose after-action failed", () => {
    // Arrange
    const status = merge({
      case: "merged",
      value: { commitsTotal: 4, afterActionError: "prompt timed out" },
    });
    // Act
    const got = mergeNoteRowHtml(status);
    // Assert
    expect(got).toContain("after-action failed: prompt timed out");
  });

  it("keeps a landed merge's note CALM, because the merge itself landed", () => {
    // Arrange — a red note would tell the user to undo work that succeeded.
    const status = merge({
      case: "merged",
      value: { commitsTotal: 4, afterActionError: "prompt timed out" },
    });
    // Act
    const got = mergeNoteRowHtml(status);
    // Assert
    expect(got).toContain('class="pfooter-merge-note ok"');
  });

  it("renders no note row for a merge with nothing to add", () => {
    // Arrange / Act
    const got = mergeNoteRowHtml(PICKING);
    // Assert
    expect(got).toBe("");
  });

  it("renders no note row when no merge touches the workspace", () => {
    // Arrange / Act
    const got = mergeNoteRowHtml(null);
    // Assert
    expect(got).toBe("");
  });

  it("shows the structured count as the ONLY merge arithmetic", () => {
    // REWRITTEN: this pair used to assert the structured chip superseded a flat
    // one, and that the flat one remained the fallback. The flat path is gone,
    // so what is left to guarantee is that the run's own figures are what
    // render.
    // Arrange
    const built = input({
      renderState: "merge_queued",
      mergeStatus: merge({ case: "enqueued", value: { position: 1, depth: 5 } }),
    });
    // Act
    const got = footerHtml(built, CLOSED, NOW);
    // Assert
    expect(got).toContain(">1/5<");
  });

  it("shows NO merge arithmetic when the daemon stamped no status", () => {
    // REWRITTEN: with the flat fallback retired, an absent status is an absent
    // chip rather than a handover to a second source.
    // Arrange
    const built = input({ renderState: "merge_queued", mergeStatus: null });
    // Act
    const got = footerHtml(built, CLOSED, NOW);
    // Assert
    expect(got).not.toContain("pfooter-merge-queue");
  });

  it("puts the merge's full account in the expansion sheet", () => {
    // Arrange — the sheet is where the detail the thin strip drops belongs.
    const built = input({ renderState: "merging", mergeStatus: PICKING });
    // Act
    const got = sheetHtml(built, NOW);
    // Assert
    expect(got).toContain("merging · 1/4 · picking · abc1234 fix it");
  });
});

describe("expensiveTurnRowHtml: the uncached-input alert", () => {
  /** The daemon's alert for a turn that crossed the threshold. */
  function alert(over: Partial<ContextCostAlert> = {}): ContextCostAlert {
    return {
      turnId: "turn-3",
      uncachedInputTokens: 48000,
      thresholdTokens: 20000,
      atMs: 1700000000000,
      promptOrigin: "PROMPT_ORIGIN_WEBAPP_USER_SENT",
      ...over,
    };
  }

  it("renders nothing after a cache-efficient turn", () => {
    // Arrange / Act
    const got = expensiveTurnRowHtml(progress());
    // Assert — absence of the field is the only reading: nothing to report.
    expect(got).toBe("");
  });

  it("names the cost a user-sent turn paid", () => {
    // Arrange / Act
    const got = expensiveTurnRowHtml(progress({ expensiveTurn: alert() }));
    // Assert
    expect(got).toContain("expensive turn");
  });

  it("quotes the observed figure against the threshold that tripped", () => {
    // Arrange — the daemon carries both so the row can say 'N over M'
    // without the webapp knowing daemon config.
    const got = expensiveTurnRowHtml(progress({ expensiveTurn: alert() }));
    // Act / Assert
    expect(got).toContain("48k uncached input tokens (threshold 20k)");
  });

  it("words a cold keep-alive as the ping failing at its one job", () => {
    // Arrange — the ping existed solely to keep the cache warm, so it bought
    // nothing at all; wording it as a generic expensive turn would bury the
    // only reading that says the cache machinery is not working.
    const got = expensiveTurnRowHtml(
      progress({ expensiveTurn: alert({ promptOrigin: "PROMPT_ORIGIN_CACHE_KEEP_ALIVE" }) }),
    );
    // Act / Assert
    expect(got).toContain("keep-alive came back COLD");
  });

  it("does not call a cold keep-alive a generic expensive turn", () => {
    // Arrange / Act
    const got = expensiveTurnRowHtml(
      progress({ expensiveTurn: alert({ promptOrigin: "PROMPT_ORIGIN_CACHE_KEEP_ALIVE" }) }),
    );
    // Assert
    expect(got).not.toContain("expensive turn:");
  });

  it("marks the cold keep-alive as its own louder variant", () => {
    // Arrange / Act
    const got = expensiveTurnRowHtml(
      progress({ expensiveTurn: alert({ promptOrigin: "PROMPT_ORIGIN_CACHE_KEEP_ALIVE" }) }),
    );
    // Assert
    expect(got).toContain("cold-keep-alive");
  });

  it("does not mark a user-sent turn as the cold-keep-alive variant", () => {
    // Arrange / Act
    const got = expensiveTurnRowHtml(progress({ expensiveTurn: alert() }));
    // Assert
    expect(got).not.toContain("cold-keep-alive");
  });

  it("joins the row to the turn that paid the cost", () => {
    // Arrange / Act
    const got = expensiveTurnRowHtml(progress({ expensiveTurn: alert({ turnId: "turn-77" }) }));
    // Assert
    expect(got).toContain('data-expensive-turn-id="turn-77"');
  });

  it("stands beside the classified-failure row rather than replacing it", () => {
    // Arrange — the two are different facts with the same lifetime, and a
    // failed expensive turn must show both.
    const got = footerHtml(
      input({ progress: progress({ failure: failureCard(), expensiveTurn: alert() }) }),
      CLOSED,
      NOW,
    );
    // Act / Assert
    expect(got).toContain("pfooter-error");
    expect(got).toContain("pfooter-expensive-turn");
  });
});
