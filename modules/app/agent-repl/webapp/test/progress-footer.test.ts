// @vitest-environment jsdom
//
// The pure builders (phase, activity, cells, click classification) need no DOM;
// jsdom is here for ProgressFooter itself, whose slot paint and disclosure
// survival are asserted against a real mount.
import { describe, expect, it } from "vitest";

import { CounterEntry } from "../src/counter-menu.js";
import {
  Activity,
  FooterDisclosure,
  FooterInput,
  ProgressFooter,
  activityDetail,
  clockTime,
  countersHtml,
  errorRowHtml,
  footerClickAction,
  footerHtml,
  hasLiveCounters,
  phaseLabel,
  runningTool,
  sheetHtml,
  tokenCellHtml,
  toolElapsed,
} from "../src/progress-footer.js";
import type { ProgressInput } from "../src/state-adapter.js";
import { ConversationItem, SystemFailureCard, ToolItem } from "../src/store.js";

const NOW = Date.parse("2024-05-01T12:00:00.000Z");

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
    ...over,
  };
}

/** A resolved progress view, defaulted to a quiet idle session. */
function progress(over: Partial<ProgressInput> = {}): ProgressInput {
  return {
    workspace: "/w",
    sessionId: "s1",
    state: "idle",
    turnStartedAtMs: 0,
    thinkingTokens: 0,
    inputTokens: 0,
    ttftMs: 0,
    compacting: null,
    retrying: null,
    authenticating: null,
    hook: null,
    blocked: null,
    rateLimited: null,
    errorSummary: "",
    failure: null,
    errorItemUuid: "",
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
    agents: [],
    tasks: [],
    items: [],
    timerLabel: "0:24",
    ...over,
  };
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
    expect(got).toEqual({ word: "thinking", tone: "thinking", spinning: true });
  });

  it("does not spin once the turn is done", () => {
    // Arrange / Act
    const got = phaseLabel("done");
    // Assert
    expect(got.spinning).toBe(false);
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
    // Arrange / Act — every other spinning phase is work happening, so
    // animating this one would say the opposite of what it means.
    const got = phaseLabel("vendor_blocked");
    // Assert
    expect(got.spinning).toBe(false);
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
    expect(got.spinning).toBe(true);
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

  it("lets a compaction supersede a running tool", () => {
    // Arrange
    const i = input({
      progress: progress({ compacting: { sinceMs: NOW - 3_000, detail: "compacting" } }),
      items: [tool()],
    });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert
    expect(got.text).toBe("compacting…");
  });

  it("lets a retry supersede a compaction", () => {
    // Arrange — a retry is the more specific account of the same dead air.
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

  it("lets a rate limit supersede a retry", () => {
    // Arrange
    const i = input({
      progress: progress({
        retrying: { sinceMs: NOW, detail: "attempt 3/10" },
        rateLimited: { resetsAt: Math.floor(NOW / 1000), utilization: 0.91, status: "allowed_warning" },
      }),
    });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert
    expect(got.text).toContain("rate-limited");
  });

  it("lets an auth prompt supersede everything, since it blocks on the reader", () => {
    // Arrange
    const i = input({
      progress: progress({
        authenticating: { sinceMs: NOW, detail: "paste your code" },
        rateLimited: { resetsAt: 0, utilization: 0, status: "limited" },
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

  it("carries the rate limit's reset time as a local clock reading", () => {
    // Arrange
    const resetsAt = Math.floor(NOW / 1000);
    const i = input({
      progress: progress({ rateLimited: { resetsAt, utilization: 0, status: "limited" } }),
    });
    // Act
    const got = activityDetail(i, NOW) as Activity;
    // Assert
    expect(got.text).toBe(`rate-limited until ${clockTime(resetsAt * 1000)}`);
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

  it("renders nothing when the turn has spent nothing yet", () => {
    // Arrange / Act — a lying `0 in` is worse than an absent cell.
    const got = tokenCellHtml(progress());
    // Assert
    expect(got).toBe("");
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

describe("footerHtml: the V4 segmented dock", () => {
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

  it("drops the token cell when the turn has spent nothing", () => {
    // Arrange / Act
    const got = footerHtml(input(), CLOSED, NOW);
    // Assert
    expect(got).not.toContain("pfooter-tokens");
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
