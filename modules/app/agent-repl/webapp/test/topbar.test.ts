/**
 * Topbar tests: the markup contract of the header (index.html), and the
 * scope-parameterized datapoint strip (src/topbar.ts) that renders both
 * in the header (session scope) and inside every subagent bubble (agent
 * scope).
 *
 * The session-gone chrome is positional: the remediation notice has to read
 * as an annotation of the alarm dot, which means sitting immediately to its
 * right. jsdom is not in the dep tree, so the ordering is asserted against
 * the document source — which is where the ordering actually lives.
 */
import { describe, expect, it } from "vitest";

import html from "../index.html?raw";
import {
  ClickTarget,
  TOPBAR_AGENT_ATTR,
  TopbarDatapoints,
  TopbarDisclosure,
  agentElapsedLabel,
  agentTopbarDatapoints,
  agentTopbarHtml,
  nextCounterMenu,
  runningAgentClocks,
  sessionTopbarDatapoints,
  topbarClickAction,
  topbarInfoHtml,
} from "../src/topbar.js";
import { CounterEntry } from "../src/counter-menu.js";
import { StoreState, ToolItem, UserTurnItem } from "../src/store.js";
import { IDLE_LABEL, TIMER_SLOT } from "../src/timer.js";

/** A counter entry, defaulted to an active one that never prunes. */
function counterEntry(over: Partial<CounterEntry> = {}): CounterEntry {
  return {
    id: "t1",
    summary: "hunt the flake",
    detail: "Explore",
    status: "running",
    nested: false,
    deactivatedAtTurn: null,
    ...over,
  };
}

/** One scope's datapoints, defaulted to an idle scope with nothing counted. */
function datapoints(over: Partial<TopbarDatapoints> = {}): TopbarDatapoints {
  return {
    parentWs: null,
    timerLabel: IDLE_LABEL,
    contextTokens: null,
    agents: [],
    tasks: [],
    currentTurn: 0,
    ...over,
  };
}

/** The strip for DATAPOINTS with both overlays closed unless OPEN says so. */
function strip(
  over: Partial<TopbarDatapoints> = {},
  open: Partial<TopbarDisclosure> = {},
): string {
  return topbarInfoHtml(datapoints(over), {
    agentsOpen: false,
    tasksOpen: false,
    ...open,
  });
}

/** A store state, defaulted to a fresh idle session. */
function storeState(over: Partial<StoreState> = {}): StoreState {
  return {
    sessionId: "s1",
    daemonVersion: "0.1.0",
    model: "m",
    models: [],
    cwd: "/w",
    claudeSessionId: "",
    permissionMode: "default",
    items: [],
    queued: [],
    turnInFlight: false,
    turnStartedAt: null,
    lastFinalResponseAt: null,
    contextTokens: null,
    compacting: false,
    interrupting: false,
    costUsd: null,
    lastSeq: 0,
    ...over,
  };
}

/** When the fixture agents started, and a clock reading 90 seconds later. */
const AGENT_START = "2026-05-24T10:00:00.000Z";
const NOW_MS = Date.parse(AGENT_START) + 90_000;

/** A subagent tool call, defaulted to one still running. */
function agentItem(over: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
    toolUseId: "a1",
    toolName: "Agent",
    messageId: "m1",
    ts: AGENT_START,
    inputJson: "",
    input: { description: "hunt the flake", subagent_type: "Explore" },
    inputDone: true,
    ...over,
  };
}

/** A counted user turn (a real prompt, not a bare built-in slash). */
function userTurn(text = "do the thing", requestId = "r1"): UserTurnItem {
  return {
    kind: "user-turn",
    requestId,
    content: [{ type: "text", text }],
    ts: "2026-05-24T09:00:00.000Z",
  };
}

/** A settled TaskCreate call naming task #1, owned by PARENT when given. */
function taskCreate(parent?: string): ToolItem {
  return {
    kind: "tool",
    toolUseId: "c1",
    toolName: "TaskCreate",
    messageId: "m1",
    parentToolUseId: parent,
    ts: AGENT_START,
    inputJson: "",
    input: { subject: "wire the counter", description: "d" },
    inputDone: true,
    result: { isError: false, content: "Task #1 created successfully: wire the counter" },
  };
}

const topbar = html.slice(html.indexOf("<header"), html.indexOf("</header>"));

describe("topbar", () => {
  it("carries a remediation notice slot", () => {
    // Arrange / Act — the topbar markup.
    // Assert
    expect(topbar).toContain(`id="remediation"`);
  });

  it("places the remediation notice to the right of the blinking dot", () => {
    // Arrange / Act — source order is layout order in the flex row.
    // Assert
    expect(topbar.indexOf(`id="remediation"`)).toBeGreaterThan(topbar.indexOf(`id="spinner"`));
  });

  it("keeps the remediation notice left of the session info block", () => {
    // Arrange / Act — #session-info is pushed to the far right by margin-left:auto.
    // Assert
    expect(topbar.indexOf(`id="remediation"`)).toBeLessThan(topbar.indexOf(`id="session-info"`));
  });

  it("starts the notice empty so a healthy session shows no alarm text", () => {
    // Arrange / Act — the notice is filled in only by the gone path.
    // Assert
    expect(topbar).toMatch(/<span id="remediation"><\/span>/);
  });

  it("renders the account chip as a button, the account menu's trigger", () => {
    // Arrange / Act — the chip replaced the old standalone login button:
    // both account verbs (re-auth, switch) now live in its dropdown.
    // Assert
    expect(topbar).toMatch(/<button id="account"/);
  });

  it("carries no standalone login button", () => {
    // Arrange / Act — the login verb moved into the account menu; a second
    // trigger would reintroduce the two-buttons-one-concern split.
    // Assert
    expect(topbar).not.toContain(`id="login-btn"`);
  });

  it("keeps the account menu container outside the topbar, starting hidden", () => {
    // Arrange / Act — the menu overlays the feed below the topbar, and a
    // healthy page shows it only after a chip click.
    // Assert
    expect(topbar).not.toContain(`id="account-menu"`);
    expect(html).toMatch(/<div id="account-menu" hidden><\/div>/);
  });

  it("carries a model picker", () => {
    // Arrange / Act — the topbar markup.
    // Assert
    expect(topbar).toContain(`id="model-select"`);
  });

  it("places the model picker immediately left of the permission-mode dropdown", () => {
    // Arrange / Act — the two session-wide switches sit together, left of
    // login, so the controls that change how a turn RUNS are one group.
    // Assert
    expect(topbar.indexOf(`id="model-select"`)).toBeLessThan(topbar.indexOf(`id="mode-select"`));
  });

  it("keeps the model picker right of the session info block", () => {
    // Arrange / Act — #session-info is pushed right by margin-left:auto, so
    // the picker lands after it rather than adrift on the left.
    // Assert
    expect(topbar.indexOf(`id="model-select"`)).toBeGreaterThan(topbar.indexOf(`id="session-info"`));
  });

  it("names the header #topbar, the hook the picker content-sizing rule scopes to", () => {
    // Arrange / Act — styles.css sizes pickers via `#topbar select`, so a
    // renamed header would silently drop both pickers back to widest-option width.
    // Assert
    expect(topbar).toContain(`id="topbar"`);
  });

  it("classes the session strip topbar-info, the styling hook it shares with bubble strips", () => {
    // Arrange / Act — styles.css scopes the datapoint colors to
    // `.topbar-info`, the class the agent bubbles' strips also carry; a
    // dropped class here would silently uncolor the header's datapoints.
    // Assert
    expect(topbar).toMatch(/<span id="session-info" class="topbar-info">/);
  });

  it("mounts the permission-mode picker inside the topbar, where the sizing rule reaches it", () => {
    // Arrange / Act — the `#topbar select` rule only sizes a picker the
    // header actually contains.
    // Assert
    expect(topbar).toContain(`id="mode-select"`);
  });

  it("starts the model picker empty so its options come only from the daemon", () => {
    // Arrange / Act — unlike #mode-select's fixed enum, the model menu is
    // whatever the account's CLI reports; hardcoding options here would let
    // the picker offer a model the session cannot actually run.
    // Assert
    expect(topbar).toMatch(/<select id="model-select"[^>]*><\/select>/);
  });
});

describe("topbarInfoHtml", () => {
  it("renders the parent workspace datapoint when the scope carries one", () => {
    // Arrange + Act
    const html = strip({ parentWs: "my-feature" });
    // Assert
    expect(html).toContain(`parent workspace: <span class="info-ws">my-feature</span>`);
  });

  it("omits the parent workspace datapoint when the scope has none", () => {
    // Arrange + Act
    const html = strip({ parentWs: null });
    // Assert — no dangling label or leading delimiter.
    expect(html).not.toContain("parent workspace");
    expect(html.startsWith("time:")).toBe(true);
  });

  it("omits the parent workspace datapoint when it is empty", () => {
    // Arrange + Act + Assert
    expect(strip({ parentWs: "" })).not.toContain("parent workspace");
  });

  it("escapes markup in the parent workspace name", () => {
    // Arrange + Act + Assert
    expect(strip({ parentWs: "<b>ws" })).not.toContain("<b>");
  });

  it("joins the datapoints with the dot separator", () => {
    // Arrange + Act
    const html = strip({ parentWs: "ws" });
    // Assert
    expect(html).toContain("</span> · tokens:");
  });

  it("does not print the model, which the picker now both names and switches", () => {
    // Arrange + Act + Assert — printing it here too would duplicate the
    // dropdown sitting immediately to its right.
    expect(strip({ parentWs: "ws" })).not.toContain("model:");
  });

  it("renders the context token count with thousands separators", () => {
    // Arrange + Act + Assert — the count is precomputed context occupancy,
    // not a usage object the strip re-sums.
    expect(strip({ contextTokens: 123456 })).toContain(
      `tokens: <span class="info-tokens">123,456</span>`,
    );
  });

  it("shows a dash when the context size is unknown", () => {
    // Arrange + Act + Assert — a `/clear`, a compaction, and a subagent
    // before its first request each leave a size unreported, so `null` is
    // unknown, not 0.
    expect(strip({ contextTokens: null })).toContain(
      `tokens: <span class="info-tokens">—</span>`,
    );
  });

  it("renders a genuine zero as zero, not a dash", () => {
    // Arrange + Act + Assert — 0 is a known-empty context, distinct from
    // the unknown `null`.
    expect(strip({ contextTokens: 0 })).toContain(
      `tokens: <span class="info-tokens">0</span>`,
    );
  });

  it("no longer renders the in/out counter or the cost estimate", () => {
    // Arrange + Act
    const html = strip({ parentWs: "ws", contextTokens: 10 });
    // Assert
    expect(html).not.toContain("in/");
    expect(html).not.toContain("out");
    expect(html).not.toContain("$");
  });

  it("appends the subagent chip after the token datapoint", () => {
    // Arrange + Act
    const html = strip({ agents: [counterEntry()] });
    // Assert
    expect(html).toContain("</span> · <span class=\"agents-menu\">");
  });

  it("counts the scope's subagents on the chip", () => {
    // Arrange
    const agents = [counterEntry(), counterEntry({ id: "t2" })];
    // Act + Assert
    expect(strip({ agents })).toContain("2 agents");
  });

  it("drops the subagent roster when the agents chip is open", () => {
    // Arrange + Act
    const html = strip({ agents: [counterEntry()] }, { agentsOpen: true });
    // Assert
    expect(html).toContain("agents-overlay");
  });

  it("omits the subagent chip when the scope spawned none", () => {
    // Arrange + Act + Assert
    expect(strip()).not.toContain("agents-menu");
  });

  it("appends the task chip to the right of the agents chip", () => {
    // Arrange + Act
    const html = strip({ agents: [counterEntry()], tasks: [counterEntry({ id: "k1" })] });
    // Assert — source order is left-to-right in the datapoint run.
    expect(html.indexOf("tasks-menu")).toBeGreaterThan(html.indexOf("agents-menu"));
  });

  it("counts the scope's tasks on the task chip", () => {
    // Arrange
    const tasks = [counterEntry({ id: "k1" }), counterEntry({ id: "k2" })];
    // Act + Assert
    expect(strip({ tasks })).toContain("2 tasks");
  });

  it("drops the task roster when the task chip is open", () => {
    // Arrange + Act
    const html = strip({ tasks: [counterEntry({ id: "k1" })] }, { tasksOpen: true });
    // Assert
    expect(html).toContain("tasks-overlay");
  });

  it("omits the task chip when the scope created none", () => {
    // Arrange + Act + Assert
    expect(strip()).not.toContain("tasks-menu");
  });

  it("leaves no dangling separator when the scope has neither counter", () => {
    // Arrange + Act
    const html = strip({ parentWs: "ws" });
    // Assert
    expect(html.endsWith("</span>")).toBe(true);
  });

  it("renders the scope's elapsed time", () => {
    // Arrange + Act
    const html = strip({ timerLabel: "5m 30s" });
    // Assert
    expect(html).toContain(
      `time: <span class="info-time" data-task-timer>5m 30s</span>`,
    );
  });

  it("reads the idle label when the scope's clock is idle", () => {
    // Arrange + Act + Assert — the fixture default, since the store starts idle.
    expect(strip()).toContain(
      `time: <span class="info-time" data-task-timer>--</span>`,
    );
  });

  it("marks the timer span so the tick can repaint it alone", () => {
    // Arrange + Act — a whole-strip rewrite once a second would be churn.
    const html = strip({ timerLabel: "12s" });
    // Assert
    expect(html).toContain(TIMER_SLOT);
  });

  it("places the timer between the parent workspace and the token count", () => {
    // Arrange + Act
    const html = strip({ parentWs: "ws", timerLabel: "12s" });
    // Assert
    expect(html.indexOf("time:")).toBeGreaterThan(html.indexOf("parent workspace:"));
    expect(html.indexOf("time:")).toBeLessThan(html.indexOf("tokens:"));
  });
});

describe("sessionTopbarDatapoints", () => {
  it("reads the session's standing context tokens", () => {
    // Arrange + Act
    const d = sessionTopbarDatapoints(storeState({ contextTokens: 200_000 }), null, IDLE_LABEL);
    // Assert
    expect(d.contextTokens).toBe(200_000);
  });

  it("passes the parent workspace through", () => {
    // Arrange + Act + Assert
    expect(sessionTopbarDatapoints(storeState(), "ws", IDLE_LABEL).parentWs).toBe("ws");
  });

  it("renders exactly what the TaskTimer's last tick painted", () => {
    // Arrange + Act — the label is handed in, never re-derived, so the two
    // paint paths cannot disagree.
    expect(sessionTopbarDatapoints(storeState(), null, "5m 30s").timerLabel).toBe("5m 30s");
  });

  it("projects the session-wide subagent roster", () => {
    // Arrange
    const state = storeState({ items: [agentItem()] });
    // Act + Assert
    expect(sessionTopbarDatapoints(state, null, IDLE_LABEL).agents.map((a) => a.id)).toEqual(["a1"]);
  });

  it("projects the session-wide task roster", () => {
    // Arrange
    const state = storeState({ items: [taskCreate()] });
    // Act + Assert
    expect(sessionTopbarDatapoints(state, null, IDLE_LABEL).tasks.map((t) => t.id)).toEqual(["1"]);
  });

  it("reads the counted-turn clock off the session's items", () => {
    // Arrange
    const state = storeState({ items: [userTurn(), userTurn("more", "r2")] });
    // Act + Assert
    expect(sessionTopbarDatapoints(state, null, IDLE_LABEL).currentTurn).toBe(2);
  });
});

describe("agentElapsedLabel", () => {
  it("counts a running agent from its start stamp to now", () => {
    // Arrange + Act + Assert — 90s after the start fixture.
    expect(agentElapsedLabel(agentItem(), NOW_MS)).toBe("1m 30s");
  });

  it("freezes a settled agent at its result stamp", () => {
    // Arrange — settled 30s in; NOW is 90s in and must not matter.
    const item = agentItem({
      result: { isError: false, content: "done" },
      resultTs: "2026-05-24T10:00:30.000Z",
    });
    // Act + Assert
    expect(agentElapsedLabel(item, NOW_MS)).toBe("30s");
  });
});

describe("agentTopbarDatapoints", () => {
  it("carries no parent workspace, which is session-only chrome", () => {
    // Arrange + Act + Assert
    expect(agentTopbarDatapoints([agentItem()], agentItem(), NOW_MS).parentWs).toBeNull();
  });

  it("reads the agent's own banked context tokens", () => {
    // Arrange
    const agent = agentItem({ contextTokens: 42_000 });
    // Act + Assert
    expect(agentTopbarDatapoints([agent], agent, NOW_MS).contextTokens).toBe(42_000);
  });

  it("reads unknown tokens for an agent before its first request", () => {
    // Arrange + Act + Assert — undefined banks nothing, so the strip dashes.
    expect(agentTopbarDatapoints([agentItem()], agentItem(), NOW_MS).contextTokens).toBeNull();
  });

  it("ticks the agent's own elapsed clock", () => {
    // Arrange + Act + Assert
    expect(agentTopbarDatapoints([agentItem()], agentItem(), NOW_MS).timerLabel).toBe("1m 30s");
  });

  it("scopes the subagent roster to the agent's direct children", () => {
    // Arrange
    const agent = agentItem();
    const child = agentItem({ toolUseId: "c1", parentToolUseId: "a1" });
    const sibling = agentItem({ toolUseId: "s1" });
    // Act
    const d = agentTopbarDatapoints([agent, child, sibling], agent, NOW_MS);
    // Assert
    expect(d.agents.map((a) => a.id)).toEqual(["c1"]);
  });

  it("scopes the task roster to the agent's own task calls", () => {
    // Arrange
    const agent = agentItem();
    // Act
    const d = agentTopbarDatapoints([agent, taskCreate("a1"), taskCreate()], agent, NOW_MS);
    // Assert
    expect(d.tasks).toHaveLength(1);
  });

  it("ages against the session's counted-turn clock", () => {
    // Arrange — the retention stamps are on the session clock, so the
    // window must be too.
    const agent = agentItem();
    // Act
    const d = agentTopbarDatapoints([userTurn(), agent], agent, NOW_MS);
    // Assert
    expect(d.currentTurn).toBe(1);
  });
});

describe("agentTopbarHtml", () => {
  it("marks the wrapper with the agent it is scoped to", () => {
    // Arrange + Act
    const html2 = agentTopbarHtml([agentItem()], agentItem(), { agentsOpen: false, tasksOpen: false }, NOW_MS);
    // Assert — the tick and the click delegation both key off this.
    expect(html2).toContain(`${TOPBAR_AGENT_ATTR}="a1"`);
  });

  it("carries the shared topbar-info styling class", () => {
    // Arrange + Act
    const html2 = agentTopbarHtml([agentItem()], agentItem(), { agentsOpen: false, tasksOpen: false }, NOW_MS);
    // Assert — one CSS contract styles the header strip and the bubble strip.
    expect(html2).toContain(`class="agent-topbar topbar-info"`);
  });

  it("carries a timer slot the bubble tick can repaint", () => {
    // Arrange + Act
    const html2 = agentTopbarHtml([agentItem()], agentItem(), { agentsOpen: false, tasksOpen: false }, NOW_MS);
    // Assert
    expect(html2).toContain(TIMER_SLOT);
  });

  it("escapes markup in the agent id", () => {
    // Arrange
    const agent = agentItem({ toolUseId: `a"1` });
    // Act + Assert
    expect(
      agentTopbarHtml([agent], agent, { agentsOpen: false, tasksOpen: false }, NOW_MS),
    ).not.toContain(`="a"1"`);
  });
});

describe("runningAgentClocks", () => {
  it("lists a live subagent with its start stamp", () => {
    // Arrange + Act + Assert
    expect(runningAgentClocks([agentItem()])).toEqual([
      { id: "a1", startedAt: AGENT_START },
    ]);
  });

  it("drops a settled agent, whose frozen span the render bakes in", () => {
    // Arrange
    const settled = agentItem({ result: { isError: false, content: "done" } });
    // Act + Assert
    expect(runningAgentClocks([settled])).toEqual([]);
  });

  it("ignores tools that are not subagents", () => {
    // Arrange
    const bash = agentItem({ toolName: "Bash" });
    // Act + Assert
    expect(runningAgentClocks([bash])).toEqual([]);
  });

  it("keeps a nested live agent, whose bubble ticks inside its parent's panel", () => {
    // Arrange
    const nested = agentItem({ toolUseId: "c1", parentToolUseId: "a1" });
    // Act + Assert
    expect(runningAgentClocks([nested]).map((c) => c.id)).toEqual(["c1"]);
  });
});

describe("topbarClickAction", () => {
  /** A ClickTarget whose closest() matches only SELECTOR, answering ATTRS. */
  function clickOn(selector: string, attrs: Record<string, string> = {}): ClickTarget {
    return {
      closest: (sel) =>
        sel === selector ? { getAttribute: (name) => attrs[name] ?? null } : null,
    };
  }

  it("classifies a click on the agents chip as the agents toggle", () => {
    // Arrange + Act + Assert
    expect(topbarClickAction(clickOn("[data-agents-toggle]"))).toEqual({
      kind: "toggle",
      menu: "agents",
    });
  });

  it("classifies a click on the tasks chip as the tasks toggle", () => {
    // Arrange + Act + Assert
    expect(topbarClickAction(clickOn("[data-tasks-toggle]"))).toEqual({
      kind: "toggle",
      menu: "tasks",
    });
  });

  it("classifies a click on a subagent row as a reveal of that agent", () => {
    // Arrange + Act + Assert
    expect(topbarClickAction(clickOn(".agent-row", { "data-agent-id": "a7" }))).toEqual({
      kind: "reveal",
      agentId: "a7",
    });
  });

  it("classifies a click on nothing actionable as null", () => {
    // Arrange + Act + Assert — plain strip text is not a control.
    expect(topbarClickAction(clickOn(".info-tokens"))).toBeNull();
  });

  it("classifies an agent row missing its id as null", () => {
    // Arrange + Act + Assert — no id means nothing to reveal.
    expect(topbarClickAction(clickOn(".agent-row"))).toBeNull();
  });
});

describe("nextCounterMenu", () => {
  it("closes the menu whose chip was clicked again", () => {
    // Arrange + Act + Assert
    expect(nextCounterMenu("agents", "agents")).toBeNull();
  });

  it("opens a menu from the all-closed state", () => {
    // Arrange + Act + Assert
    expect(nextCounterMenu(null, "tasks")).toBe("tasks");
  });

  it("switches to the other menu, so only one is ever open", () => {
    // Arrange + Act + Assert
    expect(nextCounterMenu("agents", "tasks")).toBe("tasks");
  });
});
