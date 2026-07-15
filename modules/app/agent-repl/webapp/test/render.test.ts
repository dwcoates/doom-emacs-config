import { describe, expect, it } from "vitest";
import {
  AgentReveal,
  PanelContext,
  activeGroupMember,
  activityTicker,
  backfillChunks,
  clearBoundary,
  compactionBannerHtml,
  diffHtml,
  finalResponses,
  formatBubbleTime,
  formatTurnTime,
  groupFeed,
  groupHtml,
  interruptingIndicatorHtml,
  isPulsed,
  itemKey,
  lastUserTurnId,
  modelOptionsHtml,
  openWatcherTaskIds,
  planAgentReveal,
  pulseTarget,
  renderItem,
  rendersEmpty,
  repinsToTail,
  sessionInfoHtml,
  wakeRemainingLabel,
} from "../src/render.js";
import { CounterEntry } from "../src/counter-menu.js";
import { META_CLOSE, META_OPEN } from "../src/meta.js";
import { TIMER_SLOT } from "../src/timer.js";
import {
  ConversationItem,
  ResultItem,
  TextItem,
  ThinkingItem,
  ToolItem,
} from "../src/store.js";

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

/** When the agent opened a text block, for the items that do not assert on it. */
const TEXT_TS = new Date(2026, 4, 24, 9, 5).toISOString();

/** A text item the agent opened at the given local wall-clock time. */
function textAt(hour: number, minute: number, text = "the answer"): ConversationItem {
  return {
    kind: "text",
    blockId: "b1",
    messageId: "m1",
    text,
    done: true,
    ts: new Date(2026, 4, 24, hour, minute).toISOString(),
  };
}

/** A user-turn item whose prompt was sent at the given local wall-clock time. */
function userTurnAt(
  hour: number,
  minute: number,
  text = "do the thing",
  requestId = "r1",
): ConversationItem {
  return {
    kind: "user-turn",
    requestId,
    content: [{ type: "text", text }],
    ts: new Date(2026, 4, 24, hour, minute).toISOString(),
  };
}

/** A text block item carrying the given id, finished unless DONE says otherwise. */
function text(blockId: string, done = true): ConversationItem {
  return { kind: "text", blockId, messageId: "m1", text: "hi", done, ts: TEXT_TS };
}

/** A thinking block item, finished unless DONE says otherwise. */
function thinking(blockId: string, done = true, text = "hmm"): ConversationItem {
  return { kind: "thinking", blockId, messageId: "m1", text, done };
}

/** A subagent's text block: TEXT plus the parent that confines it. */
function subagentText(blockId: string, done = true): ConversationItem {
  return { ...(text(blockId, done) as TextItem), parentToolUseId: "a1" };
}

/** A subagent's thinking block, as subagentText. */
function subagentThinking(blockId: string, done = true): ConversationItem {
  return { ...(thinking(blockId, done) as ThinkingItem), parentToolUseId: "a1" };
}

/** A result frame closing a turn with the given subtype. */
function result(subtype: ResultItem["subtype"] = "success"): ResultItem {
  return {
    kind: "result",
    subtype,
    // Distinct from sincePrevFinalMs so a chip that reads the wrong field
    // is caught: the whole-task figure the standalone chip shows.
    durationMs: 12,
    // The since-previous-final figure the final-response chip shows, above
    // the one-second floor below which that chip is dropped entirely.
    sincePrevFinalMs: 7_000,
    numTurns: 1,
    totalCostUsd: 0.5,
    usage: { input_tokens: 3, output_tokens: 4 },
    isError: subtype === "error_during_execution",
    context: { total: 300_000, delta: 100_000 },
  };
}

/**
 * A tool card item, standing in for work between two text blocks. Running by
 * default (no result badge yet); pass DONE to settle it, and TOOL-USE-ID to
 * tell parallel calls apart.
 */
function tool(done = false, toolUseId = "t1"): ConversationItem {
  return {
    kind: "tool",
    toolUseId,
    messageId: "m1",
    toolName: "Bash",
    inputJson: "{}",
    input: {},
    inputDone: true,
    ...(done ? { result: { isError: false, content: "" } } : {}),
  };
}

/** A tool call the feed draws no card for at all (see SUPPRESSED_TOOLS). */
function suppressedTool(): ConversationItem {
  return {
    kind: "tool",
    toolUseId: "t2",
    messageId: "m1",
    toolName: "ToolSearch",
    inputJson: "{}",
    input: {},
    inputDone: true,
  };
}

/** The finals of a one-turn feed that ITEM closes, as renderItem takes them. */
function finalsClosing(item: ConversationItem, subtype: ResultItem["subtype"] = "success") {
  return finalResponses([userTurnAt(9, 0), item, result(subtype)]);
}

describe("formatTurnTime", () => {
  it("renders the envelope ts as local 24-hour HH:MM", () => {
    // Arrange
    const ts = new Date(2026, 4, 24, 14, 32).toISOString();
    // Act + Assert
    expect(formatTurnTime(ts)).toBe("14:32");
  });

  it("zero-pads a single-digit hour", () => {
    // Arrange
    const ts = new Date(2026, 4, 24, 9, 5).toISOString();
    // Act + Assert
    expect(formatTurnTime(ts)).toBe("09:05");
  });
});

describe("formatBubbleTime", () => {
  it("renders a fresh stamp as seconds ago", () => {
    // Arrange — the reader's clock three seconds past the stamp.
    const ts = new Date(2026, 4, 24, 14, 32).toISOString();
    const now = Date.parse(ts) + 3_000;
    // Act + Assert
    expect(formatBubbleTime(ts, now)).toBe("3s ago");
  });

  it("renders minutes and seconds ago at two-level granularity", () => {
    // Arrange
    const ts = new Date(2026, 4, 24, 14, 32).toISOString();
    const now = Date.parse(ts) + 330_000;
    // Act + Assert — `5m 30s ago`, never `5.5m ago`.
    expect(formatBubbleTime(ts, now)).toBe("5m 30s ago");
  });

  it("drops the seconds once a stamp passes an hour", () => {
    // Arrange
    const ts = new Date(2026, 4, 24, 14, 32).toISOString();
    const now = Date.parse(ts) + 3_930_000;
    // Act + Assert — `1h 5m ago`, never `1h 5m 30s ago`.
    expect(formatBubbleTime(ts, now)).toBe("1h 5m ago");
  });

  it("defaults the clock to now so a stamp ages against the reader's time", () => {
    // Arrange — a stamp firmly in the past, so the default `Date.now()` is
    // always after it however long the test runs later.
    const ts = new Date(2026, 4, 24, 14, 32).toISOString();
    // Act + Assert
    expect(formatBubbleTime(ts).endsWith(" ago")).toBe(true);
  });
});

describe("sessionInfoHtml", () => {
  it("renders the parent workspace datapoint from parent_ws", () => {
    // Arrange + Act
    const html = sessionInfoHtml("my-feature", null);
    // Assert
    expect(html).toContain(`parent workspace: <span class="info-ws">my-feature</span>`);
  });

  it("omits the parent workspace datapoint when parent_ws is absent", () => {
    // Arrange + Act
    const html = sessionInfoHtml(null, null);
    // Assert — no dangling label or leading delimiter.
    expect(html).not.toContain("parent workspace");
    expect(html.startsWith("time:")).toBe(true);
  });

  it("omits the parent workspace datapoint when parent_ws is empty", () => {
    // Arrange + Act + Assert
    expect(sessionInfoHtml("", null)).not.toContain("parent workspace");
  });

  it("escapes markup in the parent workspace name", () => {
    // Arrange + Act + Assert
    expect(sessionInfoHtml("<b>ws", null)).not.toContain("<b>");
  });

  it("joins the datapoints with the dot separator", () => {
    // Arrange + Act
    const html = sessionInfoHtml("ws", null);
    // Assert
    expect(html).toContain("</span> · tokens:");
  });

  it("does not print the model, which the picker now both names and switches", () => {
    // Arrange + Act + Assert — printing it here too would duplicate the
    // dropdown sitting immediately to its right.
    expect(sessionInfoHtml("ws", null)).not.toContain("model:");
  });

  it("renders the context token count with thousands separators", () => {
    // Arrange + Act + Assert — the count is precomputed context occupancy,
    // not a usage object the header re-sums.
    expect(sessionInfoHtml(null, 123456)).toContain(
      `tokens: <span class="info-tokens">123,456</span>`,
    );
  });

  it("shows a dash when the context size is unknown", () => {
    // Arrange + Act + Assert — a `/clear` and a compaction each leave a
    // context size behind without reporting it, so `null` is unknown, not 0.
    expect(sessionInfoHtml(null, null)).toContain(
      `tokens: <span class="info-tokens">—</span>`,
    );
  });

  it("renders a genuine zero as zero, not a dash", () => {
    // Arrange + Act + Assert — 0 is a known-empty context, distinct from
    // the unknown `null`.
    expect(sessionInfoHtml(null, 0)).toContain(
      `tokens: <span class="info-tokens">0</span>`,
    );
  });

  it("no longer renders the in/out counter or the cost estimate", () => {
    // Arrange + Act
    const html = sessionInfoHtml("ws", 10);
    // Assert
    expect(html).not.toContain("in/");
    expect(html).not.toContain("out");
    expect(html).not.toContain("$");
  });

  it("appends the subagent chip after the token datapoint", () => {
    // Arrange
    const agents = [counterEntry()];
    // Act
    const html = sessionInfoHtml("ws", null, agents, [], 0, false, false);
    // Assert
    expect(html).toContain("</span> · <span class=\"agents-menu\">");
  });

  it("counts the session's subagents on the chip", () => {
    // Arrange
    const agents = [counterEntry(), counterEntry({ id: "t2" })];
    // Act + Assert
    expect(sessionInfoHtml("ws", null, agents, [], 0)).toContain("2 agents");
  });

  it("drops the subagent roster when the agents chip is open", () => {
    // Arrange + Act
    const html = sessionInfoHtml("ws", null, [counterEntry()], [], 0, true, false);
    // Assert
    expect(html).toContain("agents-overlay");
  });

  it("omits the subagent chip when the session spawned none", () => {
    // Arrange + Act + Assert
    expect(sessionInfoHtml("ws", null, [], [], 0)).not.toContain("agents-menu");
  });

  it("appends the task chip to the right of the agents chip", () => {
    // Arrange
    const agents = [counterEntry()];
    const tasks = [counterEntry({ id: "k1" })];
    // Act
    const html = sessionInfoHtml("ws", null, agents, tasks, 0);
    // Assert — source order is left-to-right in the datapoint run.
    expect(html.indexOf("tasks-menu")).toBeGreaterThan(html.indexOf("agents-menu"));
  });

  it("counts the session's tasks on the task chip", () => {
    // Arrange
    const tasks = [counterEntry({ id: "k1" }), counterEntry({ id: "k2" })];
    // Act + Assert
    expect(sessionInfoHtml("ws", null, [], tasks, 0)).toContain("2 tasks");
  });

  it("drops the task roster when the task chip is open", () => {
    // Arrange + Act
    const html = sessionInfoHtml("ws", null, [], [counterEntry({ id: "k1" })], 0, false, true);
    // Assert
    expect(html).toContain("tasks-overlay");
  });

  it("omits the task chip when the session created none", () => {
    // Arrange + Act + Assert
    expect(sessionInfoHtml("ws", null, [], [], 0)).not.toContain("tasks-menu");
  });

  it("leaves no dangling separator when the session has neither counter", () => {
    // Arrange + Act
    const html = sessionInfoHtml("ws", null, [], [], 0);
    // Assert
    expect(html.endsWith("</span>")).toBe(true);
  });

  it("renders the running task's elapsed time", () => {
    // Arrange + Act
    const html = sessionInfoHtml("ws", null, [], [], 0, false, false, "5m 30s");
    // Assert
    expect(html).toContain(
      `time: <span class="info-time" data-task-timer>5m 30s</span>`,
    );
  });

  it("reads the idle label when no task is running", () => {
    // Arrange + Act + Assert — the default, since the store starts idle.
    expect(sessionInfoHtml("ws", null)).toContain(
      `time: <span class="info-time" data-task-timer>--</span>`,
    );
  });

  it("marks the timer span so the tick can repaint it alone", () => {
    // Arrange + Act — a whole-strip rewrite once a second would be churn.
    const html = sessionInfoHtml("ws", null, [], [], 0, false, false, "12s");
    // Assert
    expect(html).toContain(TIMER_SLOT);
  });

  it("places the timer between the parent workspace and the token count", () => {
    // Arrange + Act
    const html = sessionInfoHtml("ws", null, [], [], 0, false, false, "12s");
    // Assert
    expect(html.indexOf("time:")).toBeGreaterThan(html.indexOf("parent workspace:"));
    expect(html.indexOf("time:")).toBeLessThan(html.indexOf("tokens:"));
  });
});

describe("compactionBannerHtml", () => {
  it("renders nothing when no compaction is running", () => {
    // Arrange / Act / Assert — an empty string collapses the banner slot.
    expect(compactionBannerHtml(false)).toBe("");
  });

  it("names the compaction in progress", () => {
    // Arrange / Act
    const html = compactionBannerHtml(true);
    // Assert
    expect(html).toContain("Compacting conversation");
  });

  it("draws an indeterminate bar rather than a percentage", () => {
    // Arrange / Act — the SDK reports no fraction, so the bar is a looping
    // track/fill with no numeric progress in it.
    const html = compactionBannerHtml(true);
    // Assert
    expect(html).toContain("compact-progress-bar");
    expect(html).not.toMatch(/%/);
  });

  it("marks the banner as a live status region for assistive tech", () => {
    // Arrange / Act
    const html = compactionBannerHtml(true);
    // Assert
    expect(html).toContain('role="status"');
    expect(html).toContain('aria-live="polite"');
  });
});

describe("interruptingIndicatorHtml", () => {
  it("renders nothing when not interrupting", () => {
    // Arrange / Act / Assert — an empty string drops the tail node.
    expect(interruptingIndicatorHtml(false)).toBe("");
  });

  it("names the interrupt in progress", () => {
    // Arrange / Act
    const html = interruptingIndicatorHtml(true);
    // Assert
    expect(html).toContain("interrupting");
  });

  it("carries the same spinner class the thinking indicator animates", () => {
    // Arrange / Act — the red twin reuses the thinking-spin animation via its
    // own spinner class, so the markup exposes that spinner hook.
    const html = interruptingIndicatorHtml(true);
    // Assert
    expect(html).toContain("interrupting-spinner");
  });

  it("marks the indicator as a live status region for assistive tech", () => {
    // Arrange / Act
    const html = interruptingIndicatorHtml(true);
    // Assert
    expect(html).toContain('role="status"');
    expect(html).toContain('aria-live="polite"');
  });
});

describe("modelOptionsHtml", () => {
  const MODELS = [
    { value: "opus", displayName: "Opus 4.5", description: "smartest" },
    { value: "haiku", displayName: "Haiku 4.5", description: "fastest" },
  ];

  it("renders one option per model the daemon offers", () => {
    // Arrange + Act
    const html = modelOptionsHtml(MODELS, "opus");
    // Assert
    expect(html).toContain(`value="opus"`);
    expect(html).toContain(`value="haiku"`);
  });

  it("labels each option with its display name", () => {
    // Arrange + Act + Assert
    expect(modelOptionsHtml(MODELS, "opus")).toContain(">Opus 4.5</option>");
  });

  it("selects the live model", () => {
    // Arrange + Act
    const html = modelOptionsHtml(MODELS, "haiku");
    // Assert
    expect(html).toContain(`<option value="haiku" selected`);
  });

  it("does not select a model the session is not on", () => {
    // Arrange + Act
    const html = modelOptionsHtml(MODELS, "haiku");
    // Assert
    expect(html).toContain(`<option value="opus" title=`);
  });

  it("selects a disabled placeholder before any model is known", () => {
    // Arrange — pre-hello. Without this the browser auto-selects the first
    // option and the picker claims a model the session is not on.
    // Act
    const html = modelOptionsHtml(MODELS, "");
    // Assert
    expect(html).toContain(`<option value="" disabled selected>`);
  });

  it("names a live model the menu does not list", () => {
    // Arrange — an id the CLI accepts but does not advertise.
    // Act
    const html = modelOptionsHtml(MODELS, "claude-secret-9");
    // Assert — the picker tells the truth about what is actually running.
    expect(html).toContain(`<option value="claude-secret-9" selected>claude-secret-9</option>`);
  });

  it("still offers the menu alongside an unlisted live model", () => {
    // Arrange + Act
    const html = modelOptionsHtml(MODELS, "claude-secret-9");
    // Assert
    expect(html).toContain(`value="opus"`);
  });

  it("renders only the placeholder when nothing is known at all", () => {
    // Arrange + Act
    const html = modelOptionsHtml([], "");
    // Assert
    expect(html).toBe(`<option value="" disabled selected>model…</option>`);
  });

  it("escapes markup in a model id", () => {
    // Arrange + Act + Assert
    expect(modelOptionsHtml([], "<b>x")).not.toContain("<b>x");
  });

  it("escapes markup in a model display name", () => {
    // Arrange
    const evil = [{ value: "m", displayName: "<b>m", description: "d" }];
    // Act + Assert
    expect(modelOptionsHtml(evil, "m")).not.toContain("<b>m");
  });
});

describe("diffHtml", () => {
  it("classes added, removed and hunk lines", () => {
    // Arrange
    const diff = "@@ -1,1 +1,1 @@\n-old\n+new";
    // Act
    const html = diffHtml(diff);
    // Assert
    expect(html).toContain(`<span class="hunk">@@ -1,1 +1,1 @@</span>`);
    expect(html).toContain(`<span class="del">-old</span>`);
    expect(html).toContain(`<span class="add">+new</span>`);
  });
});

describe("renderItem", () => {
  it("stamps a user prompt bubble with its relative age", () => {
    // Arrange — a send time firmly in the past, so the age reads `… ago`
    // however long the test runs after it (exact math is formatBubbleTime's).
    const item = userTurnAt(14, 32);
    // Act + Assert
    expect(renderItem(item)).toMatch(/<span class="turn-ts">[^<]+ ago<\/span>/);
  });

  it("keeps the prompt text alongside its send-time stamp", () => {
    // Arrange
    const item = userTurnAt(14, 32, "do the thing");
    // Act
    const html = renderItem(item);
    // Assert — the stamp trails the prompt's body column inside the same bubble.
    expect(html).toContain(
      `<div class="bubble user"><div class="bubble-body"><pre>do the thing</pre></div><span class="turn-ts">`,
    );
  });

  it("stamps an agent response bubble with a relative age", () => {
    // Arrange
    const item = textAt(14, 33);
    // Act + Assert
    expect(renderItem(item)).toMatch(/<span class="turn-ts">[^<]+ ago<\/span>/);
  });

  it("stamps a response rendered as a metaprompt tree", () => {
    // Arrange — the tree path builds its own bubble, so it needs the stamp too.
    const item = textAt(14, 34, "Response (👀 no changes made)\n\n1 👀 Answer\n└── 1.1 First");
    // Act + Assert
    expect(renderItem(item)).toMatch(/<span class="turn-ts">[^<]+ ago<\/span>/);
  });

  it("stamps the corner of a response bubble rather than its text column", () => {
    // Arrange
    const item = textAt(14, 33, "the answer");
    // Act
    const html = renderItem(item);
    // Assert — the stamp is the body column's sibling, so it never sits in the prose.
    expect(html).toMatch(/<\/div><span class="turn-ts">[^<]+ ago<\/span><\/div>/);
  });

  it("keeps a streaming response's cursor inside the body column", () => {
    // Arrange
    const item: ConversationItem = { ...(textAt(14, 33, "hel") as TextItem), done: false };
    // Act
    const html = renderItem(item);
    // Assert — the cursor trails the text, not the stamp.
    expect(html).toContain(`<span class="cursor">▍</span></div><span class="turn-ts">`);
  });

  it("hides the host's injected spans from the user bubble", () => {
    // Arrange — a workspace-generation first send: read-directive and
    // autonomous preamble bracketed as meta, the typed task in between.
    const item = userTurnAt(
      14,
      32,
      `${META_OPEN}read the file at /repo/metaprompt.md${META_CLOSE}\n\n` +
        `${META_OPEN}Do not wait for further instructions. Here is the task:\n\n${META_CLOSE}` +
        `move the metaprompt into the repo`,
    );
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(`<pre>move the metaprompt into the repo</pre>`);
    expect(html).not.toContain("read the file at");
  });

  it("renders no bubble for a turn that is nothing but injected spans", () => {
    // Arrange
    const item = userTurnAt(14, 32, `${META_OPEN}read the file${META_CLOSE}`);
    // Act + Assert
    expect(renderItem(item)).toBe("");
  });

  it("renders a streaming text block with a cursor", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "hel",
      done: false,
    };
    // Act + Assert
    expect(renderItem(item)).toContain("cursor");
  });

  it("renders text blocks through the markdown engine", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "# Hi\n**bold** and `code`",
      done: true,
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("<h1>Hi</h1>");
    expect(html).toContain("<strong>bold</strong>");
    expect(html).toContain("<code>code</code>");
  });

  it("escapes raw HTML in markdown text blocks", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "<img src=x onerror=alert(1)>",
      done: true,
    };
    // Act + Assert
    expect(renderItem(item)).not.toContain("<img");
  });

  it("renders a finished text block without a cursor", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "hello",
      done: true,
    };
    // Act + Assert
    expect(renderItem(item)).not.toContain("cursor");
  });

  it("green-borders a text block flagged as a turn's final response", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "done",
      done: true,
    };
    // Act
    const html = renderItem(item, undefined, finalsClosing(item));
    // Assert
    expect(html).toContain(`class="bubble assistant md final-response"`);
  });

  it("nests the completed turn's chip inside the final response it closes", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "done",
      done: true,
      ts: TEXT_TS,
    };
    // Act
    const html = renderItem(item, undefined, finalsClosing(item));
    // Assert — the chip's div opens before the bubble's div closes.
    expect(html).toMatch(/<div class="bubble assistant md final-response">[\s\S]*class="result ok done"/);
  });

  it("seats the nested chip below the answer's own text", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "the answer",
      done: true,
      ts: TEXT_TS,
    };
    // Act
    const html = renderItem(item, undefined, finalsClosing(item));
    // Assert — the chip trails the prose it closes rather than heading it.
    expect(html.indexOf(`class="result`)).toBeGreaterThan(html.indexOf("the answer"));
  });

  it("nests the chip inside a final response rendered as a metaprompt tree", () => {
    // Arrange — the tree path builds its own bubble, so it must carry the chip too.
    const item: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "Response (👀 no changes made)\n\n1 👀 Answer\n├── 1.1 First\n└── 1.2 Second",
      done: true,
      ts: TEXT_TS,
    };
    // Act
    const html = renderItem(item, undefined, finalsClosing(item));
    // Assert
    expect(html).toMatch(/<div class="bubble assistant md final-response">[\s\S]*class="result ok done"/);
  });

  it("withholds the chip from a bubble that is not a turn's final response", () => {
    // Arrange — commentary the agent emitted before its answer.
    const item: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "working on it",
      done: true,
      ts: TEXT_TS,
    };
    // Act
    const html = renderItem(item, undefined, finalsClosing(text("b2")));
    // Assert
    expect(html).not.toContain(`class="result`);
  });

  it("pulses a text block flagged as the working frontier", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "on it",
      done: true,
    };
    // Act
    const html = renderItem(item, undefined, undefined, true);
    // Assert
    expect(html).toContain(`class="bubble assistant md pulsing"`);
  });

  it("pulses a working-frontier response rendered as a metaprompt tree", () => {
    // Arrange — the tree path builds its own bubble, so it needs the class too.
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "Response (👀 no changes made)\n\n1 👀 Answer\n├── 1.1 First\n└── 1.2 Second",
      done: true,
    };
    // Act
    const html = renderItem(item, undefined, undefined, true);
    // Assert
    expect(html).toContain(`class="bubble assistant md pulsing"`);
  });

  it("withholds the pulse from a text block that is not the working frontier", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "on it",
      done: true,
    };
    // Act
    const html = renderItem(item, undefined, undefined, false);
    // Assert
    expect(html).not.toContain("pulsing");
  });

  it("never breathes a tool card, even when the pulse flag is set", () => {
    // Arrange — a running tool leans on its own run badge, never a breath.
    const item = tool();
    // Act — the flag is on, but ToolCard ignores it entirely now.
    const html = renderItem(item, undefined, undefined, true);
    // Assert
    expect(html).not.toContain("pulsing");
  });

  it("pulses a prompt bubble flagged as the feed's newest drawn thing", () => {
    // Arrange
    const item = userTurnAt(9, 0);
    // Act
    const html = renderItem(item, undefined, undefined, true);
    // Assert
    expect(html).toContain(`class="bubble user pulsing"`);
  });

  it("withholds the pulse from a prompt the agent has already answered", () => {
    // Arrange
    const item = userTurnAt(9, 0);
    // Act
    const html = renderItem(item, undefined, undefined, false);
    // Assert
    expect(html).not.toContain("pulsing");
  });

  it("withholds the green border from a text block that is not a final response", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "working on it",
      done: true,
    };
    // Act
    const html = renderItem(item, undefined, undefined);
    // Assert
    expect(html).not.toContain("final-response");
  });

  it("green-borders a final response rendered as a metaprompt tree", () => {
    // Arrange — the tree path builds its own bubble, so it needs the class too.
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "Response (👀 no changes made)\n\n1 👀 Answer\n├── 1.1 First\n└── 1.2 Second",
      done: true,
    };
    // Act
    const html = renderItem(item, undefined, finalsClosing(item));
    // Assert
    expect(html).toContain(`class="bubble assistant md final-response"`);
  });

  it("renders a thinking block that carries text as an expandable card", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "thinking",
      blockId: "b1",
      messageId: "m1",
      text: "step one",
      done: true,
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("<details");
    expect(html).toContain("step one");
  });

  it("shows a pending indicator instead of an empty card while a textless thinking block streams", () => {
    // Arrange — adaptive thinking: signature only, no thinking text.
    const item: ConversationItem = {
      kind: "thinking",
      blockId: "b1",
      messageId: "m1",
      text: "",
      done: false,
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("thinking-pending");
    expect(html).not.toContain("<details");
  });

  it("marks the streaming textless thinking indicator with the circular spinner", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "thinking",
      blockId: "b1",
      messageId: "m1",
      text: "",
      done: false,
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(`<span class="thinking-spinner" aria-hidden="true">`);
  });

  it("drops the ••• pulse from the streaming textless thinking indicator", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "thinking",
      blockId: "b1",
      messageId: "m1",
      text: "",
      done: false,
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).not.toContain("•••");
  });

  it("drops a textless thinking block once it closes", () => {
    // Arrange — nothing to disclose: the API withheld the thinking text.
    const item: ConversationItem = {
      kind: "thinking",
      blockId: "b1",
      messageId: "m1",
      text: "",
      done: true,
      signature: "sig",
    };
    // Act + Assert
    expect(renderItem(item)).toBe("");
  });

  it("renders known tools with their special card class", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Bash",
      messageId: "m1",
      inputJson: "",
      input: { command: "ls" },
      inputDone: true,
    };
    // Act + Assert
    expect(renderItem(item)).toContain("tool-bash");
  });

  it("renders unknown tools with the generic card class", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "WebFetch",
      messageId: "m1",
      inputJson: "{}",
      inputDone: true,
    };
    // Act + Assert
    expect(renderItem(item)).toContain("tool-generic");
  });

  it("spins the running badge of a tool call whose result has not landed", () => {
    // Arrange — input complete, result outstanding: the wait the arc marks.
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Bash",
      messageId: "m1",
      inputJson: `{"command":"sleep 5"}`,
      input: { command: "sleep 5" },
      inputDone: true,
    };
    // Act + Assert
    expect(renderItem(item)).toContain(`<span class="tool-spinner" aria-hidden="true"></span>`);
  });

  it("drops the running arc once the tool result lands", () => {
    // Arrange — a settled call carries the done badge, not motion.
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Bash",
      messageId: "m1",
      inputJson: `{"command":"ls"}`,
      input: { command: "ls" },
      inputDone: true,
      result: { isError: false, content: "a.txt" },
    };
    // Act + Assert
    expect(renderItem(item)).not.toContain("tool-spinner");
  });

  it("spins the running badge of a call whose input is still streaming", () => {
    // Arrange — in-flight is one look: the streaming-input phase carries the
    // same arc the awaiting-result phase does.
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Edit",
      messageId: "m1",
      inputJson: `{"file_`,
      inputDone: false,
    };
    // Act + Assert
    expect(renderItem(item)).toContain(`<span class="tool-spinner" aria-hidden="true"></span>`);
  });

  it("labels a still-streaming call's run badge as streaming rather than running", () => {
    // Arrange — the arc is shared across both in-flight phases, so only the
    // badge's label tells them apart.
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Edit",
      messageId: "m1",
      inputJson: `{"file_`,
      inputDone: false,
    };
    // Act + Assert
    expect(renderItem(item)).toContain("streaming input…");
  });

  it("renders the tool title inside the styled tool-name span", () => {
    // Arrange — .tool-name is the CSS hook the purple title color
    // (--tool-title) hangs off; the class must stay on the header.
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Edit",
      messageId: "m1",
      inputJson: "{}",
      inputDone: true,
    };
    // Act + Assert
    expect(renderItem(item)).toContain(`<span class="tool-name">Edit</span>`);
  });

  it("renders pending permissions with decision buttons", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "permission",
      requestId: "p1",
      toolUseId: "t1",
      toolName: "Bash",
      input: {},
      preview: { kind: "bash", command: "ls" },
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(`data-perm-allow="p1"`);
    expect(html).toContain(`data-perm-deny="p1"`);
  });

  it("renders resolved permissions without buttons", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "permission",
      requestId: "p1",
      toolUseId: "t1",
      toolName: "Bash",
      input: {},
      resolution: { decision: "deny", message: "no" },
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).not.toContain("data-perm-allow");
  });

  it("suppresses the tool card for AskUserQuestion", () => {
    // Arrange — the picker card is the question's UI; the tool card
    // would only dump the raw questions JSON next to it.
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "AskUserQuestion",
      messageId: "m1",
      input: { questions: [] },
      inputJson: `{"questions":[]}`,
      inputDone: true,
    };
    // Act + Assert
    expect(renderItem(item)).toBe("");
  });

  it("suppresses the tool card for ToolSearch", () => {
    // Arrange — deferred-tool schema loading is harness plumbing, not
    // conversation content.
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "ToolSearch",
      messageId: "m1",
      input: { query: "select:SendMessage" },
      inputJson: `{"query":"select:SendMessage"}`,
      inputDone: true,
    };
    // Act + Assert
    expect(renderItem(item)).toBe("");
  });

  it("renders SendMessage as its recipient and summary only", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "SendMessage",
      messageId: "m1",
      input: { to: "researcher", summary: "assign task 1", message: "start on task #1" },
      inputJson: `{"to":"researcher","summary":"assign task 1","message":"start on task #1"}`,
      inputDone: true,
    };
    // Act
    const html = renderItem(item);
    // Assert — the preview summary shows, the message body does not.
    expect(html).toContain("→ researcher: assign task 1");
    expect(html).not.toContain("start on task #1");
  });

  it("suppresses successful SendMessage result bodies", () => {
    // Arrange — the delivery echo adds nothing over the summary line.
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "SendMessage",
      messageId: "m1",
      input: { to: "researcher", summary: "assign task 1", message: "go" },
      inputJson: "",
      inputDone: true,
      result: { isError: false, content: "Message delivered to researcher" },
    };
    // Act + Assert
    expect(renderItem(item)).not.toContain("delivered");
  });

  it("keeps SendMessage error results visible", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "SendMessage",
      messageId: "m1",
      input: { to: "ghost", summary: "assign task 1", message: "go" },
      inputJson: "",
      inputDone: true,
      result: { isError: true, content: "no such agent: ghost" },
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("no such agent: ghost");
    expect(html).toContain("stderr");
  });

  it("renders the Skill invocation as an expandable input section, not raw input JSON", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Skill",
      messageId: "m1",
      input: { skill: "debug-logs", args: "emacs crashed twice" },
      inputJson: `{"skill":"debug-logs","args":"emacs crashed twice"}`,
      inputDone: true,
    };
    // Act
    const html = renderItem(item);
    // Assert — the skill-input class is what makes the section click-expandable.
    expect(html).toContain("skill-input");
    expect(html).toContain("/debug-logs");
    expect(html).not.toContain('"skill":"debug-logs"');
  });

  it("shows the Skill args in the invocation input section", () => {
    // Arrange — the args are what the skill was invoked with, part of the input.
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Skill",
      messageId: "m1",
      input: { skill: "debug-logs", args: "emacs crashed twice" },
      inputJson: `{"skill":"debug-logs","args":"emacs crashed twice"}`,
      inputDone: true,
    };
    // Act + Assert
    expect(renderItem(item)).toContain("/debug-logs emacs crashed twice");
  });

  it("renders the SKILL.md body as an expandable content section from a skill render hint", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Skill",
      messageId: "m1",
      input: { skill: "debug-logs" },
      inputJson: `{"skill":"debug-logs"}`,
      inputDone: true,
      result: {
        isError: false,
        content: "Launching skill: debug-logs",
        render: { kind: "skill", content: "# Debug Logs\nread the log" },
      },
    };
    // Act
    const html = renderItem(item);
    // Assert — the skill-content class is what makes the body click-expandable.
    expect(html).toContain("skill-content");
    expect(html).toContain("# Debug Logs");
    expect(html).toContain("read the log");
  });

  it("tags the Skill card with the class its turquoise wash hangs on", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Skill",
      messageId: "m1",
      input: { skill: "debug-logs" },
      inputJson: `{"skill":"debug-logs"}`,
      inputDone: true,
    };
    // Act + Assert
    expect(renderItem(item)).toContain("tool-card tool-skill");
  });

  it("suppresses a successful Skill result that carries no skill render hint", () => {
    // Arrange — no render hint means the daemon could not resolve a SKILL.md
    // (a plugin skill, a missing file), so only the raw launch echo is left.
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Skill",
      messageId: "m1",
      input: { skill: "debug-logs" },
      inputJson: "",
      inputDone: true,
      result: { isError: false, content: "Launching skill: debug-logs" },
    };
    // Act + Assert — no content section when there is nothing to show.
    const html = renderItem(item);
    expect(html).not.toContain("tool-output");
    expect(html).not.toContain("skill-content");
  });

  it("keeps Skill error results visible", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Skill",
      messageId: "m1",
      input: { skill: "ghost-skill" },
      inputJson: "",
      inputDone: true,
      result: { isError: true, content: "no such skill: ghost-skill" },
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("no such skill: ghost-skill");
    expect(html).toContain("stderr");
  });

  it("falls back to the input JSON for a Skill call with no skill name", () => {
    // Arrange — a malformed input must not silently render an empty card.
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Skill",
      messageId: "m1",
      input: {},
      inputJson: `{}`,
      inputDone: true,
    };
    // Act + Assert
    expect(renderItem(item)).toContain("tool-input");
  });

  it("renders AskUserQuestion as an option picker, not allow/deny", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "permission",
      requestId: "q1",
      toolUseId: "t1",
      toolName: "AskUserQuestion",
      input: {
        questions: [
          {
            question: "Which library?",
            header: "Library",
            multiSelect: false,
            options: [
              { label: "date-fns", description: "small" },
              { label: "moment", description: "legacy" },
            ],
          },
        ],
      },
    };
    // Act
    const html = renderItem(item);
    // Assert — options and a disabled submit, no bare Allow button.
    expect(html).toContain("Which library?");
    expect(html).toContain(`data-q-req="q1"`);
    expect(html).toContain(">date-fns</button>");
    expect(html).toContain(`data-q-submit="q1" disabled`);
    expect(html).toContain(`data-perm-deny="q1"`);
    expect(html).not.toContain("data-perm-allow");
  });

  it("marks picked options selected and enables submit when complete", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "permission",
      requestId: "q1",
      toolUseId: "t1",
      toolName: "AskUserQuestion",
      input: {
        questions: [
          {
            question: "Which library?",
            header: "Library",
            options: [
              { label: "date-fns", description: "small" },
              { label: "moment", description: "legacy" },
            ],
          },
        ],
      },
    };
    const selections = new Map([["q1 0", new Set(["date-fns"])]]);
    // Act
    const html = renderItem(item, selections);
    // Assert
    expect(html).toContain(`class="q-opt selected"`);
    expect(html).toContain(`data-q-submit="q1">`);
    expect(html).not.toContain(`data-q-submit="q1" disabled`);
  });

  it("renders an answered AskUserQuestion as resolved", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "permission",
      requestId: "q1",
      toolUseId: "t1",
      toolName: "AskUserQuestion",
      input: {
        questions: [
          {
            question: "Which library?",
            header: "Library",
            options: [
              { label: "date-fns", description: "small" },
              { label: "moment", description: "legacy" },
            ],
          },
        ],
      },
      resolution: { decision: "allow" },
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("answered");
    expect(html).not.toContain("data-q-submit");
  });

  it("renders backtick-wrapped question text as inline code", () => {
    // Arrange — a pending question whose text names a code symbol.
    const item: ConversationItem = {
      kind: "permission",
      requestId: "q1",
      toolUseId: "t1",
      toolName: "AskUserQuestion",
      input: {
        questions: [
          {
            question: "Default to `nil`?",
            header: "Default",
            options: [{ label: "yes", description: "use nil" }],
          },
        ],
      },
    };
    // Act
    const html = renderItem(item);
    // Assert — the backticks became a <code> span, not literal backticks.
    expect(html).toContain("Default to <code>nil</code>?");
    expect(html).not.toContain("`nil`");
  });

  it("renders a backtick-wrapped option label as inline code", () => {
    // Arrange — an option label naming a code literal.
    const item: ConversationItem = {
      kind: "permission",
      requestId: "q1",
      toolUseId: "t1",
      toolName: "AskUserQuestion",
      input: {
        questions: [
          {
            question: "Which value?",
            header: "Value",
            options: [{ label: "use `nil`", description: "the nil literal" }],
          },
        ],
      },
    };
    // Act
    const html = renderItem(item);
    // Assert — the button label carries a <code> span.
    expect(html).toContain("use <code>nil</code></button>");
  });

  it("renders backtick-wrapped question text as inline code when resolved", () => {
    // Arrange — an answered question whose text names a code symbol.
    const item: ConversationItem = {
      kind: "permission",
      requestId: "q1",
      toolUseId: "t1",
      toolName: "AskUserQuestion",
      input: {
        questions: [
          {
            question: "Default to `nil`?",
            header: "Default",
            options: [{ label: "yes", description: "use nil" }],
          },
        ],
      },
      resolution: { decision: "allow" },
    };
    // Act
    const html = renderItem(item);
    // Assert — the resolved echo also renders the code span.
    expect(html).toContain("Default to <code>nil</code>?");
  });

  it("leaves an option description's backticks plain in its title tooltip", () => {
    // Arrange — a description carrying backticks lives in a title attribute,
    // which cannot render markup, so it must stay literal (escape-only).
    const item: ConversationItem = {
      kind: "permission",
      requestId: "q1",
      toolUseId: "t1",
      toolName: "AskUserQuestion",
      input: {
        questions: [
          {
            question: "Which value?",
            header: "Value",
            options: [{ label: "yes", description: "the `nil` literal" }],
          },
        ],
      },
    };
    // Act
    const html = renderItem(item);
    // Assert — no <code> injected into the tooltip; backticks stay literal.
    expect(html).toContain('title="the `nil` literal"');
  });

  it("hides the raw partial input JSON of a still-streaming call", () => {
    // Arrange — input still streaming: raw partial JSON must NOT show.
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Read",
      messageId: "m1",
      inputJson: `{"file_path":"/private/e`,
      inputDone: false,
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).not.toContain("file_path");
  });

  it("drops the ••• pulse from a still-streaming call's empty body", () => {
    // Arrange — the head's running arc is the sole in-progress indicator, so
    // the body pulses nothing while it waits to be filled.
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Read",
      messageId: "m1",
      inputJson: `{"file_path":"/private/e`,
      inputDone: false,
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).not.toContain("•••");
    expect(html).not.toContain("tool-input-pending");
  });

  it("escapes untrusted content in tool output", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Bash",
      messageId: "m1",
      inputJson: "",
      input: { command: "ls" },
      inputDone: true,
      result: { isError: false, content: `<script>alert(1)</script>` },
    };
    // Act + Assert
    expect(renderItem(item)).not.toContain("<script>");
  });

  describe("Read results", () => {
    const readItem = (
      filePath: string | undefined,
      content: string,
      isError = false,
    ): ToolItem => ({
      kind: "tool",
      toolUseId: "t1",
      toolName: "Read",
      messageId: "m1",
      inputJson: "",
      input: filePath === undefined ? {} : { file_path: filePath },
      inputDone: true,
      result: { isError, content },
    });

    it("syntax-highlights the preview for a known file extension", () => {
      // Arrange + Act
      const html = renderItem(readItem("/w/app.ts", "     1\tconst x = 1;"));
      // Assert
      expect(html).toContain(`<span class="hljs-keyword">const</span>`);
    });

    it("renders the preview plain for an unknown extension", () => {
      // Arrange + Act
      const html = renderItem(readItem("/w/notes.xyz", "     1\tconst x = 1;"));
      // Assert — no token spans (the bare .hljs wrapper is fine).
      expect(html).not.toContain(`class="hljs-`);
    });

    it("renders the preview plain when file_path is missing", () => {
      // Arrange + Act
      const html = renderItem(readItem(undefined, "     1\tconst x = 1;"));
      // Assert — no language, but the numbering still gets styled.
      expect(html).not.toContain(`class="hljs-`);
      expect(html).toContain("line-no");
    });

    it("lifts cat -n number prefixes into line-no spans", () => {
      // Arrange + Act
      const html = renderItem(
        readItem("/w/app.ts", "     1\tconst x = 1;\n     2\tlet y = 2;"),
      );
      // Assert
      expect(html).toContain(`<span class="line-no">     1\t</span>`);
      expect(html).toContain(`<span class="line-no">     2\t</span>`);
    });

    it("keeps the number prefix out of the highlighted code", () => {
      // Arrange + Act
      const html = renderItem(readItem("/w/app.ts", "    12\tconst x = 1;"));
      // Assert — the keyword span starts right after the prefix span.
      expect(html).toContain(`\t</span><span class="hljs-keyword">const</span>`);
    });

    it("renders a numbered markdown Read as formatted markdown without a gutter", () => {
      // Arrange + Act
      const html = renderItem(readItem("/w/README.md", "     1\t# Title\n     2\t**bold**"));
      // Assert — rendered blocks, no line-no spans, capped container.
      expect(html).toContain("<h1>Title</h1>");
      expect(html).toContain("<strong>bold</strong>");
      expect(html).not.toContain("line-no");
      expect(html).toContain(`class="tool-output tool-read-output tool-read-md"`);
    });

    it("renders a non-numbered markdown Read as formatted markdown", () => {
      // Arrange + Act
      const html = renderItem(readItem("/w/notes.markdown", "# Heading"));
      // Assert
      expect(html).toContain("<h1>Heading</h1>");
    });

    it("keeps markdown Read errors on the plain stderr path", () => {
      // Arrange + Act
      const html = renderItem(readItem("/w/README.md", "# not rendered", true));
      // Assert
      expect(html).toContain("stderr");
      expect(html).not.toContain("<h1>");
    });

    it("highlights non-numbered content as-is", () => {
      // Arrange + Act
      const html = renderItem(readItem("/w/app.ts", "const x = 1;"));
      // Assert
      expect(html).toContain(`<span class="hljs-keyword">const</span>`);
      expect(html).not.toContain("line-no");
    });

    it("tolerates a blank trailing line in numbered output", () => {
      // Arrange + Act
      const html = renderItem(readItem("/w/app.ts", "     1\tconst x = 1;\n"));
      // Assert
      expect(html).toContain(`<span class="line-no">     1\t</span>`);
      expect(html).toContain(`<span class="hljs-keyword">const</span>`);
    });

    it("applies the 10-line preview cap class", () => {
      // Arrange + Act + Assert
      expect(renderItem(readItem("/w/app.ts", "     1\tconst x = 1;"))).toContain(
        "tool-read-output",
      );
    });

    it("escapes markup in the preview", () => {
      // Arrange + Act
      const html = renderItem(readItem("/w/page.xyz", "     1\t<script>alert(1)</script>"));
      // Assert
      expect(html).not.toContain("<script>");
    });

    it("renders Read errors through the plain error output", () => {
      // Arrange + Act
      const html = renderItem(readItem("/nope.ts", "File does not exist.", true));
      // Assert
      expect(html).toContain("stderr");
      expect(html).not.toContain("tool-read-output");
    });
  });
});

describe("clear divider", () => {
  it("draws the boundary rule beneath a /clear prompt", () => {
    // Arrange
    const item = userTurnAt(9, 0, "/clear");
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(`<div class="clear-divider"`);
  });

  it("places the boundary rule after the /clear bubble rather than inside it", () => {
    // Arrange
    const item = userTurnAt(9, 0, "/clear");
    // Act
    const html = renderItem(item);
    // Assert — the bubble closes before the rule opens.
    expect(html).toMatch(/<\/div><div class="clear-divider"[^>]*><\/div>$/);
  });

  it("draws no boundary rule beneath an ordinary prompt", () => {
    // Arrange
    const item = userTurnAt(9, 0, "do the thing");
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).not.toContain("clear-divider");
  });

  it("spots a /clear prompt padded with surrounding whitespace", () => {
    // Arrange
    const item = userTurnAt(9, 0, "  /clear\n");
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("clear-divider");
  });

  it("draws no boundary rule for a prompt that merely mentions /clear", () => {
    // Arrange
    const item = userTurnAt(9, 0, "run /clear when you are done");
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).not.toContain("clear-divider");
  });

  it("draws nothing at all for the system init a /clear or session start produces", () => {
    // Arrange
    const item: ConversationItem = { kind: "system", subtype: "init" };
    // Act
    const html = renderItem(item);
    // Assert — the breathing prompt bubble is the signal now, so the note is gone.
    expect(html).toBe("");
  });

  it("draws no (no content) reply bubble beneath a /clear divider", () => {
    // Arrange — the feed a /clear leaves: its bubble, the re-init note, and
    // the CLI's contextless placeholder answer.
    const reply: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "(no content)",
      done: true,
      ts: TEXT_TS,
    };
    const items: ConversationItem[] = [
      userTurnAt(9, 0, "/clear", "r7"),
      { kind: "system", subtype: "init" },
      reply,
      result(),
    ];
    const finals = finalResponses(items);
    // Act
    const html = renderItem(reply, undefined, finals);
    // Assert — nothing renders, so no green bubble sits under the divider.
    expect(html).toBe("");
  });

  it("swallows the /clear reply's result so no standalone chip trails the divider", () => {
    // Arrange — the placeholder answer still anchors the closing result, so
    // the result folds into the (empty-rendered) bubble rather than printing
    // its own chip beneath the divider.
    const reply: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "(no content)",
      done: true,
      ts: TEXT_TS,
    };
    const closer = result();
    const finals = finalResponses([userTurnAt(9, 0, "/clear", "r7"), reply, closer]);
    // Act + Assert
    expect(rendersEmpty(closer, finals)).toBe(true);
  });
});

describe("clearBoundary", () => {
  it("names the /clear turn a cut feed opens on by its request id", () => {
    // Arrange — an already-cut list, as itemsFromLastClear hands the renderer.
    const items = [userTurnAt(9, 0, "/clear", "r7"), textAt(9, 1)];
    // Act + Assert
    expect(clearBoundary(items)).toBe("r7");
  });

  it("reports null for an uncut feed", () => {
    // Arrange
    const items = [userTurnAt(9, 0, "do the thing"), textAt(9, 1)];
    // Act + Assert
    expect(clearBoundary(items)).toBeNull();
  });

  it("reports null for an empty feed", () => {
    // Arrange + Act + Assert
    expect(clearBoundary([])).toBeNull();
  });

  it("reports null when a non-turn item heads the feed", () => {
    // Arrange
    const items = [textAt(9, 1), userTurnAt(9, 2, "/clear")];
    // Act + Assert — a /clear that is not the head did not produce this cut.
    expect(clearBoundary(items)).toBeNull();
  });

  it("distinguishes two /clear cuts by their request ids", () => {
    // Arrange — the SECOND clear must read as a new boundary, or the
    // renderer would reconcile stale nodes across it instead of rebuilding.
    const first = clearBoundary([userTurnAt(9, 0, "/clear", "r1")]);
    // Act
    const second = clearBoundary([userTurnAt(10, 0, "/clear", "r2")]);
    // Assert
    expect(second).not.toBe(first);
  });
});

describe("ResultChip", () => {
  /** A result frame item for the given subtype. */
  function resultItem(subtype: ResultItem["subtype"], isError = false): ResultItem {
    return {
      kind: "result",
      subtype,
      // Distinct from sincePrevFinalMs: the standalone chip must read this,
      // the whole-task figure, and never the since-previous-final one.
      durationMs: 12,
      sincePrevFinalMs: 7,
      numTurns: 1,
      totalCostUsd: 0.5,
      usage: { input_tokens: 3, output_tokens: 4 },
      isError,
      context: { total: 300_000, delta: 100_000 },
    };
  }

  /** The HTML of a completed answer nesting its chip, whose turn closed
   *  ELAPSED ms after the previous final response. */
  function finalResponseHtml(elapsed: number): string {
    const answer: TextItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "done",
      done: true,
      ts: TEXT_TS,
    };
    const closing: ResultItem = { ...result("success"), sincePrevFinalMs: elapsed };
    return renderItem(
      answer,
      undefined,
      finalResponses([userTurnAt(9, 0), answer, closing]),
    );
  }

  it("marks a successful turn's chip with the muted-yellow done class", () => {
    // Arrange + Act
    const html = renderItem(resultItem("success"));
    // Assert
    expect(html).toContain(`class="result ok done"`);
  });

  it("withholds the 'turn complete' label from a successful turn's chip", () => {
    // Arrange + Act — the done wash says it, so the words never do.
    const html = renderItem(resultItem("success"));
    // Assert
    expect(html).not.toContain("turn complete");
  });

  it("withholds the turn's cost from the chip", () => {
    // Arrange + Act
    const html = renderItem(resultItem("success"));
    // Assert
    expect(html).not.toContain("$0.5000");
  });

  it("withholds the turn's own in/out token pair from the chip", () => {
    // Arrange + Act
    const html = renderItem(resultItem("success"));
    // Assert
    expect(html).not.toContain("3in/4out");
  });

  it("renders the turn's duration in whole units", () => {
    // Arrange
    const item = { ...resultItem("success"), durationMs: 330_000 };
    // Act
    const html = renderItem(item);
    // Assert — whole minutes and seconds, never the fractional 5.5m.
    expect(html).toContain("5m 30s ·");
  });

  it("renders the session's standing input tokens after the duration", () => {
    // Arrange + Act
    const html = renderItem(resultItem("success"));
    // Assert
    expect(html).toContain("12ms · 300,000 in · ");
  });

  it("signs a context increase with a plus", () => {
    // Arrange + Act
    const html = renderItem(resultItem("success"));
    // Assert
    expect(html).toContain("300,000 in · +100,000");
  });

  it("signs a context decrease with a minus", () => {
    // Arrange — the first turn after a /compact stands below the last one.
    const item = { ...resultItem("success"), context: { total: 60_000, delta: -140_000 } };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("60,000 in · -140,000");
  });

  it("renders a zero increase as a signed zero", () => {
    // Arrange
    const item = { ...resultItem("success"), context: { total: 300_000, delta: 0 } };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("300,000 in · +0");
  });

  it("withholds the token figures when the turn's context size is unknown", () => {
    // Arrange — a /clear turn: it re-inits the session and reports no new size.
    const item = { ...resultItem("success"), context: null };
    // Act
    const html = renderItem(item);
    // Assert — the duration alone, with no figure to stand beside it.
    expect(html).not.toMatch(/\bin\b/);
  });

  it("labels an aborted turn's chip 'interrupted' rather than its raw subtype", () => {
    // Arrange + Act — an interrupt-ended turn reads as interrupted, not aborted.
    const html = renderItem(resultItem("aborted"));
    // Assert
    expect(html).toContain("interrupted · 12ms");
  });

  it("never prints the raw 'aborted' subtype on an interrupted turn's chip", () => {
    // Arrange + Act — the shim's word for the subtype never reaches the reader.
    const html = renderItem(resultItem("aborted"));
    // Assert
    expect(html).not.toContain("aborted");
  });

  it("marks an aborted turn's chip with the interrupted tone", () => {
    // Arrange — an interrupt is not a failure, so its chip is yellow, not red.
    const html = renderItem(resultItem("aborted"));
    // Assert
    expect(html).toContain(`class="result interrupted"`);
  });

  it("keeps the interrupted tone even when the SDK flags the aborted turn an error", () => {
    // Arrange — the SDK still sets is_error on an abort, but the user's own
    // interrupt is never the error red.
    const html = renderItem(resultItem("aborted", true));
    // Assert
    expect(html).toContain(`class="result interrupted"`);
  });

  it("withholds the done class from a failed turn's chip", () => {
    // Arrange + Act
    const html = renderItem(resultItem("error_during_execution", true));
    // Assert
    expect(html).toContain(`class="result err"`);
  });

  it("reads the standalone chip from the whole-task duration, not the since-previous-final elapsed", () => {
    // Arrange — an aborted turn's chip stands alone in the feed.
    const item = { ...resultItem("aborted"), durationMs: 12, sincePrevFinalMs: 7 };
    // Act
    const html = renderItem(item);
    // Assert — 12ms is durationMs; 7ms (sincePrevFinalMs) must not surface.
    expect(html).toContain("interrupted · 12ms");
    expect(html).not.toContain("7ms");
  });

  it("reads a final-response chip from the since-previous-final elapsed, not the whole-task duration", () => {
    // Arrange — a completed answer whose whole-task figure differs from its
    // since-previous-final elapsed, nested inside the response it closes.
    const answer: TextItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "done",
      done: true,
      ts: TEXT_TS,
    };
    const closing: ResultItem = {
      ...result("success"),
      durationMs: 330_000,
      sincePrevFinalMs: 30_000,
    };
    const finals = finalResponses([userTurnAt(9, 0), answer, closing]);
    // Act
    const html = renderItem(answer, undefined, finals);
    // Assert — 30s is sincePrevFinalMs; the 5m 30s whole-task figure never shows.
    expect(html).toContain("30s · 300,000 in");
    expect(html).not.toContain("5m 30s");
  });

  it("drops the final-response chip when the turn closed in under a second", () => {
    // Arrange + Act — 999ms of elapsed time sits below the one-second floor.
    const html = finalResponseHtml(999);
    // Assert — no timing pill at all below a whole second.
    expect(html).not.toContain(`class="result`);
  });

  it("keeps the final-response wash when the sub-second chip is dropped", () => {
    // Arrange + Act — a sub-second close still produced the turn's answer.
    const html = finalResponseHtml(999);
    // Assert — the green border survives even with no chip beneath it.
    expect(html).toContain("final-response");
  });

  it("renders the final-response chip at the one-second boundary", () => {
    // Arrange + Act — exactly one second cleared, the floor being inclusive.
    const html = finalResponseHtml(1_000);
    // Assert
    expect(html).toContain("1s · 300,000 in");
  });

  it("rounds the final-response chip's duration up to whole seconds", () => {
    // Arrange + Act — 5984ms, a part-second past the fifth second.
    const html = finalResponseHtml(5_984);
    // Assert — 6s, never the 5s 984ms the millisecond scale would give.
    expect(html).toContain("6s · 300,000 in");
    expect(html).not.toContain("984ms");
  });
});

describe("finalResponses", () => {
  it("marks the last text block of a completed turn", () => {
    // Arrange
    const items = [userTurnAt(9, 0), text("b1"), result()];
    // Act
    const finals = finalResponses(items);
    // Assert
    expect([...finals.chips.keys()]).toEqual(["b1"]);
  });

  it("never crowns a subagent's parented text as the turn's answer", () => {
    // Arrange — the subagent's aside lands after the real answer.
    const items = [userTurnAt(9, 0), text("b1"), subagentText("b2"), result()];
    // Act
    const finals = finalResponses(items);
    // Assert — the main-chain block keeps the pairing.
    expect([...finals.chips.keys()]).toEqual(["b1"]);
  });

  it("leaves the chip standalone when only subagent text preceded it", () => {
    // Arrange — a turn that answered through tools alone.
    const closer = result();
    const items = [userTurnAt(9, 0), subagentText("b1"), closer];
    // Act
    const finals = finalResponses(items);
    // Assert
    expect(finals.chips.size).toBe(0);
    expect(finals.swallowed.has(closer)).toBe(false);
  });

  it("pairs the completed turn's answer with the very chip that closes it", () => {
    // Arrange — the pairing is what lets the bubble draw the chip itself.
    const closer = result();
    const items = [userTurnAt(9, 0), text("b1"), closer];
    // Act
    const finals = finalResponses(items);
    // Assert
    expect(finals.chips.get("b1")).toBe(closer);
  });

  it("swallows the result a final response now draws for itself", () => {
    // Arrange
    const closer = result();
    const items = [userTurnAt(9, 0), text("b1"), closer];
    // Act
    const finals = finalResponses(items);
    // Assert — the feed no longer prints this one standalone.
    expect(finals.swallowed.has(closer)).toBe(true);
  });

  it("leaves a completed turn's earlier text block unmarked", () => {
    // Arrange — commentary, then a tool call, then the answer.
    const items = [userTurnAt(9, 0), text("b1"), tool(), text("b2"), result()];
    // Act
    const finals = finalResponses(items);
    // Assert
    expect(finals.chips.has("b1")).toBe(false);
  });

  it("marks the text block that follows a completed turn's last tool call", () => {
    // Arrange
    const items = [userTurnAt(9, 0), text("b1"), tool(), text("b2"), result()];
    // Act
    const finals = finalResponses(items);
    // Assert
    expect(finals.chips.has("b2")).toBe(true);
  });

  it("leaves a still-streaming turn's text unmarked until its result lands", () => {
    // Arrange — no result frame yet, so the next block could still continue it.
    const items = [userTurnAt(9, 0), text("b1")];
    // Act
    const finals = finalResponses(items);
    // Assert
    expect(finals.chips.size).toBe(0);
  });

  it("leaves an aborted turn's last text unmarked", () => {
    // Arrange — an interrupted turn never reached the answer it worked toward.
    const items = [userTurnAt(9, 0), text("b1"), result("aborted")];
    // Act
    const finals = finalResponses(items);
    // Assert
    expect(finals.chips.size).toBe(0);
  });

  it("leaves an aborted turn's chip standing in the feed", () => {
    // Arrange — no bubble claims it, so the feed keeps printing it itself.
    const closer = result("aborted");
    const items = [userTurnAt(9, 0), text("b1"), closer];
    // Act
    const finals = finalResponses(items);
    // Assert
    expect(finals.swallowed.has(closer)).toBe(false);
  });

  it("leaves a failed turn's last text unmarked", () => {
    // Arrange
    const items = [userTurnAt(9, 0), text("b1"), result("error_during_execution")];
    // Act
    const finals = finalResponses(items);
    // Assert
    expect(finals.chips.size).toBe(0);
  });

  it("marks the final text of every completed turn in the feed", () => {
    // Arrange
    const items = [
      userTurnAt(9, 0),
      text("b1"),
      result(),
      userTurnAt(9, 5),
      text("b2"),
      result(),
    ];
    // Act
    const finals = finalResponses(items);
    // Assert
    expect([...finals.chips.keys()]).toEqual(["b1", "b2"]);
  });

  it("marks nothing for a completed turn that produced no text at all", () => {
    // Arrange — a turn that only ran tools.
    const items = [userTurnAt(9, 0), tool(), result()];
    // Act
    const finals = finalResponses(items);
    // Assert
    expect(finals.chips.size).toBe(0);
  });

  it("leaves a textless completed turn's chip standing in the feed", () => {
    // Arrange — a tools-only turn wrote no answer for the chip to sit inside.
    const closer = result();
    const items = [userTurnAt(9, 0), tool(), closer];
    // Act
    const finals = finalResponses(items);
    // Assert
    expect(finals.swallowed.has(closer)).toBe(false);
  });

  it("never lets a resultless turn's text be claimed by the next turn's result", () => {
    // Arrange — turn one never closed; turn two ran tools only and completed.
    const items = [userTurnAt(9, 0), text("b1"), userTurnAt(9, 5), tool(), result()];
    // Act
    const finals = finalResponses(items);
    // Assert
    expect(finals.chips.size).toBe(0);
  });
});

describe("swallowed chips", () => {
  it("prints no standalone chip for a result a final response swallowed", () => {
    // Arrange
    const closer = result();
    const finals = finalResponses([userTurnAt(9, 0), text("b1"), closer]);
    // Act
    const html = renderItem(closer, undefined, finals);
    // Assert — the bubble above it already drew this chip.
    expect(html).toBe("");
  });

  it("still prints the standalone chip of an aborted turn", () => {
    // Arrange
    const closer = result("aborted");
    const finals = finalResponses([userTurnAt(9, 0), text("b1"), closer]);
    // Act
    const html = renderItem(closer, undefined, finals);
    // Assert
    expect(html).toContain(`class="result interrupted"`);
  });

  it("still prints the standalone chip of a completed turn that wrote no answer", () => {
    // Arrange — a tools-only turn has no bubble to nest the chip in.
    const closer = result();
    const finals = finalResponses([userTurnAt(9, 0), tool(), closer]);
    // Act
    const html = renderItem(closer, undefined, finals);
    // Assert
    expect(html).toContain(`class="result ok done"`);
  });
});

describe("pulseTarget: the working frontier", () => {
  it("pulses the finished response once the turn's tail tool has settled", () => {
    // Arrange — the tool is done but the turn runs on, so the last word breathes.
    const items = [userTurnAt(9, 0), text("b1"), tool(true)];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toEqual({ kind: "text", blockId: "b1" });
  });

  it("pulses only the LAST finished response of the in-flight turn", () => {
    // Arrange
    const items = [userTurnAt(9, 0), text("b1"), tool(), text("b2")];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toEqual({ kind: "text", blockId: "b2" });
  });

  it("skips a subagent's parented text and pulses the main frontier behind it", () => {
    // Arrange — the subagent chatters after the main agent's last word.
    const items = [userTurnAt(9, 0), text("b1"), subagentText("b2")];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toEqual({ kind: "text", blockId: "b1" });
  });

  it("keeps pulsing the main frontier while a subagent thinks", () => {
    // Arrange — the panel's spinner covers the subagent's own beat.
    const items = [userTurnAt(9, 0), text("b1"), subagentThinking("k1", false)];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toEqual({ kind: "text", blockId: "b1" });
  });

  it("stills the pulse once the turn ends", () => {
    // Arrange — an idle session is working on nothing, so nothing breathes.
    const items = [userTurnAt(9, 0), text("b1"), result()];
    // Act
    const pulse = pulseTarget(items, false);
    // Assert
    expect(pulse).toBeNull();
  });

  it("withholds the pulse from a response still streaming", () => {
    // Arrange — the bubble's own cursor is the live signal there.
    const items = [userTurnAt(9, 0), text("b1", false)];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toBeNull();
  });

  it("withholds the pulse while a thinking indicator is active", () => {
    // Arrange — the thinking spinner is the live signal there.
    const items = [userTurnAt(9, 0), text("b1"), thinking("k1", false)];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toBeNull();
  });

  it("pulses again once that thinking block finishes", () => {
    // Arrange
    const items = [userTurnAt(9, 0), text("b1"), thinking("k1", true)];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toEqual({ kind: "text", blockId: "b1" });
  });

  it("never breathes life back into the previous turn's answer", () => {
    // Arrange — that answer belongs to a question already answered.
    const items = [userTurnAt(9, 0), text("b1"), result(), userTurnAt(9, 5), tool()];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).not.toEqual({ kind: "text", blockId: "b1" });
  });
});

describe("pulseTarget: a running tool card no longer breathes", () => {
  it("shows dead air for a running tool card at the tail", () => {
    // Arrange — a tools-only turn: the run badge is the sole live signal.
    const items = [userTurnAt(9, 0), tool()];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toBeNull();
  });

  it("does not breathe the finished text above a running tool card", () => {
    // Arrange — the running card's own arc speaks for the turn here.
    const items = [userTurnAt(9, 0), text("b1"), tool()];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toBeNull();
  });

  it("shows dead air while parallel calls are in flight", () => {
    // Arrange — two calls running at once, their badges carry the signal.
    const items = [userTurnAt(9, 0), tool(false, "t1"), tool(false, "t2")];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toBeNull();
  });

  it("breathes the finished response once the tail tool has settled", () => {
    // Arrange — a settled card is history, so the last word breathes again.
    const items = [userTurnAt(9, 0), text("b1"), tool(true)];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toEqual({ kind: "text", blockId: "b1" });
  });

  it("shows dead air when the turn is no longer in flight", () => {
    // Arrange
    const items = [userTurnAt(9, 0), tool()];
    // Act
    const pulse = pulseTarget(items, false);
    // Assert
    expect(pulse).toBeNull();
  });

  it("stays dead-air while a thinking indicator runs after the tool", () => {
    // Arrange — the thinking spinner is the live signal there, not the card.
    const items = [userTurnAt(9, 0), tool(), thinking("k1", false)];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toBeNull();
  });
});

describe("pulseTarget: the prompt just sent", () => {
  it("pulses a prompt whose turn has yet to draw anything", () => {
    // Arrange — the send itself, with the agent not yet on the page.
    const items = [userTurnAt(9, 0)];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toEqual({ kind: "user-turn", requestId: "r1" });
  });

  it("pulses the NEWEST prompt when a second one follows the first", () => {
    // Arrange
    const items = [
      userTurnAt(9, 0, "first", "r1"),
      text("b1"),
      result(),
      userTurnAt(9, 5, "second", "r2"),
    ];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toEqual({ kind: "user-turn", requestId: "r2" });
  });

  it("stills the prompt once the turn is no longer in flight", () => {
    // Arrange
    const items = [userTurnAt(9, 0)];
    // Act
    const pulse = pulseTarget(items, false);
    // Assert
    expect(pulse).toBeNull();
  });

  it("stills the prompt when the agent starts streaming a response", () => {
    // Arrange — the bubble's own cursor takes the beat over.
    const items = [userTurnAt(9, 0), text("b1", false)];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toBeNull();
  });

  it("stills the prompt when the thinking indicator renders", () => {
    // Arrange — the thinking spinner takes the beat over.
    const items = [userTurnAt(9, 0), thinking("k1", false)];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toBeNull();
  });

  it("stills the prompt to dead air when a running tool card takes the tail", () => {
    // Arrange — the running card's own run badge is the live signal there.
    const items = [userTurnAt(9, 0), tool()];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toBeNull();
  });

  it("stills the prompt when a permission card takes the feed's tail", () => {
    // Arrange
    const permission: ConversationItem = {
      kind: "permission",
      requestId: "p1",
      toolUseId: "t1",
      toolName: "Bash",
      input: {},
    };
    const items = [userTurnAt(9, 0), permission];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toBeNull();
  });

  it("stills the prompt when an error banner takes the feed's tail", () => {
    // Arrange
    const error: ConversationItem = {
      kind: "error",
      code: "overloaded",
      message: "too busy",
      recoverable: true,
    };
    const items = [userTurnAt(9, 0), error];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toBeNull();
  });

  it("stills the prompt when a retry badge takes the feed's tail", () => {
    // Arrange
    const retry: ConversationItem = {
      kind: "retry",
      attempt: 2,
      reason: "overloaded",
      fatal: false,
    };
    const items = [userTurnAt(9, 0), retry];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toBeNull();
  });

  it("keeps breathing behind a system:init note the feed no longer draws", () => {
    // Arrange — the `/clear` re-init is suppressed, so the prompt is the tail.
    const items: ConversationItem[] = [
      userTurnAt(9, 0),
      { kind: "system", subtype: "init" },
    ];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toEqual({ kind: "user-turn", requestId: "r1" });
  });

  it("stills the prompt when a non-init system note takes the tail", () => {
    // Arrange — only init is suppressed; other notes still render and pause.
    const items: ConversationItem[] = [
      userTurnAt(9, 0),
      { kind: "system", subtype: "task-notification" },
    ];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toBeNull();
  });

  it("stills the prompt when a compaction divider takes the feed's tail", () => {
    // Arrange
    const items: ConversationItem[] = [
      userTurnAt(9, 0),
      { kind: "compact-boundary", trigger: "auto", preTokens: 9, postTokens: 3 },
    ];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toBeNull();
  });

  it("keeps breathing behind a tool call the feed draws no card for", () => {
    // Arrange — a suppressed tool supersedes the prompt in NOTHING the user
    // can see, so stilling the pulse there would leave the feed dead.
    const items = [userTurnAt(9, 0), suppressedTool()];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toEqual({ kind: "user-turn", requestId: "r1" });
  });

  it("keeps breathing behind a textless thinking block that has closed", () => {
    // Arrange — its spinner is gone from the feed, so the beat comes back.
    const items = [userTurnAt(9, 0), thinking("k1", true, "")];
    // Act
    const pulse = pulseTarget(items, true);
    // Assert
    expect(pulse).toEqual({ kind: "user-turn", requestId: "r1" });
  });
});

describe("rendersEmpty", () => {
  it("counts a turn that is nothing but injected spans as undrawn", () => {
    // Arrange
    const item = userTurnAt(9, 0, `${META_OPEN}injected${META_CLOSE}`);
    // Act + Assert
    expect(rendersEmpty(item)).toBe(true);
  });

  it("counts a suppressed tool call as undrawn", () => {
    // Arrange + Act + Assert
    expect(rendersEmpty(suppressedTool())).toBe(true);
  });

  it("counts a closed textless thinking block as undrawn", () => {
    // Arrange + Act + Assert
    expect(rendersEmpty(thinking("k1", true, ""))).toBe(true);
  });

  it("counts an OPEN textless thinking block as drawn, since its spinner shows", () => {
    // Arrange + Act + Assert
    expect(rendersEmpty(thinking("k1", false, ""))).toBe(false);
  });

  it("counts the CLI's (no content) placeholder bubble as undrawn", () => {
    // Arrange — the empty reply a /clear's re-init leaves behind.
    const item: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "(no content)",
      done: true,
      ts: TEXT_TS,
    };
    // Act + Assert
    expect(rendersEmpty(item)).toBe(true);
  });

  it("counts a (no content) placeholder padded with whitespace as undrawn", () => {
    // Arrange — a trailing newline must not smuggle the placeholder past.
    const item: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "  (no content)\n",
      done: true,
      ts: TEXT_TS,
    };
    // Act + Assert
    expect(rendersEmpty(item)).toBe(true);
  });

  it("counts an ordinary text bubble as drawn", () => {
    // Arrange + Act + Assert — only the exact placeholder is suppressed.
    expect(rendersEmpty(text("b1"))).toBe(false);
  });

  it("counts a bubble that merely quotes (no content) mid-sentence as drawn", () => {
    // Arrange — a real answer discussing the placeholder is not the placeholder.
    const item: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "the CLI printed (no content) there",
      done: true,
      ts: TEXT_TS,
    };
    // Act + Assert
    expect(rendersEmpty(item)).toBe(false);
  });

  it("counts a result whose chip a response bubble swallowed as undrawn", () => {
    // Arrange
    const closer = result();
    const finals = finalResponses([userTurnAt(9, 0), text("b1"), closer]);
    // Act + Assert
    expect(rendersEmpty(closer, finals)).toBe(true);
  });

  it("counts a tool card the feed does draw as drawn", () => {
    // Arrange + Act + Assert
    expect(rendersEmpty(tool())).toBe(false);
  });

  it("counts a system:init note as undrawn, so the breathing prompt is the signal", () => {
    // Arrange + Act + Assert
    expect(rendersEmpty({ kind: "system", subtype: "init" })).toBe(true);
  });

  it("counts a non-init system note as drawn", () => {
    // Arrange + Act + Assert
    expect(rendersEmpty({ kind: "system", subtype: "task-notification" })).toBe(false);
  });
});

describe("isPulsed", () => {
  it("marks the text block the pulse names", () => {
    // Arrange + Act + Assert
    expect(isPulsed(text("b1"), { kind: "text", blockId: "b1" })).toBe(true);
  });

  it("spares a text block the pulse does not name", () => {
    // Arrange + Act + Assert
    expect(isPulsed(text("b2"), { kind: "text", blockId: "b1" })).toBe(false);
  });

  it("marks the prompt the pulse names", () => {
    // Arrange
    const pulse = { kind: "user-turn", requestId: "r1" } as const;
    // Act + Assert
    expect(isPulsed(userTurnAt(9, 0), pulse)).toBe(true);
  });

  it("spares a prompt the pulse does not name", () => {
    // Arrange
    const pulse = { kind: "user-turn", requestId: "r2" } as const;
    // Act + Assert
    expect(isPulsed(userTurnAt(9, 0), pulse)).toBe(false);
  });

  it("spares a prompt when the pulse names a text block instead", () => {
    // Arrange — the two targets never land on one bubble.
    const pulse = { kind: "text", blockId: "b1" } as const;
    // Act + Assert
    expect(isPulsed(userTurnAt(9, 0), pulse)).toBe(false);
  });

  it("spares every item when nothing pulses at all", () => {
    // Arrange + Act + Assert
    expect(isPulsed(text("b1"), null)).toBe(false);
  });
});

describe("itemKey", () => {
  it("keys block items by block id", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b7",
      messageId: "m1",
      text: "",
      done: false,
    };
    // Act + Assert
    expect(itemKey(item, 3)).toBe("text:b7");
  });

  it("keys positional items by index", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "result",
      subtype: "success",
      durationMs: 1,
      sincePrevFinalMs: 1,
      numTurns: 1,
      totalCostUsd: 0,
      usage: { input_tokens: 0, output_tokens: 0 },
      isError: false,
      context: null,
    };
    // Act + Assert
    expect(itemKey(item, 5)).toBe("result:5");
  });
});

describe("renderItem tool previews", () => {
  it("previews an Agent spawn by its description, as it does the legacy Task", () => {
    // Arrange — the CLI renamed Task to Agent; the card must not regress to raw JSON.
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Agent",
      messageId: "m1",
      input: { description: "hunt the flake", prompt: "go" },
      inputJson: `{"description":"hunt the flake"}`,
      inputDone: true,
    };
    // Act + Assert
    expect(renderItem(item)).toContain(
      `<div class="file-path agent-input-desc">hunt the flake</div>`,
    );
  });

  it("suppresses the tool card for TaskUpdate", () => {
    // Arrange — task-list bookkeeping is feed noise, not conversation.
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "TaskUpdate",
      messageId: "m1",
      input: { task_id: "1", status: "completed" },
      inputJson: `{"task_id":"1"}`,
      inputDone: true,
    };
    // Act + Assert
    expect(renderItem(item)).toBe("");
  });

  it("caps the Bash command behind the bash-input preview class", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Bash",
      messageId: "m1",
      input: { command: "ls -la" },
      inputJson: `{"command":"ls -la"}`,
      inputDone: true,
    };
    // Act + Assert
    expect(renderItem(item)).toContain(`class="cmd bash-input"`);
  });

  it("caps the Bash output behind the bash-output preview class", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Bash",
      messageId: "m1",
      input: { command: "ls" },
      inputJson: `{"command":"ls"}`,
      inputDone: true,
      result: {
        isError: false,
        content: "file.txt",
        render: { kind: "bash", stdout: "file.txt", stderr: "" },
      },
    };
    // Act + Assert
    expect(renderItem(item)).toContain(`class="tool-output bash-output"`);
  });

  it("caps diff results behind the diff-output preview class", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Edit",
      messageId: "m1",
      input: { file_path: "/tmp/a.ts" },
      inputJson: `{"file_path":"/tmp/a.ts"}`,
      inputDone: true,
      result: {
        isError: false,
        content: "ok",
        render: { kind: "diff", file_path: "/tmp/a.ts", unified_diff: "@@ -1 +1 @@\n-a\n+b" },
      },
    };
    // Act + Assert
    expect(renderItem(item)).toContain(`class="diff diff-output"`);
  });
});

describe("renderItem subagent input", () => {
  /** An Agent call spawning a subagent, its prompt pages long. */
  function agentCall(toolName = "Agent"): ToolItem {
    return {
      kind: "tool",
      toolUseId: "t1",
      toolName,
      messageId: "m1",
      input: { description: "Audit the sentinel", prompt: "Read every file and…" },
      inputJson: `{"description":"Audit the sentinel","prompt":"Read every file and…"}`,
      inputDone: true,
    };
  }

  it("leads the Agent card with the description alone", () => {
    // Arrange
    const item = agentCall();
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(`class="file-path agent-input-desc"`);
    expect(html).toContain("Audit the sentinel");
  });

  it("keeps the Agent prompt out of the description line", () => {
    // Arrange — the prompt reaches the card only inside the folded JSON.
    const item = agentCall();
    // Act
    const desc = renderItem(item).match(/class="file-path agent-input-desc">([^<]*)</)?.[1];
    // Assert
    expect(desc).toBe("Audit the sentinel");
  });

  it("carries the full input JSON in the card, folded behind .agent-json", () => {
    // Arrange
    const item = agentCall();
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(`<pre class="agent-json">`);
    expect(html).toContain("Read every file and");
  });

  it("makes the Agent input box a capped section, so a click unfolds the JSON", () => {
    // Arrange — .tool-input is what expand.ts recognizes as clickable.
    const item = agentCall();
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(`class="tool-input agent-input"`);
  });

  it("washes the Agent card teal by naming it a special tool rather than Generic", () => {
    // Arrange
    const item = agentCall();
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(`class="tool-card tool-agent"`);
  });

  it("gives the legacy Task name the same description-first card", () => {
    // Arrange — Task is what the CLI called the subagent tool before Agent.
    const item = agentCall("Task");
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(`class="tool-input agent-input"`);
  });

  it("falls back to the generic prompt-headlined fold for an Agent call carrying no description", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Agent",
      messageId: "m1",
      input: { prompt: "go" },
      inputJson: `{"prompt":"go"}`,
      inputDone: true,
    };
    // Act
    const html = renderItem(item);
    // Assert — the generic fold headlines the prompt; the agent-specific
    // description form stays off.
    expect(html).toContain("folded-headline");
    expect(html).not.toContain("agent-input-desc");
  });

  it("leaves the Agent's own output rendering untouched by the input fold", () => {
    // Arrange — only the input is description-only; the result still shows.
    const item: ToolItem = { ...agentCall(), result: { isError: false, content: "the findings" } };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(`<pre class="tool-output">the findings</pre>`);
  });
});

describe("backfillChunks", () => {
  it("orders chunks tail-first", () => {
    // Act
    const chunks = backfillChunks(7, 3);
    // Assert — newest indexes first, oldest chunk last.
    expect(chunks).toEqual([[4, 5, 6], [1, 2, 3], [0]]);
  });

  it("returns a single chunk when everything fits", () => {
    // Act + Assert
    expect(backfillChunks(3, 40)).toEqual([[0, 1, 2]]);
  });

  it("returns no chunks for an empty feed", () => {
    // Act + Assert
    expect(backfillChunks(0, 40)).toEqual([]);
  });
});

describe("TextStream metaprompt trees", () => {
  it("renders a bare tree message as hanging-indent tree lines", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "Response (✏️ changes made)\n\n1 🔧 Fixed it\n├── 1.1 Detail\n└── 1.2 More",
      done: true,
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(`class="mp-tree"`);
    expect(html).toContain(`<span class="mp-prefix">└── 1.2 </span>`);
  });

  it("routes a fenced tree message through the markdown pipeline", () => {
    // Arrange — the fence handler owns tree detection inside fences.
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "Response (✏️)\n\n```\n1 🔧 Fixed it\n├── 1.1 Detail\n```",
      done: true,
    };
    // Act
    const html = renderItem(item);
    // Assert — tree html present, produced via the fence path.
    expect(html).toContain(`class="mp-tree"`);
    expect(html).toContain(`class="bubble assistant md"`);
  });

  it("keeps non-tree text on the markdown path", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "Just **prose** here.\nSecond line.",
      done: true,
    };
    // Act + Assert
    expect(renderItem(item)).not.toContain("mp-tree");
  });
});

describe("TextStream chess-game markers", () => {
  const marker =
    "---> agent-repl-chess-game-file: /ws/.claude/emacs/cee-web-widget/chess-game-ab.pgn <---";

  it("renders a marker inside a metaprompt-tree response as a widget container", () => {
    // Arrange — a TLDR tree whose last line is the marker: the tree
    // renderer must not swallow it.
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: `Response (✏️ changes made)\n\n1 🔧 Fixed it\n├── 1.1 Detail\n└── 1.2 More\n\n${marker}`,
      done: true,
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(`class="mp-tree"`);
    expect(html).toContain(`class="chess-game"`);
    expect(html).not.toContain("agent-repl-chess-game-file");
  });

  it("keeps prose flowing around a marker in one bubble", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: `intro line\n${marker}\noutro line`,
      done: true,
    };
    // Act
    const html = renderItem(item);
    // Assert
    const intro = html.indexOf("intro line");
    const widget = html.indexOf(`class="chess-game"`);
    const outro = html.indexOf("outro line");
    expect(intro).toBeGreaterThanOrEqual(0);
    expect(widget).toBeGreaterThan(intro);
    expect(outro).toBeGreaterThan(widget);
  });
});

describe("lastUserTurnId", () => {
  /** A user turn carrying the given request id. */
  const turn = (requestId: string): ConversationItem => ({
    kind: "user-turn",
    requestId,
    content: [{ type: "text", text: "hi" }],
    ts: new Date(2026, 4, 24, 10, 0).toISOString(),
  });
  /** An assistant text item: the noise a user turn is picked out from. */
  const text = (blockId: string): ConversationItem => ({
    kind: "text",
    ts: TEXT_TS,
    blockId,
    messageId: "m1",
    text: "answering",
    done: true,
  });

  it("returns the newest user turn's request id", () => {
    // Arrange + Act + Assert
    expect(lastUserTurnId([turn("r1"), text("b1"), turn("r2")])).toBe("r2");
  });

  it("returns the user turn's id across the items answering it", () => {
    // Arrange — a send stays the newest user turn under its own replies.
    expect(lastUserTurnId([turn("r1"), text("b1"), text("b2")])).toBe("r1");
  });

  it("returns null for a feed carrying no user turn", () => {
    // Arrange + Act + Assert
    expect(lastUserTurnId([text("b1")])).toBeNull();
  });

  it("returns null for an empty feed", () => {
    // Arrange + Act + Assert
    expect(lastUserTurnId([])).toBeNull();
  });
});

describe("repinsToTail", () => {
  it("jumps a scrolled-up feed to the tail when a prompt was just sent", () => {
    // Arrange + Act + Assert
    expect(repinsToTail({ prevTurnId: "r1", nextTurnId: "r2", pinned: false })).toBe(true);
  });

  it("jumps to the tail on the feed's very first prompt", () => {
    // Arrange + Act + Assert
    expect(repinsToTail({ prevTurnId: null, nextTurnId: "r1", pinned: false })).toBe(true);
  });

  it("leaves a scrolled-up feed alone while the same turn streams its answer", () => {
    // Arrange + Act + Assert
    expect(repinsToTail({ prevTurnId: "r1", nextTurnId: "r1", pinned: false })).toBe(false);
  });

  it("leaves a scrolled-up feed alone when no prompt was ever sent", () => {
    // Arrange + Act + Assert
    expect(repinsToTail({ prevTurnId: null, nextTurnId: null, pinned: false })).toBe(false);
  });

  it("keeps a pinned feed following its tail", () => {
    // Arrange + Act + Assert
    expect(repinsToTail({ prevTurnId: "r1", nextTurnId: "r1", pinned: true })).toBe(true);
  });
});

// --- activity panels ------------------------------------------------------------

/** An Agent card the panel tests spawn children under. */
function agentTool(id = "a1"): ToolItem {
  return {
    kind: "tool",
    toolUseId: id,
    messageId: "m1",
    toolName: "Agent",
    inputJson: "{}",
    input: { description: "scout the repo" },
    inputDone: true,
  };
}

/** A nested Bash call carrying its spawner. */
function childBash(id = "t2", parent = "a1"): ToolItem {
  return {
    kind: "tool",
    toolUseId: id,
    messageId: "m1",
    toolName: "Bash",
    parentToolUseId: parent,
    inputJson: "{}",
    input: { command: "ls -la" },
    inputDone: true,
  };
}

/** A pending permission prompt gating TOOLUSEID. */
function childPermission(toolUseId: string, resolved = false): ConversationItem {
  return {
    kind: "permission",
    requestId: "p1",
    toolUseId,
    toolName: "Bash",
    input: {},
    ...(resolved ? { resolution: { decision: "allow" as const } } : {}),
  };
}

/** A PanelContext holding CHILDREN under a1, open per OPEN. */
function panelCtx(children: ConversationItem[], open = false): PanelContext {
  return {
    children: new Map([["a1", children]]),
    isOpen: () => open,
  };
}

describe("activity panel", () => {
  it("renders no activity fold on a card without children", () => {
    // Arrange + Act
    const html = renderItem(agentTool(), undefined, undefined, false, panelCtx([]));
    // Assert
    expect(html).not.toContain("agent-activity");
  });

  it("shows the ticker face while closed and none of the child feed", () => {
    // Arrange + Act
    const html = renderItem(agentTool(), undefined, undefined, false, panelCtx([childBash()]));
    // Assert
    expect(html).toContain("agent-ticker");
    expect(html).toContain("1 step · Bash: ls -la");
    expect(html).not.toContain("agent-panel");
  });

  it("renders the child feed inside the open panel", () => {
    // Arrange + Act
    const html = renderItem(agentTool(), undefined, undefined, false, panelCtx([childBash()], true));
    // Assert
    expect(html).toContain("agent-panel");
    expect(html).toContain("tool-bash");
    expect(html).toContain("$ ls -la");
  });

  it("badges the card while a child permission waits", () => {
    // Arrange + Act
    const html = renderItem(
      agentTool(),
      undefined,
      undefined,
      false,
      panelCtx([childBash(), childPermission("t2")]),
    );
    // Assert — a closed card must never silently block the turn.
    expect(html).toContain("needs permission");
  });

  it("drops the badge once the child permission resolves", () => {
    // Arrange + Act
    const html = renderItem(
      agentTool(),
      undefined,
      undefined,
      false,
      panelCtx([childBash(), childPermission("t2", true)]),
    );
    // Assert
    expect(html).not.toContain("needs permission");
  });

  it("ignores suppressed children entirely", () => {
    // Arrange — ToolSearch renders as nothing everywhere.
    const suppressed: ToolItem = { ...childBash(), toolName: "ToolSearch" };
    // Act
    const html = renderItem(agentTool(), undefined, undefined, false, panelCtx([suppressed]));
    // Assert
    expect(html).not.toContain("agent-activity");
  });

  it("attaches the fold to a non-agent spawner just the same", () => {
    // Arrange — a Workflow's children confine like an Agent's.
    const wf: ToolItem = { ...agentTool(), toolName: "Workflow" };
    // Act
    const html = renderItem(wf, undefined, undefined, false, panelCtx([childBash()]));
    // Assert
    expect(html).toContain("agent-activity");
  });
});

describe("activityTicker", () => {
  it("counts a single child in the singular", () => {
    // Arrange + Act + Assert
    expect(activityTicker([childBash()])).toBe("1 step · Bash: ls -la");
  });

  it("headlines the most recent child with something to say", () => {
    // Arrange
    const older = childBash("t2");
    const newer: ToolItem = { ...childBash("t3"), input: { command: "make test" } };
    // Act + Assert
    expect(activityTicker([older, newer])).toBe("2 steps · Bash: make test");
  });

  it("falls back to the count alone when no child offers a line", () => {
    // Arrange — a finished thinking block has nothing live to say.
    const quiet: ConversationItem = {
      kind: "thinking",
      blockId: "k1",
      messageId: "m1",
      parentToolUseId: "a1",
      text: "hmm",
      done: true,
    };
    // Act + Assert
    expect(activityTicker([quiet])).toBe("1 step");
  });

  it("caps a runaway line so the ticker stays a line", () => {
    // Arrange
    const long: ToolItem = { ...childBash(), input: { command: "x".repeat(200) } };
    // Act
    const ticker = activityTicker([long]);
    // Assert
    expect(ticker.length).toBeLessThan(100);
    expect(ticker.endsWith("…")).toBe(true);
  });

  it("voices a waiting child permission", () => {
    // Arrange + Act + Assert
    expect(activityTicker([childPermission("t2")])).toBe("1 step · awaiting permission: Bash");
  });
});

// --- consecutive-run tab groups ---------------------------------------------------

/** A Bash card with its own id and command, for building runs. */
function bash(id: string, command = "ls", result?: { isError: boolean }): ToolItem {
  return {
    kind: "tool",
    toolUseId: id,
    messageId: "m1",
    toolName: "Bash",
    inputJson: "{}",
    input: { command },
    inputDone: true,
    ...(result ? { result: { isError: result.isError, content: "out" } } : {}),
  };
}

describe("groupFeed", () => {
  it("groups two consecutive same-tool cards", () => {
    // Arrange + Act
    const entries = groupFeed([bash("t1"), bash("t2")]);
    // Assert
    expect(entries).toHaveLength(1);
    expect(entries[0].kind).toBe("group");
  });

  it("leaves a singleton as a plain item", () => {
    // Arrange + Act
    const entries = groupFeed([bash("t1"), text("b1")]);
    // Assert
    expect(entries.map((e) => e.kind)).toEqual(["item", "item"]);
  });

  it("breaks a run on a different tool name", () => {
    // Arrange
    const read: ToolItem = { ...bash("t2"), toolName: "Read" };
    // Act
    const entries = groupFeed([bash("t1"), read, bash("t3")]);
    // Assert
    expect(entries.map((e) => e.kind)).toEqual(["item", "item", "item"]);
  });

  it("breaks a run on a visible non-tool item", () => {
    // Arrange + Act
    const entries = groupFeed([bash("t1"), text("b1"), bash("t2")]);
    // Assert
    expect(entries.map((e) => e.kind)).toEqual(["item", "item", "item"]);
  });

  it("lets a suppressed tool ride through a run without breaking it", () => {
    // Arrange — ToolSearch renders nothing, so it cannot split the tabs.
    const invisible: ToolItem = { ...bash("x1"), toolName: "ToolSearch" };
    // Act
    const entries = groupFeed([bash("t1"), invisible, bash("t2")]);
    // Assert — one group, with the invisible item re-emitted after it.
    expect(entries[0].kind).toBe("group");
    expect(entries).toHaveLength(2);
  });

  it("lets a meta-only user turn ride through a run without breaking it", () => {
    // Arrange — a turn that was nothing but injected spans renders "".
    const meta = userTurnAt(9, 0, `${META_OPEN}read the metaprompt${META_CLOSE}`);
    // Act
    const entries = groupFeed([bash("t1"), meta, bash("t2")]);
    // Assert
    expect(entries[0].kind).toBe("group");
  });

  it("lets a closed textless thinking block ride through a run without breaking it", () => {
    // Arrange — an adaptive-thinking model withholds the text, so the block
    // renders nothing yet sits between two same-tool calls.
    const empty = thinking("k1", true, "");
    // Act
    const entries = groupFeed([bash("t1"), empty, bash("t2")]);
    // Assert — one group, with the invisible thinking re-emitted after it.
    expect(entries[0].kind).toBe("group");
    expect(entries).toHaveLength(2);
  });

  it("still breaks a run on a closed thinking block that has text to show", () => {
    // Arrange — a visible thinking disclosure is a real bubble.
    const shown = thinking("k1", true, "reasoning");
    // Act
    const entries = groupFeed([bash("t1"), shown, bash("t2")]);
    // Assert
    expect(entries.map((e) => e.kind)).toEqual(["item", "item", "item"]);
  });

  it("still breaks a run on an OPEN textless thinking block, whose spinner shows", () => {
    // Arrange — while streaming, an empty thinking block draws a spinner.
    const streaming = thinking("k1", false, "");
    // Act
    const entries = groupFeed([bash("t1"), streaming, bash("t2")]);
    // Assert
    expect(entries.map((e) => e.kind)).toEqual(["item", "item", "item"]);
  });

  it("lets a system:init note ride through a run without breaking it", () => {
    // Arrange — session (re)init draws no bubble.
    const init: ConversationItem = { kind: "system", subtype: "init" };
    // Act
    const entries = groupFeed([bash("t1"), init, bash("t2")]);
    // Assert
    expect(entries[0].kind).toBe("group");
    expect(entries).toHaveLength(2);
  });

  it("groups consecutive Agent cards like any other run", () => {
    // Arrange
    const a1: ToolItem = { ...agentTool("a1") };
    const a2: ToolItem = { ...agentTool("a2") };
    // Act
    const entries = groupFeed([a1, a2]);
    // Assert
    expect(entries[0].kind).toBe("group");
  });

  it("keeps each member's original index for reconciliation", () => {
    // Arrange + Act
    const entries = groupFeed([text("b1"), bash("t1"), bash("t2")]);
    // Assert
    const group = entries[1];
    if (group.kind !== "group") throw new Error("expected a group");
    expect(group.indexes).toEqual([1, 2]);
  });
});

describe("activeGroupMember", () => {
  it("auto-follows the newest still-running member", () => {
    // Arrange
    const members = [bash("t1", "a", { isError: false }), bash("t2", "b"), bash("t3", "c", { isError: false })];
    // Act + Assert
    expect(activeGroupMember(members)).toBe("t2");
  });

  it("settles on the last member when every member finished", () => {
    // Arrange
    const members = [bash("t1", "a", { isError: false }), bash("t2", "b", { isError: false })];
    // Act + Assert
    expect(activeGroupMember(members)).toBe("t2");
  });

  it("honors the user's pin over the auto-follow", () => {
    // Arrange
    const members = [bash("t1", "a", { isError: false }), bash("t2", "b")];
    // Act + Assert
    expect(activeGroupMember(members, "t1")).toBe("t1");
  });

  it("ignores a stale pin naming a member the group no longer holds", () => {
    // Arrange
    const members = [bash("t1", "a", { isError: false }), bash("t2", "b")];
    // Act + Assert
    expect(activeGroupMember(members, "t-gone")).toBe("t2");
  });
});

/** A nested subagent tool item, carrying the card that spawned it. */
function nestedAgentTool(id: string, parent: string): ToolItem {
  return { ...agentTool(id), parentToolUseId: parent };
}

describe("planAgentReveal", () => {
  it("reveals a lone top-level subagent as its own bubble with its panel open", () => {
    // Arrange — a single Agent card, ungrouped.
    const items = [userTurnAt(9, 0), agentTool("a1")];
    // Act
    const plan = planAgentReveal(items, "a1");
    // Assert
    expect(plan).toEqual<AgentReveal>({
      key: "tool:a1",
      groupKey: null,
      tabMember: null,
      panelIds: ["a1"],
    });
  });

  it("pins the clicked member's tab when the subagent sits in a run group", () => {
    // Arrange — two consecutive Agent cards collapse into one tab group.
    const items = [userTurnAt(9, 0), agentTool("a1"), agentTool("a2")];
    // Act
    const plan = planAgentReveal(items, "a2");
    // Assert — the group is keyed by its first member; a2's tab must be pinned.
    expect(plan).toEqual<AgentReveal>({
      key: "group:a1",
      groupKey: "group:a1",
      tabMember: "a2",
      panelIds: ["a2"],
    });
  });

  it("scrolls to the outermost ancestor and opens the whole panel chain for a nested subagent", () => {
    // Arrange — a2 was spawned by top-level a1, so it has no bubble of its own.
    const items = [userTurnAt(9, 0), agentTool("a1"), nestedAgentTool("a2", "a1")];
    // Act
    const plan = planAgentReveal(items, "a2");
    // Assert — land on a1's bubble, opening a2's panel (its output) and a1's (holding a2).
    expect(plan).toEqual<AgentReveal>({
      key: "tool:a1",
      groupKey: null,
      tabMember: null,
      panelIds: ["a2", "a1"],
    });
  });

  it("returns null for an id no tool item carries", () => {
    // Arrange
    const items = [userTurnAt(9, 0), agentTool("a1")];
    // Act + Assert
    expect(planAgentReveal(items, "ghost")).toBeNull();
  });

  it("returns null for a subagent a /clear discarded from the feed", () => {
    // Arrange — the agent precedes the clear, so it is off the current context.
    const items = [
      userTurnAt(9, 0),
      agentTool("a1"),
      userTurnAt(9, 1, "/clear", "r2"),
    ];
    // Act + Assert
    expect(planAgentReveal(items, "a1")).toBeNull();
  });
});

describe("groupHtml", () => {
  it("renders one status-dotted chip per member", () => {
    // Arrange
    const members = [bash("t1", "a", { isError: false }), bash("t2", "b")];
    // Act
    const html = groupHtml(members, "t2");
    // Assert
    expect(html.match(/tab-chip/g)?.length).toBe(2);
    expect(html).toContain("agent-done");
    expect(html).toContain("agent-running");
  });

  it("renders only the active member's card beneath the tabs", () => {
    // Arrange
    const members = [bash("t1", "first-cmd", { isError: false }), bash("t2", "second-cmd")];
    // Act
    const html = groupHtml(members, "t2");
    // Assert
    expect(html).toContain("$ second-cmd");
    expect(html).not.toContain("$ first-cmd");
  });

  it("keeps failures loud whichever tab is selected", () => {
    // Arrange — the failed member is NOT the active one.
    const members = [bash("t1", "a", { isError: true }), bash("t2", "b", { isError: false })];
    // Act
    const html = groupHtml(members, "t2");
    // Assert
    expect(html).toContain("1 failed");
  });

  it("marks the selected chip active and wires both data attributes", () => {
    // Arrange
    const members = [bash("t1", "a"), bash("t2", "b")];
    // Act
    const html = groupHtml(members, "t1");
    // Assert
    expect(html).toContain(`class="tab-chip active" data-tab-group="t1" data-tab-member="t1"`);
    expect(html).toContain(`data-tab-member="t2"`);
  });

  it("renders the tab bar INSIDE the card, after the card opens", () => {
    // Arrange
    const members = [bash("t1", "a"), bash("t2", "b")];
    // Act
    const html = groupHtml(members, "t2");
    // Assert — the card opens first, then the tab bar sits within it.
    expect(html.indexOf(`class="tool-card`)).toBeLessThan(html.indexOf(`class="tab-bar"`));
  });

  it("does not render the tab bar as a sibling above the card", () => {
    // Arrange
    const members = [bash("t1", "a"), bash("t2", "b")];
    // Act
    const html = groupHtml(members, "t2");
    // Assert — the feed-group's first child is the card, not the tab bar.
    expect(html).not.toMatch(/<div class="feed-group">\s*<div class="tab-bar">/);
    expect(html).toMatch(/<div class="feed-group">\s*<div class="tool-card/);
  });

  it("places the tab bar above the tool head within the card", () => {
    // Arrange
    const members = [bash("t1", "a"), bash("t2", "b")];
    // Act
    const html = groupHtml(members, "t2");
    // Assert — inside the card the chip row precedes the tool head row.
    expect(html.indexOf(`class="tab-bar"`)).toBeLessThan(html.indexOf(`class="tool-head"`));
  });
});

describe("generic folded input", () => {
  /** A generic tool card with the given input. */
  function generic(name: string, input: Record<string, unknown>): ToolItem {
    return {
      kind: "tool",
      toolUseId: "t1",
      messageId: "m1",
      toolName: name,
      inputJson: JSON.stringify(input),
      input,
      inputDone: true,
    };
  }

  it("headlines a WebFetch on its url with the JSON folded away", () => {
    // Arrange + Act
    const html = renderItem(generic("WebFetch", { url: "https://x.test/a" }));
    // Assert
    expect(html).toContain("folded-headline");
    expect(html).toContain("https://x.test/a");
    expect(html).toContain("folded-json");
  });

  it("headlines a TaskCreate on its subject", () => {
    // Arrange + Act
    const html = renderItem(generic("TaskCreate", { subject: "Fix the build" }));
    // Assert
    expect(html).toContain("Fix the build");
    expect(html).toContain("folded-headline");
  });

  it("headlines a TaskOutput on the task id it polls", () => {
    // Arrange + Act
    const html = renderItem(generic("TaskOutput", { task_id: "bg1" }));
    // Assert
    expect(html).toContain("bg1");
    expect(html).toContain("folded-headline");
  });

  it("keeps the raw capped JSON for an input offering no headline", () => {
    // Arrange — TodoWrite's input is a list, nothing to headline.
    const html = renderItem(generic("TodoWrite", { todos: [] }));
    // Assert
    expect(html).toContain(`<pre class="tool-input">`);
    expect(html).not.toContain("folded-headline");
  });
});

describe("scheduled-wakeup anchor", () => {
  /** A settled ScheduleWakeup call with INPUT and a result at RESULTTS. */
  function wake(input: Record<string, unknown>, resultTs?: string): ToolItem {
    return {
      kind: "tool",
      toolUseId: "t1",
      messageId: "m1",
      toolName: "ScheduleWakeup",
      inputJson: JSON.stringify(input),
      input,
      inputDone: true,
      resultTs,
      result: { isError: false, content: "scheduled" },
    };
  }

  it("names the wall-clock moment the wakeup fires", () => {
    // Arrange — scheduled at 10:00 local with a 10-minute delay.
    const at = new Date(2026, 4, 24, 10, 0, 0);
    const item = wake({ delaySeconds: 600, reason: "watching CI" }, at.toISOString());
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("wakes ~10:10");
    expect(html).toContain("(firing…)");
    expect(html).toContain("watching CI");
  });

  it("acknowledges a stop instead of computing a moment", () => {
    // Arrange + Act
    const html = renderItem(wake({ stop: true }));
    // Assert
    expect(html).toContain("loop stopped");
  });

  it("echoes the raw ack when the input offers no anchor", () => {
    // Arrange + Act — no delaySeconds to compute from.
    const html = renderItem(wake({ prompt: "loop" }));
    // Assert
    expect(html).toContain("scheduled");
    expect(html).not.toContain("wakes ~");
  });

  it("keeps a failed schedule loud", () => {
    // Arrange
    const item = { ...wake({ delaySeconds: 60 }), result: { isError: true, content: "bad" } };
    // Act
    const html = renderItem(item as ToolItem);
    // Assert
    expect(html).toContain("stderr");
  });
});

describe("task stop control", () => {
  /** A Bash spawn whose result announced a background task. */
  function bgSpawn(notified = false): ToolItem {
    return {
      kind: "tool",
      toolUseId: "t1",
      messageId: "m1",
      toolName: "Bash",
      inputJson: "{}",
      input: { command: "make", run_in_background: true },
      inputDone: true,
      result: { isError: false, content: "Command running in background with ID: bg7." },
      ...(notified
        ? { notification: { taskId: "bg7", status: "completed", text: "<task-notification/>" } }
        : {}),
    };
  }

  it("offers a stop button while the spawned task still runs", () => {
    // Arrange + Act
    const html = renderItem(bgSpawn());
    // Assert — the label owns being prompt-mediated.
    expect(html).toContain("task-stop");
    expect(html).toContain("stop bg7 · asks the agent");
    expect(html).toContain(`data-send-prompt="Stop the background task bg7`);
  });

  it("withdraws the button once the completion notification lands", () => {
    // Arrange + Act
    const html = renderItem(bgSpawn(true));
    // Assert
    expect(html).not.toContain("task-stop");
  });

  it("offers no control on a card that spawned nothing", () => {
    // Arrange + Act
    const html = renderItem(tool());
    // Assert
    expect(html).not.toContain("task-controls");
  });
});

describe("live task output", () => {
  it("streams the daemon's file tail into a capped output box", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "t1",
      messageId: "m1",
      toolName: "Bash",
      inputJson: "{}",
      input: { command: "make" },
      inputDone: true,
      taskOutput: "compiling…\nlinking…\n",
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("task-live-output");
    expect(html).toContain("linking…");
  });

  it("renders no tail box before any output streams", () => {
    // Arrange + Act
    const html = renderItem(tool());
    // Assert
    expect(html).not.toContain("task-live-output");
  });
});

describe("wakeRemainingLabel", () => {
  it("counts whole minutes while the moment is far off", () => {
    // Arrange + Act + Assert
    expect(wakeRemainingLabel(600_000, 0)).toBe("in 10m");
  });

  it("switches to seconds inside the last minute and a half", () => {
    // Arrange + Act + Assert
    expect(wakeRemainingLabel(45_000, 0)).toBe("in 45s");
  });

  it("reads firing once the moment passes", () => {
    // Arrange + Act + Assert
    expect(wakeRemainingLabel(1000, 5000)).toBe("firing…");
  });
});

describe("agent message composer", () => {
  /** An Agent spawn whose result announced a background agent id. */
  function bgAgent(notified = false): ToolItem {
    return {
      kind: "tool",
      toolUseId: "t1",
      messageId: "m1",
      toolName: "Agent",
      inputJson: "{}",
      input: { description: "scout" },
      inputDone: true,
      result: {
        isError: false,
        content: "Async agent launched successfully. agentId: abc9 (internal ID)",
      },
      ...(notified
        ? { notification: { taskId: "abc9", status: "completed", text: "<task-notification/>" } }
        : {}),
    };
  }

  it("offers a freeform composer while the background agent runs", () => {
    // Arrange + Act
    const html = renderItem(bgAgent());
    // Assert — the send owns being prompt-mediated.
    expect(html).toContain(`data-msg-for="abc9"`);
    expect(html).toContain(`data-msg-send="abc9"`);
    expect(html).toContain("send · asks the agent");
  });

  it("withdraws the composer once the completion notification lands", () => {
    // Arrange + Act
    const html = renderItem(bgAgent(true));
    // Assert
    expect(html).not.toContain("agent-msg");
  });

  it("offers no composer on a card that spawned no agent", () => {
    // Arrange + Act
    const html = renderItem(tool());
    // Assert
    expect(html).not.toContain("agent-msg");
  });

  it("re-renders a half-typed draft back into the input", () => {
    // Arrange — the draft lives in renderer state (PanelContext.drafts).
    const panels: PanelContext = {
      children: new Map(),
      isOpen: () => false,
      drafts: new Map([["abc9", "status pls"]]),
    };
    // Act
    const html = renderItem(bgAgent(), undefined, undefined, false, panels);
    // Assert
    expect(html).toContain(`value="status pls"`);
  });
});

// --- watcher fold (in a final-response bubble) ---------------------------------

/** A backgrounded-Bash watcher announcing TASKID, plus any overrides. */
function watcher(taskId = "bg1", over: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
    toolUseId: "w1",
    messageId: "m1",
    toolName: "Bash",
    inputJson: "{}",
    input: { command: "poll.sh" },
    inputDone: true,
    result: { isError: false, content: `Command running in background with ID: ${taskId}.` },
    ...over,
  };
}

/** A PanelContext folding WATCHERS into block b1, open per OPEN. */
function watcherPanels(watchers: ToolItem[], open = false): PanelContext {
  return {
    children: new Map(),
    isOpen: () => open,
    watchers: new Map([["b1", watchers]]),
  };
}

describe("watcher fold", () => {
  it("renders no fold on a final bubble whose turn armed no watcher", () => {
    // Arrange + Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), false, watcherPanels([]));
    // Assert
    expect(html).not.toContain("watcher-fold");
  });

  it("shows a live ticker face and none of the row while closed", () => {
    // Arrange + Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), false, watcherPanels([watcher()]));
    // Assert
    expect(html).toContain("watcher-fold");
    expect(html).toContain("1 watcher · 1 live");
    expect(html).toContain(`<span class="tool-spinner" aria-hidden="true"></span>`);
    expect(html).not.toContain("watcher-row");
  });

  it("reveals the watcher row and its live tail when open", () => {
    // Arrange
    const w = watcher("bg1", { taskOutput: "polling the queue…" });
    // Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), false, watcherPanels([w], true));
    // Assert
    expect(html).toContain("watcher-row");
    expect(html).toContain("task-live-output");
    expect(html).toContain("polling the queue…");
  });

  it("drops the arc for a done face once the notification lands", () => {
    // Arrange — the completion notification settles the watcher.
    const w = watcher("bg1", { notification: { taskId: "bg1", text: "<task-notification/>" } });
    // Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), false, watcherPanels([w]));
    // Assert
    expect(html).toContain("1 watcher · done");
    expect(html).not.toContain("tool-spinner");
  });

  it("never folds watchers into a non-final commentary bubble", () => {
    // Arrange — no finals, so the bubble carries no closing chip.
    const html = renderItem(text("b1"), undefined, undefined, false, watcherPanels([watcher()]));
    // Assert
    expect(html).not.toContain("watcher-fold");
  });

  it("renders a watcher's polled tail when the store streamed none", () => {
    // Arrange — a background agent has no store taskOutput, only the poll.
    const panels: PanelContext = {
      children: new Map(),
      isOpen: () => true,
      watchers: new Map([["b1", [watcher("bg1")]]]),
      taskTail: (id) =>
        id === "bg1" ? { text: "polled line", offset: 11, done: false, elapsedMs: 3000 } : undefined,
    };
    // Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), false, panels);
    // Assert
    expect(html).toContain("polled line");
    expect(html).toContain("watcher-elapsed");
  });

  it("prefers the store-streamed tail over the polled one", () => {
    // Arrange — a backgrounded Bash streams over the WS, so the store wins.
    const panels: PanelContext = {
      children: new Map(),
      isOpen: () => true,
      watchers: new Map([["b1", [watcher("bg1", { taskOutput: "streamed line" })]]]),
      taskTail: () => ({ text: "polled line", offset: 11, done: false, elapsedMs: 0 }),
    };
    // Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), false, panels);
    // Assert
    expect(html).toContain("streamed line");
    expect(html).not.toContain("polled line");
  });
});

describe("openWatcherTaskIds", () => {
  it("collects task ids only from open folds", () => {
    // Arrange
    const watchers = new Map<string, ToolItem[]>([
      ["b1", [watcher("bg1")]],
      ["b2", [watcher("bg2")]],
    ]);
    // Act — only b1's fold is open.
    const ids = openWatcherTaskIds(watchers, (id) => id === "watchers:b1");
    // Assert
    expect([...ids]).toEqual(["bg1"]);
  });

  it("is empty when no fold is open", () => {
    // Arrange
    const watchers = new Map<string, ToolItem[]>([["b1", [watcher("bg1")]]]);
    // Act
    const ids = openWatcherTaskIds(watchers, () => false);
    // Assert
    expect(ids.size).toBe(0);
  });
});
