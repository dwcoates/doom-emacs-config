// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  Actions,
  FeedRenderer,
  PanelContext,
  ToolReveal,
  activeGroupMember,
  activityTicker,
  anyLiveAsync,
  anyLiveThinking,
  backfillChunks,
  compactionBannerHtml,
  diffHtml,
  finalResponses,
  formatBubbleTime,
  formatTurnTime,
  groupFeed,
  groupHtml,
  itemKey,
  lastUserTurnId,
  MERGE_CARD_BODY,
  memberBadge,
  modelOptionsHtml,
  panelSeedsOnOpen,
  panelToggleTarget,
  planToolReveal,
  renderItem,
  rendersEmpty,
  repinsToTail,
  showsMonitoringRow,
  wakeRemainingLabel,
  navTokensForEntry,
} from "../src/render.js";
import { ForwardingLogger, resetLoggingForTests, setLogger } from "../src/wslog.js";
import { META_CLOSE, META_OPEN } from "../src/meta.js";
import { AsyncSource } from "../src/protocol.js";
import {
  ContextClearedItem,
  ContextCompactedItem,
  ConversationItem,
  ConversationStore,
  ResultItem,
  StoreState,
  SystemFailureCard,
  TextItem,
  ThinkingItem,
  ToolItem,
} from "../src/store.js";

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

/** A merge-failure remediation user-turn: origin "merge", carrying the hidden directive. */
function mergeTurn(directive = "SECRET rebase directive", requestId = "m1"): ConversationItem {
  return {
    kind: "user-turn",
    requestId,
    content: [{ type: "text", text: directive }],
    ts: new Date(2026, 4, 24, 9, 0).toISOString(),
    origin: "merge",
  };
}

describe("MergeCard (merge-origin user-turn renders a status card, not a prompt)", () => {
  it("renders the Merge tool-card variant for a merge-origin turn", () => {
    // Arrange / Act
    const html = renderItem(mergeTurn());
    // Assert
    expect(html).toContain("tool-card tool-merge");
    expect(html).toContain(`<span class="tool-name">Merge</span>`);
  });

  it("shows the fixed status body", () => {
    // Arrange / Act
    const html = renderItem(mergeTurn());
    // Assert
    expect(html).toContain(MERGE_CARD_BODY);
  });

  it("never leaks the injected directive text into the feed", () => {
    // Arrange — the directive drives the agent under the hood, never shown.
    const html = renderItem(mergeTurn("git rebase master and fix conflicts"));
    // Assert
    expect(html).not.toContain("git rebase master and fix conflicts");
  });

  it("does not render a user-prompt bubble for a merge-origin turn", () => {
    // Arrange / Act
    const html = renderItem(mergeTurn());
    // Assert
    expect(html).not.toContain("bubble user");
  });

  it("still renders a normal user-prompt bubble when origin is absent", () => {
    // Arrange — a real user prompt is untouched by the merge path.
    const html = renderItem(userTurnAt(9, 0, "do the thing"));
    // Assert
    expect(html).toContain("bubble user");
    expect(html).not.toContain("tool-merge");
  });
});

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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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





describe("showsMonitoringRow (global monitoring-row precedence)", () => {
  it("shows on an idle session with live async somewhere", () => {
    // Arrange / Act — the quiescent-but-still-watching case.
    const show = showsMonitoringRow({ turnInFlight: false, interrupting: false, thinking: false, anyLiveAsync: true });
    // Assert
    expect(show).toBe(true);
  });

  it("hides while a turn is in flight, ceding the tail to the working/thinking rows", () => {
    // Arrange / Act — the main chain is active, so a bucket-1 row speaks instead.
    const show = showsMonitoringRow({ turnInFlight: true, interrupting: false, thinking: false, anyLiveAsync: true });
    // Assert
    expect(show).toBe(false);
  });

  it("hides while interrupting, which the alarm-red row owns", () => {
    // Arrange / Act
    const show = showsMonitoringRow({ turnInFlight: false, interrupting: true, thinking: false, anyLiveAsync: true });
    // Assert
    expect(show).toBe(false);
  });

  it("hides while a thinking indicator is present, ceding the shared tail slot to it", () => {
    // Arrange / Act — main chain idle, but a subagent's `thinking…` spinner is live.
    const show = showsMonitoringRow({ turnInFlight: false, interrupting: false, thinking: true, anyLiveAsync: true });
    // Assert
    expect(show).toBe(false);
  });

  it("hides on an idle session with no live async at all", () => {
    // Arrange / Act — nothing left to monitor.
    const show = showsMonitoringRow({ turnInFlight: false, interrupting: false, thinking: false, anyLiveAsync: false });
    // Assert
    expect(show).toBe(false);
  });
});


describe("anyLiveAsync (feed-wide liveness for the global row)", () => {
  const noChildren: PanelContext = { children: new Map(), isOpen: () => false };

  it("is true when a watcher item is still live", () => {
    // Arrange / Act — a backgrounded Bash with no notification and no folded stop.
    const live = anyLiveAsync([watcher("bg1")], noChildren);
    // Assert
    expect(live).toBe(true);
  });

  it("is false when every watcher has settled", () => {
    // Arrange — the completion notification settles it.
    const settled = watcher("bg1", { notification: { taskId: "bg1", text: "<task-notification/>" } });
    // Act
    const live = anyLiveAsync([settled], noChildren);
    // Assert
    expect(live).toBe(false);
  });

  it("is false for a feed carrying no async members at all", () => {
    // Arrange / Act — a plain text bubble owns no background work.
    const live = anyLiveAsync([text("b1")], noChildren);
    // Assert
    expect(live).toBe(false);
  });
});

describe("anyLiveThinking (thinking-indicator liveness for the monitoring gate)", () => {
  it("is true when a thinking block is still live (not done)", () => {
    // Arrange / Act — an open block still spins its `thinking…` indicator.
    const live = anyLiveThinking([thinking("t1", false)]);
    // Assert
    expect(live).toBe(true);
  });

  it("is false when the only thinking block has finished", () => {
    // Arrange / Act — a done block renders no live spinner.
    const live = anyLiveThinking([thinking("t1", true)]);
    // Assert
    expect(live).toBe(false);
  });

  it("is true for a live parented (subagent) thinking block", () => {
    // Arrange / Act — the overlap case: main chain idle, a subagent mid-thought.
    const live = anyLiveThinking([subagentThinking("t1", false)]);
    // Assert
    expect(live).toBe(true);
  });

  it("is false for a feed carrying no thinking blocks at all", () => {
    // Arrange / Act — a plain text bubble spins nothing.
    const live = anyLiveThinking([text("b1")]);
    // Assert
    expect(live).toBe(false);
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

/** A compaction item, defaulted to a successful automatic one. */
function compacted(over: Partial<ContextCompactedItem> = {}): ContextCompactedItem {
  return {
    kind: "context-compacted",
    uuid: "k1",
    trigger: "auto",
    preTokens: 287028,
    postTokens: 9001,
    durationMs: 4200,
    summary: "the story so far",
    result: "success",
    error: "",
    ...over,
  };
}

describe("context clear", () => {
  const cleared: ContextClearedItem = { kind: "context-cleared", uuid: "c1" };

  it("draws the red rule as a separator", () => {
    // Arrange + Act
    const html = renderItem(cleared);
    // Assert — the bar is a separator element the stylesheet paints red.
    expect(html).toContain(`<div class="clear-divider" role="separator"`);
  });

  it("labels the rule for a screen reader", () => {
    // Arrange + Act
    const html = renderItem(cleared);
    // Assert
    expect(html).toContain(`aria-label="context cleared"`);
  });

  it("draws the rule and NOTHING else, the event carrying no other fact", () => {
    // Arrange + Act
    const html = renderItem(cleared);
    // Assert
    expect(html).toBe(
      `<div class="clear-divider" role="separator" aria-label="context cleared"></div>`,
    );
  });

  it("draws no bubble, since a clear carries no summary", () => {
    // Arrange + Act
    const html = renderItem(cleared);
    // Assert
    expect(html).not.toContain("bubble");
  });
});

describe("context compaction", () => {
  it("draws the orange rule as a separator", () => {
    // Arrange + Act
    const html = renderItem(compacted());
    // Assert — the bar is a separator element the stylesheet paints orange.
    expect(html).toContain(`<div class="compact-rule" role="separator"`);
  });

  it("labels the rule for a screen reader", () => {
    // Arrange + Act
    const html = renderItem(compacted());
    // Assert
    expect(html).toContain(`aria-label="context compacted"`);
  });

  it("seats the orange rule above the context-compacted stamp", () => {
    // Arrange + Act — the rule must precede the label so it opens the feed.
    const html = renderItem(compacted());
    // Assert
    expect(html.indexOf("compact-rule")).toBeLessThan(html.indexOf("compact-divider"));
  });

  it("stamps the compaction with the tokens it traded", () => {
    // Arrange + Act
    const html = renderItem(compacted());
    // Assert
    expect(html).toContain("context compacted (287,028 → 9,001 tokens)");
  });

  it("renders the summary as a response bubble", () => {
    // Arrange + Act
    const html = renderItem(compacted());
    // Assert — the class the stylesheet paints purple-on-green.
    expect(html).toContain(`class="bubble assistant md compact-summary"`);
  });

  it("carries the summary text into that bubble", () => {
    // Arrange + Act
    const html = renderItem(compacted({ summary: "we fixed the flake" }));
    // Assert
    expect(html).toContain("we fixed the flake");
  });

  it("renders the summary as markdown, since the vendor writes prose", () => {
    // Arrange + Act
    const html = renderItem(compacted({ summary: "# heading" }));
    // Assert
    expect(html).toContain("<h1");
  });

  it("seats the summary bubble BELOW the rule it explains", () => {
    // Arrange + Act
    const html = renderItem(compacted());
    // Assert
    expect(html.indexOf("compact-rule")).toBeLessThan(html.indexOf("compact-summary"));
  });

  it("draws no bubble at all for an EMPTY summary", () => {
    // Arrange + Act — an empty bubble would claim an account exists.
    const html = renderItem(compacted({ summary: "" }));
    // Assert
    expect(html).not.toContain("compact-summary");
  });

  it("still draws the rule for an empty summary", () => {
    // Arrange + Act
    const html = renderItem(compacted({ summary: "" }));
    // Assert
    expect(html).toContain("compact-rule");
  });

  it("renders an automatic trigger and a manual one IDENTICALLY", () => {
    // Arrange + Act — nothing in the feed branches on who asked.
    const auto = renderItem(compacted({ trigger: "auto" }));
    const manual = renderItem(compacted({ trigger: "manual" }));
    // Assert
    expect(manual).toBe(auto);
  });

  it("names neither trigger in the output", () => {
    // Arrange + Act
    const html = renderItem(compacted({ trigger: "manual" }));
    // Assert
    expect(html).not.toContain("manual");
  });

  it("says so when the compaction FAILED", () => {
    // Arrange + Act
    const html = renderItem(compacted({ result: "failed", error: "context window exceeded" }));
    // Assert — the daemon's report is drawn, never swallowed.
    expect(html).toContain("compaction failed: context window exceeded");
  });

  it("says a failure happened even when the daemon gave no reason", () => {
    // Arrange + Act
    const html = renderItem(compacted({ result: "failed", error: "" }));
    // Assert
    expect(html).toContain("compaction failed");
  });

  it("escapes a failure reason rather than injecting it as markup", () => {
    // Arrange + Act
    const html = renderItem(compacted({ result: "failed", error: "<img onerror=x>" }));
    // Assert
    expect(html).toContain("&lt;img onerror=x&gt;");
  });

  it("draws no failure line for a successful compaction", () => {
    // Arrange + Act
    const html = renderItem(compacted());
    // Assert
    expect(html).not.toContain("compact-error");
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
    // Assert — the corner (with its stamp) trails the prompt's body column.
    expect(html).toContain(
      `<div class="bubble user"><div class="bubble-body"><pre>do the thing</pre></div><span class="turn-meta"><span class="turn-ts">`,
    );
  });

  it("renders a fenced code block in the prompt bubble as a highlighted card", () => {
    // Arrange — a prompt carrying a language-tagged markdown code block.
    const item = userTurnAt(14, 32, "fix this:\n```python\ndef foo():\n    pass\n```");
    // Act
    const html = renderItem(item);
    // Assert — the code renders as the md-code card, not literal backticks.
    expect(html).toContain(`<pre class="md-code"><code class="hljs lang-python">`);
    expect(html).toContain(`<span class="hljs-keyword">def</span>`);
    expect(html).not.toContain("```");
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
    // Assert — the corner is the body column's sibling, so it never sits in the prose.
    expect(html).toMatch(
      /<\/div><span class="turn-meta"><span class="turn-ts">[^<]+ ago<\/span><\/span><\/div>/,
    );
  });

  it("keeps a streaming response's cursor inside the body column", () => {
    // Arrange
    const item: ConversationItem = { ...(textAt(14, 33, "hel") as TextItem), done: false };
    // Act
    const html = renderItem(item);
    // Assert — the cursor trails the text, not the corner.
    expect(html).toContain(`<span class="cursor">▍</span></div><span class="turn-meta">`);
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

  it("never emits the retired error-response class on a text bubble", () => {
    // Arrange — an ordinary completed answer: API-level failures now arrive
    // as their own SystemFailureItem card, so no text bubble ever wears the
    // old red error-response border.
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "You've hit your session limit",
      done: true,
    };
    // Act
    const html = renderItem(item, undefined, finalsClosing(item));
    // Assert
    expect(html).not.toContain("error-response");
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
    // Assert — the corner (turn-meta with the turn's duration) rides in the bubble.
    expect(html).toMatch(
      /<div class="bubble assistant md final-response">[\s\S]*<span class="turn-meta"><span class="turn-stats"><span class="turn-dur">/,
    );
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
    // Assert — the corner trails the prose it closes rather than heading it.
    expect(html.indexOf(`class="turn-meta"`)).toBeGreaterThan(html.indexOf("the answer"));
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
    expect(html).toMatch(
      /<div class="bubble assistant md final-response">[\s\S]*<span class="turn-meta"><span class="turn-stats"><span class="turn-dur">/,
    );
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

  it("bakes no breath into a working-frontier text's HTML", () => {
    // Arrange — the breath is a live-view accent the reconcile toggles as a
    // class after mounting (see applyPulse), never part of an item's HTML, so
    // a moving pulse never rewrites the bubble and restarts its animation.
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "on it",
      done: true,
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).not.toContain("pulsing");
  });

  it("bakes no breath into a frontier rendered as a metaprompt tree", () => {
    // Arrange — the tree path builds its own bubble, which must stay just as
    // pulse-free as the markdown path so both breathe by class alone.
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "Response (👀 no changes made)\n\n1 👀 Answer\n├── 1.1 First\n└── 1.2 Second",
      done: true,
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).not.toContain("pulsing");
  });

  it("bakes no breath into a tool card's HTML", () => {
    // Arrange — a running tool leans on its own run badge, never a breath.
    const item = tool();
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).not.toContain("pulsing");
  });

  it("bakes no breath into a prompt bubble's HTML", () => {
    // Arrange — the prompt breath is retired: a just-sent prompt is covered by
    // the progress footer now, so UserTurn never
    // breathes, the same way a running tool card never does.
    const item = userTurnAt(9, 0);
    // Act
    const html = renderItem(item);
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

  it("spins a live arc in a streaming texted thinking summary", () => {
    // Arrange — a summarized thinking block still streaming: the arc beside the
    // `(thinking…)` label marks it live, not a finished card left open.
    const item: ConversationItem = {
      kind: "thinking",
      blockId: "b1",
      messageId: "m1",
      text: "step one",
      done: false,
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(`<span class="thinking-spinner" aria-hidden="true">`);
  });

  it("trails the streaming texted summary's (thinking) label with the animated ellipsis", () => {
    // Arrange — a summarized thinking block still streaming.
    const item: ConversationItem = {
      kind: "thinking",
      blockId: "b1",
      messageId: "m1",
      text: "step one",
      done: false,
    };
    // Act
    const html = renderItem(item);
    // Assert — the animated span replaced the literal "…" inside "(thinking…)".
    expect(html).toContain("animated-ellipsis");
    expect(html).not.toContain("(thinking…)");
  });

  it("drops the arc from a texted thinking block once it is done", () => {
    // Arrange — a settled disclosure carries no motion.
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
    expect(html).not.toContain("thinking-spinner");
  });

  it("draws no inline card while a textless thinking block streams, ceding to the tail slot", () => {
    // Arrange — adaptive thinking: signature only, no thinking text. Its
    // `thinking…` beat lives in the bottom-pinned tail slot now (see
    // the progress footer), not as a card just under the feed.
    const item: ConversationItem = {
      kind: "thinking",
      blockId: "b1",
      messageId: "m1",
      text: "",
      done: false,
    };
    // Act + Assert
    expect(renderItem(item)).toBe("");
  });

  it("keeps a live textless thinking block from opening a disclosure card", () => {
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
    // Assert — no inline spinner, no <details>: the tail slot speaks for it.
    expect(html).not.toContain("thinking-spinner");
    expect(html).not.toContain("<details");
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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

  it("shows the command a pending permission would run when no preview was sent", () => {
    // Arrange — a pushed core.v1.PermissionItem carries no preview, only input.
    const item: ConversationItem = {
      kind: "permission",
      requestId: "p1",
      toolUseId: "t1",
      toolName: "Bash",
      input: { command: "rm -rf /tmp/x" },
    };
    // Act
    const html = renderItem(item);
    // Assert — the user can see what they are approving.
    expect(html).toContain("rm -rf /tmp/x");
  });

  it("states the deny reason the daemon sent on a refused permission", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "permission",
      requestId: "p1",
      toolUseId: "t1",
      toolName: "Bash",
      input: {},
      resolution: { decision: "deny", message: "not in this workspace" },
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("denied — not in this workspace");
  });

  it("labels a refusal that carried no reason", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "permission",
      requestId: "p1",
      toolUseId: "t1",
      toolName: "Bash",
      input: {},
      resolution: { decision: "deny" },
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(">denied<");
  });

  it("escapes a deny reason rather than letting it inject markup", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "permission",
      requestId: "p1",
      toolUseId: "t1",
      toolName: "Bash",
      input: {},
      resolution: { decision: "deny", message: "<img src=x>" },
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).not.toContain("<img src=x>");
  });

  it("labels an allowed permission", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "permission",
      requestId: "p1",
      toolUseId: "t1",
      toolName: "Bash",
      input: {},
      resolution: { decision: "allow" },
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(">allowed<");
  });

  it("suppresses the tool card for AskUserQuestion", () => {
    // Arrange — the picker card is the question's UI; the tool card
    // would only dump the raw questions JSON next to it.
    const item: ToolItem = {
      kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    expect(html).toContain("read the log");
  });

  it("renders the SKILL.md body as formatted markdown, not escaped plain text", () => {
    // Arrange — a skill IS a markdown file, so its heading and bold text
    // should render as HTML rather than surviving as literal `#`/`**`.
    const item: ToolItem = {
      kind: "tool",
      ts: "2026-05-24T10:00:00.000Z",
      toolUseId: "t1",
      toolName: "Skill",
      messageId: "m1",
      input: { skill: "debug-logs" },
      inputJson: `{"skill":"debug-logs"}`,
      inputDone: true,
      result: {
        isError: false,
        content: "Launching skill: debug-logs",
        render: { kind: "skill", content: "# Debug Logs\n**read** the log" },
      },
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("<h1>Debug Logs</h1>");
    expect(html).toContain("<strong>read</strong>");
    expect(html).not.toContain("# Debug Logs");
  });

  it("wraps the rendered SKILL.md body in the skill-content-md class", () => {
    // Arrange — the class the phantom-top-margin reset in styles.css hangs on.
    const item: ToolItem = {
      kind: "tool",
      ts: "2026-05-24T10:00:00.000Z",
      toolUseId: "t1",
      toolName: "Skill",
      messageId: "m1",
      input: { skill: "debug-logs" },
      inputJson: `{"skill":"debug-logs"}`,
      inputDone: true,
      result: {
        isError: false,
        content: "Launching skill: debug-logs",
        render: { kind: "skill", content: "read the log" },
      },
    };
    // Act + Assert
    expect(renderItem(item)).toContain(
      `class="tool-output skill-content skill-content-md"`,
    );
  });

  it("tags the Skill card with the class its turquoise wash hangs on", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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

  it("marks the resolved Q&A card with the question class the feed centers on", () => {
    // Arrange — an answered AskUserQuestion, so the CSS centering selector matches.
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
            options: [{ label: "date-fns", description: "small" }],
          },
        ],
      },
      resolution: { decision: "allow" },
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("permission resolved question");
  });

  it("marks the pending Q&A card with the question class the feed centers on", () => {
    // Arrange — an unresolved AskUserQuestion, so the CSS centering selector matches.
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
            options: [{ label: "date-fns", description: "small" }],
          },
        ],
      },
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("permission pending question");
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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

describe("the chrome a clear leaves behind", () => {
  it("draws no rule beneath an ordinary prompt, the rule being its own item now", () => {
    // Arrange — the prompt text is no longer what draws the boundary.
    const item = userTurnAt(9, 0, "/clear");
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

  it("withholds the standing context total from the chip, now that it stands in the topbar", () => {
    // Arrange + Act
    const html = renderItem(resultItem("success"));
    // Assert — only the delta rides the chip; the standing figure moved to the header.
    expect(html).not.toContain("300,000 in");
  });

  it("signs a context increase with a plus after the duration", () => {
    // Arrange + Act — the chip carries the turn's delta, not the standing total.
    const html = renderItem(resultItem("success"));
    // Assert
    expect(html).toContain("12ms · +100,000");
  });

  it("signs a context decrease with a minus", () => {
    // Arrange — the first turn after a /compact stands below the last one.
    const item = { ...resultItem("success"), context: { total: 60_000, delta: -140_000 } };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("12ms · -140,000");
  });

  it("renders a zero increase as a signed zero", () => {
    // Arrange
    const item = { ...resultItem("success"), context: { total: 300_000, delta: 0 } };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("12ms · +0");
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
    expect(html).toContain(`<span class="turn-dur">30s</span>`);
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
    expect(html).toContain(`<span class="turn-dur">1s</span>`);
  });

  it("rounds the final-response chip's duration up to whole seconds", () => {
    // Arrange + Act — 5984ms, a part-second past the fifth second.
    const html = finalResponseHtml(5_984);
    // Assert — 6s, never the 5s 984ms the millisecond scale would give.
    expect(html).toContain(`<span class="turn-dur">6s</span>`);
    expect(html).not.toContain("984ms");
  });

  it("groups the duration and delta under one stats span so the bullet stays inline", () => {
    // Arrange + Act — the wrapping .turn-stats item keeps the `·` between the
    // two figures on one line rather than dropping it onto its own row in the
    // .turn-meta column.
    const html = finalResponseHtml(1_000);
    // Assert
    expect(html).toContain(
      `<span class="turn-stats"><span class="turn-dur">1s</span> · <span class="turn-diff">+100,000</span></span>`,
    );
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

  it("counts an OPEN textless thinking block as drawn, so the tail scan can name it", () => {
    // Arrange + Act + Assert — it renders no inline card, but staying non-empty
    // lets the nav scan reach it; its `thinking…` beat is the footer's now.
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
    // Arrange + Act + Assert — a bubble with real body content is drawn.
    expect(rendersEmpty(text("b1"))).toBe(false);
  });

  it("counts a text block still empty before its first delta as undrawn", () => {
    // Arrange — the text-start/first-text-delta gap, when the bubble would
    // otherwise render nothing but its floating turn-ts stamp.
    const item: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "",
      done: false,
      ts: TEXT_TS,
    };
    // Act + Assert
    expect(rendersEmpty(item)).toBe(true);
  });

  it("counts a text block that closed on blank final_text as undrawn", () => {
    // Arrange — a done bubble whose whole body is whitespace would leave a
    // lone timestamp with no visible bubble around it.
    const item: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "  \n",
      done: true,
      ts: TEXT_TS,
    };
    // Act + Assert
    expect(rendersEmpty(item)).toBe(true);
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

  it("counts the interrupt sentinel bubble as undrawn", () => {
    // Arrange — the sentinel Claude Code injects for a plain user interrupt;
    // the yellow aborted chip is the only denotation the feed keeps.
    const item: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "[Request interrupted by user]",
      done: true,
      ts: TEXT_TS,
    };
    // Act + Assert
    expect(rendersEmpty(item)).toBe(true);
  });

  it("counts the tool-use interrupt sentinel bubble as undrawn", () => {
    // Arrange — the variant emitted when a tool call was in flight.
    const item: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "[Request interrupted by user for tool use]",
      done: true,
      ts: TEXT_TS,
    };
    // Act + Assert
    expect(rendersEmpty(item)).toBe(true);
  });

  it("counts an interrupt sentinel padded with whitespace as undrawn", () => {
    // Arrange — a trailing newline must not smuggle the sentinel past.
    const item: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "  [Request interrupted by user]\n",
      done: true,
      ts: TEXT_TS,
    };
    // Act + Assert
    expect(rendersEmpty(item)).toBe(true);
  });

  it("counts a bubble that merely quotes the interrupt sentinel mid-sentence as drawn", () => {
    // Arrange — a real answer discussing the sentinel is not the sentinel.
    const item: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "the CLI prints [Request interrupted by user] on abort",
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
  it("hides the input section of an Agent spawn, as it does the legacy Task", () => {
    // Arrange — the description-and-JSON box is dropped from the card entirely
    // (see toolInput); the CLI renamed Task to Agent.
    const item: ToolItem = {
      kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
      toolUseId: "t1",
      toolName: "Agent",
      messageId: "m1",
      input: { description: "hunt the flake", prompt: "go" },
      inputJson: `{"description":"hunt the flake"}`,
      inputDone: true,
    };
    // Act + Assert
    expect(renderItem(item)).not.toContain("agent-input");
  });

  it("renders nothing for a TaskUpdate whose input is still streaming", () => {
    // Arrange — pre-input the partition cannot yet claim the update, and a
    // card that flashed top-level then jumped into a panel would glitch.
    const item: ToolItem = {
      kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
      toolUseId: "t1",
      toolName: "TaskUpdate",
      messageId: "m1",
      inputJson: `{"taskId":"1`,
      inputDone: false,
    };
    // Act + Assert
    expect(renderItem(item)).toBe("");
  });

  it("caps the Bash command behind the bash-input preview class", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
      toolUseId: "t1",
      toolName,
      messageId: "m1",
      input: { description: "Audit the sentinel", prompt: "Read every file and…" },
      inputJson: `{"description":"Audit the sentinel","prompt":"Read every file and…"}`,
      inputDone: true,
    };
  }

  it("drops the Agent card's description box entirely", () => {
    // Arrange
    const item = agentCall();
    // Act
    const html = renderItem(item);
    // Assert — the whole clickable description box (its only marker was the
    // agent-input class) is gone.
    expect(html).not.toContain("agent-input");
  });

  it("keeps the Agent description text off the card", () => {
    // Arrange
    const item = agentCall();
    // Act
    const html = renderItem(item);
    // Assert — with the box dropped, the description no longer prints.
    expect(html).not.toContain("Audit the sentinel");
  });

  it("keeps the Agent prompt off the card", () => {
    // Arrange — the prompt used to ride the folded JSON, which is now gone.
    const item = agentCall();
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).not.toContain("Read every file and");
  });

  it("carries none of the low-level call JSON, so .agent-json never renders", () => {
    // Arrange
    const item = agentCall();
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).not.toContain("agent-json");
  });

  it("washes the Agent card teal by naming it a special tool rather than Generic", () => {
    // Arrange
    const item = agentCall();
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain(`class="tool-card tool-agent"`);
  });

  it("hides the legacy Task name's input section too", () => {
    // Arrange — Task is what the CLI called the subagent tool before Agent.
    const item = agentCall("Task");
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).not.toContain("agent-input");
  });

  it("hides the input section for an Agent call carrying no description too", () => {
    // Arrange — even with no description, no generic JSON fold takes its place.
    const item: ToolItem = {
      kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
      toolUseId: "t1",
      toolName: "Agent",
      messageId: "m1",
      input: { prompt: "go" },
      inputJson: `{"prompt":"go"}`,
      inputDone: true,
    };
    // Act
    const html = renderItem(item);
    // Assert — no input section of any kind renders for a subagent card.
    expect(html).not.toContain("tool-input");
  });

  it("leaves the Agent's own output rendering untouched by the hidden input", () => {
    // Arrange — only the input is dropped; the result still shows.
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

  it("renders the tree while keeping leading prose on the markdown path", () => {
    // Arrange — the model prepended a preamble line before the tree.
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b1",
      messageId: "m1",
      text: "Here is my answer:\n\nResponse (✏️ changes made)\n\n1 🔧 Fixed it\n├── 1.1 Detail\n└── 1.2 More",
      done: true,
    };
    // Act
    const html = renderItem(item);
    // Assert — tree still postprocessed, preamble still rendered as prose.
    expect(html).toContain(`class="mp-tree"`);
    expect(html).toContain("Here is my answer");
  });

  it("renders the tree even when a stray code fence trails it", () => {
    // Arrange — a trailing fence used to suppress the whole bare tree.
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "b2",
      messageId: "m1",
      text: "Response (✏️ changes made)\n\n1 🔧 Fixed it\n├── 1.1 Detail\n└── 1.2 More\n\n```\ncode\n```",
      done: true,
    };
    // Act
    const html = renderItem(item);
    // Assert — both the tree and the fenced block render.
    expect(html).toContain(`class="mp-tree"`);
    expect(html).toContain(`class="md-code"`);
  });

  it("warns once when a header-led response yields no tree region", () => {
    // Arrange — the mandated header with no tree beneath it is a misfire.
    const warn = vi.spyOn(console, "warn").mockImplementation(() => {});
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "mp-misfire-1",
      messageId: "m1",
      text: "Response (👀 no changes made)\n\nJust prose, no tree at all.",
      done: true,
    };
    // Act — a persistent misfire re-renders, but must warn only once.
    renderItem(item);
    renderItem(item);
    // Assert
    expect(warn).toHaveBeenCalledTimes(1);
    warn.mockRestore();
  });

  it("does not warn while a header-led response is still streaming", () => {
    // Arrange — a partial tree mid-stream must not log a misfire.
    const warn = vi.spyOn(console, "warn").mockImplementation(() => {});
    const item: ConversationItem = {
      kind: "text",
      ts: TEXT_TS,
      blockId: "mp-misfire-2",
      messageId: "m1",
      text: "Response (👀 no changes made)\n\nJust prose, no tree at all.",
      done: false,
    };
    // Act
    renderItem(item);
    // Assert
    expect(warn).not.toHaveBeenCalled();
    warn.mockRestore();
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
    expect(repinsToTail({ prevTurnId: "r1", nextTurnId: "r2", pinned: false, frozen: false })).toBe(true);
  });

  it("jumps to the tail on the feed's very first prompt", () => {
    // Arrange + Act + Assert
    expect(repinsToTail({ prevTurnId: null, nextTurnId: "r1", pinned: false, frozen: false })).toBe(true);
  });

  it("leaves a scrolled-up feed alone while the same turn streams its answer", () => {
    // Arrange + Act + Assert
    expect(repinsToTail({ prevTurnId: "r1", nextTurnId: "r1", pinned: false, frozen: false })).toBe(false);
  });

  it("leaves a scrolled-up feed alone when no prompt was ever sent", () => {
    // Arrange + Act + Assert
    expect(repinsToTail({ prevTurnId: null, nextTurnId: null, pinned: false, frozen: false })).toBe(false);
  });

  it("keeps a pinned feed following its tail", () => {
    // Arrange + Act + Assert
    expect(repinsToTail({ prevTurnId: "r1", nextTurnId: "r1", pinned: true, frozen: false })).toBe(true);
  });

  it("holds a frozen feed off its tail even when it is pinned to the bottom", () => {
    // Arrange + Act + Assert
    expect(repinsToTail({ prevTurnId: "r1", nextTurnId: "r1", pinned: true, frozen: true })).toBe(false);
  });

  it("lets a fresh prompt override the freeze and jump to the tail", () => {
    // Arrange + Act + Assert
    expect(repinsToTail({ prevTurnId: "r1", nextTurnId: "r2", pinned: false, frozen: true })).toBe(true);
  });
});

// --- activity panels ------------------------------------------------------------

/** An Agent card the panel tests spawn children under. */
function agentTool(id = "a1"): ToolItem {
  return {
    kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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

describe("agent bubble topbar", () => {
  /** panelCtx plus a stub strip renderer that names the agent it was asked for. */
  function topbarPanels(): PanelContext {
    return {
      ...panelCtx([]),
      agentTopbar: (agent) => `<div class="agent-topbar">strip:${agent.toolUseId}</div>`,
    };
  }

  it("folds the agent-scoped strip into a subagent card through the panels hook", () => {
    // Arrange + Act
    const html = renderItem(agentTool(), undefined, undefined, topbarPanels());
    // Assert — the hook was called with THIS agent's item.
    expect(html).toContain("strip:a1");
  });

  it("keeps the strip out of a non-subagent card", () => {
    // Arrange + Act — a Bash card with the same hook wired.
    const html = renderItem(tool(), undefined, undefined, topbarPanels());
    // Assert
    expect(html).not.toContain("strip:");
  });

  it("renders no strip when the hook is not wired", () => {
    // Arrange + Act — a PanelContext without agentTopbar (older callers).
    const html = renderItem(agentTool(), undefined, undefined, panelCtx([]));
    // Assert
    expect(html).not.toContain("agent-topbar");
  });

  it("seats the strip under the card head", () => {
    // Arrange + Act — the input box the strip once sat above is gone (see
    // toolInput), so the head is the only anchor left to seat it under.
    const html = renderItem(agentTool(), undefined, undefined, topbarPanels());
    // Assert
    expect(html.indexOf("agent-topbar")).toBeGreaterThan(html.indexOf("tool-head"));
  });
});

describe("activity panel", () => {
  it("renders no activity fold on a card without children", () => {
    // Arrange + Act
    const html = renderItem(agentTool(), undefined, undefined, panelCtx([]));
    // Assert
    expect(html).not.toContain("agent-activity");
  });

  it("shows the ticker face while closed and none of the child feed", () => {
    // Arrange + Act
    const html = renderItem(agentTool(), undefined, undefined, panelCtx([childBash()]));
    // Assert
    expect(html).toContain("agent-ticker");
    expect(html).toContain("1 step · Bash: ls -la");
    expect(html).not.toContain("agent-panel");
  });

  it("renders the child feed inside the open panel", () => {
    // Arrange + Act
    const html = renderItem(agentTool(), undefined, undefined, panelCtx([childBash()], true));
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
      panelCtx([childBash(), childPermission("t2", true)]),
    );
    // Assert
    expect(html).not.toContain("needs permission");
  });

  it("ignores suppressed children entirely", () => {
    // Arrange — ToolSearch renders as nothing everywhere.
    const suppressed: ToolItem = { ...childBash(), toolName: "ToolSearch" };
    // Act
    const html = renderItem(agentTool(), undefined, undefined, panelCtx([suppressed]));
    // Assert
    expect(html).not.toContain("agent-activity");
  });

  it("attaches the fold to a non-agent spawner just the same", () => {
    // Arrange — a Workflow's children confine like an Agent's.
    const wf: ToolItem = { ...agentTool(), toolName: "Workflow" };
    // Act
    const html = renderItem(wf, undefined, undefined, panelCtx([childBash()]));
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

// --- task update stream ------------------------------------------------------------

/** A settled TaskCreate card whose result named task #1. */
function taskCreateTool(id = "tc1"): ToolItem {
  return {
    kind: "tool",
    ts: TEXT_TS,
    toolUseId: id,
    messageId: "m1",
    toolName: "TaskCreate",
    inputJson: "{}",
    input: { subject: "wire the counter" },
    inputDone: true,
    result: { isError: false, content: "Task #1 created successfully: wire the counter" },
  };
}

/** A settled TaskUpdate against task #1. */
function taskUpdateTool(over: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
    ts: TEXT_TS,
    toolUseId: "u1",
    messageId: "m1",
    toolName: "TaskUpdate",
    inputJson: "{}",
    input: { taskId: "1", status: "in_progress" },
    inputDone: true,
    ...over,
  };
}

/** A PanelContext confining CHILDREN under the tc1 create card, open per OPEN. */
function taskChildrenCtx(children: ConversationItem[], open = false): PanelContext {
  return {
    children: new Map([["tc1", children]]),
    isOpen: () => open,
  };
}

describe("task update card", () => {
  it("badges the transition with the working tone while the task moves", () => {
    // Arrange + Act + Assert
    expect(renderItem(taskUpdateTool())).toContain(
      `<span class="badge run">in progress</span>`,
    );
  });

  it("lands a completed transition on the settled ok tone", () => {
    // Arrange
    const done = taskUpdateTool({ input: { taskId: "1", status: "completed" } });
    // Act + Assert
    expect(renderItem(done)).toContain(`<span class="badge ok">completed</span>`);
  });

  it("lands a deleted transition on the error tone", () => {
    // Arrange
    const gone = taskUpdateTool({ input: { taskId: "1", status: "deleted" } });
    // Act + Assert
    expect(renderItem(gone)).toContain(`<span class="badge err">deleted</span>`);
  });

  it("carries the subject the update renamed", () => {
    // Arrange
    const renamed = taskUpdateTool({ input: { taskId: "1", subject: "new name" } });
    // Act + Assert
    expect(renderItem(renamed)).toContain("new name");
  });

  it("escapes markup in the subject", () => {
    // Arrange
    const sly = taskUpdateTool({ input: { taskId: "1", subject: "<img src=x>" } });
    // Act + Assert
    expect(renderItem(sly)).not.toContain("<img");
  });

  it("suppresses the success echo the harness prints", () => {
    // Arrange — the ack adds nothing over the transition line.
    const settled = taskUpdateTool({
      result: { isError: false, content: "Task #1 updated successfully" },
    });
    // Act + Assert
    expect(renderItem(settled)).not.toContain("updated successfully");
  });

  it("keeps a failed update's error text loud", () => {
    // Arrange
    const failed = taskUpdateTool({ result: { isError: true, content: "no such task" } });
    // Act
    const html = renderItem(failed);
    // Assert
    expect(html).toContain("no such task");
    expect(html).toContain("stderr");
  });

  it("falls back to the raw input JSON when neither status nor subject moved", () => {
    // Arrange — a metadata-only update has no transition line to draw.
    const bare = taskUpdateTool({ input: { taskId: "1" }, inputJson: `{"taskId":"1"}` });
    // Act + Assert
    expect(renderItem(bare)).toContain(`<pre class="tool-input">`);
  });

  it("nests a task's updates inside the create card's open activity panel", () => {
    // Arrange + Act — the partition confines updates as ordinary children.
    const html = renderItem(
      taskCreateTool(),
      undefined,
      undefined,
      taskChildrenCtx([taskUpdateTool()], true),
    );
    // Assert
    expect(html).toContain("agent-panel");
    expect(html).toContain(`<span class="badge run">in progress</span>`);
  });

  it("faces the closed create-card fold with the update count", () => {
    // Arrange + Act
    const html = renderItem(
      taskCreateTool(),
      undefined,
      undefined,
      taskChildrenCtx([taskUpdateTool()]),
    );
    // Assert
    expect(html).toContain("1 step · TaskUpdate");
  });
});

// --- consecutive-run tab groups ---------------------------------------------------

/** A Bash card with its own id and command, for building runs. */
function bash(id: string, command = "ls", result?: { isError: boolean }): ToolItem {
  return {
    kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
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

describe("planToolReveal", () => {
  it("reveals a lone top-level subagent as its own bubble with its panel open", () => {
    // Arrange — a single Agent card, ungrouped.
    const items = [userTurnAt(9, 0), agentTool("a1")];
    // Act
    const plan = planToolReveal(items, "a1");
    // Assert
    expect(plan).toEqual<ToolReveal>({
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
    const plan = planToolReveal(items, "a2");
    // Assert — the group is keyed by its first member; a2's tab must be pinned.
    expect(plan).toEqual<ToolReveal>({
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
    const plan = planToolReveal(items, "a2");
    // Assert — land on a1's bubble, opening a2's panel (its output) and a1's (holding a2).
    expect(plan).toEqual<ToolReveal>({
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
    expect(planToolReveal(items, "ghost")).toBeNull();
  });

  it("returns null for a subagent a clear discarded from the feed", () => {
    // Arrange — the agent precedes the clear, so it is off the current context.
    const items: ConversationItem[] = [
      userTurnAt(9, 0),
      agentTool("a1"),
      { kind: "context-cleared", uuid: "c1" },
    ];
    // Act + Assert
    expect(planToolReveal(items, "a1")).toBeNull();
  });

  it("plans a reveal for a non-subagent card, a task's TaskCreate", () => {
    // Arrange — the task roster resolves its rows to TaskCreate tool-use ids.
    const create: ToolItem = {
      kind: "tool",
      ts: TEXT_TS,
      toolUseId: "tc1",
      messageId: "m1",
      toolName: "TaskCreate",
      inputJson: "{}",
      input: { subject: "wire the counter" },
      inputDone: true,
    };
    // Act
    const plan = planToolReveal([userTurnAt(9, 0), create], "tc1");
    // Assert
    expect(plan).toEqual<ToolReveal>({
      key: "tool:tc1",
      groupKey: null,
      tabMember: null,
      panelIds: ["tc1"],
    });
  });
});

describe("groupHtml", () => {
  it("keeps a background agent's chip running until its detached work settles", () => {
    // Arrange — the spawn's own result landed at launch, but the WORK runs on.
    const launched: ToolItem = {
      kind: "tool",
      ts: "2026-05-24T10:00:00.000Z",
      toolUseId: "g1",
      messageId: "m1",
      toolName: "Agent",
      inputJson: "{}",
      input: {},
      inputDone: true,
      result: { isError: false, content: "Async agent launched. agentId: gag1" },
      asyncSource: {
        source_id: "gag1",
        kind: "agent",
        status: "running",
        stream: { transport: "poll", format: "jsonl-transcript" },
      },
    };
    // Act
    const html = groupHtml([launched], "g1");
    // Assert — before the resolver, this chip lied green at spawn time.
    expect(html).toContain("agent-running");
    expect(html).not.toContain("agent-done");
  });

  it("settles a background agent's chip once its notification lands", () => {
    // Arrange
    const settled: ToolItem = {
      kind: "tool",
      ts: "2026-05-24T10:00:00.000Z",
      toolUseId: "g1",
      messageId: "m1",
      toolName: "Agent",
      inputJson: "{}",
      input: {},
      inputDone: true,
      result: { isError: false, content: "Async agent launched. agentId: gag1" },
      asyncSource: {
        source_id: "gag1",
        kind: "agent",
        status: "running",
        stream: { transport: "poll", format: "jsonl-transcript" },
      },
      notification: { taskId: "gag1", status: "completed", text: "<t/>" },
    };
    // Act
    const html = groupHtml([settled], "g1");
    // Assert
    expect(html).toContain("agent-done");
    expect(html).not.toContain("agent-running");
  });

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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
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
    ts: "2026-05-24T10:00:00.000Z",
      toolUseId: "t1",
      messageId: "m1",
      toolName: "Bash",
      inputJson: "{}",
      input: { command: "make", run_in_background: true },
      inputDone: true,
      result: { isError: false, content: "Command running in background with ID: bg7. Output is being written to: /tmp/claude-1/s/tasks/bg7.output" },
      ...(notified
        ? { notification: { taskId: "bg7", status: "completed", text: "<task-notification/>" } }
        : {}),
    };
  }

  it("offers a bare Stop verb on the face while the spawned task still runs", () => {
    // Arrange + Act
    const html = renderItem(bgSpawn());
    // Assert — the tooltip owns being prompt-mediated; the label stays bare.
    expect(html).toContain("face-stop");
    expect(html).toContain(">Stop</button>");
    expect(html).toContain(`title="asks the agent to stop bg7 via TaskStop"`);
    expect(html).toContain(`data-send-prompt="Stop the background task bg7`);
  });

  it("rides the head's right side rather than the card bottom", () => {
    // Arrange + Act
    const html = renderItem(bgSpawn());
    // Assert — the Stop is inside the tool-head's face-side span.
    const head = html.slice(html.indexOf("tool-head"), html.indexOf("</div>"));
    expect(head).toContain("face-stop");
  });

  it("withdraws the button once the completion notification lands", () => {
    // Arrange + Act
    const html = renderItem(bgSpawn(true));
    // Assert
    expect(html).not.toContain("face-stop");
  });

  it("offers one Stop covering every id a multi-spawn card announced", () => {
    // Arrange
    const item: ToolItem = {
      ...bgSpawn(),
      result: {
        isError: false,
        content:
          "Running with ID: bg7. Output is being written to: /tmp/claude-1/s/tasks/bg7.output. Also running with ID: bg8. Output is being written to: /tmp/claude-1/s/tasks/bg8.output",
      },
    };
    // Act
    const html = renderItem(item);
    // Assert
    expect(html.match(/face-stop/g)?.length).toBe(1);
    expect(html).toContain(`data-send-prompt="Stop the background tasks bg7, bg8`);
  });

  it("keeps a spawning card's badge running until the notification lands", () => {
    // Arrange + Act — the result landed, the detached task did not.
    const html = renderItem(bgSpawn());
    // Assert
    expect(html).toContain("running…");
    expect(renderItem(bgSpawn(true))).toContain(`<span class="badge ok">done</span>`);
  });

  it("surfaces a killed notification as a stopped badge", () => {
    // Arrange
    const item = {
      ...bgSpawn(),
      notification: { taskId: "bg7", status: "killed", text: "<task-notification/>" },
    };
    // Act + Assert
    expect(renderItem(item)).toContain(`<span class="badge err">stopped</span>`);
  });

  it("surfaces an errored notification as an error badge", () => {
    // Arrange
    const item = {
      ...bgSpawn(),
      notification: { taskId: "bg7", status: "failed", text: "<task-notification/>" },
    };
    // Act + Assert
    expect(renderItem(item)).toContain(`<span class="badge err">error</span>`);
  });

  it("offers no control on a card that spawned nothing", () => {
    // Arrange + Act
    const html = renderItem(tool());
    // Assert
    expect(html).not.toContain("face-stop");
  });
});

describe("dual-body stacking (Shape A)", () => {
  it("stacks the child-feed fold above the async fold on one card", () => {
    // Arrange — an inline subagent that ALSO spawned detached work.
    const item: ToolItem = {
      kind: "tool",
      ts: "2026-05-24T10:00:00.000Z",
      toolUseId: "t1",
      messageId: "m1",
      toolName: "Agent",
      inputJson: "{}",
      input: { description: "spawner" },
      inputDone: true,
      result: { isError: false, content: "Command running in background with ID: bg1. Output is being written to: /tmp/claude-1/s/tasks/bg1.output" },
      asyncSource: { source_id: "bg1", kind: "shell", status: "running" },
    };
    const child: ToolItem = { ...item, toolUseId: "c1", toolName: "Read", result: undefined, asyncSource: undefined };
    const panels: PanelContext = {
      children: new Map([["t1", [child]]]),
      isOpen: () => false,
    };
    // Act
    const html = renderItem(item, undefined, undefined, panels);
    // Assert — both folds render, activity first (Shape A order).
    const activityAt = html.indexOf("agent-activity");
    const asyncAt = html.indexOf("async-fold");
    expect(activityAt).toBeGreaterThan(-1);
    expect(asyncAt).toBeGreaterThan(-1);
    expect(activityAt).toBeLessThan(asyncAt);
  });
});

describe("live task output", () => {
  /** A tail with NO spawn announcement: the announcement-less raw member. */
  function tailOnly(): ToolItem {
    return {
      kind: "tool",
      ts: "2026-05-24T10:00:00.000Z",
      toolUseId: "t1",
      messageId: "m1",
      toolName: "Bash",
      inputJson: "{}",
      input: { command: "make" },
      inputDone: true,
      taskOutput: "compiling…\nlinking…\n",
    };
  }

  it("wears the shared fold dress with an output face, never a zero-click pre", () => {
    // Arrange
    const panels: PanelContext = { children: new Map(), isOpen: () => false };
    // Act
    const html = renderItem(tailOnly(), undefined, undefined, panels);
    // Assert — the announcement-less tail is a raw member now.
    expect(html).toContain("async-fold");
    expect(html).toContain("output · make · running");
    expect(html).not.toContain("task-live-output");
  });

  it("streams the daemon's file tail into the open fold's panel", () => {
    // Arrange
    const panels: PanelContext = { children: new Map(), isOpen: (id) => id === "async:t1" };
    // Act
    const html = renderItem(tailOnly(), undefined, undefined, panels);
    // Assert
    expect(html).toContain("task-live-output");
    expect(html).toContain("linking…");
  });

  it("renders no tail fold before any output streams", () => {
    // Arrange + Act
    const html = renderItem(tool(), undefined, undefined, {
      children: new Map(),
      isOpen: () => true,
    });
    // Assert
    expect(html).not.toContain("task-live-output");
    expect(html).not.toContain("async-fold");
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
    ts: "2026-05-24T10:00:00.000Z",
      toolUseId: "t1",
      messageId: "m1",
      toolName: "Agent",
      inputJson: "{}",
      input: { description: "scout" },
      inputDone: true,
      result: {
        isError: false,
        content: "Async agent launched successfully. agentId: abc9, output_file: /tmp/claude-1/s/tasks/abc9.output",
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
    const html = renderItem(bgAgent(), undefined, undefined, panels);
    // Assert
    expect(html).toContain(`value="status pls"`);
  });
});

// --- async fold (on the spawning card) ----------------------------------------

/** One JSONL entry of a subagent transcript. */
function txLine(o: unknown): string {
  return JSON.stringify(o);
}

const agentTranscript = [
  txLine({ type: "assistant", message: { id: "m1", content: [{ type: "text", text: "scanning the repo" }] } }),
  txLine({
    type: "assistant",
    message: { id: "m2", content: [{ type: "tool_use", id: "tu1", name: "Grep", input: { pattern: "foo" } }] },
  }),
].join("\n");

/** A card that spawned a detached agent, per its §2.6 descriptor. */
function sourcedCard(source: Partial<AsyncSource> = {}, over: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
    toolUseId: "t1",
    messageId: "m1",
    toolName: "Agent",
    ts: "2026-07-17T10:00:00.000Z",
    inputJson: "{}",
    input: { description: "find the thing" },
    inputDone: true,
    result: { isError: false, content: "launched" },
    asyncSource: {
      source_id: "a9",
      kind: "agent",
      label: "find the thing",
      status: "running",
      stream: { transport: "poll", format: "jsonl-transcript" },
      ...source,
    },
    ...over,
  };
}

/** A PanelContext with the fold open per OPEN, serving TAIL from the poller. */
function asyncPanels(open: boolean, tail = "", extra: Partial<PanelContext> = {}): PanelContext {
  return {
    children: new Map(),
    isOpen: () => open,
    taskTail: () => ({ text: tail, offset: tail.length, done: false, elapsedMs: 1000 }),
    ...extra,
  };
}

describe("async fold", () => {
  it("renders no fold on a card that spawned nothing detached", () => {
    // Arrange
    const card = sourcedCard();
    delete card.asyncSource;
    // Act
    const html = renderItem(card, undefined, undefined, asyncPanels(false));
    // Assert
    expect(html).not.toContain("async-fold");
  });

  it("names the work and its state on the collapsed face", () => {
    // Arrange / Act
    const html = renderItem(sourcedCard(), undefined, undefined, asyncPanels(false));
    // Assert
    expect(html).toContain("agent · find the thing · running");
  });

  it("falls back to the source id when the spawn announced no label", () => {
    // Arrange / Act
    const html = renderItem(sourcedCard({ label: "" }), undefined, undefined, asyncPanels(false));
    // Assert
    expect(html).toContain("agent · a9 · running");
  });

  it("arcs the ticker while the work runs, since a settled card never breathes", () => {
    // Arrange / Act
    const html = renderItem(sourcedCard(), undefined, undefined, asyncPanels(false));
    // Assert
    expect(html).toContain(`<span class="tool-spinner" aria-hidden="true"></span>`);
  });

  it("drops the arc once the work is done", () => {
    // Arrange / Act
    const html = renderItem(sourcedCard({ status: "done" }), undefined, undefined, asyncPanels(false));
    // Assert
    expect(html).not.toContain("tool-spinner");
  });

  it("costs nothing while closed, rendering none of the stream", () => {
    // Arrange / Act
    const html = renderItem(sourcedCard(), undefined, undefined, asyncPanels(false, agentTranscript));
    // Assert
    expect(html).not.toContain("scanning the repo");
  });

  it("renders a background agent's transcript as NESTED BUBBLES when open", () => {
    // Arrange / Act — the payoff: it was an opaque <pre> before.
    const html = renderItem(sourcedCard(), undefined, undefined, asyncPanels(true, agentTranscript));
    // Assert
    expect(html).toContain("bubble assistant");
    expect(html).toContain("scanning the repo");
  });

  it("renders the transcript's tool calls as cards, not as text", () => {
    // Arrange / Act
    const html = renderItem(sourcedCard(), undefined, undefined, asyncPanels(true, agentTranscript));
    // Assert
    expect(html).toContain("tool-card tool-grep");
  });

  it("does not paint a transcript as a raw pre, which is what it used to be", () => {
    // Arrange / Act
    const html = renderItem(sourcedCard(), undefined, undefined, asyncPanels(true, agentTranscript));
    // Assert
    expect(html).not.toContain("task-live-output");
  });

  /** A transcript whose Agent call announced a detached agent. */
  function nestedSpawnTranscript(announcedId: string): string {
    return [
      txLine({
        type: "assistant",
        message: {
          id: "m6",
          content: [{ type: "tool_use", id: "ag1", name: "Agent", input: { description: "watch the queue" } }],
        },
      }),
      txLine({
        type: "user",
        message: {
          id: "m7",
          content: [{ type: "tool_result", tool_use_id: "ag1", content: `Async agent launched. agentId: ${announcedId}, output_file: /tmp/claude-1/s/tasks/${announcedId}.output` }],
        },
      }),
    ].join("\n");
  }

  it("folds a nested spawn card inside a transcript — the recursion payoff", () => {
    // Arrange / Act — the transcript's own Agent spawn wears a fold too.
    const html = renderItem(
      sourcedCard(),
      undefined,
      undefined,
      asyncPanels(true, nestedSpawnTranscript("a7")),
    );
    // Assert
    expect(html).toContain(`data-panel-toggle="async:ag1"`);
  });

  it("cuts the fold of a nested spawn announcing an ancestor's own id", () => {
    // Arrange / Act — the nested card names the OUTER source a9: a cycle.
    const html = renderItem(
      sourcedCard(),
      undefined,
      undefined,
      asyncPanels(true, nestedSpawnTranscript("a9")),
    );
    // Assert — only the outer fold renders.
    expect(html.match(/async-fold/g)).toHaveLength(1);
  });

  it("nests a transcript's task history inside its create card's own fold", () => {
    // Arrange — the transcript's TaskUpdate is confined under its create
    // exactly as the live partition confines it at depth one.
    const taskTranscript = [
      txLine({
        type: "assistant",
        message: {
          id: "m3",
          content: [{ type: "tool_use", id: "tc1", name: "TaskCreate", input: { subject: "fix the bug" } }],
        },
      }),
      txLine({
        type: "user",
        message: {
          id: "m4",
          content: [{ type: "tool_result", tool_use_id: "tc1", content: "Task #7 created successfully: fix the bug" }],
        },
      }),
      txLine({
        type: "assistant",
        message: {
          id: "m5",
          content: [{ type: "tool_use", id: "tu2", name: "TaskUpdate", input: { taskId: "7", status: "completed" } }],
        },
      }),
    ].join("\n");
    // Act — asyncPanels(true) opens every fold, the nested activity one included.
    const html = renderItem(sourcedCard(), undefined, undefined, asyncPanels(true, taskTranscript));
    // Assert — the create card carries an activity fold holding the update.
    expect(html).toContain("agent-activity");
    expect(html).toContain(">TaskUpdate<");
  });

  it("keeps a shell's spool bytes as a pre, having no structure to recover", () => {
    // Arrange
    const shell = sourcedCard(
      { kind: "shell", label: "", stream: { transport: "ws", format: "text" } },
      { toolName: "Bash", taskOutput: "line one\nline two" },
    );
    // Act
    const html = renderItem(shell, undefined, undefined, asyncPanels(true));
    // Assert
    expect(html).toContain("task-live-output");
    expect(html).toContain("line one");
  });

  it("renders a workflow journal as rows rather than bubbles", () => {
    // Arrange
    const journal = txLine({ label: "review:bugs", result: "3 findings" });
    const wf = sourcedCard({ kind: "workflow", stream: { transport: "poll", format: "jsonl-journal" } });
    // Act
    const html = renderItem(wf, undefined, undefined, asyncPanels(true, journal));
    // Assert
    expect(html).toContain("stream-row");
    expect(html).toContain("review:bugs");
    expect(html).not.toContain("bubble assistant");
  });

  it("says what the cap left out rather than posing as the whole stream", () => {
    // Arrange
    const long = Array.from({ length: 3 }, (_, i) =>
      txLine({ type: "assistant", message: { id: `m${i}`, content: [{ type: "text", text: `t${i}` }] } }),
    ).join("\n");
    // Act — STREAM_ITEM_CAP is large, so drive the cap through the parser
    // directly; here just assert the fold shows the whole short stream.
    const html = renderItem(sourcedCard(), undefined, undefined, asyncPanels(true, long));
    // Assert
    expect(html).not.toContain("stream-dropped");
  });

  it("does not double the tail, painting it in the fold and beneath the card", () => {
    // Arrange
    const shell = sourcedCard(
      { kind: "shell", stream: { transport: "ws", format: "text" } },
      { toolName: "Bash", taskOutput: "once" },
    );
    // Act
    const html = renderItem(shell, undefined, undefined, asyncPanels(true));
    // Assert
    expect(html.match(/task-live-output/g)).toHaveLength(1);
  });
});

// --- watcher fold (in a final-response bubble) ---------------------------------

/** A backgrounded-Bash watcher announcing TASKID, plus any overrides. */
function watcher(taskId = "bg1", over: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
    toolUseId: "w1",
    messageId: "m1",
    toolName: "Bash",
    inputJson: "{}",
    input: { command: "poll.sh" },
    inputDone: true,
    result: { isError: false, content: `Command running in background with ID: ${taskId}. Output is being written to: /tmp/claude-1/s/tasks/${taskId}.output` },
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

/** A TaskStop addressing TASKID, folded under the watcher card w1. */
function taskStop(taskId = "bg1", over: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
    ts: "2026-05-24T10:01:00.000Z",
    toolUseId: "ts1",
    messageId: "m2",
    toolName: "TaskStop",
    inputJson: "{}",
    input: { task_id: taskId },
    inputDone: true,
    result: { isError: false, content: `Stopped ${taskId}.` },
    ...over,
  };
}

/** watcherPanels plus CHILDREN folded under the watcher card w1. */
function watcherPanelsWithChildren(
  watchers: ToolItem[],
  children: ToolItem[],
  open = false,
): PanelContext {
  return {
    children: new Map([["w1", children]]),
    isOpen: () => open,
    watchers: new Map([["b1", watchers]]),
  };
}

describe("async catalog", () => {
  it("renders no catalog on a bubble the projection does not host", () => {
    // Arrange + Act — no members mapped to this bubble.
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), watcherPanels([]));
    // Assert
    expect(html).not.toContain("async-catalog");
  });

  it("shows a member badge while live, with no detail closed", () => {
    // Arrange + Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), watcherPanels([watcher()]));
    // Assert
    expect(html).toContain("async-catalog");
    expect(html).toContain(`data-panel-toggle="member:b1:w1"`);
    expect(html).not.toContain("tool-card");
  });

  it("never repeats the monitoring line inside the bubble catalog while a member is live", () => {
    // Arrange + Act — a live member drives the catalog, but the animated
    // monitoring row lives ONLY at the global feed tail, never in the bubble.
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), watcherPanels([watcher()]));
    // Assert
    expect(html).toContain("async-catalog");
    expect(html).not.toContain("monitoring…");
  });

  it("renders each member badge as a div-toggle, not a click-through button", () => {
    // Arrange — handlePanelToggle's click-through guard (a, button, summary)
    // would swallow a <button> badge, so the badge must be a plain toggle div.
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), watcherPanels([watcher()]));
    // Assert
    expect(html).toContain(`<div class="async-badge" data-panel-toggle="member:b1:w1"`);
  });

  it("expands a badge into the member's own card inside the panel inset", () => {
    // Arrange
    const w = watcher("bg1", { taskOutput: "polling the queue…" });
    // Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), watcherPanels([w], true));
    // Assert — the detail is the very ToolCard the feed renders, in an
    // .agent-panel wrap, its tail behind the card's own (open) fold.
    expect(html).toContain(`<div class="agent-panel"><div class="feed-child">`);
    expect(html).toContain("tool-card tool-bash");
    expect(html).toContain("task-live-output");
    expect(html).toContain("polling the queue…");
  });

  it("settles a member's badge once its notification lands", () => {
    // Arrange — the completion notification settles the member.
    const w = watcher("bg1", { notification: { taskId: "bg1", text: "<task-notification/>" } });
    // Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), watcherPanels([w]));
    // Assert
    expect(html).toContain("async-badge settled");
  });

  it("settles a member's badge when its folded TaskStop succeeded", () => {
    // Arrange — the prompt-mediated stop yields no notification, only a settled TaskStop card.
    const w = watcher("bg1");
    // Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), watcherPanelsWithChildren([w], [taskStop("bg1")]));
    // Assert
    expect(html).toContain("async-badge settled");
  });

  it("keeps a member's badge live while its TaskStop is still in flight", () => {
    // Arrange — a resultless TaskStop has not confirmed the stop yet.
    const w = watcher("bg1");
    // Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), watcherPanelsWithChildren([w], [taskStop("bg1", { result: undefined })]));
    // Assert — the live dot, not a monitoring line, carries the in-bubble signal.
    expect(html).toContain("agent-running");
    expect(html).not.toContain("async-badge settled");
  });

  it("keeps a member's badge live when its TaskStop errored", () => {
    // Arrange — a failed stop must not settle the member.
    const w = watcher("bg1");
    // Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), watcherPanelsWithChildren([w], [taskStop("bg1", { result: { isError: true, content: "no such task" } })]));
    // Assert
    expect(html).toContain("agent-running");
    expect(html).not.toContain("async-badge settled");
  });

  it("catalogs a non-final host bubble too (an interrupted turn's survivors)", () => {
    // Arrange — no finals (no chip), but the projection still hosts a live member here.
    const html = renderItem(text("b1"), undefined, undefined, watcherPanels([watcher()]));
    // Assert
    expect(html).toContain("async-catalog");
    expect(html).toContain(`data-panel-toggle="member:b1:w1"`);
  });

  it("renders a member's polled tail when the store streamed none", () => {
    // Arrange — a background agent has no store taskOutput, only the poll.
    const panels: PanelContext = {
      children: new Map(),
      isOpen: () => true,
      watchers: new Map([["b1", [watcher("bg1")]]]),
      taskTail: (id) =>
        id === "bg1" ? { text: "polled line", offset: 11, done: false, elapsedMs: 3000 } : undefined,
    };
    // Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), panels);
    // Assert
    expect(html).toContain("polled line");
    expect(html).toContain("face-elapsed");
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
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), panels);
    // Assert
    expect(html).toContain("streamed line");
    expect(html).not.toContain("polled line");
  });

  it("shows the member's live token spend on its badge", () => {
    // Arrange — a detached agent whose polled transcript meters 12,340 tokens.
    const agent = watcher("ag1", {
      toolName: "Agent",
      result: { isError: false, content: "Async agent launched. agentId: ag1, output_file: /tmp/claude-1/s/tasks/ag1.output" },
      asyncSource: {
        source_id: "ag1",
        kind: "agent",
        status: "running",
        stream: { transport: "poll", format: "jsonl-transcript" },
      },
    });
    const tail = `{"type":"assistant","message":{"usage":{"output_tokens":12340},"content":[]}}`;
    const panels: PanelContext = {
      children: new Map(),
      isOpen: () => false,
      watchers: new Map([["b1", [agent]]]),
      taskTail: (id) =>
        id === "ag1" ? { text: tail, offset: 1, done: false, elapsedMs: 0 } : undefined,
    };
    // Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), panels);
    // Assert — the collapsed pill carries the compact figure.
    expect(html).toContain(`<span class="async-badge-tokens">12k tok</span>`);
  });

  it("shows no token figure on a badge whose stream meters none", () => {
    // Arrange — a shell spool carries no usage records.
    const html = renderItem(
      text("b1"),
      undefined,
      finalsClosing(text("b1")),
      watcherPanels([watcher("bg1", { taskOutput: "bytes" })]),
    );
    // Assert
    expect(html).not.toContain("async-badge-tokens");
  });
});

describe("async-quiescence border (the invariant)", () => {
  it("amber-borders a bubble with a live async member instead of the green final-response", () => {
    // Arrange + Act — a live member makes the bubble amber, not green.
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), watcherPanels([watcher()]));
    // Assert
    expect(html).toContain(`class="bubble assistant md async-live"`);
    expect(html).not.toContain("final-response");
  });

  it("flips the border to green once every async member settles (amber → green quiescence)", () => {
    // Arrange — the member's notification settles it.
    const w = watcher("bg1", { notification: { taskId: "bg1", text: "<task-notification/>" } });
    // Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), watcherPanels([w]));
    // Assert
    expect(html).toContain("final-response");
    expect(html).not.toContain("async-live");
  });

  it("enumerates every live member as a selectable badge — the invariant the amber border promises", () => {
    // Arrange + Act — the live member that causes the amber border MUST appear as a badge.
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), watcherPanels([watcher()]));
    // Assert
    expect(html).toContain("async-live");
    expect(html).toContain(`data-panel-toggle="member:b1:w1"`);
  });

  it("amber-borders a prompt bubble hosting a tools-only turn's live async", () => {
    // Arrange — the projection hosts the survivor on the user-turn's request id (r1).
    const panels: PanelContext = {
      children: new Map(),
      isOpen: () => false,
      watchers: new Map([["r1", [watcher()]]]),
    };
    // Act
    const html = renderItem(userTurnAt(9, 0), undefined, undefined, panels);
    // Assert
    expect(html).toContain("bubble user async-live");
    expect(html).toContain(`data-panel-toggle="member:r1:w1"`);
  });
});

// --- gns-sockets fold (in a final-response bubble) ------------------------------

/** A gns-sockets bridge respawn spawn card, the fold's typical content. */
function bridgeSpawnItem(): ToolItem {
  return {
    kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
    toolUseId: "g1",
    messageId: "m1",
    toolName: "Agent",
    inputJson: `{"subagent_type":"sockets-listener"}`,
    input: { subagent_type: "sockets-listener", description: "gns-sockets bridge" },
    inputDone: true,
    result: { isError: false, content: "Async agent launched successfully. agentId: abc1" },
  };
}

/** A PanelContext folding ITEMS into block b1, open per OPEN. */
function gnsPanels(items: ConversationItem[], open = false): PanelContext {
  return {
    children: new Map(),
    isOpen: () => open,
    gnsFolds: new Map([["b1", items]]),
  };
}

describe("gns-sockets fold", () => {
  it("renders no fold on a final bubble no bridge upkeep attached to", () => {
    // Arrange + Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), gnsPanels([]));
    // Assert
    expect(html).not.toContain("gns-fold");
  });

  it("shows the ticker face and none of the body while closed", () => {
    // Arrange + Act
    const html = renderItem(
      text("b1"),
      undefined,
      finalsClosing(text("b1")),
      gnsPanels([bridgeSpawnItem()]),
    );
    // Assert
    expect(html).toContain("gns-fold");
    expect(html).toContain("gns-sockets bridge · 1 step");
    expect(html).not.toContain("agent-panel");
  });

  it("reveals the folded child feed when open", () => {
    // Arrange — a spawn card plus the acknowledgment bubble it folded with.
    const ack: ConversationItem = {
      kind: "text",
      blockId: "ack",
      messageId: "m2",
      text: "Bridge respawned in the background.",
      done: true,
      ts: "2026-05-24T10:00:00.000Z",
    };
    // Act
    const html = renderItem(
      text("b1"),
      undefined,
      finalsClosing(text("b1")),
      gnsPanels([bridgeSpawnItem(), ack], true),
    );
    // Assert
    expect(html).toContain("agent-panel");
    expect(html).toContain("feed-child");
    expect(html).toContain("Bridge respawned in the background.");
  });

  it("never folds bridge upkeep into a non-final commentary bubble", () => {
    // Arrange — no finals, so the bubble carries no closing chip.
    const html = renderItem(text("b1"), undefined, undefined, gnsPanels([bridgeSpawnItem()]));
    // Assert
    expect(html).not.toContain("gns-fold");
  });

  it("keys the catalog badge beside the gns fold without colliding", () => {
    // Arrange — one bubble carrying both surfaces, only the gns fold open.
    const panels: PanelContext = {
      children: new Map(),
      isOpen: (id) => id === "gns:b1",
      watchers: new Map([["b1", [watcher()]]]),
      gnsFolds: new Map([["b1", [bridgeSpawnItem()]]]),
    };
    // Act
    const html = renderItem(text("b1"), undefined, finalsClosing(text("b1")), panels);
    // Assert — the member badge stays closed while the gns fold opens: the
    // Bash watcher's card (the badge's detail) never mounts, while the gns
    // panel and its folded Agent card do.
    expect(html).toContain(`data-panel-toggle="member:b1:w1"`);
    expect(html).toContain(`data-panel-toggle="gns:b1"`);
    expect(html).not.toContain("tool-bash");
    expect(html).toContain("agent-panel");
  });
});

describe("panelToggleTarget", () => {
  /** Fake element: contains() answers from an explicit descendant set. */
  function el(name: string, descendants: string[] = []): { name: string; contains(n: { name: string }): boolean } {
    const inside = new Set(descendants);
    return { name, contains: (n) => inside.has(n.name) };
  }

  it("flips the fold when the click lands outside any panel", () => {
    // Arrange
    const toggle = el("fold");
    // Act + Assert
    expect(panelToggleTarget(toggle, null)).toBe(toggle);
  });

  it("leaves a click inside the toggle's own panel alone", () => {
    // Arrange
    const panel = el("panel");
    const toggle = el("fold", ["panel"]);
    // Act + Assert
    expect(panelToggleTarget(toggle, panel)).toBeNull();
  });

  it("flips a nested fold whose ticker sits inside an ancestor's panel", () => {
    // Arrange — the ancestor's panel contains the nested toggle, never the reverse.
    const outerPanel = el("outer-panel", ["nested-fold"]);
    const nested = el("nested-fold");
    // Act + Assert
    expect(panelToggleTarget(nested, outerPanel)).toBe(nested);
  });

  it("does nothing when no toggle is above the click", () => {
    // Arrange + Act + Assert
    expect(panelToggleTarget(null, el("panel"))).toBeNull();
  });
});

describe("unsupported slash-command card", () => {
  function refusal(resultText: string | undefined): ResultItem {
    return {
      kind: "result",
      subtype: "success",
      durationMs: 1,
      sincePrevFinalMs: 1,
      numTurns: 1,
      totalCostUsd: 0,
      usage: { input_tokens: 0, output_tokens: 0 },
      isError: false,
      resultText,
      context: null,
    };
  }

  /** A PanelContext carrying only the support state a card reads. */
  function ctx(over: Partial<PanelContext> = {}): PanelContext {
    return {
      children: new Map(),
      isOpen: () => false,
      canAddSupport: true,
      ...over,
    };
  }

  it("replaces the result chip with the card for a refused command", () => {
    // Arrange + Act
    const html = renderItem(
      refusal("/status isn't available in this environment."),
      undefined,
      undefined,
      ctx(),
    );
    // Assert
    expect(html).toContain("unsupported");
    expect(html).not.toContain(`class="result`);
  });

  it("offers the button naming the refused command", () => {
    // Arrange + Act
    const html = renderItem(
      refusal("/status isn't available in this environment."),
      undefined,
      undefined,
      ctx(),
    );
    // Assert
    expect(html).toContain(`data-add-support="status"`);
    expect(html).toContain("Create workspace to add support");
  });

  it("still renders the ordinary chip for a turn that answered normally", () => {
    // Arrange + Act
    const html = renderItem(refusal("all done"), undefined, undefined, ctx());
    // Assert
    expect(html).toContain(`class="result`);
    expect(html).not.toContain("data-add-support");
  });

  it("still renders the ordinary chip for a turn with no result text", () => {
    // Arrange + Act
    const html = renderItem(refusal(undefined), undefined, undefined, ctx());
    // Assert
    expect(html).toContain(`class="result`);
  });

  it("withholds the button when no daemon backs the webapp", () => {
    // Arrange + Act — a button that cannot ask anything is worse than none.
    const html = renderItem(
      refusal("/status isn't available in this environment."),
      undefined,
      undefined,
      ctx({ canAddSupport: false }),
    );
    // Assert
    expect(html).toContain("unsupported");
    expect(html).not.toContain("data-add-support");
  });

  it("disables the button while the ask is in flight", () => {
    // Arrange + Act
    const html = renderItem(
      refusal("/status isn't available in this environment."),
      undefined,
      undefined,
      ctx({ supportPhases: new Map([["status", { kind: "asking" }]]) }),
    );
    // Assert
    expect(html).toContain("disabled");
    expect(html).toContain("Asking Emacs");
  });

  it("names the requested workspace once the ask landed", () => {
    // Arrange + Act
    const html = renderItem(
      refusal("/status isn't available in this environment."),
      undefined,
      undefined,
      ctx({ supportPhases: new Map([["status", { kind: "asked", workspace: "add-support-status" }]]) }),
    );
    // Assert
    expect(html).toContain("add-support-status");
    expect(html).not.toContain("data-add-support");
  });

  it("surfaces a failed ask rather than silently reoffering", () => {
    // Arrange + Act
    const html = renderItem(
      refusal("/status isn't available in this environment."),
      undefined,
      undefined,
      ctx({ supportPhases: new Map([["status", { kind: "failed", error: "404 nope" }]]) }),
    );
    // Assert
    expect(html).toContain("Asking failed");
    expect(html).toContain("404 nope");
  });

  it("survives a finals pass that would swallow an ordinary chip", () => {
    // Arrange — swallowing would drop the button along with the refusal.
    const closer = refusal("/status isn't available in this environment.");
    const finals = { swallowed: new Set([closer]), chips: new Map() } as unknown as ReturnType<
      typeof finalResponses
    >;
    // Act
    const html = renderItem(closer, undefined, finals, ctx());
    // Assert
    expect(html).toContain("data-add-support");
  });

  it("escapes a command name into the button attribute", () => {
    // Arrange + Act — the name reaches an HTML attribute, so it is escaped.
    const html = renderItem(
      refusal("/plugin:cmd isn't available in this environment."),
      undefined,
      undefined,
      ctx(),
    );
    // Assert
    expect(html).toContain(`data-add-support="plugin:cmd"`);
  });

  it("renders the rich status panel for a refused /status when one is wired", () => {
    // Arrange + Act — the whole point of this workspace: /status stops being
    // dead weight and becomes a real panel.
    const html = renderItem(
      refusal("/status isn't available in this environment."),
      undefined,
      undefined,
      ctx({ statusCard: `<div class="status-panel">STATUS PANEL</div>` }),
    );
    // Assert
    expect(html).toContain("STATUS PANEL");
    expect(html).not.toContain("data-add-support");
  });

  it("falls back to the add-support card for /status when no panel is wired", () => {
    // Arrange + Act — a webapp with no status support (no getStatus action)
    // still offers to build it, exactly as for any other refused command.
    const html = renderItem(
      refusal("/status isn't available in this environment."),
      undefined,
      undefined,
      ctx({ statusCard: undefined }),
    );
    // Assert
    expect(html).toContain(`data-add-support="status"`);
  });

  it("ignores the status panel for a non-status refusal", () => {
    // Arrange + Act — the panel is /status-specific; another refused command
    // never renders it.
    const html = renderItem(
      refusal("/foo isn't available in this environment."),
      undefined,
      undefined,
      ctx({ statusCard: `<div class="status-panel">STATUS PANEL</div>` }),
    );
    // Assert
    expect(html).toContain(`data-add-support="foo"`);
    expect(html).not.toContain("STATUS PANEL");
  });
});

/**
 * The nav tokens the renderer stamps on each feed-item wrapper, which are
 * the anchors the keyboard cycle (nav.ts) walks. Kept honest here rather
 * than in nav.test.ts because finality is a RENDER-time derivation: only
 * this module knows which text block closes a turn.
 */
describe("navTokensForEntry", () => {
  /** The entry a single item renders as, with its finality resolved. */
  const entryFor = (items: ConversationItem[], index = 0) => {
    const entries = groupFeed(items);
    return { entry: entries[index], finals: finalResponses(items) };
  };

  it("stamps a user turn as a prompt stop", () => {
    // Arrange
    const { entry, finals } = entryFor([userTurnAt(9, 0)]);
    // Act
    const tokens = navTokensForEntry(entry, finals);
    // Assert
    expect(tokens).toBe("prompt");
  });

  it("stamps a turn-closing text block as the turn's final response", () => {
    // Arrange
    const items = [userTurnAt(9, 0), text("b1"), result("success")];
    const { entry, finals } = entryFor(items, 1);
    // Act
    const tokens = navTokensForEntry(entry, finals);
    // Assert
    expect(tokens).toBe("response final");
  });

  it("withholds the final token from an aborted turn's last text block", () => {
    // Arrange: an aborted turn never produced a final response, so its
    // trailing commentary must not become a stop on the response cycle.
    const items = [userTurnAt(9, 0), text("b1"), result("error_during_execution")];
    const { entry, finals } = entryFor(items, 1);
    // Act
    const tokens = navTokensForEntry(entry, finals);
    // Assert
    expect(tokens).toBe("response");
  });

  it("withholds the final token from a still-streaming text block", () => {
    // Arrange: no result has landed, so the turn has no final response yet.
    const items = [userTurnAt(9, 0), text("b1", false)];
    const { entry, finals } = entryFor(items, 1);
    // Act
    const tokens = navTokensForEntry(entry, finals);
    // Assert
    expect(tokens).toBe("response");
  });

  it("stamps a tool card as a tool stop", () => {
    // Arrange
    const { entry, finals } = entryFor([bash("t1")]);
    // Act
    const tokens = navTokensForEntry(entry, finals);
    // Assert
    expect(tokens).toBe("tool");
  });

  it("stamps a whole consecutive-run tab group as one tool stop", () => {
    // Arrange: a run of same-tool calls renders as a single tab group, so
    // the cycle must stop on it once rather than once per member.
    const { entry, finals } = entryFor([bash("t1"), bash("t2")]);
    // Act
    const tokens = navTokensForEntry(entry, finals);
    // Assert
    expect(tokens).toBe("tool");
  });

  it("leaves a thinking block off every cycle", () => {
    // Arrange
    const { entry, finals } = entryFor([thinking("k1")]);
    // Act
    const tokens = navTokensForEntry(entry, finals);
    // Assert
    expect(tokens).toBe("");
  });
});

/**
 * render() reuses mounted nodes in place and, before the reorder pass, mounted
 * a NEW node with a bare appendChild — correct only while every new entry is
 * the newest item (the live-stream invariant). A gap-fill revisit (§2.10)
 * reconciles a whole backlog burst at once, where a batched entry can belong
 * above an already-mounted node; it must slot at its feed rank, not the tail,
 * so the revisited feed matches the live one (and renderRestored's rebuild).
 */
describe("FeedRenderer: reconcile honors feed order on a batched revisit", () => {
  const NOOP_ACTIONS: Actions = {
    decidePermission() {},
    answerQuestions() {},
    cancelQueued() {},
    runQueuedNow() {},
    acceptQueued() {},
  };

  function mount(): { container: HTMLElement; feed: FeedRenderer } {
    const container = document.createElement("div");
    return { container, feed: new FeedRenderer(container, NOOP_ACTIONS) };
  }

  function stateOf(items: ConversationItem[]): StoreState {
    const state = new ConversationStore().state;
    state.items = items;
    return state;
  }

  const keysInDom = (container: HTMLElement): string[] =>
    [...container.querySelectorAll<HTMLElement>(".feed-item[data-key]")].map(
      (el) => el.dataset.key ?? "",
    );

  it("slots a batched entry above an already-mounted node instead of at the tail", () => {
    // Arrange — first paint mounts the text bubble alone (the state before a
    // background turn streamed a tool call in above it).
    const { container, feed } = mount();
    feed.render(stateOf([userTurnAt(9, 0), text("b1")]));
    // Act — a gap-fill revisit reconciles the burst at once: the tool now
    // precedes the text in feed order while the text node is already mounted.
    feed.render(stateOf([userTurnAt(9, 0), bash("t1"), text("b1")]));
    // Assert — the new tool node lands ABOVE the mounted text, not beneath it.
    const keys = keysInDom(container);
    expect(keys.indexOf("tool:t1")).toBeLessThan(keys.indexOf("text:b1"));
  });

  it("still appends a genuinely-newest tail item beneath the mounted feed", () => {
    // Arrange
    const { container, feed } = mount();
    feed.render(stateOf([userTurnAt(9, 0), text("b1")]));
    // Act — the live path: the new item is the newest, so it belongs last.
    feed.render(stateOf([userTurnAt(9, 0), text("b1"), bash("t1")]));
    // Assert
    const keys = keysInDom(container);
    expect(keys.indexOf("text:b1")).toBeLessThan(keys.indexOf("tool:t1"));
  });
});


/**
 * The `monitoring…` signal moved from a feed-tail row to the topbar strip's
 * left-most datapoint: the feed renderer no longer appends a row, it computes
 * the gate (`showsMonitoringRow`) at the tail of every render and banks it, and
 * the chrome paint reads it back through `isMonitoring` (see main.ts).
 */
describe("FeedRenderer.isMonitoring (topbar monitoring datapoint gate)", () => {
  const NOOP_ACTIONS: Actions = {
    decidePermission() {},
    answerQuestions() {},
    cancelQueued() {},
    runQueuedNow() {},
    acceptQueued() {},
  };

  const mountFeed = (): FeedRenderer =>
    new FeedRenderer(document.createElement("div"), NOOP_ACTIONS);

  /** An idle session (no turn in flight) carrying a live background watcher. */
  function watchingState(over: Partial<StoreState> = {}): StoreState {
    const state = new ConversationStore().state;
    state.items = [userTurnAt(9, 0), text("b1"), watcher()];
    state.turnInFlight = false;
    return { ...state, ...over };
  }

  it("defaults to not monitoring before the first render", () => {
    // Arrange / Act — a fresh renderer has partitioned no feed yet, so a chrome
    // paint landing before the first feed render claims nothing to monitor.
    const feed = mountFeed();
    // Assert
    expect(feed.isMonitoring()).toBe(false);
  });

  it("reports monitoring once an idle session's render finds live async", () => {
    // Arrange
    const feed = mountFeed();
    // Act — the quiescent-but-still-watching case the strip's amber datapoint speaks for.
    feed.render(watchingState());
    // Assert
    expect(feed.isMonitoring()).toBe(true);
  });

  it("reports not monitoring while a turn is in flight, ceding to the working row", () => {
    // Arrange — the same live watcher, but the main chain is active.
    const feed = mountFeed();
    // Act
    feed.render(watchingState({ turnInFlight: true }));
    // Assert
    expect(feed.isMonitoring()).toBe(false);
  });

  it("reports not monitoring once the render finds no live async left", () => {
    // Arrange — an idle session whose only content has already settled.
    const feed = mountFeed();
    // Act — no watcher among the items, so nothing to monitor.
    const state = new ConversationStore().state;
    state.items = [userTurnAt(9, 0), text("b1")];
    state.turnInFlight = false;
    feed.render(state);
    // Assert
    expect(feed.isMonitoring()).toBe(false);
  });

  it("reports monitoring after a fresh-join renderRestored finds live async", () => {
    // Arrange — the restored path computes the same gate as the live render, so
    // a fresh join landing on a still-watching session lights the strip at once.
    const feed = mountFeed();
    // Act
    feed.renderRestored(watchingState());
    // Assert
    expect(feed.isMonitoring()).toBe(true);
  });

  it("never appends a monitoring row to the feed itself", () => {
    // Arrange — the signal lives in the topbar strip now, not the feed tail.
    const container = document.createElement("div");
    const feed = new FeedRenderer(container, NOOP_ACTIONS);
    // Act
    feed.render(watchingState());
    // Assert — no leftover tail node carries the old row.
    expect(container.querySelector('[data-key="monitoring"]')).toBeNull();
    expect(container.innerHTML).not.toContain("monitoring-pending");
  });
});

describe("panelSeedsOnOpen", () => {
  it("seeds a member badge's stream fold open alongside the badge", () => {
    // Arrange / Act / Assert — one open, not two, reaches the stream.
    expect(panelSeedsOnOpen("member:b1:w1")).toEqual(["async:w1"]);
  });

  it("splits the tool use id from the right, leaving the host's shape alone", () => {
    // Arrange — a host id carrying its own colon.
    expect(panelSeedsOnOpen("member:req:1:w9")).toEqual(["async:w9"]);
  });

  it("seeds nothing for a non-member toggle", () => {
    // Arrange / Act / Assert
    expect(panelSeedsOnOpen("async:t1")).toEqual([]);
    expect(panelSeedsOnOpen("watchers:b1")).toEqual([]);
  });
});

/**
 * The wslog helper prepends its own `HH:MM:SS.mmm ` client stamp to every line
 * (wslog.test.ts owns that contract). These suites assert on the MESSAGE, so
 * they record it stamp-free rather than pinning a wall clock.
 */
const CLIENT_STAMP = /^\d{2}:\d{2}:\d{2}\.\d{3} /;

describe("off-enum status/kind logging", () => {
  function spyLines(): string[] {
    const lines: string[] = [];
    setLogger(
      new ForwardingLogger(
        () => true,
        (level, line) => lines.push(`${level}: ${line.replace(CLIENT_STAMP, "")}`),
      ),
    );
    return lines;
  }
  afterEach(() => resetLoggingForTests());

  it("memberBadge logs and renders empty for an off-enum status", () => {
    // Arrange — a status value outside MemberStatus (a newer daemon's addition).
    const lines = spyLines();
    // Act
    const html = memberBadge("teleporting" as unknown as Parameters<typeof memberBadge>[0]);
    // Assert
    expect(html).toBe("");
    expect(lines).toContain("warn: memberBadge: unexpected status teleporting");
  });

  it("memberBadge dedups a repeated off-enum status", () => {
    // Arrange
    const lines = spyLines();
    const bad = "teleporting" as unknown as Parameters<typeof memberBadge>[0];
    // Act
    memberBadge(bad);
    memberBadge(bad);
    // Assert — one line despite two calls (hot render path).
    expect(lines.filter((l) => l.includes("memberBadge"))).toHaveLength(1);
  });

});

describe("FeedRenderer.render/renderRestored: logs then rethrows a mid-reconcile throw", () => {
  const NOOP_ACTIONS: Actions = {
    decidePermission() {},
    answerQuestions() {},
    cancelQueued() {},
    runQueuedNow() {},
    acceptQueued() {},
  };

  function spyLines(): string[] {
    const lines: string[] = [];
    setLogger(
      new ForwardingLogger(
        () => true,
        (level, line) => lines.push(`${level}: ${line.replace(CLIENT_STAMP, "")}`),
      ),
    );
    return lines;
  }
  afterEach(() => resetLoggingForTests());

  // A chess-game marker with a supported extension: hydrateChessGames reaches
  // its "config === null" throw (this suite never calls configureChessGames)
  // instead of the earlier "unsupported game file type" one — the same
  // synchronous throw source the coalescer sees in production.
  const CHESS_MARKER =
    "---> agent-repl-chess-game-file: /ws/.claude/emacs/cee-web-widget/chess-game-ab.pgn <---";
  const THROW_MSG = "Error: chess-game hydrator is not configured";

  /** A session whose only response is an unconfigured chess-game marker. */
  function throwingState(): StoreState {
    const state = new ConversationStore().state;
    state.items = [
      userTurnAt(9, 0),
      { kind: "text", blockId: "b1", messageId: "m1", text: CHESS_MARKER, done: true, ts: TEXT_TS },
    ];
    return state;
  }

  /** A session with nothing that trips hydrateChessGames. */
  function cleanState(): StoreState {
    const state = new ConversationStore().state;
    state.items = [userTurnAt(9, 0), text("b1")];
    return state;
  }

  it("render() logs once via feed-render and still throws", () => {
    // Arrange
    const lines = spyLines();
    const feed = new FeedRenderer(document.createElement("div"), NOOP_ACTIONS);
    // Act / Assert — the throw propagates unchanged...
    expect(() => feed.render(throwingState())).toThrow("chess-game hydrator is not configured");
    // ...and the daemon-log evidence is there too.
    expect(lines).toContain(`error: ${THROW_MSG}`);
  });

  it("render() dedups a repeated identical throw (hot rAF path)", () => {
    // Arrange — the coalescer calls render() again next frame with no fix in between.
    const lines = spyLines();
    const feed = new FeedRenderer(document.createElement("div"), NOOP_ACTIONS);
    // Act
    expect(() => feed.render(throwingState())).toThrow();
    expect(() => feed.render(throwingState())).toThrow();
    // Assert — one line despite two throws.
    expect(lines.filter((l) => l === `error: ${THROW_MSG}`)).toHaveLength(1);
  });

  it("render() re-arms feed-render after a clean render, so a later recurrence logs again", () => {
    // Arrange
    const lines = spyLines();
    const feed = new FeedRenderer(document.createElement("div"), NOOP_ACTIONS);
    // Act
    expect(() => feed.render(throwingState())).toThrow();
    feed.render(cleanState()); // completes without throwing — clears the dedup key
    expect(() => feed.render(throwingState())).toThrow();
    // Assert — both throws logged, since the clean render re-armed the key.
    expect(lines.filter((l) => l === `error: ${THROW_MSG}`)).toHaveLength(2);
  });

  it("renderRestored() logs once via feed-render-restored and still throws", () => {
    // Arrange
    const lines = spyLines();
    const feed = new FeedRenderer(document.createElement("div"), NOOP_ACTIONS);
    // Act / Assert
    expect(() => feed.renderRestored(throwingState())).toThrow(
      "chess-game hydrator is not configured",
    );
    expect(lines).toContain(`error: ${THROW_MSG}`);
  });

  it("renderRestored() re-arms feed-render-restored after a clean render", () => {
    // Arrange
    const lines = spyLines();
    const feed = new FeedRenderer(document.createElement("div"), NOOP_ACTIONS);
    // Act
    expect(() => feed.renderRestored(throwingState())).toThrow();
    feed.renderRestored(cleanState());
    expect(() => feed.renderRestored(throwingState())).toThrow();
    // Assert
    expect(lines.filter((l) => l === `error: ${THROW_MSG}`)).toHaveLength(2);
  });
});

describe("a heartbeat alone puts elapsed on a plain running tool (MEDIUM)", () => {
  it("renders the elapsed element for a running tool whose only signal is a heartbeat", () => {
    // Arrange — a slow Bash still in flight: no result, no async source, no
    // spawned tasks, no children, no tail. Only HeartbeatProgress's elapsed,
    // which is exactly the case the heartbeat was added for.
    const item = { ...tool(), progressElapsedS: 7 } as ConversationItem;
    // Act
    const html = renderItem(item);
    // Assert
    expect(html).toContain("face-elapsed");
    expect(html).toContain("7s");
  });

  it("renders no elapsed element for the same tool without a heartbeat", () => {
    // Arrange / Act — the contrast case, so the assertion above cannot pass
    // for some unrelated reason.
    const html = renderItem(tool());
    // Assert
    expect(html).not.toContain("face-elapsed");
  });
});

// --- the system-failure card (F4) --------------------------------------------
//
// ErrorBanner and RetryBadge had NO render test of any kind. Their replacement
// gets one per edge case, because it is now the only place a user learns why a
// workspace changed color.

/** A daemon-classified failure, defaulted to an open API failure. */
function failure(over: Partial<SystemFailureCard> = {}): SystemFailureCard {
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

describe("the system-failure card", () => {
  it("renders the daemon's message", () => {
    // Arrange / Act
    const html = renderItem(failure());
    // Assert
    expect(html).toContain("the API is overloaded");
  });

  it("shows the raw account beside the prose rather than instead of it", () => {
    // Arrange / Act — the structured evidence is for whoever debugs this.
    const html = renderItem(failure());
    // Assert
    expect(html).toContain("status=529");
  });

  it("omits the detail block when the source gave none", () => {
    // Arrange / Act
    const html = renderItem(failure({ sourceDetail: "" }));
    // Assert
    expect(html).not.toContain("failure-detail");
  });

  it("takes the API class's color", () => {
    // Arrange / Act — a vendor block, which resolves the workspace purple.
    const html = renderItem(failure({ errorClass: "API" }));
    // Assert
    expect(html).toContain("failure-api");
  });

  it("takes the INTERNAL class's color", () => {
    // Arrange / Act — our own machinery, which resolves the workspace blue.
    const html = renderItem(failure({ errorClass: "INTERNAL", errorType: "shim.degraded" }));
    // Assert
    expect(html).toContain("failure-internal");
  });

  it("marks an OPEN failure with the alarm glyph", () => {
    // Arrange / Act
    const html = renderItem(failure());
    // Assert
    expect(html).toContain("✕");
  });

  it("marks a RESOLVED failure with a check instead", () => {
    // Arrange / Act — the window ended; a card still shouting would be lying
    // about the present to be accurate about the past.
    const html = renderItem(failure({ resolvedAtMs: Date.parse("2026-05-24T09:05:00Z") }));
    // Assert
    expect(html).toContain("✓");
  });

  it("adds the settled class to a resolved failure", () => {
    // Arrange / Act
    const html = renderItem(failure({ resolvedAtMs: Date.parse("2026-05-24T09:05:00Z") }));
    // Assert
    expect(html).toContain("resolved");
  });

  it("stamps the time a resolved window closed", () => {
    // Arrange / Act
    const html = renderItem(failure({ resolvedAtMs: Date.parse("2026-05-24T09:05:00Z") }));
    // Assert
    expect(html).toContain("failure-resolved");
  });

  it("carries the error type as data, so a test or a stylesheet can key on it", () => {
    // Arrange / Act
    const html = renderItem(failure({ errorType: "shim.rejected" }));
    // Assert
    expect(html).toContain('data-error-type="shim.rejected"');
  });

  it("escapes the message", () => {
    // Arrange / Act
    const html = renderItem(failure({ message: "<img src=x>" }));
    // Assert
    expect(html).not.toContain("<img");
  });

  it("escapes the source detail", () => {
    // Arrange / Act
    const html = renderItem(failure({ sourceDetail: "<script>x</script>" }));
    // Assert
    expect(html).not.toContain("<script>");
  });
});

describe("failure card identity", () => {
  it("keys a failure by its uuid so a resolution replaces its own alarm", () => {
    // Arrange / Act — an index key would strand the two edges on separate
    // DOM nodes, leaving the alarm standing beside its own all-clear.
    const open = itemKey(failure(), 3);
    const closed = itemKey(failure({ resolvedAtMs: 123 }), 7);
    // Assert
    expect(open).toBe(closed);
  });

  it("gives two DIFFERENT failures different keys", () => {
    // Arrange / Act
    const a = itemKey(failure({ uuid: "failure:e1" }), 0);
    const b = itemKey(failure({ uuid: "failure:e2" }), 0);
    // Assert
    expect(a).not.toBe(b);
  });
});

/**
 * The prompt round-trip's last receipt: a render that draws a user turn the
 * previous render had not seen logs one line, carrying whether the turn ranks
 * at the feed tail — the position a just-sent prompt must land at.
 */
describe("FeedRenderer: fresh user turn logs a rendering receipt", () => {
  const NOOP_ACTIONS: Actions = {
    decidePermission() {},
    answerQuestions() {},
    cancelQueued() {},
    runQueuedNow() {},
    acceptQueued() {},
  };

  function mount(): FeedRenderer {
    return new FeedRenderer(document.createElement("div"), NOOP_ACTIONS);
  }

  function stateOf(items: ConversationItem[]): StoreState {
    const state = new ConversationStore().state;
    state.items = items;
    return state;
  }

  function recordLines(): string[] {
    const lines: string[] = [];
    setLogger(
      new ForwardingLogger(
        () => true,
        (level, l) => lines.push(`${level}: ${l.replace(CLIENT_STAMP, "")}`),
      ),
    );
    return lines;
  }

  afterEach(() => resetLoggingForTests());

  it("logs last=true when the fresh turn is the feed tail", () => {
    // Arrange
    const lines = recordLines();
    const feed = mount();
    // Act
    feed.render(stateOf([text("b1"), userTurnAt(9, 0)]));
    // Assert
    expect(lines).toContain("info: feed: user turn rendering request_id=r1 last=true");
  });

  it("logs last=false when items rank below the fresh turn", () => {
    // Arrange — the misorder case this receipt exists to catch.
    const lines = recordLines();
    const feed = mount();
    // Act
    feed.render(stateOf([userTurnAt(9, 0), text("b1")]));
    // Assert
    expect(lines).toContain("info: feed: user turn rendering request_id=r1 last=false");
  });

  it("does not re-log an already-seen turn on the next render", () => {
    // Arrange
    const lines = recordLines();
    const feed = mount();
    const state = stateOf([userTurnAt(9, 0)]);
    feed.render(state);
    const after = lines.length;
    // Act
    feed.render(state);
    // Assert
    expect(lines.length).toBe(after);
  });
});

/**
 * The feed's TRUNCATION at a clear or a compaction: everything above the
 * event leaves the screen, not merely the agent's context.
 *
 * The daemon floors every REPLAY at the newest of the two, so a fresh
 * subscription arrives already truncated. These cover the LIVE case, which is
 * why the client-side slice survives at all: an event landing over items this
 * end has already drawn must still wipe them.
 */
describe("FeedRenderer: a clear or a compaction truncates the feed", () => {
  const NOOP_ACTIONS: Actions = {
    decidePermission() {},
    answerQuestions() {},
    cancelQueued() {},
    runQueuedNow() {},
    acceptQueued() {},
  };

  function mount(): { container: HTMLElement; feed: FeedRenderer } {
    const container = document.createElement("div");
    return { container, feed: new FeedRenderer(container, NOOP_ACTIONS) };
  }

  function stateOf(items: ConversationItem[]): StoreState {
    const state = new ConversationStore().state;
    state.items = items;
    return state;
  }

  const cleared: ContextClearedItem = { kind: "context-cleared", uuid: "c1" };

  /** A compaction item, the other truncating event. */
  function compaction(uuid = "k1"): ContextCompactedItem {
    return {
      kind: "context-compacted",
      uuid,
      trigger: "auto",
      preTokens: 287028,
      postTokens: 9001,
      durationMs: 4200,
      summary: "the story so far",
      result: "success",
      error: "",
    };
  }

  it("drops the items above a clear on a live render", () => {
    // Arrange
    const { container, feed } = mount();
    // Act
    feed.render(stateOf([userTurnAt(9, 0, "the old question"), cleared]));
    // Assert
    expect(container.innerHTML).not.toContain("the old question");
  });

  it("drops the items above a compaction on a live render", () => {
    // Arrange
    const { container, feed } = mount();
    // Act
    feed.render(stateOf([userTurnAt(9, 0, "the old question"), compaction()]));
    // Assert
    expect(container.innerHTML).not.toContain("the old question");
  });

  it("wipes items ALREADY DRAWN when the clear arrives live over them", () => {
    // Arrange — the live case the client-side truncation exists for.
    const { container, feed } = mount();
    feed.render(stateOf([userTurnAt(9, 0, "the old question")]));
    // Act
    feed.render(stateOf([userTurnAt(9, 0, "the old question"), cleared]));
    // Assert
    expect(container.innerHTML).not.toContain("the old question");
  });

  it("wipes items ALREADY DRAWN when the compaction arrives live over them", () => {
    // Arrange
    const { container, feed } = mount();
    feed.render(stateOf([userTurnAt(9, 0, "the old question")]));
    // Act
    feed.render(stateOf([userTurnAt(9, 0, "the old question"), compaction()]));
    // Assert
    expect(container.innerHTML).not.toContain("the old question");
  });

  it("draws the red rule the clear brought with it", () => {
    // Arrange
    const { container, feed } = mount();
    // Act
    feed.render(stateOf([userTurnAt(9, 0), cleared]));
    // Assert
    expect(container.innerHTML).toContain("clear-divider");
  });

  it("draws the orange rule and the summary bubble the compaction brought", () => {
    // Arrange
    const { container, feed } = mount();
    // Act
    feed.render(stateOf([userTurnAt(9, 0), compaction()]));
    // Assert
    expect(container.innerHTML).toContain("compact-rule");
    expect(container.innerHTML).toContain("compact-summary");
  });

  it("keeps the turns BELOW the clear", () => {
    // Arrange
    const { container, feed } = mount();
    // Act
    feed.render(stateOf([userTurnAt(9, 0, "the old question"), cleared, userTurnAt(9, 1, "the new question")]));
    // Assert
    expect(container.innerHTML).toContain("the new question");
  });

  it("keeps the turns BELOW the compaction", () => {
    // Arrange
    const { container, feed } = mount();
    // Act
    feed.render(
      stateOf([userTurnAt(9, 0, "the old question"), compaction(), userTurnAt(9, 1, "the new question")]),
    );
    // Assert
    expect(container.innerHTML).toContain("the new question");
  });

  it("lets the LATER of two events win when both sit in one feed", () => {
    // Arrange — a clear, then a compaction; only the compaction's survivors draw.
    const { container, feed } = mount();
    // Act
    feed.render(
      stateOf([
        cleared,
        userTurnAt(9, 1, "the middle question"),
        compaction(),
        userTurnAt(9, 2, "the new question"),
      ]),
    );
    // Assert
    expect(container.innerHTML).not.toContain("the middle question");
    expect(container.innerHTML).toContain("the new question");
  });

  it("keeps the whole feed when neither event ever happened", () => {
    // Arrange
    const { container, feed } = mount();
    // Act
    feed.render(stateOf([userTurnAt(9, 0, "the only question")]));
    // Assert
    expect(container.innerHTML).toContain("the only question");
  });

  it("truncates a RESTORED render at the clear too", () => {
    // Arrange
    const { container, feed } = mount();
    // Act
    feed.renderRestored(stateOf([userTurnAt(9, 0, "the old question"), cleared]));
    // Assert
    expect(container.innerHTML).not.toContain("the old question");
  });

  it("truncates a RESTORED render at the compaction too", () => {
    // Arrange
    const { container, feed } = mount();
    // Act
    feed.renderRestored(stateOf([userTurnAt(9, 0, "the old question"), compaction()]));
    // Assert
    expect(container.innerHTML).not.toContain("the old question");
  });

  it("keys the clear's node by uuid, so it survives the rebuild it triggers", () => {
    // Arrange + Act + Assert
    expect(itemKey(cleared, 3)).toBe("context-cleared:c1");
  });

  it("keys the compaction's node by uuid too", () => {
    // Arrange + Act + Assert
    expect(itemKey(compaction("k7"), 3)).toBe("context-compacted:k7");
  });
});
