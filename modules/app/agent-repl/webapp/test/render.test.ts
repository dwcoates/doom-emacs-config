import { describe, expect, it } from "vitest";
import {
  backfillChunks,
  diffHtml,
  finalResponseBlockIds,
  formatDuration,
  formatTurnTime,
  itemKey,
  lastUserTurnId,
  modelOptionsHtml,
  renderItem,
  repinsToTail,
  sessionInfoHtml,
} from "../src/render.js";
import { SubagentEntry } from "../src/agents.js";
import { META_CLOSE, META_OPEN } from "../src/meta.js";
import { ConversationItem, ResultItem, ToolItem } from "../src/store.js";

/** A settled roster entry for one spawned subagent. */
function subagentEntry(over: Partial<SubagentEntry> = {}): SubagentEntry {
  return {
    toolUseId: "t1",
    description: "hunt the flake",
    agentType: "Explore",
    status: "done",
    nested: false,
    ...over,
  };
}

/** A user-turn item whose prompt was sent at the given local wall-clock time. */
function userTurnAt(hour: number, minute: number, text = "do the thing"): ConversationItem {
  return {
    kind: "user-turn",
    requestId: "r1",
    content: [{ type: "text", text }],
    ts: new Date(2026, 4, 24, hour, minute).toISOString(),
  };
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
    expect(html.startsWith("tokens:")).toBe(true);
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

  it("sums input and cache tokens with thousands separators", () => {
    // Arrange
    const usage = {
      input_tokens: 1200,
      output_tokens: 5,
      cache_read_input_tokens: 100000,
      cache_creation_input_tokens: 22256,
    };
    // Act + Assert
    expect(sessionInfoHtml(null, usage)).toContain(
      `tokens: <span class="info-tokens">123,456</span>`,
    );
  });

  it("treats missing cache fields as zero", () => {
    // Arrange + Act + Assert
    expect(sessionInfoHtml(null, { input_tokens: 5, output_tokens: 1 })).toContain(
      `tokens: <span class="info-tokens">5</span>`,
    );
  });

  it("shows zero tokens before any usage arrives", () => {
    // Arrange + Act + Assert
    expect(sessionInfoHtml(null, null)).toContain(
      `tokens: <span class="info-tokens">0</span>`,
    );
  });

  it("no longer renders the in/out counter or the cost estimate", () => {
    // Arrange + Act
    const html = sessionInfoHtml("ws", { input_tokens: 3, output_tokens: 7 });
    // Assert
    expect(html).not.toContain("in/");
    expect(html).not.toContain("out");
    expect(html).not.toContain("$");
  });

  it("appends the subagent chip after the token datapoint", () => {
    // Arrange
    const agents = [subagentEntry()];
    // Act
    const html = sessionInfoHtml("ws", null, agents, false);
    // Assert
    expect(html).toContain("</span> · <span class=\"agents-menu\">");
  });

  it("counts the session's subagents on the chip", () => {
    // Arrange
    const agents = [subagentEntry(), subagentEntry({ toolUseId: "t2" })];
    // Act + Assert
    expect(sessionInfoHtml("ws", null, agents, false)).toContain("2 agents");
  });

  it("drops the subagent roster when the chip is open", () => {
    // Arrange + Act
    const html = sessionInfoHtml("ws", null, [subagentEntry()], true);
    // Assert
    expect(html).toContain("agents-overlay");
  });

  it("omits the subagent chip when the session spawned none", () => {
    // Arrange + Act + Assert
    expect(sessionInfoHtml("ws", null, [], false)).not.toContain("agents-menu");
  });

  it("leaves no dangling separator when the session spawned no subagents", () => {
    // Arrange + Act
    const html = sessionInfoHtml("ws", null, [], false);
    // Assert
    expect(html.endsWith("</span>")).toBe(true);
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
  it("stamps a user prompt bubble with its send time", () => {
    // Arrange
    const item = userTurnAt(14, 32);
    // Act + Assert
    expect(renderItem(item)).toContain(`<span class="turn-ts">14:32</span>`);
  });

  it("keeps the prompt text alongside its send-time stamp", () => {
    // Arrange
    const item = userTurnAt(14, 32, "do the thing");
    // Act
    const html = renderItem(item);
    // Assert — the stamp trails the prompt inside the same bubble.
    expect(html).toContain(`<div class="bubble user"><pre>do the thing</pre><span class="turn-ts">`);
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
      blockId: "b1",
      messageId: "m1",
      text: "done",
      done: true,
    };
    // Act
    const html = renderItem(item, undefined, true);
    // Assert
    expect(html).toContain(`class="bubble assistant md final-response"`);
  });

  it("withholds the green border from a text block that is not a final response", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "working on it",
      done: true,
    };
    // Act
    const html = renderItem(item, undefined, false);
    // Assert
    expect(html).not.toContain("final-response");
  });

  it("green-borders a final response rendered as a metaprompt tree", () => {
    // Arrange — the tree path builds its own bubble, so it needs the class too.
    const item: ConversationItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "Response (👀 no changes made)\n\n1 👀 Answer\n├── 1.1 First\n└── 1.2 Second",
      done: true,
    };
    // Act
    const html = renderItem(item, undefined, true);
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

  it("renders Skill as its launch line, not its input JSON", () => {
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
    // Assert
    expect(html).toContain("Launching skill: debug-logs");
    expect(html).not.toContain("tool-input");
  });

  it("omits the Skill args from the launch line", () => {
    // Arrange — the skill's prompt is already in the feed as the turn that asked for it.
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
    expect(renderItem(item)).not.toContain("emacs crashed twice");
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

  it("suppresses the successful Skill result, which only echoes the launch line", () => {
    // Arrange
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
    // Act + Assert — the launch line renders once (from the input), not twice.
    expect(renderItem(item)).not.toContain("tool-output");
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

  it("leaves the boundary rule off the system init the /clear produces", () => {
    // Arrange
    const item: ConversationItem = { kind: "system", subtype: "init" };
    // Act
    const html = renderItem(item);
    // Assert — the rule hangs off the prompt, so a plain session start has none.
    expect(html).not.toContain("clear-divider");
  });
});

describe("formatDuration", () => {
  it("keeps a sub-second duration in whole milliseconds", () => {
    // Arrange + Act + Assert — no finer unit exists to promote a fraction into.
    expect(formatDuration(850)).toBe("850ms");
  });

  it("reports a zero duration in milliseconds", () => {
    // Arrange + Act + Assert
    expect(formatDuration(0)).toBe("0ms");
  });

  it("carries a second's leftover in whole milliseconds", () => {
    // Arrange + Act + Assert — not the fractional 1.03s.
    expect(formatDuration(1033)).toBe("1s 33ms");
  });

  it("drops the leftover off a whole second count", () => {
    // Arrange + Act + Assert
    expect(formatDuration(1000)).toBe("1s");
  });

  it("promotes a millisecond count that rounds up to a full second", () => {
    // Arrange + Act + Assert — 999.6ms would otherwise render as 1000ms.
    expect(formatDuration(999.6)).toBe("1s");
  });

  it("carries a minute's leftover in whole seconds", () => {
    // Arrange + Act + Assert — not the fractional 5.5m.
    expect(formatDuration(330_000)).toBe("5m 30s");
  });

  it("rounds a minute's fractional second leftover to a whole second", () => {
    // Arrange + Act + Assert — 93.6s is 1m plus 33.6s.
    expect(formatDuration(93_600)).toBe("1m 34s");
  });

  it("drops the leftover off a whole minute count", () => {
    // Arrange + Act + Assert
    expect(formatDuration(120_000)).toBe("2m");
  });

  it("carries an hour's leftover in whole minutes", () => {
    // Arrange + Act + Assert — not the fractional 1.5h.
    expect(formatDuration(5_400_000)).toBe("1h 30m");
  });

  it("promotes a leftover that rounds up to a full major unit", () => {
    // Arrange + Act + Assert — 59m 59.999s renders as 1h, never 59m 60s.
    expect(formatDuration(3_599_999)).toBe("1h");
  });

  it("keeps a three-digit hour count in whole hours", () => {
    // Arrange + Act + Assert
    expect(formatDuration(360_000_000)).toBe("100h");
  });
});

describe("ResultChip", () => {
  /** A result frame item for the given subtype. */
  function resultItem(subtype: ResultItem["subtype"], isError = false): ResultItem {
    return {
      kind: "result",
      subtype,
      durationMs: 12,
      numTurns: 1,
      totalCostUsd: 0.5,
      usage: { input_tokens: 3, output_tokens: 4 },
      isError,
      context: { total: 300_000, delta: 100_000 },
    };
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

  it("labels an aborted turn's chip with its subtype", () => {
    // Arrange + Act — only a turn that ended some other way names how.
    const html = renderItem(resultItem("aborted"));
    // Assert
    expect(html).toContain("aborted · 12ms");
  });

  it("withholds the done class from an aborted turn's chip", () => {
    // Arrange — aborted is not an error, so its chip is .ok but not complete.
    const html = renderItem(resultItem("aborted"));
    // Assert
    expect(html).toContain(`class="result ok"`);
  });

  it("withholds the done class from a failed turn's chip", () => {
    // Arrange + Act
    const html = renderItem(resultItem("error_during_execution", true));
    // Assert
    expect(html).toContain(`class="result err"`);
  });
});

describe("finalResponseBlockIds", () => {
  /** A text block item carrying the given id. */
  function text(blockId: string): ConversationItem {
    return { kind: "text", blockId, messageId: "m1", text: "hi", done: true };
  }

  /** A result frame closing a turn with the given subtype. */
  function result(subtype: ResultItem["subtype"] = "success"): ConversationItem {
    return {
      kind: "result",
      subtype,
      durationMs: 12,
      numTurns: 1,
      totalCostUsd: 0.5,
      usage: { input_tokens: 3, output_tokens: 4 },
      isError: subtype === "error_during_execution",
      context: { total: 300_000, delta: 100_000 },
    };
  }

  /** A tool card item, standing in for work between two text blocks. */
  function tool(): ConversationItem {
    return {
      kind: "tool",
      toolUseId: "t1",
      messageId: "m1",
      toolName: "Bash",
      inputJson: "{}",
      input: {},
      inputDone: true,
    };
  }

  it("marks the last text block of a completed turn", () => {
    // Arrange
    const items = [userTurnAt(9, 0), text("b1"), result()];
    // Act
    const finals = finalResponseBlockIds(items);
    // Assert
    expect([...finals]).toEqual(["b1"]);
  });

  it("leaves a completed turn's earlier text block unmarked", () => {
    // Arrange — commentary, then a tool call, then the answer.
    const items = [userTurnAt(9, 0), text("b1"), tool(), text("b2"), result()];
    // Act
    const finals = finalResponseBlockIds(items);
    // Assert
    expect(finals.has("b1")).toBe(false);
  });

  it("marks the text block that follows a completed turn's last tool call", () => {
    // Arrange
    const items = [userTurnAt(9, 0), text("b1"), tool(), text("b2"), result()];
    // Act
    const finals = finalResponseBlockIds(items);
    // Assert
    expect(finals.has("b2")).toBe(true);
  });

  it("leaves a still-streaming turn's text unmarked until its result lands", () => {
    // Arrange — no result frame yet, so the next block could still continue it.
    const items = [userTurnAt(9, 0), text("b1")];
    // Act
    const finals = finalResponseBlockIds(items);
    // Assert
    expect(finals.size).toBe(0);
  });

  it("leaves an aborted turn's last text unmarked", () => {
    // Arrange — an interrupted turn never reached the answer it worked toward.
    const items = [userTurnAt(9, 0), text("b1"), result("aborted")];
    // Act
    const finals = finalResponseBlockIds(items);
    // Assert
    expect(finals.size).toBe(0);
  });

  it("leaves a failed turn's last text unmarked", () => {
    // Arrange
    const items = [userTurnAt(9, 0), text("b1"), result("error_during_execution")];
    // Act
    const finals = finalResponseBlockIds(items);
    // Assert
    expect(finals.size).toBe(0);
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
    const finals = finalResponseBlockIds(items);
    // Assert
    expect([...finals]).toEqual(["b1", "b2"]);
  });

  it("marks nothing for a completed turn that produced no text at all", () => {
    // Arrange — a turn that only ran tools.
    const items = [userTurnAt(9, 0), tool(), result()];
    // Act
    const finals = finalResponseBlockIds(items);
    // Assert
    expect(finals.size).toBe(0);
  });

  it("never lets a resultless turn's text be claimed by the next turn's result", () => {
    // Arrange — turn one never closed; turn two ran tools only and completed.
    const items = [userTurnAt(9, 0), text("b1"), userTurnAt(9, 5), tool(), result()];
    // Act
    const finals = finalResponseBlockIds(items);
    // Assert
    expect(finals.size).toBe(0);
  });
});

describe("itemKey", () => {
  it("keys block items by block id", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "text",
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

  it("falls back to the plain JSON dump for an Agent call carrying no description", () => {
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
    // Assert
    expect(html).toContain(`<pre class="tool-input">`);
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
      blockId: "b1",
      messageId: "m1",
      text: "Just **prose** here.\nSecond line.",
      done: true,
    };
    // Act + Assert
    expect(renderItem(item)).not.toContain("mp-tree");
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
