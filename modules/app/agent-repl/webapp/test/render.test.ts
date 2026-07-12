import { describe, expect, it } from "vitest";
import {
  backfillChunks,
  diffHtml,
  formatTurnTime,
  itemKey,
  renderItem,
  sessionInfoHtml,
} from "../src/render.js";
import { ConversationItem, ResultItem, ToolItem } from "../src/store.js";

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
    const html = sessionInfoHtml("my-feature", "claude-fable-5", null);
    // Assert
    expect(html).toContain(`parent workspace: <span class="info-ws">my-feature</span>`);
  });

  it("omits the parent workspace datapoint when parent_ws is absent", () => {
    // Arrange + Act
    const html = sessionInfoHtml(null, "claude-fable-5", null);
    // Assert — no dangling label or leading delimiter.
    expect(html).not.toContain("parent workspace");
    expect(html.startsWith("model:")).toBe(true);
  });

  it("omits the parent workspace datapoint when parent_ws is empty", () => {
    // Arrange + Act + Assert
    expect(sessionInfoHtml("", "claude-fable-5", null)).not.toContain("parent workspace");
  });

  it("escapes markup in the parent workspace name", () => {
    // Arrange + Act + Assert
    expect(sessionInfoHtml("<b>ws", "m", null)).not.toContain("<b>");
  });

  it("joins the datapoints with the dot separator", () => {
    // Arrange + Act
    const html = sessionInfoHtml("ws", "m", null);
    // Assert
    expect(html).toContain("</span> · model:");
    expect(html).toContain("</span> · tokens:");
  });

  it("omits the model datapoint before hello delivers one", () => {
    // Arrange + Act + Assert
    expect(sessionInfoHtml(null, "", null)).not.toContain("model:");
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
    expect(sessionInfoHtml(null, "m", usage)).toContain(
      `tokens: <span class="info-tokens">123,456</span>`,
    );
  });

  it("treats missing cache fields as zero", () => {
    // Arrange + Act + Assert
    expect(sessionInfoHtml(null, "m", { input_tokens: 5, output_tokens: 1 })).toContain(
      `tokens: <span class="info-tokens">5</span>`,
    );
  });

  it("shows zero tokens before any usage arrives", () => {
    // Arrange + Act + Assert
    expect(sessionInfoHtml(null, "m", null)).toContain(
      `tokens: <span class="info-tokens">0</span>`,
    );
  });

  it("no longer renders the in/out counter or the cost estimate", () => {
    // Arrange + Act
    const html = sessionInfoHtml("ws", "m", { input_tokens: 3, output_tokens: 7 });
    // Assert
    expect(html).not.toContain("in/");
    expect(html).not.toContain("out");
    expect(html).not.toContain("$");
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

  it("hides streaming partial input JSON behind a pulse indicator", () => {
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
    expect(html).toContain("tool-input-pending");
    expect(html).not.toContain("file_path");
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
    };
  }

  it("marks a successful turn's chip with the muted-yellow done class", () => {
    // Arrange + Act
    const html = renderItem(resultItem("success"));
    // Assert
    expect(html).toContain(`class="result ok done"`);
  });

  it("still labels a successful turn's chip 'turn complete'", () => {
    // Arrange + Act
    const html = renderItem(resultItem("success"));
    // Assert
    expect(html).toContain("turn complete");
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
    };
    // Act + Assert
    expect(itemKey(item, 5)).toBe("result:5");
  });
});

describe("renderItem tool previews", () => {
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
