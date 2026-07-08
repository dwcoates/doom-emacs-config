import { describe, expect, it } from "vitest";
import { diffHtml, escapeHtml, itemKey, renderItem } from "../src/render.js";
import { ConversationItem, ToolItem } from "../src/store.js";

describe("escapeHtml", () => {
  it("escapes markup-significant characters", () => {
    // Arrange + Act + Assert
    expect(escapeHtml(`<b a="x">&`)).toBe("&lt;b a=&quot;x&quot;&gt;&amp;");
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
