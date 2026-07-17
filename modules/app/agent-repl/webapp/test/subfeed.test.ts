import { describe, expect, it } from "vitest";
import {
  SUBFEED_DEPTH_CAP,
  effectiveAsyncSource,
  mayNest,
  mergeChildren,
  transcriptFeed,
} from "../src/subfeed.js";
import { ConversationItem, ToolItem } from "../src/store.js";

/** One JSONL entry of a subagent transcript. */
function txLine(o: unknown): string {
  return JSON.stringify(o);
}

/** A transcript whose TaskCreate settles to #7 and is then updated. */
const taskTranscript = [
  txLine({
    type: "assistant",
    message: {
      id: "m1",
      content: [{ type: "tool_use", id: "tc1", name: "TaskCreate", input: { subject: "fix the bug" } }],
    },
  }),
  txLine({
    type: "user",
    message: {
      id: "m2",
      content: [{ type: "tool_result", tool_use_id: "tc1", content: "Task #7 created successfully: fix the bug" }],
    },
  }),
  txLine({
    type: "assistant",
    message: {
      id: "m3",
      content: [{ type: "tool_use", id: "tu1", name: "TaskUpdate", input: { taskId: "7", status: "completed" } }],
    },
  }),
].join("\n");

/** A minimal text item for populating fake child maps. */
function textItem(blockId: string): ConversationItem {
  return { kind: "text", blockId, messageId: "m", text: "hi", done: true, ts: "" };
}

describe("transcriptFeed", () => {
  it("confines a task-addressed call inside its create, exactly like the live feed", () => {
    // Act
    const feed = transcriptFeed(taskTranscript);
    // Assert — the update leaves the top level and lands under tc1.
    expect(feed.top.map((i) => (i.kind === "tool" ? i.toolUseId : ""))).toEqual(["tc1"]);
    expect(feed.children.get("tc1")?.length).toBe(1);
  });

  it("keeps an orphan claim top-level when the transcript window missed its create", () => {
    // Arrange — only the update line survives a mid-file tail read.
    const orphan = txLine({
      type: "assistant",
      message: {
        id: "m3",
        content: [{ type: "tool_use", id: "tu1", name: "TaskUpdate", input: { taskId: "7", status: "completed" } }],
      },
    });
    // Act
    const feed = transcriptFeed(orphan);
    // Assert
    expect(feed.top.length).toBe(1);
    expect(feed.children.size).toBe(0);
  });

  it("reports what the parse cap dropped", () => {
    // Arrange
    const lines = Array.from({ length: 5 }, (_, i) =>
      txLine({ type: "assistant", message: { id: `m${i}`, content: [{ type: "text", text: `t${i}` }] } }),
    ).join("\n");
    // Act
    const feed = transcriptFeed(lines, 2);
    // Assert
    expect(feed.dropped).toBe(3);
    expect(feed.top.length).toBe(2);
  });
});

describe("mergeChildren", () => {
  it("keeps every base entry while adding the extras", () => {
    // Arrange
    const base = new Map([["a", [textItem("b1")]]]);
    const extra = new Map([["b", [textItem("b2")]]]);
    // Act
    const merged = mergeChildren(base, extra);
    // Assert
    expect(merged.get("a")?.length).toBe(1);
    expect(merged.get("b")?.length).toBe(1);
  });

  it("returns base unchanged when there is nothing to add", () => {
    // Arrange
    const base = new Map([["a", [textItem("b1")]]]);
    // Act + Assert
    expect(mergeChildren(base, new Map())).toBe(base);
  });

  it("lets the fresher parse win a same-id collision", () => {
    // Arrange
    const base = new Map([["a", [textItem("stale")]]]);
    const fresh = [textItem("fresh")];
    // Act
    const merged = mergeChildren(base, new Map([["a", fresh]]));
    // Assert
    expect(merged.get("a")).toBe(fresh);
  });
});

/** A settled tool call whose result announced the given text. */
function spawnCard(toolName: string, resultText: string, over: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
    toolUseId: "t1",
    messageId: "m1",
    toolName,
    ts: "",
    inputJson: "{}",
    input: {},
    inputDone: true,
    result: { isError: false, content: resultText },
    ...over,
  };
}

describe("effectiveAsyncSource", () => {
  it("passes the daemon's classification through untouched", () => {
    // Arrange
    const classified = spawnCard("Agent", "launched", {
      asyncSource: { source_id: "a1", kind: "agent", status: "running" },
    });
    // Act + Assert
    expect(effectiveAsyncSource(classified)).toBe(classified.asyncSource);
  });

  it("synthesizes a poll transcript source for an Agent spawn", () => {
    // Act
    const source = effectiveAsyncSource(spawnCard("Agent", "Async agent launched. agentId: a7"));
    // Assert
    expect(source).toMatchObject({
      source_id: "a7",
      kind: "agent",
      status: "running",
      stream: { transport: "poll", format: "jsonl-transcript" },
    });
  });

  it("synthesizes a journal source for a Workflow spawn", () => {
    // Act
    const source = effectiveAsyncSource(spawnCard("Workflow", "Workflow started. Task ID: wf1"));
    // Assert
    expect(source).toMatchObject({
      source_id: "wf1",
      kind: "workflow",
      stream: { transport: "poll", format: "jsonl-journal" },
    });
  });

  it("synthesizes a raw text source for a backgrounded shell", () => {
    // Act
    const source = effectiveAsyncSource(
      spawnCard("Bash", "Command running in background with ID: bg1"),
    );
    // Assert
    expect(source).toMatchObject({
      source_id: "bg1",
      kind: "shell",
      stream: { transport: "poll", format: "text" },
    });
  });

  it("returns undefined for a call that spawned nothing detached", () => {
    // Act + Assert
    expect(effectiveAsyncSource(spawnCard("Bash", "all done"))).toBeUndefined();
  });

  it("labels the synthetic source with the call's description", () => {
    // Arrange
    const card = spawnCard("Agent", "agentId: a7", { input: { description: "watch the queue" } });
    // Act + Assert
    expect(effectiveAsyncSource(card)?.label).toBe("watch the queue");
  });

  it("settles the synthetic status once the completion notification lands", () => {
    // Arrange
    const card = spawnCard("Bash", "with ID: bg1", {
      notification: { taskId: "bg1", text: "done" },
    });
    // Act + Assert
    expect(effectiveAsyncSource(card)?.status).toBe("done");
  });
});

describe("mayNest", () => {
  it("allows a fresh source below the depth cap", () => {
    // Act + Assert
    expect(mayNest(1, new Set(["other"]), "a7")).toBe(true);
  });

  it("refuses at the depth cap", () => {
    // Act + Assert
    expect(mayNest(SUBFEED_DEPTH_CAP, undefined, "a7")).toBe(false);
  });

  it("refuses a source already rendering above, cutting the cycle", () => {
    // Act + Assert
    expect(mayNest(1, new Set(["a7"]), "a7")).toBe(false);
  });
});
