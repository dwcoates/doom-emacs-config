import { describe, expect, it } from "vitest";
import { mergeChildren, transcriptFeed } from "../src/subfeed.js";
import { ConversationItem } from "../src/store.js";

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
