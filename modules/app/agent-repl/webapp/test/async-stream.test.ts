import { describe, expect, it } from "vitest";
import {
  classifyAsyncSource,
  parseJournal,
  parseTranscript,
  transcriptStats,
  transcriptStatsCached,
} from "../src/async-stream.js";
import { TextItem, ToolItem } from "../src/store.js";

/** One JSONL line of a subagent transcript. */
function line(o: unknown): string {
  return JSON.stringify(o);
}

const assistantText = line({
  type: "assistant",
  timestamp: "2026-07-17T10:00:00.000Z",
  message: { id: "m1", role: "assistant", content: [{ type: "text", text: "found it" }] },
});
const assistantToolUse = line({
  type: "assistant",
  message: {
    id: "m2",
    role: "assistant",
    content: [{ type: "tool_use", id: "tu1", name: "Grep", input: { pattern: "x" } }],
  },
});
const userToolResult = line({
  type: "user",
  message: {
    role: "user",
    content: [{ type: "tool_result", tool_use_id: "tu1", content: "a.ts:1: x", is_error: false }],
  },
});

describe("parseTranscript", () => {
  it("decodes an assistant text entry into a text item, which is what makes it a bubble", () => {
    // Arrange / Act
    const { items } = parseTranscript(assistantText);
    // Assert
    expect(items).toHaveLength(1);
    expect(items[0]).toMatchObject({ kind: "text", text: "found it" });
  });

  it("carries the entry's timestamp onto the item, so the bubble dates itself", () => {
    // Arrange / Act
    const { items } = parseTranscript(assistantText);
    // Assert
    expect((items[0] as TextItem).ts).toBe("2026-07-17T10:00:00.000Z");
  });

  it("marks every parsed block done, since a transcript entry is a finished block", () => {
    // Arrange / Act
    const { items } = parseTranscript(assistantText);
    // Assert — a cursor on a settled block would lie.
    expect((items[0] as TextItem).done).toBe(true);
  });

  it("decodes a thinking entry into a thinking item", () => {
    // Arrange
    const raw = line({
      type: "assistant",
      message: { id: "m1", content: [{ type: "thinking", thinking: "hmm" }] },
    });
    // Act
    const { items } = parseTranscript(raw);
    // Assert
    expect(items[0]).toMatchObject({ kind: "thinking", text: "hmm" });
  });

  it("decodes a tool_use entry into a tool item", () => {
    // Arrange / Act
    const { items } = parseTranscript(assistantToolUse);
    // Assert
    expect(items[0]).toMatchObject({ kind: "tool", toolUseId: "tu1", toolName: "Grep" });
  });

  it("lands a tool_result on the call it answers", () => {
    // Arrange / Act
    const { items } = parseTranscript(`${assistantToolUse}\n${userToolResult}`);
    // Assert
    expect((items[0] as ToolItem).result).toMatchObject({ isError: false, content: "a.ts:1: x" });
  });

  it("skips a tool_result whose call this tail never saw", () => {
    // Arrange — a poll that began mid-transcript.
    // Act
    const { items } = parseTranscript(userToolResult);
    // Assert — no phantom card invented for it.
    expect(items).toEqual([]);
  });

  it("skips a truncated line rather than failing the whole parse", () => {
    // Arrange — a tail read routinely starts and ends mid-line.
    const raw = `{"type":"assistant","mess\n${assistantText}`;
    // Act
    const { items } = parseTranscript(raw);
    // Assert
    expect(items).toHaveLength(1);
  });

  it("drops an empty text block rather than rendering an empty bubble", () => {
    // Arrange
    const raw = line({ type: "assistant", message: { id: "m1", content: [{ type: "text", text: "" }] } });
    // Act
    const { items } = parseTranscript(raw);
    // Assert
    expect(items).toEqual([]);
  });

  it("keeps the tail when the stream exceeds the cap, since newest work is the interesting part", () => {
    // Arrange
    const raw = Array.from({ length: 5 }, (_, i) =>
      line({ type: "assistant", message: { id: `m${i}`, content: [{ type: "text", text: `t${i}` }] } }),
    ).join("\n");
    // Act
    const { items } = parseTranscript(raw, 2);
    // Assert
    expect(items.map((i) => (i as TextItem).text)).toEqual(["t3", "t4"]);
  });

  it("reports what the cap dropped rather than truncating silently", () => {
    // Arrange
    const raw = Array.from({ length: 5 }, (_, i) =>
      line({ type: "assistant", message: { id: `m${i}`, content: [{ type: "text", text: `t${i}` }] } }),
    ).join("\n");
    // Act
    const { dropped } = parseTranscript(raw, 2);
    // Assert
    expect(dropped).toBe(3);
  });

  it("reports nothing dropped when the whole stream fits", () => {
    // Arrange / Act
    const { dropped } = parseTranscript(assistantText, 10);
    // Assert
    expect(dropped).toBe(0);
  });

  it("gives distinct block ids to entries sharing a message id", () => {
    // Arrange — ids are synthesized per entry, and must not collide.
    const raw = `${assistantText}\n${assistantText}`;
    // Act
    const { items } = parseTranscript(raw);
    // Assert
    expect((items[0] as TextItem).blockId).not.toBe((items[1] as TextItem).blockId);
  });

  it("returns nothing for an empty stream", () => {
    // Arrange / Act
    const { items } = parseTranscript("");
    // Assert
    expect(items).toEqual([]);
  });
});

describe("parseJournal", () => {
  it("decodes a completed record into a done row", () => {
    // Arrange
    const raw = line({ label: "review:bugs", result: "3 findings" });
    // Act
    const { rows } = parseJournal(raw);
    // Assert
    expect(rows[0]).toEqual({ label: "review:bugs", detail: "3 findings", status: "done" });
  });

  it("decodes a record with no result yet as running", () => {
    // Arrange
    const raw = line({ label: "review:perf", prompt: "check hot paths" });
    // Act
    const { rows } = parseJournal(raw);
    // Assert
    expect(rows[0].status).toBe("running");
  });

  it("decodes an errored record as error, keeping the error as the detail", () => {
    // Arrange
    const raw = line({ label: "review:perf", error: "agent died" });
    // Act
    const { rows } = parseJournal(raw);
    // Assert
    expect(rows[0]).toEqual({ label: "review:perf", detail: "agent died", status: "error" });
  });

  it("skips a record naming no agent, which is not a step the fold can show", () => {
    // Arrange
    const raw = line({ note: "something else" });
    // Act
    const { rows } = parseJournal(raw);
    // Assert
    expect(rows).toEqual([]);
  });
});

/** An assistant transcript entry carrying an output-token usage figure. */
function usageMsg(tokens: number): string {
  return line({
    type: "assistant",
    message: { id: "m1", role: "assistant", usage: { output_tokens: tokens }, content: [] },
  });
}

describe("transcriptStats", () => {
  const resultLine = (o: Record<string, unknown> = {}): string =>
    line({ type: "result", subtype: "success", is_error: false, ...o });

  it("reads an unterminated stream as unfinished", () => {
    // Arrange / Act
    const stats = transcriptStats(usageMsg(5));
    // Assert
    expect(stats.finished).toBe(false);
  });

  it("reads the terminal result record as finished", () => {
    // Arrange / Act
    const stats = transcriptStats([usageMsg(5), resultLine()].join("\n"));
    // Assert
    expect(stats).toMatchObject({ finished: true, error: false });
  });

  it("reads a failing terminal record as an error", () => {
    // Arrange / Act
    const stats = transcriptStats(resultLine({ subtype: "error_during_execution" }));
    // Assert
    expect(stats).toMatchObject({ finished: true, error: true });
  });

  it("sums per-message usage into a live token figure", () => {
    // Arrange / Act
    const stats = transcriptStats([usageMsg(5), usageMsg(7)].join("\n"));
    // Assert
    expect(stats.outputTokens).toBe(12);
  });

  it("prefers the terminal record's authoritative total over the sum", () => {
    // Arrange
    const raw = [usageMsg(5), resultLine({ usage: { output_tokens: 40 } })].join("\n");
    // Act / Assert
    expect(transcriptStats(raw).outputTokens).toBe(40);
  });

  it("reports no token figure when no record carried usage", () => {
    // Arrange / Act
    const stats = transcriptStats(line({ type: "assistant", message: { content: [] } }));
    // Assert
    expect(stats.outputTokens).toBeUndefined();
  });

  it("treats a truncated terminal line as not yet finished", () => {
    // Arrange — the poll cut the result record mid-line.
    const raw = [usageMsg(5), resultLine().slice(0, 10)].join("\n");
    // Act / Assert
    expect(transcriptStats(raw).finished).toBe(false);
  });
});

describe("transcriptStatsCached", () => {
  it("rescans when the tail grows, so the figure tracks the stream", () => {
    // Arrange
    const first = usageMsg(5);
    const grown = [first, usageMsg(7)].join("\n");
    // Act
    transcriptStatsCached("src-grow", first);
    const stats = transcriptStatsCached("src-grow", grown);
    // Assert
    expect(stats.outputTokens).toBe(12);
  });

  it("reuses the scan while the tail is unchanged", () => {
    // Arrange
    const raw = usageMsg(5);
    // Act — same length, same source: the very object comes back.
    const a = transcriptStatsCached("src-same", raw);
    const b = transcriptStatsCached("src-same", raw);
    // Assert
    expect(b).toBe(a);
  });
});

describe("classifyAsyncSource (the daemon mirror)", () => {
  const agentSpawn = {
    isAsync: true,
    status: "async_launched",
    agentId: "ag1",
    description: "hunt bugs",
    outputFile: "/tmp/claude-1/s/tasks/ag1.output",
    canReadOutputFile: true,
  };

  it("classifies a detached agent spawn with a polled transcript stream", () => {
    // Arrange / Act
    const src = classifyAsyncSource("Agent", agentSpawn, false);
    // Assert
    expect(src).toMatchObject({
      source_id: "ag1",
      kind: "agent",
      status: "running",
      stream: { transport: "poll", format: "jsonl-transcript" },
    });
  });

  it("refuses a synchronous subagent, whose stream already arrived inline", () => {
    // Arrange — the completion sidecar of a sync agent carries no isAsync.
    const sync = { agentId: "ag1", status: "completed", totalTokens: 5 };
    // Act / Assert
    expect(classifyAsyncSource("Agent", sync, false)).toBeUndefined();
  });

  it("withholds the stream when the output file is unreadable", () => {
    // Arrange
    const spawn = { ...agentSpawn, canReadOutputFile: false };
    // Act
    const src = classifyAsyncSource("Agent", spawn, false);
    // Assert — the source is real but nothing may be read from it.
    expect(src?.stream).toBeUndefined();
  });

  it("classifies a backgrounded shell as a ws text stream", () => {
    // Arrange / Act
    const src = classifyAsyncSource("Bash", { backgroundTaskId: "bg1" }, false);
    // Assert
    expect(src).toMatchObject({
      source_id: "bg1",
      kind: "shell",
      stream: { transport: "ws", format: "text" },
    });
  });

  it("classifies a workflow spawn as a polled journal", () => {
    // Arrange
    const spawn = {
      status: "async_launched",
      taskId: "wj1",
      workflowName: "review",
      transcriptDir: "/cfg/projects/-s/subagents/workflows/wf_1",
    };
    // Act
    const src = classifyAsyncSource("Workflow", spawn, false);
    // Assert
    expect(src).toMatchObject({
      source_id: "wj1",
      kind: "workflow",
      label: "review",
      stream: { transport: "poll", format: "jsonl-journal" },
    });
  });

  it("refuses an errored spawn no matter what the payload says", () => {
    // Arrange / Act / Assert
    expect(classifyAsyncSource("Agent", agentSpawn, true)).toBeUndefined();
  });

  it("reads an unknown status as running rather than terminal", () => {
    // Arrange / Act
    const src = classifyAsyncSource("Agent", { ...agentSpawn, status: "reticulating" }, false);
    // Assert — a wrong done hides live output.
    expect(src?.status).toBe("running");
  });
});

describe("parseTranscript sidecar classification", () => {
  it("attaches the structural classification to a parsed spawn card", () => {
    // Arrange — a depth-two agent spawn with its toolUseResult sidecar.
    const spawn = line({
      type: "assistant",
      message: { id: "m1", content: [{ type: "tool_use", id: "tu1", name: "Agent", input: {} }] },
    });
    const result = line({
      type: "user",
      toolUseResult: { isAsync: true, agentId: "ag9", status: "async_launched" },
      message: {
        content: [{ type: "tool_result", tool_use_id: "tu1", content: "launched", is_error: false }],
      },
    });
    // Act
    const { items } = parseTranscript(`${spawn}\n${result}`);
    // Assert — the parsed card carries the same verdict a live card gets.
    expect((items[0] as ToolItem).asyncSource).toMatchObject({ source_id: "ag9", kind: "agent" });
  });

  it("leaves a sync agent's parsed card unclassified", () => {
    // Arrange — no isAsync in the sidecar.
    const spawn = line({
      type: "assistant",
      message: { id: "m1", content: [{ type: "tool_use", id: "tu1", name: "Agent", input: {} }] },
    });
    const result = line({
      type: "user",
      toolUseResult: { agentId: "ag9", status: "completed" },
      message: {
        content: [{ type: "tool_result", tool_use_id: "tu1", content: "done", is_error: false }],
      },
    });
    // Act
    const { items } = parseTranscript(`${spawn}\n${result}`);
    // Assert
    expect((items[0] as ToolItem).asyncSource).toBeUndefined();
  });
});
