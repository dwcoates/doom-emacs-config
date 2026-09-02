/**
 * The read converter's extent decision. The extent arm IS the claim about
 * whether more of the file exists, so a wrong one is not a crash — it is a card
 * that says a truncated file was read whole. Each arm is asserted separately,
 * and the range case is driven by the real corpus result.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import { readConverter } from "../../../src/convert/tools/read.js";
import { toolProgress } from "../../../src/convert/entries.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";

const AGENT = create(conversationv1.AgentIdSchema, { value: "session-1" });

/** The `toolUseResult` of the first line of a tool-results corpus file. */
function corpusResult(name: string): unknown {
  const path = fileURLToPath(
    new URL(`../../../../../../testdata/corpus/tool-results/${name}.jsonl`, import.meta.url),
  );
  const line = readFileSync(path, "utf8").split("\n").find((entry) => entry.trim() !== "");
  return (JSON.parse(line as string) as { toolUseResult: unknown }).toolUseResult;
}

function call(input: Record<string, unknown>): PendingCall {
  return {
    toolUseId: "toolu_read",
    toolName: "Read",
    input,
    startedAtMs: 1_700_000_000_000,
    agentId: AGENT,
  };
}

function outcome(structured: unknown, isError = false): ToolOutcome {
  return { content: undefined, isError, structured, settledAtMs: 1_700_000_001_000 };
}

/** A `text` FileReadOutput with the fields a case needs. */
function textOutput(file: Record<string, unknown>): unknown {
  return { type: "text", file };
}

describe("readConverter.start", () => {
  it("announces the path the CALLER named", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt" });

    // Act.
    const item = readConverter.start(pending);

    // Assert.
    expect(item.case).toBe("read");
    const read = item.value as conversationv1.AgentRead;
    expect(read.result.case).toBe("start");
    const start = read.result.value as conversationv1.AgentReadStart;
    expect(start.path?.path).toBe("/tmp/a.txt");
    expect(start.startedAt?.atMs).toBe(1_700_000_000_000n);
  });

  it("produces NO message when the call named no path", () => {
    // Arrange.
    const pending = call({});

    // Act.
    const item = readConverter.start(pending);

    // Assert.
    expect(item.case).toBeUndefined();
  });
});

describe("readConverter.settle", () => {
  it("is WHOLE when nothing was asked for and nothing was left out", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt" });
    const result = textOutput({
      filePath: "/tmp/a.txt",
      content: "one\ntwo",
      numLines: 2,
      startLine: 1,
      totalLines: 2,
    });

    // Act.
    const item = readConverter.settle(pending, outcome(result));

    // Assert.
    const success = (item?.value as conversationv1.AgentRead).result
      .value as conversationv1.AgentReadSuccess;
    expect(success.extent.case).toBe("whole");
    expect((success.extent.value as conversationv1.AgentReadWhole).contents).toBe("one\ntwo");
    expect(success.settledAt?.atMs).toBe(1_700_000_001_000n);
  });

  it("is a RANGE when the caller asked for an offset — the real corpus read", () => {
    // Arrange.
    const pending = call({
      file_path:
        "/Users/dodgecoates/.config/doom-worktrees/agent-repl-startup-sync/modules/app/agent-repl/frontends.el",
      offset: 289,
      limit: 56,
    });

    // Act.
    const item = readConverter.settle(pending, outcome(corpusResult("read")));

    // Assert.
    const success = (item?.value as conversationv1.AgentRead).result
      .value as conversationv1.AgentReadSuccess;
    expect(success.extent.case).toBe("range");
    const range = success.extent.value as conversationv1.AgentReadRange;
    expect(range.firstLine).toBe(289);
    expect(range.lineCount).toBe(56);
    expect(range.totalLines).toBe(344);
  });

  it("is a HEAD cut at the TOKEN cap when the vendor auto-paginated", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/big.txt" });
    const result = textOutput({
      filePath: "/tmp/big.txt",
      content: "lead",
      numLines: 100,
      startLine: 1,
      totalLines: 9_000,
      truncatedByTokenCap: true,
    });

    // Act.
    const item = readConverter.settle(pending, outcome(result));

    // Assert.
    const success = (item?.value as conversationv1.AgentRead).result
      .value as conversationv1.AgentReadSuccess;
    const head = success.extent.value as conversationv1.AgentReadHead;
    expect(success.extent.case).toBe("head");
    expect(head.cut.case).toBe("tokenCap");
    expect(head.totalLines).toBe(9_000);
  });

  it("is a HEAD cut at the LINE cap when a limit stopped a read at line 1", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/big.txt", limit: 10 });
    const result = textOutput({
      filePath: "/tmp/big.txt",
      content: "lead",
      numLines: 10,
      startLine: 1,
      totalLines: 500,
    });

    // Act.
    const item = readConverter.settle(pending, outcome(result));

    // Assert.
    const success = (item?.value as conversationv1.AgentRead).result
      .value as conversationv1.AgentReadSuccess;
    const head = success.extent.value as conversationv1.AgentReadHead;
    expect(success.extent.case).toBe("head");
    expect(head.cut.case).toBe("lineCap");
  });

  it("produces NO frame for an IMAGE read, whose extent arm is retired", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/shot.png" });

    // Act.
    const item = readConverter.settle(pending, outcome(corpusResult("read-image")));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when a token-capped read stated no total line count", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/big.txt" });
    const result = textOutput({
      filePath: "/tmp/big.txt",
      content: "lead",
      startLine: 1,
      truncatedByTokenCap: true,
    });

    // Act.
    const item = readConverter.settle(pending, outcome(result));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when an offset read stated no slice bounds", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt", offset: 10 });
    const result = textOutput({ filePath: "/tmp/a.txt", content: "slice" });

    // Act.
    const item = readConverter.settle(pending, outcome(result));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("carries the failure arm when the vendor marked the result an error", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt" });

    // Act.
    const item = readConverter.settle(pending, outcome("Error: no such file", true));

    // Assert.
    const read = item?.value as conversationv1.AgentRead;
    expect(read.result.case).toBe("failure");
    const failure = read.result.value as conversationv1.AgentReadFailure;
    expect(failure.error?.settledAt?.atMs).toBe(1_700_000_001_000n);
    expect(failure.error?.content).toBeUndefined();
  });
});

describe("readConverter.progress", () => {
  it("relays the vendor's beat on the read's own progress arm", () => {
    // Arrange, Act.
    const item = readConverter.progress?.(toolProgress(1_700_000_000_500));

    // Assert.
    const read = item?.value as conversationv1.AgentRead;
    expect(read.result.case).toBe("progress");
    expect((read.result.value as conversationv1.AgentToolCallProgress).lastProgressAtMs).toBe(
      1_700_000_000_500n,
    );
  });
});
