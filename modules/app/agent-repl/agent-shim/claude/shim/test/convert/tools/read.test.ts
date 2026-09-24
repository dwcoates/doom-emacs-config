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
    expect(item?.case).toBe("read");
    const read = item?.value as conversationv1.AgentRead;
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
    expect(item?.case).toBeUndefined();
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

  it("settles an IMAGE read as a success with NO extent, whose arm is retired", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/shot.png" });

    // Act.
    const item = readConverter.settle(pending, outcome(corpusResult("read-image")));

    // Assert.
    const read = item?.value as conversationv1.AgentRead;
    expect(read.result.case).toBe("success");
    expect((read.result.value as conversationv1.AgentReadSuccess).extent.case).toBeUndefined();
  });

  it("states an image read's path from the CALLER, the only place it appears", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/shot.png" });

    // Act.
    const item = readConverter.settle(pending, outcome(corpusResult("read-image")));

    // Assert.
    const success = (item?.value as conversationv1.AgentRead).result
      .value as conversationv1.AgentReadSuccess;
    expect(success.path?.path).toBe("/tmp/shot.png");
  });

  it("states an image read's settle instant, so the card stops running", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/shot.png" });

    // Act.
    const item = readConverter.settle(pending, outcome(corpusResult("read-image")));

    // Assert.
    const success = (item?.value as conversationv1.AgentRead).result
      .value as conversationv1.AgentReadSuccess;
    expect(success.settledAt?.atMs).toBe(1_700_000_001_000n);
  });

  it("produces NO frame for a non-text read whose path is nowhere stated", () => {
    // Arrange.
    const pending = call({});

    // Act.
    const item = readConverter.settle(pending, outcome({ type: "image", file: {} }));

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

  it("produces NO frame when a text read carried no content at all", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt" });
    const result = textOutput({ filePath: "/tmp/a.txt", numLines: 2, startLine: 1, totalLines: 2 });

    // Act.
    const item = readConverter.settle(pending, outcome(result));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("is a RANGE when a short read began PAST line 1 with no offset asked", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt" });
    const result = textOutput({
      filePath: "/tmp/a.txt",
      content: "mid",
      numLines: 3,
      startLine: 40,
      totalLines: 500,
    });

    // Act.
    const item = readConverter.settle(pending, outcome(result));

    // Assert.
    const success = (item?.value as conversationv1.AgentRead).result
      .value as conversationv1.AgentReadSuccess;
    expect(success.extent.case).toBe("range");
    const range = success.extent.value as conversationv1.AgentReadRange;
    expect(range.firstLine).toBe(40);
    expect(range.lineCount).toBe(3);
    expect(range.totalLines).toBe(500);
  });

  it("is a HEAD cut at the LINE cap when a short read began at line 1 with NO limit asked", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt" });
    const result = textOutput({
      filePath: "/tmp/a.txt",
      content: "lead",
      numLines: 4,
      startLine: 1,
      totalLines: 90,
    });

    // Act.
    const item = readConverter.settle(pending, outcome(result));

    // Assert.
    const success = (item?.value as conversationv1.AgentRead).result
      .value as conversationv1.AgentReadSuccess;
    expect(success.extent.case).toBe("head");
    const head = success.extent.value as conversationv1.AgentReadHead;
    expect(head.cut.case).toBe("lineCap");
    expect(head.totalLines).toBe(90);
  });

  it("produces NO frame when a text read carried no file object", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt" });

    // Act.
    const item = readConverter.settle(pending, outcome({ type: "text" }));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when a text read states no path and the caller named none", () => {
    // Arrange.
    const pending = call({});

    // Act.
    const item = readConverter.settle(pending, outcome(textOutput({ content: "x" })));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when a read settled with no typed output at all", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt" });

    // Act.
    const item = readConverter.settle(pending, outcome("just prose"));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("settles a non-text read carrying NO file object on the CALLER's path", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/doc.pdf" });

    // Act.
    const item = readConverter.settle(pending, outcome({ type: "pdf" }));

    // Assert.
    const success = (item?.value as conversationv1.AgentRead).result
      .value as conversationv1.AgentReadSuccess;
    expect(success.path?.path).toBe("/tmp/doc.pdf");
    expect(success.extent.case).toBeUndefined();
  });

  it("settles a read whose type the vendor left UNSTATED as an extentless success", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/mystery.bin" });

    // Act.
    const item = readConverter.settle(pending, outcome({ file: { size: 12 } }));

    // Assert.
    const success = (item?.value as conversationv1.AgentRead).result
      .value as conversationv1.AgentReadSuccess;
    expect(success.path?.path).toBe("/tmp/mystery.bin");
    expect(success.extent.case).toBeUndefined();
  });

  it("produces NO frame when an UNSTATED-type read names no path anywhere", () => {
    // Arrange.
    const pending = call({});

    // Act.
    const item = readConverter.settle(pending, outcome({ file: { size: 12 } }));

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

  // THE SETTLED FRAME STANDS ALONE: the start it upserts over is gone once it
  // lands, so a replay of the failure alone must still name the file.
  it("restates the requested path on the failure arm", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt" });

    // Act.
    const item = readConverter.settle(pending, outcome("Error: no such file", true));

    // Assert.
    const failure = (item?.value as conversationv1.AgentRead).result.value as conversationv1.AgentReadFailure;
    expect(failure.path?.path).toBe("/tmp/a.txt");
  });

  it("produces NO failure frame for a call that named no path, which had no start either", () => {
    // Arrange, Act, Assert.
    expect(readConverter.settle(call({}), outcome("Error: file_path required", true))).toBeUndefined();
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
