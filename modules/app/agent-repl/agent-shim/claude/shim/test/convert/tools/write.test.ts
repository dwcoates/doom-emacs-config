/**
 * The write converter. Two things can go silently wrong here: the create/update
 * distinction (a creation drawn as a rewrite of nothing) and the diff (a card
 * showing the whole file instead of the change), so both are asserted
 * literally, the creation case against the real corpus result.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import { writeConverter } from "../../../src/convert/tools/write.js";
import { toolProgress } from "../../../src/convert/entries.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";

const AGENT = create(conversationv1.AgentIdSchema, { value: "session-1" });

function corpusResult(name: string): Record<string, unknown> {
  const path = fileURLToPath(
    new URL(`../../../../../../testdata/corpus/tool-results/${name}.jsonl`, import.meta.url),
  );
  const line = readFileSync(path, "utf8").split("\n").find((entry) => entry.trim() !== "");
  return (JSON.parse(line as string) as { toolUseResult: Record<string, unknown> }).toolUseResult;
}

function call(input: Record<string, unknown>): PendingCall {
  return {
    toolUseId: "toolu_write",
    toolName: "Write",
    input,
    startedAtMs: 1_700_000_000_000,
    agentId: AGENT,
  };
}

function outcome(structured: unknown, isError = false): ToolOutcome {
  return { content: undefined, isError, structured, settledAtMs: 1_700_000_001_000 };
}

function successOf(item: ReturnType<typeof writeConverter.settle>): conversationv1.AgentWriteSuccess {
  return (item?.value as conversationv1.AgentWrite).result.value as conversationv1.AgentWriteSuccess;
}

describe("writeConverter.start", () => {
  it("announces the path and says NOTHING about create versus update", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/new.go", content: "package main\n" });

    // Act.
    const item = writeConverter.start(pending);

    // Assert.
    const start = (item?.value as conversationv1.AgentWrite).result
      .value as conversationv1.AgentWriteStart;
    expect(start.path?.path).toBe("/tmp/new.go");
    expect(start.startedAt?.atMs).toBe(1_700_000_000_000n);
  });

  it("produces NO message when the call named no path", () => {
    // Arrange, Act.
    const item = writeConverter.start(call({ content: "x" }));

    // Assert.
    expect(item?.case).toBeUndefined();
  });
});

describe("writeConverter.settle", () => {
  it("is CREATED for the corpus create, whose whole content is one addition hunk", () => {
    // Arrange.
    const result = corpusResult("write");
    const pending = call({ file_path: result["filePath"], content: result["content"] });

    // Act.
    const success = successOf(writeConverter.settle(pending, outcome(result)));

    // Assert.
    expect(success.outcome.case).toBe("created");
    expect(success.patch).toHaveLength(1);
    expect(success.patch[0]?.lines.every((line) => line.startsWith("+"))).toBe(true);
  });

  it("is UPDATED when the vendor said the file already existed", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt", content: "one\nTWO\nthree\n" });
    const result = {
      type: "update",
      filePath: "/tmp/a.txt",
      content: "one\nTWO\nthree\n",
      originalFile: "one\ntwo\nthree\n",
      structuredPatch: [],
    };

    // Act.
    const success = successOf(writeConverter.settle(pending, outcome(result)));

    // Assert.
    expect(success.outcome.case).toBe("updated");
  });

  it("DIFFS the two versions itself: one hunk holding the changed line only", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt", content: "one\nTWO\nthree\n" });
    const result = {
      type: "update",
      filePath: "/tmp/a.txt",
      content: "one\nTWO\nthree\n",
      originalFile: "one\ntwo\nthree\n",
      structuredPatch: [],
    };

    // Act.
    const success = successOf(writeConverter.settle(pending, outcome(result)));

    // Assert. The hunk ends at "three": both versions end with a terminating
    // newline, and a terminator is not a line. This previously expected a
    // further, EMPTY context row for it -- the same terminator that drew a
    // created file as a blank second addition.
    expect(success.patch[0]?.lines).toEqual([" one", "-two", "+TWO", " three"]);
  });

  it("carries user_modified when the user altered the content at the gate", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt", content: "b\n" });
    const result = {
      type: "update",
      filePath: "/tmp/a.txt",
      content: "b\n",
      originalFile: "a\n",
      userModified: true,
    };

    // Act.
    const success = successOf(writeConverter.settle(pending, outcome(result)));

    // Assert.
    expect(success.userModified).toBe(true);
  });

  it("produces NO frame when the vendor stated neither create nor update", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt", content: "b\n" });

    // Act.
    const item = writeConverter.settle(
      pending,
      outcome({ filePath: "/tmp/a.txt", content: "b\n", originalFile: null }),
    );

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when the vendor stated no written content to diff", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt", content: "b\n" });

    // Act.
    const item = writeConverter.settle(pending, outcome({ type: "create", filePath: "/tmp/a.txt" }));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when a write settled with no typed output at all", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.txt", content: "x" });

    // Act.
    const item = writeConverter.settle(pending, outcome("File created successfully"));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when neither the result nor the call names a path", () => {
    // Arrange.
    const pending = call({ content: "x" });

    // Act.
    const item = writeConverter.settle(pending, outcome({ type: "create", content: "x" }));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("carries the failure arm when the vendor marked the result an error", () => {
    // Arrange, Act.
    const item = writeConverter.settle(call({ file_path: "/tmp/a.txt" }), outcome("nope", true));

    // Assert.
    const write = item?.value as conversationv1.AgentWrite;
    expect(write.result.case).toBe("failure");
    expect(
      (write.result.value as conversationv1.AgentWriteFailure).error?.settledAt?.atMs,
    ).toBe(1_700_000_001_000n);
  });

  it("restates the requested path on the failure arm, so the settled frame stands alone", () => {
    // Arrange, Act.
    const item = writeConverter.settle(call({ file_path: "/tmp/a.txt" }), outcome("nope", true));

    // Assert.
    const write = item?.value as conversationv1.AgentWrite;
    expect((write.result.value as conversationv1.AgentWriteFailure).path?.path).toBe("/tmp/a.txt");
  });

  it("produces NO failure frame for a call that named no path, which had no start either", () => {
    // Arrange, Act, Assert.
    expect(writeConverter.settle(call({}), outcome("nope", true))).toBeUndefined();
  });
});

describe("writeConverter.progress", () => {
  it("relays the vendor's beat on the write's own progress arm", () => {
    // Arrange, Act.
    const item = writeConverter.progress?.(toolProgress(1_700_000_000_500));

    // Assert.
    expect((item?.value as conversationv1.AgentWrite).result.case).toBe("progress");
  });
});
