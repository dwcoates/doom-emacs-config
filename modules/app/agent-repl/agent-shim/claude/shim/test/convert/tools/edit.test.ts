/**
 * The edit converter. Unlike a write, the vendor already diffed this one, so
 * the assertion that matters is that its `structuredPatch` is CARRIED — line
 * numbers and markers intact — rather than re-derived from the old and new
 * strings.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import { editConverter } from "../../../src/convert/tools/edit.js";
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
    toolUseId: "toolu_edit",
    toolName: "Edit",
    input,
    startedAtMs: 1_700_000_000_000,
    agentId: AGENT,
  };
}

function outcome(structured: unknown, isError = false): ToolOutcome {
  return { content: undefined, isError, structured, settledAtMs: 1_700_000_001_000 };
}

function successOf(item: ReturnType<typeof editConverter.settle>): conversationv1.AgentEditSuccess {
  return (item?.value as conversationv1.AgentEdit).result.value as conversationv1.AgentEditSuccess;
}

describe("editConverter.start", () => {
  it("announces the path and carries neither the matched nor the replacement text", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.ts", old_string: "a", new_string: "b" });

    // Act.
    const item = editConverter.start(pending);

    // Assert.
    const start = (item?.value as conversationv1.AgentEdit).result
      .value as conversationv1.AgentEditStart;
    expect(start.path?.path).toBe("/tmp/a.ts");
    expect(start.startedAt?.atMs).toBe(1_700_000_000_000n);
  });

  it("produces NO message when the call named no path", () => {
    // Arrange, Act.
    const item = editConverter.start(call({ old_string: "a", new_string: "b" }));

    // Assert.
    expect(item?.case).toBeUndefined();
  });
});

describe("editConverter.settle", () => {
  it("CARRIES the vendor's own hunk from the corpus edit, line numbers intact", () => {
    // Arrange.
    const result = corpusResult("edit");
    const pending = call({ file_path: result["filePath"] });

    // Act.
    const success = successOf(editConverter.settle(pending, outcome(result)));

    // Assert.
    expect(success.patch).toHaveLength(1);
    expect(success.patch[0]?.oldRange).toEqual(
      create(conversationv1.FilePatchHunkRangeSchema, { start: 411, lines: 6 }),
    );
    expect(success.patch[0]?.newRange).toEqual(
      create(conversationv1.FilePatchHunkRangeSchema, { start: 411, lines: 8 }),
    );
  });

  it("restates the path the vendor resolved, not the one the caller typed", () => {
    // Arrange.
    const pending = call({ file_path: "a.ts" });
    const result = { filePath: "/abs/a.ts", structuredPatch: [], userModified: false };

    // Act.
    const success = successOf(editConverter.settle(pending, outcome(result)));

    // Assert.
    expect(success.path?.path).toBe("/abs/a.ts");
  });

  it("carries user_modified when the user altered the edit at the gate", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.ts" });
    const result = { filePath: "/tmp/a.ts", structuredPatch: [], userModified: true };

    // Act.
    const success = successOf(editConverter.settle(pending, outcome(result)));

    // Assert.
    expect(success.userModified).toBe(true);
  });

  it("drops a hunk that states no range rather than drawing one at the file's top", () => {
    // Arrange.
    const pending = call({ file_path: "/tmp/a.ts" });
    const result = { filePath: "/tmp/a.ts", structuredPatch: [{ lines: ["+x"] }] };

    // Act.
    const success = successOf(editConverter.settle(pending, outcome(result)));

    // Assert.
    expect(success.patch).toHaveLength(0);
  });

  it("produces NO frame when the edit settled with no typed output at all", () => {
    // Arrange, Act.
    const item = editConverter.settle(call({ file_path: "/tmp/a.ts" }), outcome("done"));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when neither the result nor the call names a path", () => {
    // Arrange.
    const pending = call({ old_string: "a", new_string: "b" });

    // Act.
    const item = editConverter.settle(pending, outcome({ structuredPatch: [] }));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("carries the failure arm when the matched text was absent", () => {
    // Arrange, Act.
    const item = editConverter.settle(
      call({ file_path: "/tmp/a.ts" }),
      outcome("String not found", true),
    );

    // Assert.
    const edit = item?.value as conversationv1.AgentEdit;
    expect(edit.result.case).toBe("failure");
    expect((edit.result.value as conversationv1.AgentEditFailure).error?.settledAt?.atMs).toBe(
      1_700_000_001_000n,
    );
  });

  it("restates the requested path on the failure arm, so the settled frame stands alone", () => {
    // Arrange, Act.
    const item = editConverter.settle(call({ file_path: "/tmp/a.ts" }), outcome("String not found", true));

    // Assert.
    const edit = item?.value as conversationv1.AgentEdit;
    expect((edit.result.value as conversationv1.AgentEditFailure).path?.path).toBe("/tmp/a.ts");
  });

  it("produces NO failure frame for a call that named no path, which had no start either", () => {
    // Arrange, Act, Assert.
    expect(editConverter.settle(call({}), outcome("String not found", true))).toBeUndefined();
  });
});

describe("editConverter.progress", () => {
  it("relays the vendor's beat on the edit's own progress arm", () => {
    // Arrange, Act.
    const item = editConverter.progress?.(toolProgress(1_700_000_000_500));

    // Assert.
    expect((item?.value as conversationv1.AgentEdit).result.case).toBe("progress");
  });
});
