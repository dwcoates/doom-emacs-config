/**
 * The glob converter. The load-bearing distinction is EXACT versus AT-LEAST:
 * "42 more" and "at least 42 more" are different claims, and only one of them
 * is safe to make when the search capped its own counting.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import { globConverter } from "../../../src/convert/tools/glob.js";
import { toolProgress } from "../../../src/convert/entries.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";

const AGENT = create(conversationv1.AgentIdSchema, { value: "session-1" });

function call(input: Record<string, unknown>): PendingCall {
  return {
    toolUseId: "toolu_glob",
    toolName: "Glob",
    input,
    startedAtMs: 1_700_000_000_000,
    agentId: AGENT,
  };
}

function outcome(structured: unknown, isError = false): ToolOutcome {
  return { content: undefined, isError, structured, settledAtMs: 1_700_000_001_000 };
}

function successOf(item: ReturnType<typeof globConverter.settle>): conversationv1.AgentGlobSuccess {
  return (item?.value as conversationv1.AgentGlob).result.value as conversationv1.AgentGlobSuccess;
}

describe("globConverter.start", () => {
  it("announces the corpus call's pattern, with no root the caller never named", () => {
    // Arrange.
    const pending = call({ pattern: "**/AGENTS.md" });

    // Act.
    const item = globConverter.start(pending);

    // Assert.
    const start = (item?.value as conversationv1.AgentGlob).result
      .value as conversationv1.AgentGlobStart;
    expect(start.query?.pattern).toBe("**/AGENTS.md");
    expect(start.query?.path).toBeUndefined();
  });

  it("carries the root when the caller named one", () => {
    // Arrange, Act.
    const item = globConverter.start(call({ pattern: "*.go", path: "/src" }));

    // Assert.
    const start = (item?.value as conversationv1.AgentGlob).result
      .value as conversationv1.AgentGlobStart;
    expect(start.query?.path).toBe("/src");
  });

  it("produces NO message when the call named no pattern", () => {
    // Arrange, Act.
    const item = globConverter.start(call({ path: "/src" }));

    // Assert.
    expect(item?.case).toBeUndefined();
  });
});

describe("globConverter.settle", () => {
  it("says ALL when the walk was not truncated", () => {
    // Arrange.
    const pending = call({ pattern: "*.go" });

    // Act.
    const success = successOf(
      globConverter.settle(pending, outcome({ numFiles: 2, filenames: ["a.go", "b.go"], truncated: false })),
    );

    // Assert.
    expect(success.paths).toEqual(["a.go", "b.go"]);
    expect(success.extent.value).toEqual(
      create(conversationv1.AgentGlobAllSchema, { filesReturned: 2 }),
    );
  });

  it("says EXACT when the total is a real total", () => {
    // Arrange.
    const pending = call({ pattern: "*.go" });
    const result = { numFiles: 100, filenames: [], truncated: true, totalMatches: 142, countIsComplete: true };

    // Act.
    const success = successOf(globConverter.settle(pending, outcome(result)));

    // Assert.
    const partial = success.extent.value as conversationv1.AgentGlobPartial;
    expect(partial.omitted.value).toEqual(
      create(conversationv1.AgentGlobOmittedExactSchema, { filesOmitted: 42 }),
    );
  });

  it("says AT LEAST when the search capped its OWN counting", () => {
    // Arrange.
    const pending = call({ pattern: "*.go" });
    const result = { numFiles: 100, filenames: [], truncated: true, totalMatches: 142, countIsComplete: false };

    // Act.
    const success = successOf(globConverter.settle(pending, outcome(result)));

    // Assert.
    const partial = success.extent.value as conversationv1.AgentGlobPartial;
    expect(partial.omitted.value).toEqual(
      create(conversationv1.AgentGlobOmittedAtLeastSchema, { filesOmittedAtLeast: 42 }),
    );
  });

  it("claims only a FLOOR OF ZERO when an older result stated no total at all", () => {
    // Arrange.
    const pending = call({ pattern: "*.go" });

    // Act.
    const success = successOf(
      globConverter.settle(pending, outcome({ numFiles: 100, filenames: [], truncated: true })),
    );

    // Assert.
    const partial = success.extent.value as conversationv1.AgentGlobPartial;
    expect(partial.omitted.value).toEqual(
      create(conversationv1.AgentGlobOmittedAtLeastSchema, { filesOmittedAtLeast: 0 }),
    );
  });

  it("produces NO frame when the walk stated no returned-file count", () => {
    // Arrange, Act.
    const item = globConverter.settle(call({ pattern: "*.go" }), outcome({ filenames: ["a.go"] }));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("matching NOTHING is a success with an empty list", () => {
    // Arrange, Act.
    const item = globConverter.settle(
      call({ pattern: "*.nope" }),
      outcome({ numFiles: 0, filenames: [], truncated: false }),
    );

    // Assert.
    expect(successOf(item).paths).toEqual([]);
  });

  it("claims ZERO omitted when a truncated walk's total trails the returned count", () => {
    // Arrange.
    const pending = call({ pattern: "*.go" });
    const result = { numFiles: 100, filenames: [], truncated: true, totalMatches: 42, countIsComplete: true };

    // Act.
    const success = successOf(globConverter.settle(pending, outcome(result)));

    // Assert.
    const partial = success.extent.value as conversationv1.AgentGlobPartial;
    expect((partial.omitted.value as conversationv1.AgentGlobOmittedExact).filesOmitted).toBe(0);
  });

  it("lists NO paths when the walk stated a count but no filenames array", () => {
    // Arrange.
    const pending = call({ pattern: "*.go" });

    // Act.
    const success = successOf(globConverter.settle(pending, outcome({ numFiles: 0 })));

    // Assert.
    expect(success.paths).toEqual([]);
  });

  it("produces NO frame when a walk settled with no typed output at all", () => {
    // Arrange, Act.
    const item = globConverter.settle(call({ pattern: "*.go" }), outcome("a.go\nb.go"));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when a settled walk has no pattern to restate", () => {
    // Arrange, Act.
    const item = globConverter.settle(call({}), outcome({ numFiles: 0, filenames: [] }));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("carries the failure arm when the walk could not run", () => {
    // Arrange, Act.
    const item = globConverter.settle(call({ pattern: "[" }), outcome("bad pattern", true));

    // Assert.
    const glob = item?.value as conversationv1.AgentGlob;
    expect(glob.result.case).toBe("failure");
    expect((glob.result.value as conversationv1.AgentGlobFailure).error?.settledAt?.atMs).toBe(
      1_700_000_001_000n,
    );
  });

  it("restates the query on the failure arm, so the settled frame stands alone", () => {
    // Arrange, Act.
    const item = globConverter.settle(call({ pattern: "[", path: "src" }), outcome("bad pattern", true));

    // Assert.
    const failure = (item?.value as conversationv1.AgentGlob).result.value as conversationv1.AgentGlobFailure;
    expect({ pattern: failure.query?.pattern, path: failure.query?.path }).toEqual({ pattern: "[", path: "src" });
  });

  it("produces NO failure frame for a call that named no pattern, which had no start either", () => {
    // Arrange, Act, Assert.
    expect(globConverter.settle(call({}), outcome("pattern required", true))).toBeUndefined();
  });
});

describe("globConverter.progress", () => {
  it("relays the vendor's beat on the walk's own progress arm", () => {
    // Arrange, Act.
    const item = globConverter.progress?.(toolProgress(1_700_000_000_500));

    // Assert.
    expect((item?.value as conversationv1.AgentGlob).result.case).toBe("progress");
  });
});
