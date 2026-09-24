/**
 * The grep converter. Three answer shapes, two extents each, and one
 * subtraction the shim owns — the OMITTED figure — so each shape and each
 * extent is asserted alone, and the subtraction is asserted at its clamp.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import { grepConverter } from "../../../src/convert/tools/grep.js";
import { toolProgress } from "../../../src/convert/entries.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";

const AGENT = create(conversationv1.AgentIdSchema, { value: "session-1" });

function call(input: Record<string, unknown>): PendingCall {
  return {
    toolUseId: "toolu_grep",
    toolName: "Grep",
    input,
    startedAtMs: 1_700_000_000_000,
    agentId: AGENT,
  };
}

function outcome(structured: unknown, isError = false): ToolOutcome {
  return { content: undefined, isError, structured, settledAtMs: 1_700_000_001_000 };
}

function successOf(item: ReturnType<typeof grepConverter.settle>): conversationv1.AgentGrepSuccess {
  return (item?.value as conversationv1.AgentGrep).result.value as conversationv1.AgentGrepSuccess;
}

describe("grepConverter.start", () => {
  it("builds the query entirely from the CALL, since the result states none of it", () => {
    // Arrange.
    const pending = call({
      pattern: "TODO",
      path: "/src",
      glob: "*.ts",
      type: "go",
      "-i": true,
      multiline: true,
    });

    // Act.
    const item = grepConverter.start(pending);

    // Assert.
    const start = (item?.value as conversationv1.AgentGrep).result
      .value as conversationv1.AgentGrepStart;
    expect(start.query).toEqual(
      create(conversationv1.AgentGrepQuerySchema, {
        pattern: "TODO",
        path: "/src",
        glob: "*.ts",
        fileType: "go",
        caseInsensitive: true,
        multiline: true,
      }),
    );
  });

  it("leaves the optional filters UNSET rather than empty when the caller named none", () => {
    // Arrange, Act.
    const item = grepConverter.start(call({ pattern: "TODO" }));

    // Assert.
    const start = (item?.value as conversationv1.AgentGrep).result
      .value as conversationv1.AgentGrepStart;
    expect(start.query?.path).toBeUndefined();
    expect(start.query?.glob).toBeUndefined();
    expect(start.query?.fileType).toBeUndefined();
    expect(start.query?.caseInsensitive).toBe(false);
  });

  it("produces NO message when the call named no pattern", () => {
    // Arrange, Act.
    const item = grepConverter.start(call({ path: "/src" }));

    // Assert.
    expect(item?.case).toBeUndefined();
  });
});

describe("grepConverter.settle", () => {
  it("defaults to the FILENAMES shape when the result states no mode", () => {
    // Arrange.
    const pending = call({ pattern: "TODO" });

    // Act.
    const success = successOf(
      grepConverter.settle(pending, outcome({ numFiles: 2, filenames: ["a.go", "b.go"] })),
    );

    // Assert.
    expect(success.matches.case).toBe("files");
    expect((success.matches.value as conversationv1.AgentGrepFiles).paths).toEqual(["a.go", "b.go"]);
  });

  it("says ALL for a filenames answer the search did not cap", () => {
    // Arrange.
    const pending = call({ pattern: "TODO" });
    const result = { mode: "files_with_matches", numFiles: 2, filenames: ["a", "b"], totalFiles: 2 };

    // Act.
    const success = successOf(grepConverter.settle(pending, outcome(result)));

    // Assert.
    const files = success.matches.value as conversationv1.AgentGrepFiles;
    expect(files.extent.case).toBe("all");
    expect((files.extent.value as conversationv1.AgentGrepFilesAll).filesReturned).toBe(2);
  });

  it("SUBTRACTS the omitted file count once when the search capped the list", () => {
    // Arrange.
    const pending = call({ pattern: "TODO" });
    const result = { mode: "files_with_matches", numFiles: 2, filenames: ["a", "b"], totalFiles: 44 };

    // Act.
    const success = successOf(grepConverter.settle(pending, outcome(result)));

    // Assert.
    const files = success.matches.value as conversationv1.AgentGrepFiles;
    expect(files.extent.value).toEqual(
      create(conversationv1.AgentGrepFilesPartialSchema, { filesReturned: 2, filesOmitted: 42 }),
    );
  });

  it("carries the rendered lines for a CONTENT answer", () => {
    // Arrange.
    const pending = call({ pattern: "TODO", output_mode: "content" });
    const result = { mode: "content", numFiles: 1, filenames: ["a"], content: "a:1:TODO", numLines: 1 };

    // Act.
    const success = successOf(grepConverter.settle(pending, outcome(result)));

    // Assert.
    const content = success.matches.value as conversationv1.AgentGrepContent;
    expect(content.content).toBe("a:1:TODO");
    expect(content.extent.case).toBe("all");
  });

  it("SUBTRACTS the omitted line count for a capped content answer", () => {
    // Arrange.
    const pending = call({ pattern: "TODO", output_mode: "content" });
    const result = {
      mode: "content",
      numFiles: 1,
      filenames: ["a"],
      content: "a:1:TODO",
      numLines: 1,
      totalLines: 9,
    };

    // Act.
    const success = successOf(grepConverter.settle(pending, outcome(result)));

    // Assert.
    const content = success.matches.value as conversationv1.AgentGrepContent;
    expect(content.extent.value).toEqual(
      create(conversationv1.AgentGrepContentPartialSchema, { linesReturned: 1, linesOmitted: 8 }),
    );
  });

  it("never states a NEGATIVE omission when the total trails the returned count", () => {
    // Arrange.
    const pending = call({ pattern: "TODO" });
    const result = { mode: "files_with_matches", numFiles: 5, filenames: [], totalFiles: 2 };

    // Act.
    const success = successOf(grepConverter.settle(pending, outcome(result)));

    // Assert.
    expect((success.matches.value as conversationv1.AgentGrepFiles).extent.case).toBe("all");
  });

  it("carries only the total for a COUNT answer", () => {
    // Arrange.
    const pending = call({ pattern: "TODO", output_mode: "count" });

    // Act.
    const success = successOf(
      grepConverter.settle(pending, outcome({ mode: "count", numFiles: 3, filenames: [], numMatches: 17 })),
    );

    // Assert.
    expect(success.matches.value).toEqual(
      create(conversationv1.AgentGrepCountSchema, { matches: 17 }),
    );
  });

  it("produces NO frame for a content answer that stated no line count", () => {
    // Arrange.
    const pending = call({ pattern: "TODO" });

    // Act.
    const item = grepConverter.settle(
      pending,
      outcome({ mode: "content", numFiles: 1, filenames: ["a"], content: "a:1:TODO" }),
    );

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame for an output mode this contract has no shape for", () => {
    // Arrange.
    const pending = call({ pattern: "TODO" });

    // Act.
    const item = grepConverter.settle(pending, outcome({ mode: "histogram", numFiles: 0, filenames: [] }));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("matching NOTHING is a success with an empty answer, never a failure", () => {
    // Arrange.
    const pending = call({ pattern: "nowhere" });

    // Act.
    const item = grepConverter.settle(pending, outcome({ numFiles: 0, filenames: [] }));

    // Assert.
    expect((item?.value as conversationv1.AgentGrep).result.case).toBe("success");
  });

  it("produces NO frame for a filenames answer that stated no file count", () => {
    // Arrange.
    const pending = call({ pattern: "TODO" });

    // Act.
    const item = grepConverter.settle(pending, outcome({ mode: "files_with_matches" }));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("lists NO paths when a filenames answer stated a count but no filenames array", () => {
    // Arrange.
    const pending = call({ pattern: "TODO" });

    // Act.
    const item = grepConverter.settle(
      pending,
      outcome({ mode: "files_with_matches", numFiles: 0 }),
    );

    // Assert.
    const files = successOf(item).matches.value as conversationv1.AgentGrepFiles;
    expect(files.paths).toEqual([]);
    expect(files.extent.case).toBe("all");
  });

  it("produces NO frame for a count answer that stated no match count", () => {
    // Arrange.
    const pending = call({ pattern: "TODO" });

    // Act.
    const item = grepConverter.settle(pending, outcome({ mode: "count" }));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when a search settled with no typed output at all", () => {
    // Arrange.
    const pending = call({ pattern: "TODO" });

    // Act.
    const item = grepConverter.settle(pending, outcome("3 matches"));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when a settled search has no pattern to restate", () => {
    // Arrange.
    const pending = call({});

    // Act.
    const item = grepConverter.settle(pending, outcome({ mode: "count", numMatches: 1 }));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("carries the failure arm when the search could not run", () => {
    // Arrange, Act.
    const item = grepConverter.settle(call({ pattern: "(" }), outcome("regex parse error", true));

    // Assert.
    const grep = item?.value as conversationv1.AgentGrep;
    expect(grep.result.case).toBe("failure");
    expect((grep.result.value as conversationv1.AgentGrepFailure).error?.settledAt?.atMs).toBe(
      1_700_000_001_000n,
    );
  });

  it("restates the query on the failure arm, so the settled frame stands alone", () => {
    // Arrange, Act.
    const item = grepConverter.settle(call({ pattern: "(", path: "src" }), outcome("regex parse error", true));

    // Assert.
    const failure = (item?.value as conversationv1.AgentGrep).result.value as conversationv1.AgentGrepFailure;
    expect({ pattern: failure.query?.pattern, path: failure.query?.path }).toEqual({ pattern: "(", path: "src" });
  });

  it("produces NO failure frame for a call that named no pattern, which had no start either", () => {
    // Arrange, Act, Assert.
    expect(grepConverter.settle(call({}), outcome("pattern required", true))).toBeUndefined();
  });
});

describe("grepConverter.progress", () => {
  it("relays the vendor's beat on the search's own progress arm", () => {
    // Arrange, Act.
    const item = grepConverter.progress?.(toolProgress(1_700_000_000_500));

    // Assert.
    expect((item?.value as conversationv1.AgentGrep).result.case).toBe("progress");
  });
});
