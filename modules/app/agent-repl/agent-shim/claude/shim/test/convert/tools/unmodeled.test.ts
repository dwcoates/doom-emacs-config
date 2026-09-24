/**
 * The unmodeled converter — the arm for a tool whose schema genuinely cannot be
 * known. The assertion that matters is the refusal: an empty result is recorded
 * as empty rather than as nothing. An MCP tool never lands here (mcp.test.ts).
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import { unmodeledConverter } from "../../../src/convert/tools/unmodeled.js";
import { toolProgress, toolResultText } from "../../../src/convert/entries.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";

const AGENT = create(conversationv1.AgentIdSchema, { value: "session-1" });

function call(toolName: string, input: Record<string, unknown> = {}): PendingCall {
  return {
    toolUseId: "toolu_unmodeled",
    toolName,
    input,
    startedAtMs: 1_700_000_000_000,
    agentId: AGENT,
  };
}

function outcome(
  content: conversationv1.ToolResultContent | undefined,
  isError = false,
): ToolOutcome {
  return { content, isError, structured: undefined, settledAtMs: 1_700_000_001_000 };
}

function startOf(item: ReturnType<typeof unmodeledConverter.start>): conversationv1.AgentUnmodeledStart {
  return (item?.value as conversationv1.AgentUnmodeled).result
    .value as conversationv1.AgentUnmodeledStart;
}

describe("unmodeledConverter.start", () => {
  it("carries the tool as the agent named it", () => {
    // Arrange, Act.
    const item = unmodeledConverter.start(call("StructuredOutput"));

    // Assert.
    expect(startOf(item).toolName).toBe("StructuredOutput");
  });

  it("carries the arguments as an untyped Struct, verbatim", () => {
    // Arrange, Act.
    const item = unmodeledConverter.start(
      call("StructuredOutput", { to: "a@b.example", count: 3, draft: true }),
    );

    // Assert.
    expect(startOf(item).arguments).toEqual({ to: "a@b.example", count: 3, draft: true });
  });

  it("stamps the instant the call was announced", () => {
    // Arrange, Act.
    const item = unmodeledConverter.start(call("StructuredOutput"));

    // Assert.
    expect(startOf(item).startedAt?.atMs).toBe(1_700_000_000_000n);
  });

  it("leaves the arguments UNSET when they cannot be represented as JSON", () => {
    // Arrange.
    const cyclic: Record<string, unknown> = {};
    cyclic["self"] = cyclic;

    // Act.
    const item = unmodeledConverter.start(call("StructuredOutput", cyclic));

    // Assert.
    expect(startOf(item).arguments).toBeUndefined();
  });
});

describe("unmodeledConverter.settle", () => {
  it("carries what the tool returned, typed", () => {
    // Arrange, Act.
    const item = unmodeledConverter.settle(call("StructuredOutput"), outcome(toolResultText("done")));

    // Assert.
    const success = (item?.value as conversationv1.AgentUnmodeled).result
      .value as conversationv1.AgentUnmodeledSuccess;
    expect(success.content).toEqual(toolResultText("done"));
    expect(success.settledAt?.atMs).toBe(1_700_000_001_000n);
  });

  it("records an EMPTY result when the vendor returned nothing at all", () => {
    // Arrange, Act.
    const item = unmodeledConverter.settle(call("StructuredOutput"), outcome(undefined));

    // Assert.
    const success = (item?.value as conversationv1.AgentUnmodeled).result
      .value as conversationv1.AgentUnmodeledSuccess;
    expect(success.content).toEqual(create(conversationv1.ToolResultContentSchema, {}));
  });

  it("carries the tool's own error text on the failure arm, which is all anyone has", () => {
    // Arrange, Act.
    const item = unmodeledConverter.settle(
      call("StructuredOutput"),
      outcome(toolResultText("upstream 503"), true),
    );

    // Assert.
    const unmodeled = item?.value as conversationv1.AgentUnmodeled;
    expect(unmodeled.result.case).toBe("failure");
    const failure = unmodeled.result.value as conversationv1.AgentUnmodeledFailure;
    expect(failure.toolName).toBe("StructuredOutput");
    expect(failure.content).toEqual(toolResultText("upstream 503"));
  });

});

describe("unmodeledConverter.progress", () => {
  it("relays the vendor's beat on the call's own progress arm", () => {
    // Arrange, Act.
    const item = unmodeledConverter.progress?.(toolProgress(1_700_000_000_500));

    // Assert.
    expect((item?.value as conversationv1.AgentUnmodeled).result.case).toBe("progress");
  });
});
