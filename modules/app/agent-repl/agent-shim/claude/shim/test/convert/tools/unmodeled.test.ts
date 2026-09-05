/**
 * The unmodeled converter — the arm for a tool whose schema genuinely cannot be
 * known. The assertions that matter are the two refusals: the qualified name is
 * never split to guess an MCP server, and an empty result is recorded as empty
 * rather than as nothing.
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
  it("carries the tool as the agent named it, qualification and all", () => {
    // Arrange, Act.
    const item = unmodeledConverter.start(call("mcp__claude_ai_Gmail__send_message"));

    // Assert.
    expect(startOf(item).toolName).toBe("mcp__claude_ai_Gmail__send_message");
  });

  it("carries the arguments as an untyped Struct, verbatim", () => {
    // Arrange, Act.
    const item = unmodeledConverter.start(
      call("mcp__x__y", { to: "a@b.example", count: 3, draft: true }),
    );

    // Assert.
    expect(startOf(item).arguments).toEqual({ to: "a@b.example", count: 3, draft: true });
  });

  it("stamps the instant the call was announced", () => {
    // Arrange, Act.
    const item = unmodeledConverter.start(call("mcp__x__y"));

    // Assert.
    expect(startOf(item).startedAt?.atMs).toBe(1_700_000_000_000n);
  });

  it("leaves mcp_server UNSET rather than splitting the qualified name", () => {
    // Arrange, Act.
    const item = unmodeledConverter.start(call("mcp__claude_ai_Gmail__send_message"));

    // Assert.
    expect(startOf(item).mcpServer).toBeUndefined();
  });

  it("NAMES the MCP server when the session knows one the qualified name matches", () => {
    // Arrange.
    const environment = { mcpServerNames: ["claude_ai_Gmail"] };

    // Act.
    const item = unmodeledConverter.start(call("mcp__claude_ai_Gmail__send_message"), environment);

    // Assert.
    expect(startOf(item).mcpServer).toBe("claude_ai_Gmail");
  });

  it("leaves mcp_server UNSET when the qualified name matches no server the session knows", () => {
    // Arrange.
    const environment = { mcpServerNames: ["other_server"] };

    // Act.
    const item = unmodeledConverter.start(call("mcp__claude_ai_Gmail__send_message"), environment);

    // Assert.
    expect(startOf(item).mcpServer).toBeUndefined();
  });

  it("leaves mcp_server UNSET when the whole remainder IS a known server and no tool follows it", () => {
    // Arrange: `mcp__gmail` names the server with no separator or tool after it,
    // so the exact-match arm refuses it rather than naming a server for a call
    // that addresses no tool.
    const environment = { mcpServerNames: ["gmail"] };

    // Act.
    const item = unmodeledConverter.start(call("mcp__gmail"), environment);

    // Assert.
    expect(startOf(item).mcpServer).toBeUndefined();
  });

  it("leaves the arguments UNSET when they cannot be represented as JSON", () => {
    // Arrange.
    const cyclic: Record<string, unknown> = {};
    cyclic["self"] = cyclic;

    // Act.
    const item = unmodeledConverter.start(call("mcp__x__y", cyclic));

    // Assert.
    expect(startOf(item).arguments).toBeUndefined();
  });
});

describe("unmodeledConverter.settle", () => {
  it("carries what the tool returned, typed", () => {
    // Arrange, Act.
    const item = unmodeledConverter.settle(call("mcp__x__y"), outcome(toolResultText("done")));

    // Assert.
    const success = (item?.value as conversationv1.AgentUnmodeled).result
      .value as conversationv1.AgentUnmodeledSuccess;
    expect(success.content).toEqual(toolResultText("done"));
    expect(success.settledAt?.atMs).toBe(1_700_000_001_000n);
  });

  it("records an EMPTY result when the vendor returned nothing at all", () => {
    // Arrange, Act.
    const item = unmodeledConverter.settle(call("mcp__x__y"), outcome(undefined));

    // Assert.
    const success = (item?.value as conversationv1.AgentUnmodeled).result
      .value as conversationv1.AgentUnmodeledSuccess;
    expect(success.content).toEqual(create(conversationv1.ToolResultContentSchema, {}));
  });

  it("carries the tool's own error text on the failure arm, which is all anyone has", () => {
    // Arrange, Act.
    const item = unmodeledConverter.settle(
      call("mcp__x__y"),
      outcome(toolResultText("upstream 503"), true),
    );

    // Assert.
    const unmodeled = item?.value as conversationv1.AgentUnmodeled;
    expect(unmodeled.result.case).toBe("failure");
    const failure = unmodeled.result.value as conversationv1.AgentUnmodeledFailure;
    expect(failure.toolName).toBe("mcp__x__y");
    expect(failure.content).toEqual(toolResultText("upstream 503"));
  });

  it("leaves mcp_server UNSET on the terminal arm too", () => {
    // Arrange, Act.
    const item = unmodeledConverter.settle(call("mcp__x__y"), outcome(toolResultText("ok")));

    // Assert.
    const success = (item?.value as conversationv1.AgentUnmodeled).result
      .value as conversationv1.AgentUnmodeledSuccess;
    expect(success.mcpServer).toBeUndefined();
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
