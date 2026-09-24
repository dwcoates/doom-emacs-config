/**
 * The MCP tool converter — an MCP server's tool is an ORDINARY tool call. The
 * assertions that matter: the address is resolved by lookup and never guessed,
 * and both settled arms restate the tool and the arguments their start carried.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import { mcpToolConverter, resolveMcpAddress } from "../../../src/convert/tools/mcp.js";
import { toolProgress, toolResultText } from "../../../src/convert/entries.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";

const AGENT = create(conversationv1.AgentIdSchema, { value: "session-1" });
const CHROME = { mcpServerNames: ["claude-in-chrome"] };

function call(toolName: string, input: Record<string, unknown> = {}): PendingCall {
  return { toolUseId: "toolu_mcp", toolName, input, startedAtMs: 1_700_000_000_000, agentId: AGENT };
}

function outcome(content: conversationv1.ToolResultContent | undefined, isError = false): ToolOutcome {
  return { content, isError, structured: undefined, settledAtMs: 1_700_000_001_000 };
}

function mcpOf(item: ReturnType<typeof mcpToolConverter.start>): conversationv1.AgentMcpToolCall {
  expect(item?.case).toBe("mcpToolCall");
  return item?.value as conversationv1.AgentMcpToolCall;
}

function startOf(item: ReturnType<typeof mcpToolConverter.start>): conversationv1.AgentMcpToolCallStart {
  return mcpOf(item).result.value as conversationv1.AgentMcpToolCallStart;
}

function successOf(item: ReturnType<typeof mcpToolConverter.settle>): conversationv1.AgentMcpToolCallSuccess {
  return mcpOf(item).result.value as conversationv1.AgentMcpToolCallSuccess;
}

function failureOf(item: ReturnType<typeof mcpToolConverter.settle>): conversationv1.AgentMcpToolCallFailure {
  return mcpOf(item).result.value as conversationv1.AgentMcpToolCallFailure;
}

describe("resolveMcpAddress", () => {
  it.each([
    ["a known server", "mcp__claude-in-chrome__navigate", ["claude-in-chrome"], { server: "claude-in-chrome", tool: "navigate" }],
    ["a server whose name holds the separator", "mcp__my__server__run", ["my__server"], { server: "my__server", tool: "run" }],
    ["no known server", "mcp__claude-in-chrome__navigate", ["other"], undefined],
    ["a split that only a shorter wrong server matches", "mcp__my__server__run", ["my__serverx"], undefined],
    ["a known server and no tool after it", "mcp__gmail", ["gmail"], undefined],
    ["a known server and an empty tool", "mcp__gmail__", ["gmail"], undefined],
  ])("resolves %s", (_name, toolName, known, want) => {
    // Arrange, Act.
    const address = resolveMcpAddress(toolName, { mcpServerNames: known });

    // Assert.
    expect(address === undefined ? undefined : { server: address.server, tool: address.tool }).toEqual(want);
  });
});

describe("mcpToolConverter.start", () => {
  it("names the tool as the agent named it", () => {
    // Arrange, Act.
    const start = startOf(mcpToolConverter.start(call("mcp__claude-in-chrome__tabs_context_mcp"), CHROME));

    // Assert.
    expect(start.tool?.name).toBe("mcp__claude-in-chrome__tabs_context_mcp");
  });

  it("states the address it resolved", () => {
    // Arrange, Act.
    const start = startOf(mcpToolConverter.start(call("mcp__claude-in-chrome__tabs_context_mcp"), CHROME));

    // Assert.
    expect({ server: start.tool?.address?.server, tool: start.tool?.address?.tool }).toEqual({
      server: "claude-in-chrome",
      tool: "tabs_context_mcp",
    });
  });

  it("leaves the address unset when no server the session knows matches", () => {
    // Arrange, Act.
    const start = startOf(mcpToolConverter.start(call("mcp__claude-in-chrome__navigate"), { mcpServerNames: [] }));

    // Assert.
    expect(start.tool?.address).toBeUndefined();
  });

  it("carries the arguments as an untyped Struct, verbatim", () => {
    // Arrange, Act.
    const start = startOf(mcpToolConverter.start(call("mcp__claude-in-chrome__navigate", { tabId: 7, url: "https://a.example" }), CHROME));

    // Assert.
    expect(start.arguments).toEqual({ tabId: 7, url: "https://a.example" });
  });

  it("leaves the arguments unset when they cannot be represented as JSON", () => {
    // Arrange.
    const cyclic: Record<string, unknown> = {};
    cyclic["self"] = cyclic;

    // Act.
    const start = startOf(mcpToolConverter.start(call("mcp__claude-in-chrome__navigate", cyclic), CHROME));

    // Assert.
    expect(start.arguments).toBeUndefined();
  });

  it("stamps the instant the call was announced", () => {
    // Arrange, Act.
    const start = startOf(mcpToolConverter.start(call("mcp__claude-in-chrome__navigate"), CHROME));

    // Assert.
    expect(start.startedAt?.atMs).toBe(1_700_000_000_000n);
  });
});

describe("mcpToolConverter.settle", () => {
  it("carries what the tool returned, typed", () => {
    // Arrange, Act.
    const success = successOf(mcpToolConverter.settle(call("mcp__claude-in-chrome__navigate"), outcome(toolResultText("Navigated")), CHROME));

    // Assert.
    expect(success.content).toEqual(toolResultText("Navigated"));
  });

  it("records an EMPTY result when the vendor returned nothing at all", () => {
    // Arrange, Act.
    const success = successOf(mcpToolConverter.settle(call("mcp__claude-in-chrome__navigate"), outcome(undefined), CHROME));

    // Assert.
    expect(success.content).toEqual(create(conversationv1.ToolResultContentSchema, {}));
  });

  it("restates the tool on the success arm", () => {
    // Arrange, Act.
    const success = successOf(mcpToolConverter.settle(call("mcp__claude-in-chrome__navigate"), outcome(toolResultText("ok")), CHROME));

    // Assert.
    expect({ name: success.tool?.name, server: success.tool?.address?.server }).toEqual({
      name: "mcp__claude-in-chrome__navigate",
      server: "claude-in-chrome",
    });
  });

  it("restates the arguments on the success arm", () => {
    // Arrange, Act.
    const success = successOf(mcpToolConverter.settle(call("mcp__claude-in-chrome__navigate", { tabId: 7 }), outcome(toolResultText("ok")), CHROME));

    // Assert.
    expect(success.arguments).toEqual({ tabId: 7 });
  });

  it("restates the start instant beside the settle instant", () => {
    // Arrange, Act.
    const success = successOf(mcpToolConverter.settle(call("mcp__claude-in-chrome__navigate"), outcome(toolResultText("ok")), CHROME));

    // Assert.
    expect({ at: success.settledAt?.atMs, started: success.settledAt?.startedAt?.atMs }).toEqual({
      at: 1_700_000_001_000n,
      started: 1_700_000_000_000n,
    });
  });

  it("carries the tool's own error text on the failure arm", () => {
    // Arrange, Act.
    const failure = failureOf(
      mcpToolConverter.settle(call("mcp__claude-in-chrome__computer"), outcome(toolResultText("Error: no page"), true), CHROME),
    );

    // Assert.
    expect(failure.error?.content).toEqual(toolResultText("Error: no page"));
  });

  it("restates the tool and the arguments on the failure arm", () => {
    // Arrange, Act.
    const failure = failureOf(
      mcpToolConverter.settle(call("mcp__claude-in-chrome__computer", { action: "screenshot" }), outcome(toolResultText("x"), true), CHROME),
    );

    // Assert.
    expect({ name: failure.tool?.name, args: failure.arguments }).toEqual({
      name: "mcp__claude-in-chrome__computer",
      args: { action: "screenshot" },
    });
  });
});

describe("mcpToolConverter.progress", () => {
  it("relays the vendor's beat on the call's own progress arm", () => {
    // Arrange, Act.
    const item = mcpToolConverter.progress?.(toolProgress(1_700_000_000_500));

    // Assert.
    expect(mcpOf(item).result.case).toBe("progress");
  });
});
