/**
 * The monitor converter. The load-bearing claim is that a monitor's tool result
 * is an ARMING RECEIPT, not an ending: the corpus's one observed result says
 * "Monitor started (task b1xo9bsxw, timeout 600000ms)" while the watch is still
 * live, so settling on it would draw a running watch as over.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { toolResultText } from "../../../src/convert/entries.js";
import { monitorConverter, monitorEnded } from "../../../src/convert/tools/monitor.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { conversationv1 } from "../../../src/proto.js";

/** The one observed Monitor result's own `toolUseResult`, verbatim. */
function corpusOutput(): Record<string, unknown> {
  const path = fileURLToPath(
    new URL("../../../../../../testdata/corpus/tool-results/monitor.jsonl", import.meta.url),
  );
  const line = readFileSync(path, "utf8").trim().split("\n")[0];
  return (JSON.parse(line) as { toolUseResult: Record<string, unknown> }).toolUseResult;
}

const AGENT_ID = create(conversationv1.AgentIdSchema, { value: "session-1" });

function callWith(input: Record<string, unknown>): PendingCall {
  return {
    toolUseId: "toolu_monitor",
    toolName: "Monitor",
    input,
    startedAtMs: 1_700_000_000_000,
    agentId: AGENT_ID,
  };
}

function outcomeWith(structured: unknown, isError = false): ToolOutcome {
  return {
    content: toolResultText("Monitor started"),
    isError,
    structured,
    settledAtMs: 1_700_000_000_500,
  };
}

function monitorOf(item: conversationv1.AgentActivity["item"] | undefined): conversationv1.AgentMonitor {
  expect(item?.case).toBe("monitor");
  return item?.value as conversationv1.AgentMonitor;
}

function startOf(call: PendingCall): conversationv1.AgentMonitorStart {
  const monitor = monitorOf(monitorConverter.start(call));
  expect(monitor.result.case).toBe("start");
  return monitor.result.value as conversationv1.AgentMonitorStart;
}

describe("monitorConverter kind and arms", () => {
  it("declares the monitor kind", () => {
    // Arrange, Act, Assert.
    expect(monitorConverter.kind).toBe("monitor");
  });

  it("carries NO progress, because AgentMonitor declares no such arm", () => {
    // Arrange, Act, Assert.
    expect(monitorConverter.carriesProgress).toBe(false);
  });
});

describe("monitorConverter.start", () => {
  it("draws its text from the agent's own description", () => {
    // Arrange.
    const call = callWith({ description: "watching the deploy", command: "tail -f log", timeout_ms: 600000, persistent: false });

    // Act, Assert.
    expect(startOf(call).description).toBe("watching the deploy");
  });

  it("states a bounded watch's lifetime as its configured deadline", () => {
    // Arrange.
    const call = callWith({ description: "d", command: "c", timeout_ms: 600000, persistent: false });

    // Act, Assert.
    expect(startOf(call).lifetime).toEqual({
      case: "deadline",
      value: create(conversationv1.AgentMonitorDeadlineSchema, { timeoutMs: 600000n }),
    });
  });

  it("states a persistent watch's lifetime as persistent, ignoring the timeout", () => {
    // Arrange.
    const call = callWith({ description: "d", command: "c", timeout_ms: 600000, persistent: true });

    // Act, Assert.
    expect(startOf(call).lifetime.case).toBe("persistent");
  });

  it("leaves the lifetime unstated when the call configured neither", () => {
    // Arrange.
    const call = callWith({ description: "d", command: "c" });

    // Act, Assert.
    expect(startOf(call).lifetime.case).toBeUndefined();
  });

  it("names a shell source by its command", () => {
    // Arrange.
    const call = callWith({ description: "d", command: "gh pr checks --watch", persistent: true });

    // Act, Assert.
    expect(startOf(call).source).toEqual({
      case: "command",
      value: create(conversationv1.AgentMonitorCommandSchema, { command: "gh pr checks --watch" }),
    });
  });

  it("names a websocket source by its url", () => {
    // Arrange.
    const call = callWith({ description: "d", ws: { url: "wss://example.test/feed" }, persistent: true });

    // Act, Assert.
    expect(startOf(call).source).toEqual({
      case: "websocket",
      value: create(conversationv1.AgentMonitorWebsocketSchema, { url: "wss://example.test/feed" }),
    });
  });

  it("leaves the source unstated when the call named neither", () => {
    // Arrange.
    const call = callWith({ description: "d", persistent: true });

    // Act, Assert.
    expect(startOf(call).source.case).toBeUndefined();
  });

  it("stamps the instant the watch was armed", () => {
    // Arrange.
    const call = callWith({ description: "d", command: "c", persistent: true });

    // Act, Assert.
    expect(startOf(call).startedAtMs).toBe(1_700_000_000_000n);
  });

  it("still announces a watch whose call carried no description", () => {
    // Arrange.
    const call = callWith({ command: "c", persistent: true });

    // Act, Assert.
    expect(startOf(call).description).toBe("");
  });
});

describe("monitorConverter.settle", () => {
  it("does NOT settle on the corpus's arming receipt: the watch is still live", () => {
    // Arrange.
    const call = callWith({ description: "d", command: "c", timeout_ms: 600000 });

    // Act.
    const item = monitorConverter.settle(call, outcomeWith(corpusOutput()));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("settles an errored arming as the failure arm: nothing was ever watching", () => {
    // Arrange.
    const call = callWith({ description: "d", command: "c" });

    // Act.
    const monitor = monitorOf(monitorConverter.settle(call, outcomeWith(undefined, true)));

    // Assert.
    const failure = monitor.result.value as conversationv1.AgentMonitorFailure;
    expect(monitor.result.case).toBe("failure");
    expect(failure.failure?.settledAt?.atMs).toBe(1_700_000_000_500n);
  });

  it("restates the call on a failed arming, so the settle stands alone", () => {
    // Arrange.
    const call = callWith({ description: "d", command: "tail -f log" });

    // Act.
    const monitor = monitorOf(monitorConverter.settle(call, outcomeWith(undefined, true)));

    // Assert.
    const failure = monitor.result.value as conversationv1.AgentMonitorFailure;
    expect(failure.call).toEqual(startOf(call));
  });
});

describe("monitorEnded", () => {
  it("mints the ended arm, which claims no cause", () => {
    // Arrange, Act.
    const monitor = monitorOf(monitorEnded(undefined));

    // Assert.
    expect(monitor.result).toEqual({
      case: "ended",
      value: create(conversationv1.AgentMonitorEndedSchema, {}),
    });
  });

  it("restates the call the watch was armed with", () => {
    // Arrange.
    const armed = startOf(callWith({ description: "d", command: "tail -f log" }));

    // Act.
    const monitor = monitorOf(monitorEnded(armed));

    // Assert.
    expect((monitor.result.value as conversationv1.AgentMonitorEnded).call).toEqual(armed);
  });
});
