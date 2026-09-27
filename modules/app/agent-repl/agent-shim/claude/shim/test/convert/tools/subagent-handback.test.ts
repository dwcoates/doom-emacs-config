/**
 * A subagent's final report to its parent. The report rides the call's INPUT
 * on every arm, because the call's own result is a bare acknowledgement; the
 * tests read both halves from the real corpus pair
 * (`tool-inputs/subagent_handback.jsonl`, `tool-results/subagent_handback.jsonl`).
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import { subagentHandbackConverter } from "../../../src/convert/tools/subagent-handback.js";
import { toolProgress } from "../../../src/convert/entries.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { toolInput, toolResultText } from "./corpus.js";

function call(input: Record<string, unknown>): PendingCall {
  return {
    toolUseId: "toolu_01XimbQmvHTszbgRxyRy5VEf",
    toolName: "SubagentHandback",
    input,
    startedAtMs: 3_000,
    agentId: create(conversationv1.AgentIdSchema, { value: "a1d968043b47deee9" }),
  };
}

/** The corpus result: an acknowledgement text block and no structured output. */
function outcome(isError = false): ToolOutcome {
  return {
    content: create(conversationv1.ToolResultContentSchema, {
      blocks: [{ block: { case: "text", value: { text: toolResultText("subagent_handback") } } }],
    }),
    isError,
    structured: undefined,
    settledAtMs: 8_000,
  };
}

function armOf(
  item: conversationv1.AgentActivity["item"] | undefined,
): conversationv1.AgentSubagentHandback["result"] {
  expect(item?.case).toBe("subagentHandback");
  return (item?.value as conversationv1.AgentSubagentHandback).result;
}

function startOf(item: conversationv1.AgentActivity["item"] | undefined): conversationv1.AgentSubagentHandbackStart {
  const arm = armOf(item);
  expect(arm.case).toBe("start");
  return arm.value as conversationv1.AgentSubagentHandbackStart;
}

function successOf(
  item: conversationv1.AgentActivity["item"] | undefined,
): conversationv1.AgentSubagentHandbackSuccess {
  const arm = armOf(item);
  expect(arm.case).toBe("success");
  return arm.value as conversationv1.AgentSubagentHandbackSuccess;
}

function failureOf(
  item: conversationv1.AgentActivity["item"] | undefined,
): conversationv1.AgentSubagentHandbackFailure {
  const arm = armOf(item);
  expect(arm.case).toBe("failure");
  return arm.value as conversationv1.AgentSubagentHandbackFailure;
}

const REPORT = toolInput("subagent_handback").message as string;

describe("subagentHandbackConverter.start", () => {
  it("carries the corpus report verbatim", () => {
    // Arrange, Act.
    const start = startOf(subagentHandbackConverter.start(call(toolInput("subagent_handback"))));

    // Assert.
    expect(start.report?.text).toBe(REPORT);
  });

  it("stamps the issue instant", () => {
    // Arrange, Act.
    const start = startOf(subagentHandbackConverter.start(call({ message: "done" })));

    // Assert.
    expect(start.startedAt?.atMs).toBe(3_000n);
  });

  it("carries an empty report rather than losing the hand-back when no text was stated", () => {
    // Arrange, Act.
    const start = startOf(subagentHandbackConverter.start(call({})));

    // Assert.
    expect(start.report?.text).toBe("");
  });
});

describe("subagentHandbackConverter.settle", () => {
  it("settles the corpus acknowledgement as the success arm", () => {
    // Arrange, Act.
    const arm = armOf(subagentHandbackConverter.settle(call(toolInput("subagent_handback")), outcome()));

    // Assert.
    expect(arm.case).toBe("success");
  });

  it("restates the report on the success arm", () => {
    // Arrange, Act.
    const success = successOf(
      subagentHandbackConverter.settle(call(toolInput("subagent_handback")), outcome()),
    );

    // Assert.
    expect(success.report?.text).toBe(REPORT);
  });

  it("stamps the settle instant, restating the start", () => {
    // Arrange, Act.
    const success = successOf(subagentHandbackConverter.settle(call({ message: "done" }), outcome()));

    // Assert.
    expect(success.settledAt).toMatchObject({ atMs: 8_000n, startedAt: { atMs: 3_000n } });
  });

  it("settles a vendor-marked error as the failure arm", () => {
    // Arrange, Act.
    const arm = armOf(subagentHandbackConverter.settle(call({ message: "done" }), outcome(true)));

    // Assert.
    expect(arm.case).toBe("failure");
  });

  it("carries the vendor's result content into the failure's own error", () => {
    // Arrange, Act.
    const failure = failureOf(subagentHandbackConverter.settle(call({ message: "done" }), outcome(true)));

    // Assert.
    expect(failure.error?.content?.blocks[0]?.block.value).toMatchObject({
      text: toolResultText("subagent_handback"),
    });
  });

  it("restates the report on the failure arm", () => {
    // Arrange, Act.
    const failure = failureOf(
      subagentHandbackConverter.settle(call(toolInput("subagent_handback")), outcome(true)),
    );

    // Assert.
    expect(failure.report?.text).toBe(REPORT);
  });
});

describe("subagentHandbackConverter.progress", () => {
  it("relays the vendor's liveness beat on the progress arm", () => {
    // Arrange.
    const beat = toolProgress(5_000);

    // Act.
    const arm = armOf(subagentHandbackConverter.progress?.(beat));

    // Assert.
    expect(arm).toEqual({ case: "progress", value: beat });
  });
});
