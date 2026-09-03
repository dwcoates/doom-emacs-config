/**
 * The plan-mode converter. Two vendor tool names, one unit kind: the act comes
 * off the NAME, because EnterPlanMode's input is empty and nothing else could
 * tell the two calls apart.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { prose, toolResultText } from "../../../src/convert/entries.js";
import { planModeConverter } from "../../../src/convert/tools/plan-mode.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { conversationv1 } from "../../../src/proto.js";

const AGENT_ID = create(conversationv1.AgentIdSchema, { value: "session-1" });

function callNamed(toolName: string, input: Record<string, unknown> = {}): PendingCall {
  return {
    toolUseId: "toolu_plan",
    toolName,
    input,
    startedAtMs: 1_700_000_000_000,
    agentId: AGENT_ID,
  };
}

function outcomeWith(structured: unknown, isError = false): ToolOutcome {
  return {
    content: toolResultText("what the model was shown"),
    isError,
    structured,
    settledAtMs: 1_700_000_004_000,
  };
}

function planOf(item: conversationv1.AgentActivity["item"] | undefined): conversationv1.AgentPlanMode {
  expect(item?.case).toBe("planMode");
  return item?.value as conversationv1.AgentPlanMode;
}

function startOf(call: PendingCall): conversationv1.AgentPlanModeStart {
  const plan = planOf(planModeConverter.start(call));
  expect(plan.state.case).toBe("start");
  return plan.state.value as conversationv1.AgentPlanModeStart;
}

function successOf(toolName: string, structured: unknown): conversationv1.AgentPlanModeSuccess {
  const plan = planOf(planModeConverter.settle(callNamed(toolName), outcomeWith(structured))!);
  expect(plan.state.case).toBe("success");
  return plan.state.value as conversationv1.AgentPlanModeSuccess;
}

describe("planModeConverter kind and arms", () => {
  it("declares the plan_mode kind", () => {
    // Arrange, Act, Assert.
    expect(planModeConverter.kind).toBe("plan_mode");
  });

  it("carries NO progress, because AgentPlanMode declares no such arm", () => {
    // Arrange, Act, Assert.
    expect(planModeConverter.carriesProgress).toBe(false);
  });
});

describe("planModeConverter.start", () => {
  it("reads the enter act off the tool NAME", () => {
    // Arrange, Act, Assert.
    expect(startOf(callNamed("EnterPlanMode")).act.case).toBe("enter");
  });

  it("reads the exit act off the tool NAME", () => {
    // Arrange, Act, Assert.
    expect(startOf(callNamed("ExitPlanMode")).act.case).toBe("exit");
  });

  it("stamps the instant the call was issued", () => {
    // Arrange, Act, Assert.
    expect(startOf(callNamed("EnterPlanMode")).startedAt?.atMs).toBe(1_700_000_000_000n);
  });

  it("leaves the act unstated for a name that is neither call", () => {
    // Arrange, Act, Assert.
    expect(startOf(callNamed("SomethingElse")).act.case).toBeUndefined();
  });
});

describe("planModeConverter.settle", () => {
  it("carries the vendor's acknowledgment on the entered arm", () => {
    // Arrange, Act.
    const success = successOf("EnterPlanMode", { message: "Plan mode is on." });

    // Assert.
    expect(success.act).toEqual({
      case: "entered",
      value: create(conversationv1.AgentPlanModeEnteredSchema, { message: "Plan mode is on." }),
    });
  });

  it("carries the plan document as prose on the exited arm", () => {
    // Arrange, Act.
    const success = successOf("ExitPlanMode", { plan: "## Step one\n", isAgent: false });

    // Assert.
    expect((success.act.value as conversationv1.AgentPlanModeExited).plan).toEqual(
      prose("## Step one\n"),
    );
  });

  it("leaves the plan UNSET when the vendor stated a null plan", () => {
    // Arrange, Act.
    const success = successOf("ExitPlanMode", { plan: null, isAgent: false });

    // Assert.
    expect((success.act.value as conversationv1.AgentPlanModeExited).plan).toBeUndefined();
  });

  it("carries the file path the plan was written to, which the feed opens", () => {
    // Arrange, Act.
    const success = successOf("ExitPlanMode", { plan: "p", filePath: "/tmp/plan.md", isAgent: false });

    // Assert.
    expect((success.act.value as conversationv1.AgentPlanModeExited).filePath).toBe("/tmp/plan.md");
  });

  it("leaves the file path UNSET when the vendor named none", () => {
    // Arrange, Act.
    const success = successOf("ExitPlanMode", { plan: "p", isAgent: false });

    // Assert.
    expect((success.act.value as conversationv1.AgentPlanModeExited).filePath).toBeUndefined();
  });

  it("carries every EXPECTED UNMAPPED flag the vendor stated", () => {
    // Arrange, Act.
    const success = successOf("ExitPlanMode", {
      plan: "p",
      planWasEdited: true,
      isAgent: true,
      hasTaskTool: true,
      awaitingLeaderApproval: true,
    });

    // Assert.
    expect(success.act.value).toEqual(
      create(conversationv1.AgentPlanModeExitedSchema, {
        plan: prose("p"),
        planWasEdited: true,
        isAgent: true,
        hasTaskTool: true,
        awaitingLeaderApproval: true,
      }),
    );
  });

  it("does NOT carry the vendor's requestId: vendor identity spaces do not cross", () => {
    // Arrange, Act.
    const success = successOf("ExitPlanMode", { plan: "p", isAgent: false, requestId: "req_01ABC" });

    // Assert.
    expect(JSON.stringify(success.act.value)).not.toContain("req_01ABC");
  });

  it("stamps the settle instant on the success arm", () => {
    // Arrange, Act.
    const success = successOf("EnterPlanMode", { message: "on" });

    // Assert.
    expect(success.settledAt?.atMs).toBe(1_700_000_004_000n);
  });

  it("records an act whose typed output was missing rather than dropping the call", () => {
    // Arrange, Act.
    const success = successOf("EnterPlanMode", "just prose");

    // Assert.
    expect(success.act).toEqual({
      case: "entered",
      value: create(conversationv1.AgentPlanModeEnteredSchema, { message: "" }),
    });
  });

  it("produces NO frame when the settled call's name names no act", () => {
    // Arrange, Act.
    const item = planModeConverter.settle(callNamed("SomethingElse"), outcomeWith({}));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("settles an errored call as the failure arm", () => {
    // Arrange, Act.
    const plan = planOf(
      planModeConverter.settle(callNamed("ExitPlanMode"), outcomeWith(undefined, true))!,
    );

    // Assert.
    const failure = plan.state.value as conversationv1.AgentPlanModeFailure;
    expect(plan.state.case).toBe("failure");
    expect(failure.error?.settledAt?.atMs).toBe(1_700_000_004_000n);
  });
});
