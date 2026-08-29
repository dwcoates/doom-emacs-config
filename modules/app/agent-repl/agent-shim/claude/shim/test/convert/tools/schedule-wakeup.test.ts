/**
 * The schedule-wakeup converter. Two acts made exclusive by the vendor's own
 * rule (`stop: true` ignores every other field), and an outcome whose scheduled
 * arm exists to give the footer an ABSOLUTE instant to count down from.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { toolResultText } from "../../../src/convert/entries.js";
import { scheduleWakeupConverter } from "../../../src/convert/tools/schedule-wakeup.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { conversationv1 } from "../../../src/proto.js";

/** The one observed ScheduleWakeup result's own `toolUseResult`, verbatim. */
function corpusOutput(): Record<string, unknown> {
  const path = fileURLToPath(
    new URL(
      "../../../../../../testdata/corpus/tool-results/schedule_wakeup.jsonl",
      import.meta.url,
    ),
  );
  const line = readFileSync(path, "utf8").trim().split("\n")[0] as string;
  return (JSON.parse(line) as { toolUseResult: Record<string, unknown> }).toolUseResult;
}

const AGENT_ID = create(conversationv1.AgentIdSchema, { value: "session-1" });

function callWith(input: Record<string, unknown>): PendingCall {
  return {
    toolUseId: "toolu_wakeup",
    toolName: "ScheduleWakeup",
    input,
    startedAtMs: 1_700_000_000_000,
    agentId: AGENT_ID,
  };
}

function outcomeWith(structured: unknown, isError = false): ToolOutcome {
  return {
    content: toolResultText("Next wakeup scheduled"),
    isError,
    structured,
    settledAtMs: 1_700_000_000_100,
  };
}

function wakeupOf(
  item: conversationv1.AgentActivity["item"],
): conversationv1.AgentScheduleWakeup {
  expect(item.case).toBe("scheduleWakeup");
  return item.value as conversationv1.AgentScheduleWakeup;
}

function startOf(call: PendingCall): conversationv1.AgentScheduleWakeupStart {
  const wakeup = wakeupOf(scheduleWakeupConverter.start(call));
  expect(wakeup.result.case).toBe("start");
  return wakeup.result.value as conversationv1.AgentScheduleWakeupStart;
}

function successOf(structured: unknown): conversationv1.AgentScheduleWakeupSuccess {
  const wakeup = wakeupOf(
    scheduleWakeupConverter.settle(callWith({ delaySeconds: 1200 }), outcomeWith(structured))!,
  );
  expect(wakeup.result.case).toBe("success");
  return wakeup.result.value as conversationv1.AgentScheduleWakeupSuccess;
}

describe("scheduleWakeupConverter kind and arms", () => {
  it("declares the schedule_wakeup kind", () => {
    // Arrange, Act, Assert.
    expect(scheduleWakeupConverter.kind).toBe("schedule_wakeup");
  });

  it("carries NO progress, because AgentScheduleWakeup declares no such arm", () => {
    // Arrange, Act, Assert.
    expect(scheduleWakeupConverter.carriesProgress).toBe(false);
  });
});

describe("scheduleWakeupConverter.start", () => {
  it("states the schedule act with the delay, reason and prompt as asked", () => {
    // Arrange.
    const call = callWith({ delaySeconds: 1200, reason: "the build takes 20 minutes", prompt: "/loop check" });

    // Act, Assert.
    expect(startOf(call).act).toEqual({
      case: "schedule",
      value: create(conversationv1.AgentScheduleWakeupScheduleSchema, {
        delaySeconds: 1200,
        reason: "the build takes 20 minutes",
        prompt: "/loop check",
      }),
    });
  });

  it("states the stop act when the call set stop, ignoring the other fields", () => {
    // Arrange.
    const call = callWith({ stop: true, delaySeconds: 1200, reason: "ignored" });

    // Act, Assert.
    expect(startOf(call).act.case).toBe("stop");
  });

  it("stamps the instant the call was issued", () => {
    // Arrange.
    const call = callWith({ delaySeconds: 60, reason: "r", prompt: "p" });

    // Act, Assert.
    expect(startOf(call).startedAtMs).toBe(1_700_000_000_000n);
  });
});

describe("scheduleWakeupConverter.settle", () => {
  it("builds the scheduled arm from the corpus's own typed output", () => {
    // Arrange, Act.
    const success = successOf(corpusOutput());

    // Assert.
    expect(success.outcome).toEqual({
      case: "scheduled",
      value: create(conversationv1.AgentScheduleWakeupScheduledSchema, {
        wakeAtMs: 1784408640000n,
        clampedDelaySeconds: 1200,
        wasClamped: false,
      }),
    });
  });

  it("records a clamped delay as the runtime reported it", () => {
    // Arrange, Act.
    const success = successOf({ scheduledFor: 1784408640000, clampedDelaySeconds: 60, wasClamped: true });

    // Assert.
    const scheduled = success.outcome.value as conversationv1.AgentScheduleWakeupScheduled;
    expect(scheduled.wasClamped).toBe(true);
  });

  it("picks the stopped arm from the output's own stopped flag", () => {
    // Arrange, Act.
    const success = successOf({ stopped: true, cancelledWakeups: 2 });

    // Assert.
    expect(success.outcome).toEqual({
      case: "stopped",
      value: create(conversationv1.AgentScheduleWakeupStoppedSchema, { cancelledWakeups: 2 }),
    });
  });

  it("records a stop that cancelled nothing as zero, since the vendor stated it", () => {
    // Arrange, Act.
    const success = successOf({ stopped: true, cancelledWakeups: 0 });

    // Assert.
    expect((success.outcome.value as conversationv1.AgentScheduleWakeupStopped).cancelledWakeups).toBe(0);
  });

  it("produces NO frame when a scheduled wakeup named no instant to fire at", () => {
    // Arrange.
    const call = callWith({ delaySeconds: 60 });

    // Act.
    const item = scheduleWakeupConverter.settle(call, outcomeWith({ clampedDelaySeconds: 60 }));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when the settled call carried no typed output", () => {
    // Arrange.
    const call = callWith({ delaySeconds: 60 });

    // Act.
    const item = scheduleWakeupConverter.settle(call, outcomeWith("just prose"));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("settles an errored call as the failure arm", () => {
    // Arrange.
    const call = callWith({ delaySeconds: 60 });

    // Act.
    const wakeup = wakeupOf(scheduleWakeupConverter.settle(call, outcomeWith(undefined, true))!);

    // Assert.
    const failure = wakeup.result.value as conversationv1.AgentScheduleWakeupFailure;
    expect(wakeup.result.case).toBe("failure");
    expect(failure.failure?.settledAt?.atMs).toBe(1_700_000_000_100n);
  });
});
