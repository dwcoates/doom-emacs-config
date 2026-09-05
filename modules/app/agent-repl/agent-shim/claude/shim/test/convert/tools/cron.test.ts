/**
 * The cron converter. Three vendor tool names, one unit kind. The listing is
 * REPLACE semantics for its reader, which is why a listing whose typed output
 * states no jobs array produces no frame at all.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { toolResultText } from "../../../src/convert/entries.js";
import { cronConverter } from "../../../src/convert/tools/cron.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { conversationv1 } from "../../../src/proto.js";

const AGENT_ID = create(conversationv1.AgentIdSchema, { value: "session-1" });

function callNamed(toolName: string, input: Record<string, unknown> = {}): PendingCall {
  return {
    toolUseId: "toolu_cron",
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
    settledAtMs: 1_700_000_006_000,
  };
}

function cronOf(item: conversationv1.AgentActivity["item"] | undefined): conversationv1.AgentCron {
  expect(item?.case).toBe("cron");
  return item?.value as conversationv1.AgentCron;
}

function startOf(call: PendingCall): conversationv1.AgentCronStart {
  const cron = cronOf(cronConverter.start(call));
  expect(cron.state.case).toBe("start");
  return cron.state.value as conversationv1.AgentCronStart;
}

function successOf(toolName: string, structured: unknown): conversationv1.AgentCronSuccess {
  const cron = cronOf(cronConverter.settle(callNamed(toolName), outcomeWith(structured))!);
  expect(cron.state.case).toBe("success");
  return cron.state.value as conversationv1.AgentCronSuccess;
}

describe("cronConverter kind and arms", () => {
  it("declares the cron kind", () => {
    // Arrange, Act, Assert.
    expect(cronConverter.kind).toBe("cron");
  });

  it("carries NO progress, because AgentCron declares no such arm", () => {
    // Arrange, Act, Assert.
    expect(cronConverter.carriesProgress).toBe(false);
  });
});

describe("cronConverter.start", () => {
  it("states a create with the schedule and prompt as asked", () => {
    // Arrange, Act.
    const call = callNamed("CronCreate", { cron: "*/5 * * * *", prompt: "check the deploy", recurring: true, durable: true });

    // Assert.
    expect(startOf(call).act).toEqual({
      case: "create",
      value: create(conversationv1.AgentCronCreateSchema, {
        cron: "*/5 * * * *",
        prompt: "check the deploy",
        recurring: true,
        durable: true,
      }),
    });
  });

  it("defaults a create's unstated durability to the vendor's own false", () => {
    // Arrange, Act.
    const call = callNamed("CronCreate", { cron: "* * * * *", prompt: "p" });

    // Assert.
    expect((startOf(call).act.value as conversationv1.AgentCronCreate).durable).toBe(false);
  });

  it("states a delete by the job id the call named", () => {
    // Arrange, Act.
    const call = callNamed("CronDelete", { id: "job_01ABC" });

    // Assert.
    expect(startOf(call).act).toEqual({
      case: "delete",
      value: create(conversationv1.AgentCronDeleteSchema, { jobId: "job_01ABC" }),
    });
  });

  it("still announces a delete whose call named no job id", () => {
    // Arrange, Act.
    const call = callNamed("CronDelete", {});

    // Assert.
    expect((startOf(call).act.value as conversationv1.AgentCronDelete).jobId).toBe("");
  });

  it("states a list as the empty act it is", () => {
    // Arrange, Act, Assert.
    expect(startOf(callNamed("CronList")).act.case).toBe("list");
  });

  it("leaves the act unstated for a name that is none of the three calls", () => {
    // Arrange, Act, Assert.
    expect(startOf(callNamed("SomethingElse")).act.case).toBeUndefined();
  });

  it("stamps the instant the call was issued", () => {
    // Arrange, Act, Assert.
    expect(startOf(callNamed("CronList")).startedAt?.atMs).toBe(1_700_000_000_000n);
  });
});

describe("cronConverter.settle", () => {
  it("states the created job by the vendor's own opaque id", () => {
    // Arrange, Act.
    const success = successOf("CronCreate", {
      id: "job_01ABC",
      humanSchedule: "every 5 minutes",
      recurring: true,
      durable: false,
    });

    // Assert.
    expect(success.act).toEqual({
      case: "created",
      value: create(conversationv1.AgentCronCreatedSchema, {
        jobId: "job_01ABC",
        humanSchedule: "every 5 minutes",
        recurring: true,
        durable: false,
      }),
    });
  });

  it("states the deleted job by the id the result reported", () => {
    // Arrange, Act.
    const success = successOf("CronDelete", { id: "job_01ABC" });

    // Assert.
    expect(success.act).toEqual({
      case: "deleted",
      value: create(conversationv1.AgentCronDeletedSchema, { jobId: "job_01ABC" }),
    });
  });

  it("states the job set WHOLE, row by row", () => {
    // Arrange, Act.
    const success = successOf("CronList", {
      jobs: [
        { id: "job_1", cron: "* * * * *", humanSchedule: "every minute", prompt: "p", recurring: true, durable: false },
      ],
    });

    // Assert.
    expect(success.act).toEqual({
      case: "listed",
      value: create(conversationv1.AgentCronListedSchema, {
        jobs: [
          create(conversationv1.AgentCronJobSchema, {
            jobId: "job_1",
            cron: "* * * * *",
            humanSchedule: "every minute",
            prompt: "p",
            recurring: true,
            durable: false,
          }),
        ],
      }),
    });
  });

  it("defaults a listing row's unstated recurrence and durability to the vendor's own false", () => {
    // Arrange, Act.
    const success = successOf("CronList", { jobs: [{ id: "job_2", cron: "* * * * *", prompt: "p" }] });

    // Assert.
    const job = (success.act.value as conversationv1.AgentCronListed).jobs[0]!;
    expect([job.recurring, job.durable]).toEqual([false, false]);
  });

  it("states an EMPTY job set when the vendor really reported none", () => {
    // Arrange, Act.
    const success = successOf("CronList", { jobs: [] });

    // Assert.
    expect((success.act.value as conversationv1.AgentCronListed).jobs).toEqual([]);
  });

  it("drops a listing row that named no job id", () => {
    // Arrange, Act.
    const success = successOf("CronList", { jobs: [{ cron: "* * * * *", prompt: "p" }] });

    // Assert.
    expect((success.act.value as conversationv1.AgentCronListed).jobs).toEqual([]);
  });

  it("defaults a created job's unstated recurrence and durability to the vendor's own false", () => {
    // Arrange, Act.
    const success = successOf("CronCreate", { id: "job_01ABC", humanSchedule: "hourly" });

    // Assert.
    const created = success.act.value as conversationv1.AgentCronCreated;
    expect([created.recurring, created.durable]).toEqual([false, false]);
  });

  it("stamps the settle instant on the success arm", () => {
    // Arrange, Act.
    const success = successOf("CronDelete", { id: "job_01ABC" });

    // Assert.
    expect(success.settledAt?.atMs).toBe(1_700_000_006_000n);
  });

  it("produces NO frame for a listing that stated no jobs array", () => {
    // Arrange, Act: an empty listed arm would tell the reader to forget every
    // job it knows about, on a field the vendor never sent.
    const item = cronConverter.settle(callNamed("CronList"), outcomeWith({}));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when a settled create named no job id", () => {
    // Arrange, Act.
    const item = cronConverter.settle(callNamed("CronCreate"), outcomeWith({ humanSchedule: "hourly" }));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when the settled call carried no typed output", () => {
    // Arrange, Act.
    const item = cronConverter.settle(callNamed("CronCreate"), outcomeWith("just prose"));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("settles an errored call as the failure arm", () => {
    // Arrange, Act.
    const cron = cronOf(cronConverter.settle(callNamed("CronCreate"), outcomeWith(undefined, true))!);

    // Assert.
    const failure = cron.state.value as conversationv1.AgentCronFailure;
    expect(cron.state.case).toBe("failure");
    expect(failure.error?.settledAt?.atMs).toBe(1_700_000_006_000n);
  });
});
