/**
 * test/engine/network-resume.test.ts — which failures are resumed, the one
 * fixed-interval probe loop, the bounded wait, and the resume itself.
 *
 * No network and no clock: the probe is scripted, the scheduler is fired by
 * hand (or vitest's fake timers drive the real one), and `nowMs` is a variable.
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  NETWORK_RESUME_PROBE_INTERVAL_MS,
  NETWORK_RESUME_WINDOW_MS,
  NetworkResume,
  classifyAgentFailure,
  type FailureEvidence,
  type ResumeDelivery,
} from "../../src/engine/network-resume.js";
import { resumePromptTargets } from "../../src/engine/network-resume-prompt.js";
import { REAL_SCHEDULER } from "../../src/engine/keepalive.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import { ManualScheduler, ScriptedProbe } from "./fakes.js";
import { logRecordsSince, logSinkMark } from "../log-records.js";

const ENOTFOUND_NOTICE = "API Error: Can't reach the API server — check your internet or DNS (ENOTFOUND)";
const ENOTFOUND_SUMMARY =
  'Agent "Feed scroll anchoring" failed: Agent terminated early due to an API error: ' +
  "API Error: Can't reach the API server — check your internet or DNS (ENOTFOUND) (error type server_error)";

// ---------------------------------------------------------------------------
// SDK message builders
// ---------------------------------------------------------------------------

function taskStarted(taskId: string, toolUseId: string, taskType = "local_agent"): SdkMessage {
  return {
    type: "system",
    subtype: "task_started",
    task_id: taskId,
    tool_use_id: toolUseId,
    description: `agent ${taskId}`,
    task_type: taskType,
    uuid: "00000000-0000-4000-8000-000000000001",
    session_id: "s",
  } as unknown as SdkMessage;
}

function subagentError(parentToolUseId: string, errorClass: string, text: string): SdkMessage {
  return {
    type: "assistant",
    message: { model: "<synthetic>", content: [{ type: "text", text }] },
    parent_tool_use_id: parentToolUseId,
    error: errorClass,
    uuid: "00000000-0000-4000-8000-000000000002",
    session_id: "s",
  } as unknown as SdkMessage;
}

function subagentAnswer(parentToolUseId: string, model = "claude-opus-5-5"): SdkMessage {
  return {
    type: "assistant",
    message: { model, content: [{ type: "text", text: "working on it" }] },
    parent_tool_use_id: parentToolUseId,
    uuid: "00000000-0000-4000-8000-000000000003",
    session_id: "s",
  } as unknown as SdkMessage;
}

function notification(taskId: string, status: "completed" | "failed" | "stopped", summary: string): SdkMessage {
  return {
    type: "system",
    subtype: "task_notification",
    task_id: taskId,
    status,
    output_file: `/tmp/${taskId}.output`,
    summary,
    uuid: "00000000-0000-4000-8000-000000000004",
    session_id: "s",
  } as unknown as SdkMessage;
}

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

interface Harness {
  readonly resume: NetworkResume;
  readonly scheduler: ManualScheduler;
  readonly probe: ScriptedProbe;
  readonly prompts: string[];
  readonly clock: { now: number };
  delivery: ResumeDelivery | Error;
}

function harness(): Harness {
  const scheduler = new ManualScheduler();
  const probe = new ScriptedProbe();
  const prompts: string[] = [];
  const clock = { now: 1_000_000 };
  const h: Harness = {
    scheduler,
    probe,
    prompts,
    clock,
    delivery: { kind: "delivered" },
    resume: new NetworkResume({
      probe: probe.probe,
      deliver: (prompt) => {
        prompts.push(prompt);
        const answer = h.delivery;
        return answer instanceof Error ? Promise.reject(answer) : Promise.resolve(answer);
      },
      nowMs: () => clock.now,
      scheduler,
    }),
  };
  return h;
}

/** Spawn agent `taskId` under call `toolUseId` and fail it with an ENOTFOUND outage. */
function failWithOutage(h: Harness, taskId: string, toolUseId: string): void {
  h.resume.observe(taskStarted(taskId, toolUseId));
  h.resume.observe(subagentError(toolUseId, "server_error", ENOTFOUND_NOTICE));
  h.resume.observe(notification(taskId, "failed", ENOTFOUND_SUMMARY));
}

afterEach(() => {
  vi.useRealTimers();
});

// ---------------------------------------------------------------------------
// Classification
// ---------------------------------------------------------------------------

describe("classifyAgentFailure", () => {
  const NETWORK: readonly [string, FailureEvidence][] = [
    ["ENOTFOUND in the vendor's notice under server_error", { errorClass: "server_error", text: ENOTFOUND_NOTICE }],
    ["ECONNREFUSED in the prose", { errorClass: "server_error", text: "API Error: connect ECONNREFUSED 1.2.3.4:443" }],
    ["ECONNRESET in the prose", { errorClass: "unknown", text: "API Error: socket hang up (ECONNRESET)" }],
    ["ETIMEDOUT in the prose", { text: "API Error: connect ETIMEDOUT 1.2.3.4:443" }],
    ["EAI_AGAIN in the prose", { text: "getaddrinfo EAI_AGAIN api.anthropic.com" }],
    ["ENETUNREACH in the prose", { text: "connect ENETUNREACH" }],
    ["EHOSTUNREACH in the prose", { text: "connect EHOSTUNREACH" }],
    ["ENETDOWN in the prose", { text: "connect ENETDOWN" }],
    ["the vendor's can't-reach sentence with no code", { errorClass: "server_error", text: "Can't reach the API server" }],
    ["the vendor's connection-error sentence", { text: "API Error: Connection error." }],
    ["a structured ENOTFOUND connection code", { errorClass: "server_error", connectionCode: "ENOTFOUND" }],
    ["a structured ECONNREFUSED connection code with no class", { connectionCode: "ECONNREFUSED" }],
  ];
  it.each(NETWORK)("%s resumes", (_name, evidence) => {
    // Act
    const verdict = classifyAgentFailure(evidence);

    // Assert
    expect(verdict.network).toBe(true);
  });

  const NOT_NETWORK: readonly [string, FailureEvidence][] = [
    ["authentication_failed", { errorClass: "authentication_failed", text: ENOTFOUND_NOTICE }],
    ["oauth_org_not_allowed", { errorClass: "oauth_org_not_allowed", text: ENOTFOUND_NOTICE }],
    ["account_on_hold", { errorClass: "account_on_hold", text: ENOTFOUND_NOTICE }],
    ["verification_required", { errorClass: "verification_required", text: ENOTFOUND_NOTICE }],
    ["billing_error", { errorClass: "billing_error", text: ENOTFOUND_NOTICE }],
    ["rate_limit", { errorClass: "rate_limit", text: ENOTFOUND_NOTICE }],
    ["overloaded", { errorClass: "overloaded", text: ENOTFOUND_NOTICE }],
    ["invalid_request", { errorClass: "invalid_request", text: ENOTFOUND_NOTICE }],
    ["model_not_found", { errorClass: "model_not_found", text: ENOTFOUND_NOTICE }],
    ["max_output_tokens", { errorClass: "max_output_tokens", text: ENOTFOUND_NOTICE }],
    ["cloud_credential_error", { errorClass: "cloud_credential_error", text: ENOTFOUND_NOTICE }],
  ];
  it.each(NOT_NETWORK)("the %s class is never resumed, whatever the prose says", (_name, evidence) => {
    // Act
    const verdict = classifyAgentFailure(evidence);

    // Assert
    expect(verdict).toMatchObject({ network: false, basis: "error_class" });
  });

  it("a structured connection code that is not a reachability failure is not resumed", () => {
    // Act
    const verdict = classifyAgentFailure({ errorClass: "server_error", connectionCode: "CERT_HAS_EXPIRED", text: ENOTFOUND_NOTICE });

    // Assert
    expect(verdict).toMatchObject({ network: false, basis: "connection_code" });
  });

  it("an HTTP status means the API answered, so it is not resumed", () => {
    // Act
    const verdict = classifyAgentFailure({ errorClass: "server_error", httpStatus: 500, text: ENOTFOUND_NOTICE });

    // Assert
    expect(verdict).toMatchObject({ network: false, basis: "http_status" });
  });

  it("a summary naming a non-network class is not resumed when no structured class was seen", () => {
    // Act
    const verdict = classifyAgentFailure({ text: "Agent failed: API Error: 429 ENOTFOUND (error type rate_limit)" });

    // Assert
    expect(verdict).toMatchObject({ network: false, basis: "text" });
  });

  it("a server_error with ordinary prose is not resumed", () => {
    // Act
    const verdict = classifyAgentFailure({ errorClass: "server_error", text: "API Error: 500 Internal server error" });

    // Assert
    expect(verdict.network).toBe(false);
  });

  it("a failure with no evidence at all is not resumed", () => {
    // Act
    const verdict = classifyAgentFailure({});

    // Assert
    expect(verdict.network).toBe(false);
  });
});

// ---------------------------------------------------------------------------
// What is waited on
// ---------------------------------------------------------------------------

describe("which failures wait for the API", () => {
  it("a subagent cut off by ENOTFOUND waits to be resumed", () => {
    // Arrange
    const h = harness();

    // Act
    failWithOutage(h, "a1", "toolu_spawn");

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual(["a1"]);
  });

  it("the wait is logged at INFO with the agent and the verdict's basis", () => {
    // Arrange
    const h = harness();
    const mark = logSinkMark();

    // Act
    failWithOutage(h, "a1", "toolu_spawn");

    // Assert
    const waiting = logRecordsSince(mark).find((record) => record.context.outcome === "waiting");
    expect(waiting).toMatchObject({ level: "info", context: { task_id: "a1", basis: "text" } });
  });

  it("the text fallback alone is enough when the agent's own error message never arrived", () => {
    // Arrange
    const h = harness();
    h.resume.observe(taskStarted("a1", "toolu_spawn"));

    // Act
    h.resume.observe(notification("a1", "failed", ENOTFOUND_SUMMARY));

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual(["a1"]);
  });

  it("a subagent that failed on a rate limit does not wait", () => {
    // Arrange
    const h = harness();
    h.resume.observe(taskStarted("a1", "toolu_spawn"));
    h.resume.observe(subagentError("toolu_spawn", "rate_limit", "API Error: rate limited"));

    // Act
    h.resume.observe(notification("a1", "failed", "Agent failed: rate limited (error type rate_limit)"));

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual([]);
    expect(h.resume.looping()).toBe(false);
  });

  it("a non-network failure is logged as not resumed", () => {
    // Arrange
    const h = harness();
    h.resume.observe(taskStarted("a1", "toolu_spawn"));
    const mark = logSinkMark();

    // Act
    h.resume.observe(notification("a1", "failed", "The agent raised."));

    // Assert
    const record = logRecordsSince(mark).find((r) => r.context.outcome === "not_resumed");
    expect(record).toMatchObject({ level: "info", context: { task_id: "a1" } });
  });

  it("a failed shell is never an agent to resume", () => {
    // Arrange
    const h = harness();
    h.resume.observe(taskStarted("b1", "toolu_bash", "local_bash"));

    // Act
    h.resume.observe(notification("b1", "failed", ENOTFOUND_SUMMARY));

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual([]);
  });

  it("a failed task this process never saw start is not resumed", () => {
    // Arrange
    const h = harness();

    // Act
    h.resume.observe(notification("a9", "failed", ENOTFOUND_SUMMARY));

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual([]);
  });

  it("an earlier run's error does not classify a later run's failure", () => {
    // Arrange: the first run stated a network error, then a new run starts and fails on auth.
    const h = harness();
    h.resume.observe(taskStarted("a1", "toolu_spawn"));
    h.resume.observe(subagentError("toolu_spawn", "server_error", ENOTFOUND_NOTICE));
    h.resume.observe(taskStarted("a1", "toolu_send"));

    // Act
    h.resume.observe(notification("a1", "failed", "Agent failed: login required"));

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// The one probe loop
// ---------------------------------------------------------------------------

describe("the probe loop", () => {
  it("is scheduled at exactly five seconds", () => {
    // Arrange
    const h = harness();

    // Act
    failWithOutage(h, "a1", "toolu_spawn");

    // Assert
    expect(h.scheduler.intervals).toEqual([NETWORK_RESUME_PROBE_INTERVAL_MS]);
    expect(NETWORK_RESUME_PROBE_INTERVAL_MS).toBe(5_000);
  });

  it("is ONE loop for many waiting agents", () => {
    // Arrange
    const h = harness();

    // Act
    failWithOutage(h, "a1", "toolu_1");
    failWithOutage(h, "a2", "toolu_2");
    failWithOutage(h, "a3", "toolu_3");

    // Assert
    expect(h.scheduler.handlers).toHaveLength(1);
    expect(h.resume.waitingTaskIds()).toEqual(["a1", "a2", "a3"]);
  });

  it("probes once per beat for every waiting agent together", async () => {
    // Arrange
    const h = harness();
    h.probe.reachable = false;
    failWithOutage(h, "a1", "toolu_1");
    failWithOutage(h, "a2", "toolu_2");

    // Act
    await h.resume.tick();

    // Assert
    expect(h.probe.calls).toBe(1);
  });

  it("probes on a FIXED interval: no backoff however long the API stays down", async () => {
    // Arrange: the REAL scheduler under fake timers, so the beat's timing is what is measured.
    vi.useFakeTimers();
    const probe = new ScriptedProbe();
    probe.reachable = false;
    const probeTimes: number[] = [];
    const resume = new NetworkResume({
      probe: () => {
        probeTimes.push(Date.now());
        return probe.probe();
      },
      deliver: () => Promise.resolve({ kind: "delivered" }),
      nowMs: () => Date.now(),
      scheduler: REAL_SCHEDULER,
    });
    const start = Date.now();
    failWithOutage({ resume } as Harness, "a1", "toolu_spawn");

    // Act
    await vi.advanceTimersByTimeAsync(60_000);

    // Assert
    expect(probeTimes.map((at) => at - start)).toEqual(
      Array.from({ length: 12 }, (_unused, index) => (index + 1) * NETWORK_RESUME_PROBE_INTERVAL_MS),
    );
    resume.stop("the test is over");
  });

  it("skips a beat while the previous probe is still out", async () => {
    // Arrange
    const h = harness();
    let answer: (value: { reachable: boolean; detail: string }) => void = () => undefined;
    let calls = 0;
    const resume = new NetworkResume({
      probe: () => {
        calls++;
        return new Promise((resolve) => {
          answer = resolve;
        });
      },
      deliver: () => Promise.resolve({ kind: "delivered" }),
      nowMs: () => h.clock.now,
      scheduler: h.scheduler,
    });
    failWithOutage({ ...h, resume }, "a1", "toolu_spawn");
    const first = resume.tick();

    // Act
    await resume.tick();

    // Assert
    expect(calls).toBe(1);
    answer({ reachable: false, detail: "down" });
    await first;
  });

  it("an unreachable API delivers nothing and the agent keeps waiting", async () => {
    // Arrange
    const h = harness();
    h.probe.reachable = false;
    failWithOutage(h, "a1", "toolu_spawn");

    // Act
    await h.resume.tick();

    // Assert
    expect(h.prompts).toEqual([]);
    expect(h.resume.waitingTaskIds()).toEqual(["a1"]);
  });

  it("a probe that throws is recorded at ERROR and the agent keeps waiting", async () => {
    // Arrange
    const h = harness();
    const resume = new NetworkResume({
      probe: () => Promise.reject(new Error("probe bug")),
      deliver: () => Promise.resolve({ kind: "delivered" }),
      nowMs: () => h.clock.now,
      scheduler: h.scheduler,
    });
    failWithOutage({ ...h, resume }, "a1", "toolu_spawn");
    const mark = logSinkMark();

    // Act
    await resume.tick();

    // Assert
    expect(logRecordsSince(mark).find((r) => r.level === "error")?.context.cause).toBe("probe bug");
    expect(resume.waitingTaskIds()).toEqual(["a1"]);
  });

  it("stand-down cancels the loop", () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");

    // Act
    h.resume.stop("the session was killed");

    // Assert
    expect(h.scheduler.cleared).toBe(1);
    expect(h.resume.looping()).toBe(false);
  });

  it("stand-down abandons every wait, naming each at INFO", () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    const mark = logSinkMark();

    // Act
    h.resume.stop("the session was killed");

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual([]);
    expect(logRecordsSince(mark).find((r) => r.context.outcome === "abandoned")).toMatchObject({
      level: "info",
      context: { task_id: "a1", reason: "the session was killed" },
    });
  });

  it("after stand-down a new failure starts no loop", () => {
    // Arrange
    const h = harness();
    h.resume.stop("the session was killed");

    // Act
    failWithOutage(h, "a1", "toolu_spawn");

    // Assert
    expect(h.scheduler.handlers).toHaveLength(0);
  });

  it("a beat after stand-down probes nothing", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    h.resume.stop("the session was killed");

    // Act
    await h.resume.tick();

    // Assert
    expect(h.probe.calls).toBe(0);
  });
});

// ---------------------------------------------------------------------------
// The resume
// ---------------------------------------------------------------------------

describe("the resume", () => {
  it("continues the SAME agent: the prompt addresses the failed agent's own id", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a8586cc4fd87da466", "toolu_spawn");

    // Act
    await h.resume.tick();

    // Assert
    expect(h.prompts).toHaveLength(1);
    expect(resumePromptTargets(h.prompts[0] ?? "")).toEqual(["a8586cc4fd87da466"]);
  });

  it("one prompt continues every agent that was waiting", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_1");
    failWithOutage(h, "a2", "toolu_2");

    // Act
    await h.resume.tick();

    // Assert
    expect(h.prompts.map(resumePromptTargets)).toEqual([["a1", "a2"]]);
  });

  it("a delivered resume empties the wait and stops the loop", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");

    // Act
    await h.resume.tick();

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual([]);
    expect(h.resume.looping()).toBe(false);
  });

  it("a delivered resume is logged at INFO", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    const mark = logSinkMark();

    // Act
    await h.resume.tick();

    // Assert
    expect(logRecordsSince(mark).find((r) => r.context.outcome === "resumed")).toMatchObject({
      level: "info",
      context: { task_id: "a1", resumes: 1 },
    });
  });

  it("resumes at most once per failure event", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    await h.resume.tick();

    // Act
    await h.resume.tick();
    await h.resume.tick();

    // Assert
    expect(h.prompts).toHaveLength(1);
  });

  it("a busy main agent defers the resume to a later beat", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    h.delivery = { kind: "busy", detail: "turn t1 is open" };
    await h.resume.tick();

    // Act
    h.delivery = { kind: "delivered" };
    await h.resume.tick();

    // Assert
    expect(h.prompts).toHaveLength(2);
    expect(h.resume.waitingTaskIds()).toEqual([]);
  });

  it("a busy main agent keeps the agent waiting", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    h.delivery = { kind: "busy", detail: "turn t1 is open" };

    // Act
    await h.resume.tick();

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual(["a1"]);
  });

  it("a session that can take no prompt gives up at ERROR", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    h.delivery = { kind: "unavailable", detail: "no vendor query is accepting prompts" };
    const mark = logSinkMark();

    // Act
    await h.resume.tick();

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual([]);
    expect(logRecordsSince(mark).find((r) => r.context.outcome === "gave_up")?.level).toBe("error");
  });

  it("a delivery that throws gives up at ERROR, naming the cause", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    h.delivery = new Error("the queue is closed");
    const mark = logSinkMark();

    // Act
    await h.resume.tick();

    // Assert
    const gaveUp = logRecordsSince(mark).find((r) => r.context.outcome === "gave_up");
    expect(gaveUp?.level).toBe("error");
    expect(String(gaveUp?.context.detail)).toContain("the queue is closed");
  });
});

// ---------------------------------------------------------------------------
// The bounded wait
// ---------------------------------------------------------------------------

describe("the thirty-minute window", () => {
  it("is thirty minutes", () => {
    // Assert
    expect(NETWORK_RESUME_WINDOW_MS).toBe(30 * 60 * 1_000);
  });

  it("an agent still waits one millisecond before the window closes", async () => {
    // Arrange
    const h = harness();
    h.probe.reachable = false;
    failWithOutage(h, "a1", "toolu_spawn");

    // Act
    h.clock.now += NETWORK_RESUME_WINDOW_MS - 1;
    await h.resume.tick();

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual(["a1"]);
  });

  it("gives up when the window closes", async () => {
    // Arrange
    const h = harness();
    h.probe.reachable = false;
    failWithOutage(h, "a1", "toolu_spawn");

    // Act
    h.clock.now += NETWORK_RESUME_WINDOW_MS;
    await h.resume.tick();

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual([]);
    expect(h.resume.looping()).toBe(false);
  });

  it("giving up is logged at ERROR with how long it waited", async () => {
    // Arrange
    const h = harness();
    h.probe.reachable = false;
    failWithOutage(h, "a1", "toolu_spawn");
    const mark = logSinkMark();

    // Act
    h.clock.now += NETWORK_RESUME_WINDOW_MS;
    await h.resume.tick();

    // Assert
    expect(logRecordsSince(mark).find((r) => r.context.outcome === "gave_up")).toMatchObject({
      level: "error",
      context: { task_id: "a1", waited_ms: NETWORK_RESUME_WINDOW_MS },
    });
  });

  it("a give-up does not probe", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");

    // Act
    h.clock.now += NETWORK_RESUME_WINDOW_MS;
    await h.resume.tick();

    // Assert
    expect(h.probe.calls).toBe(0);
  });
});

// ---------------------------------------------------------------------------
// No infinite resume loop
// ---------------------------------------------------------------------------

describe("a resumed agent that fails again", () => {
  /** Fail a1, resume it, and start its resumed run under a SendMessage call. */
  async function resumedOnce(h: Harness): Promise<void> {
    failWithOutage(h, "a1", "toolu_spawn");
    await h.resume.tick();
    h.resume.observe(taskStarted("a1", "toolu_send"));
  }

  it("without progress keeps the window its FIRST failure opened", async () => {
    // Arrange
    const h = harness();
    await resumedOnce(h);
    h.clock.now += NETWORK_RESUME_WINDOW_MS - 10;
    h.resume.observe(subagentError("toolu_send", "server_error", ENOTFOUND_NOTICE));
    h.resume.observe(notification("a1", "failed", ENOTFOUND_SUMMARY));
    h.probe.reachable = false;

    // Act
    h.clock.now += 10;
    await h.resume.tick();

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual([]);
  });

  it("without progress past its first window gives up at once", async () => {
    // Arrange
    const h = harness();
    await resumedOnce(h);
    h.clock.now += NETWORK_RESUME_WINDOW_MS;
    const mark = logSinkMark();

    // Act
    h.resume.observe(notification("a1", "failed", ENOTFOUND_SUMMARY));

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual([]);
    expect(logRecordsSince(mark).find((r) => r.context.outcome === "gave_up")?.level).toBe("error");
  });

  it("after the model answered starts a fresh window", async () => {
    // Arrange
    const h = harness();
    await resumedOnce(h);
    h.resume.observe(subagentAnswer("toolu_send"));
    h.clock.now += NETWORK_RESUME_WINDOW_MS - 10;
    h.resume.observe(subagentError("toolu_send", "server_error", ENOTFOUND_NOTICE));
    h.resume.observe(notification("a1", "failed", ENOTFOUND_SUMMARY));
    h.probe.reachable = false;

    // Act
    h.clock.now += 10;
    await h.resume.tick();

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual(["a1"]);
  });

  it("a synthetic notice is not progress", async () => {
    // Arrange
    const h = harness();
    await resumedOnce(h);
    h.resume.observe(subagentAnswer("toolu_send", "<synthetic>"));
    h.clock.now += NETWORK_RESUME_WINDOW_MS - 10;
    h.resume.observe(notification("a1", "failed", ENOTFOUND_SUMMARY));
    h.probe.reachable = false;

    // Act
    h.clock.now += 10;
    await h.resume.tick();

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual([]);
  });

  it("is resumed once more for the new failure event", async () => {
    // Arrange
    const h = harness();
    await resumedOnce(h);
    h.resume.observe(notification("a1", "failed", ENOTFOUND_SUMMARY));

    // Act
    await h.resume.tick();

    // Assert
    expect(h.prompts).toHaveLength(2);
  });

  it("that completes ends its history, so a later outage opens a fresh window", async () => {
    // Arrange
    const h = harness();
    await resumedOnce(h);
    h.resume.observe(notification("a1", "completed", "done"));
    h.clock.now += NETWORK_RESUME_WINDOW_MS + 1;
    failWithOutage(h, "a1", "toolu_send2");
    h.probe.reachable = false;

    // Act
    await h.resume.tick();

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual(["a1"]);
  });
});
