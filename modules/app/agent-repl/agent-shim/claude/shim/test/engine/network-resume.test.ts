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
import { conversationv1 } from "../../src/proto.js";
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

function notification(
  taskId: string,
  status: "completed" | "failed" | "stopped",
  summary: string,
  toolUseId?: string,
): SdkMessage {
  return {
    type: "system",
    subtype: "task_notification",
    task_id: taskId,
    ...(toolUseId === undefined ? {} : { tool_use_id: toolUseId }),
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
  /** Every update the seam stated on the session stream, in order. */
  readonly emitted: conversationv1.SessionUpdate[];
  delivery: ResumeDelivery | Error;
}

function harness(): Harness {
  const scheduler = new ManualScheduler();
  const probe = new ScriptedProbe();
  const prompts: string[] = [];
  const clock = { now: 1_000_000 };
  const emitted: conversationv1.SessionUpdate[] = [];
  const h: Harness = {
    scheduler,
    probe,
    prompts,
    clock,
    emitted,
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
      emit: (update) => emitted.push(update),
    }),
  };
  return h;
}

/** One stated wait, flattened for comparison. */
interface StatedWait {
  readonly work: string;
  readonly failedAtMs: number;
  readonly givesUpAtMs: number;
  readonly resumesDelivered: number;
}

/** Every waiting set the seam stated, in order, each flattened. */
function statedSets(h: Harness): StatedWait[][] {
  return h.emitted.flatMap((update) =>
    update.update.case === "networkResumeWaits"
      ? [
          update.update.value.waits.map((wait) => ({
            work: wait.work?.value ?? "",
            failedAtMs: Number(wait.failedAtMs),
            givesUpAtMs: Number(wait.givesUpAtMs),
            resumesDelivered: wait.resumesDelivered,
          })),
        ]
      : [],
  );
}

/** The last waiting set the seam stated. */
function lastSet(h: Harness): StatedWait[] | undefined {
  return statedSets(h).at(-1);
}

/** One stated outcome, flattened: the work and the arm (plus an abandonment's reason). */
function statedOutcomes(h: Harness): { work: string; end: string; reason?: string }[] {
  return h.emitted.flatMap((update) => {
    if (update.update.case !== "networkResumeOutcome") return [];
    const { work, outcome } = update.update.value;
    return [
      {
        work: work?.value ?? "",
        end: outcome.case ?? "",
        ...(outcome.case === "abandoned" ? { reason: outcome.value.reason } : {}),
      },
    ];
  });
}

/** The arm of every update the seam stated, in order. */
function statedArms(h: Harness): string[] {
  return h.emitted.map((update) => update.update.case ?? "");
}

/** Spawn agent `taskId` under call `toolUseId` and fail it with an ENOTFOUND outage. */
function failWithOutage(h: Harness, taskId: string, toolUseId: string): void {
  h.resume.observe(taskStarted(taskId, toolUseId));
  h.resume.observe(subagentError(toolUseId, "server_error", ENOTFOUND_NOTICE));
  h.resume.observe(notification(taskId, "failed", ENOTFOUND_SUMMARY));
}

/** Fail a1, resume it, and start its resumed run under a SendMessage call. */
async function resumedOnce(h: Harness): Promise<void> {
  failWithOutage(h, "a1", "toolu_spawn");
  await h.resume.tick();
  h.resume.observe(taskStarted("a1", "toolu_send"));
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
    ["Node's fetch failure", { text: "TypeError: fetch failed" }],
    ["Node's socket hang up with no code", { text: "socket hang up" }],
    ["a refused connection in words", { text: "connection refused" }],
    ["a reset connection in words", { text: "connection reset by peer" }],
    ["a connect that timed out in words", { text: "the request timed out" }],
    ["the word network", { text: "a network error occurred" }],
    ["a lowercase errno", { text: "getaddrinfo enotfound api.anthropic.com" }],
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
      emit: () => undefined,
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
      emit: () => undefined,
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
      emit: () => undefined,
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

// ---------------------------------------------------------------------------
// What the session stream is told (session.proto, network_resume_waits and
// network_resume_outcome; footer-activity-tiers.md, landed change 2)
// ---------------------------------------------------------------------------

describe("the waiting set on the session stream", () => {
  it("a wait opening states the set with the failed run's work, failure instant, deadline and no resumes", () => {
    // Arrange
    const h = harness();
    const failedAt = h.clock.now;

    // Act
    failWithOutage(h, "a1", "toolu_spawn");

    // Assert
    expect(lastSet(h)).toEqual([
      {
        work: "toolu_spawn",
        failedAtMs: failedAt,
        givesUpAtMs: failedAt + NETWORK_RESUME_WINDOW_MS,
        resumesDelivered: 0,
      },
    ]);
  });

  it("a second wait opening states both waits, in the order they opened", () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_1");

    // Act
    failWithOutage(h, "a2", "toolu_2");

    // Assert
    expect(lastSet(h)?.map((wait) => wait.work)).toEqual(["toolu_1", "toolu_2"]);
  });

  it("a failure notification that names its call names that call as the work", () => {
    // Arrange
    const h = harness();
    h.resume.observe(taskStarted("a1", "toolu_spawn"));
    h.resume.observe(taskStarted("a1", "toolu_later"));
    h.resume.observe(subagentError("toolu_spawn", "server_error", ENOTFOUND_NOTICE));

    // Act
    h.resume.observe(notification("a1", "failed", ENOTFOUND_SUMMARY, "toolu_spawn"));

    // Assert
    expect(lastSet(h)?.map((wait) => wait.work)).toEqual(["toolu_spawn"]);
  });

  it("an unreachable beat states nothing, because nothing changed", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    h.probe.reachable = false;
    const before = h.emitted.length;

    // Act
    await h.resume.tick();

    // Assert
    expect(h.emitted).toHaveLength(before);
  });

  it("a busy main agent states nothing, because nothing changed", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    h.delivery = { kind: "busy", detail: "a turn is open" };
    const before = h.emitted.length;

    // Act
    await h.resume.tick();

    // Assert
    expect(h.emitted).toHaveLength(before);
  });

  it("a failure that is not a network outage states nothing", () => {
    // Arrange
    const h = harness();
    h.resume.observe(taskStarted("a1", "toolu_spawn"));
    h.resume.observe(subagentError("toolu_spawn", "rate_limit", "API Error: 429"));

    // Act
    h.resume.observe(notification("a1", "failed", "failed (error type rate_limit)"));

    // Assert
    expect(h.emitted).toEqual([]);
  });
});

describe("a wait's outcome on the session stream", () => {
  it("a delivered resume states resumed for the failed run's work", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");

    // Act
    await h.resume.tick();

    // Assert
    expect(statedOutcomes(h)).toEqual([{ work: "toolu_spawn", end: "resumed" }]);
  });

  it("a delivered resume states the outcome, then the set without the wait", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    h.emitted.length = 0;

    // Act
    await h.resume.tick();

    // Assert
    expect([statedArms(h), lastSet(h)]).toEqual([["networkResumeOutcome", "networkResumeWaits"], []]);
  });

  it("a resume that reaches only the waits due states the set still holding a wait opened meanwhile", async () => {
    // Arrange
    const h = harness();
    let release: (delivery: ResumeDelivery) => void = () => undefined;
    let delivering: () => void = () => undefined;
    const asked = new Promise<void>((resolve) => {
      delivering = resolve;
    });
    const resume = new NetworkResume({
      probe: h.probe.probe,
      deliver: () =>
        new Promise((resolve) => {
          release = resolve;
          delivering();
        }),
      nowMs: () => h.clock.now,
      scheduler: h.scheduler,
      emit: (update) => h.emitted.push(update),
    });
    failWithOutage({ ...h, resume }, "a1", "toolu_1");
    const beat = resume.tick();
    await asked;
    failWithOutage({ ...h, resume }, "a2", "toolu_2");

    // Act
    release({ kind: "delivered" });
    await beat;

    // Assert
    expect(lastSet(h)?.map((wait) => wait.work)).toEqual(["toolu_2"]);
  });

  it("a window that runs out states gave_up for the work", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    h.clock.now += NETWORK_RESUME_WINDOW_MS;

    // Act
    await h.resume.tick();

    // Assert
    expect(statedOutcomes(h)).toEqual([{ work: "toolu_spawn", end: "gaveUp" }]);
  });

  it("a window that runs out states the empty set once the last wait ended", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    h.clock.now += NETWORK_RESUME_WINDOW_MS;

    // Act
    await h.resume.tick();

    // Assert
    expect(lastSet(h)).toEqual([]);
  });

  it("a resume that cannot be delivered states gave_up for the work", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    h.delivery = { kind: "unavailable", detail: "no query" };

    // Act
    await h.resume.tick();

    // Assert
    expect(statedOutcomes(h)).toEqual([{ work: "toolu_spawn", end: "gaveUp" }]);
  });

  it("a resume that cannot be delivered states the empty set", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    h.delivery = { kind: "unavailable", detail: "no query" };

    // Act
    await h.resume.tick();

    // Assert
    expect(lastSet(h)).toEqual([]);
  });

  it("stand-down states abandoned with its reason for every wait", () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_1");
    failWithOutage(h, "a2", "toolu_2");

    // Act
    h.resume.stop("session closing");

    // Assert
    expect(statedOutcomes(h)).toEqual([
      { work: "toolu_1", end: "abandoned", reason: "session closing" },
      { work: "toolu_2", end: "abandoned", reason: "session closing" },
    ]);
  });

  it("stand-down states the empty set after the abandonments", () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    h.emitted.length = 0;

    // Act
    h.resume.stop("session closing");

    // Assert
    expect([statedArms(h), lastSet(h)]).toEqual([["networkResumeOutcome", "networkResumeWaits"], []]);
  });

  it("stand-down with nothing waiting states nothing", () => {
    // Arrange
    const h = harness();

    // Act
    h.resume.stop("session closing");

    // Assert
    expect(h.emitted).toEqual([]);
  });
});

describe("a resumed agent's next wait on the session stream", () => {
  it("names the resuming send's run as the work", async () => {
    // Arrange
    const h = harness();
    await resumedOnce(h);

    // Act
    h.resume.observe(notification("a1", "failed", ENOTFOUND_SUMMARY));

    // Assert
    expect(lastSet(h)?.map((wait) => wait.work)).toEqual(["toolu_send"]);
  });

  it("counts the resume already delivered", async () => {
    // Arrange
    const h = harness();
    await resumedOnce(h);

    // Act
    h.resume.observe(notification("a1", "failed", ENOTFOUND_SUMMARY));

    // Assert
    expect(lastSet(h)?.map((wait) => wait.resumesDelivered)).toEqual([1]);
  });

  it("states the instant of the new failure", async () => {
    // Arrange
    const h = harness();
    await resumedOnce(h);
    h.clock.now += 5_000;

    // Act
    h.resume.observe(notification("a1", "failed", ENOTFOUND_SUMMARY));

    // Assert
    expect(lastSet(h)?.map((wait) => wait.failedAtMs)).toEqual([h.clock.now]);
  });

  it("without progress keeps the deadline its first failure set", async () => {
    // Arrange
    const h = harness();
    const firstFailure = h.clock.now;
    await resumedOnce(h);
    h.clock.now += 5_000;

    // Act
    h.resume.observe(notification("a1", "failed", ENOTFOUND_SUMMARY));

    // Assert
    expect(lastSet(h)?.map((wait) => wait.givesUpAtMs)).toEqual([firstFailure + NETWORK_RESUME_WINDOW_MS]);
  });

  it("after progress moves the deadline to a window from the new failure", async () => {
    // Arrange
    const h = harness();
    await resumedOnce(h);
    h.resume.observe(subagentAnswer("toolu_send"));
    h.clock.now += 5_000;

    // Act
    h.resume.observe(notification("a1", "failed", ENOTFOUND_SUMMARY));

    // Assert
    expect(lastSet(h)?.map((wait) => wait.givesUpAtMs)).toEqual([h.clock.now + NETWORK_RESUME_WINDOW_MS]);
  });

  it("past its first window without progress states nothing, because no wait opened", async () => {
    // Arrange
    const h = harness();
    await resumedOnce(h);
    h.clock.now += NETWORK_RESUME_WINDOW_MS;
    h.emitted.length = 0;

    // Act
    h.resume.observe(notification("a1", "failed", ENOTFOUND_SUMMARY));

    // Assert
    expect(h.emitted).toEqual([]);
  });
});

describe("a delivery that completes after its wait was given up", () => {
  /**
   * A harness whose delivery is held open: the beat reaches `deliver`, a second
   * beat past the window gives the wait up, and only then is the delivery
   * answered with `answer`.
   */
  async function expiredDuringDelivery(answer: ResumeDelivery): Promise<Harness> {
    const h = harness();
    let release: (delivery: ResumeDelivery) => void = () => undefined;
    let delivering: () => void = () => undefined;
    const asked = new Promise<void>((resolve) => {
      delivering = resolve;
    });
    const resume = new NetworkResume({
      probe: h.probe.probe,
      deliver: () =>
        new Promise((resolve) => {
          release = resolve;
          delivering();
        }),
      nowMs: () => h.clock.now,
      scheduler: h.scheduler,
      emit: (update) => h.emitted.push(update),
    });
    const held = { ...h, resume };
    failWithOutage(held, "a1", "toolu_spawn");
    const pending = resume.tick();
    await asked;
    h.clock.now += NETWORK_RESUME_WINDOW_MS;
    await resume.tick();
    release(answer);
    await pending;
    return held;
  }

  it("states exactly one outcome, the give-up", async () => {
    // Arrange / Act
    const h = await expiredDuringDelivery({ kind: "delivered" });

    // Assert
    expect(statedOutcomes(h)).toEqual([{ work: "toolu_spawn", end: "gaveUp" }]);
  });

  it("leaves the stated set empty", async () => {
    // Arrange / Act
    const h = await expiredDuringDelivery({ kind: "delivered" });

    // Assert
    expect(lastSet(h)).toEqual([]);
  });

  it("records the suppressed resumed outcome at INFO, naming the give-up that stood", async () => {
    // Arrange
    const mark = logSinkMark();

    // Act
    await expiredDuringDelivery({ kind: "delivered" });

    // Assert
    const suppressed = logRecordsSince(mark).find((r) => r.context.suppressed !== undefined);
    expect([suppressed?.level, suppressed?.context]).toEqual([
      "info",
      expect.objectContaining({ task_id: "a1", work: "toolu_spawn", suppressed: "resumed", already: "gaveUp" }),
    ]);
  });

  it("still logs the late resume as delivered, since the rule is unchanged", async () => {
    // Arrange
    const mark = logSinkMark();

    // Act
    await expiredDuringDelivery({ kind: "delivered" });

    // Assert
    expect(logRecordsSince(mark).filter((r) => r.context.outcome === "resumed")).toHaveLength(1);
  });

  it("an undeliverable answer states no second give-up", async () => {
    // Arrange / Act
    const h = await expiredDuringDelivery({ kind: "unavailable", detail: "no query" });

    // Assert
    expect(statedOutcomes(h)).toEqual([{ work: "toolu_spawn", end: "gaveUp" }]);
  });
});

describe("a delivery that completes late, after a newer wait of the same agent opened", () => {
  /**
   * The first delivery is held open while its wait runs out and the agent
   * (resumed meanwhile) fails again, opening a NEWER wait; only then is the
   * first delivery answered with `answer`. Every later delivery answers
   * `later` at once.
   */
  async function lateWithNewerWait(answer: ResumeDelivery, later: ResumeDelivery = { kind: "delivered" }): Promise<Harness> {
    const h = harness();
    let release: (delivery: ResumeDelivery) => void = () => undefined;
    let delivering: () => void = () => undefined;
    const asked = new Promise<void>((resolve) => {
      delivering = resolve;
    });
    let calls = 0;
    const resume = new NetworkResume({
      probe: h.probe.probe,
      deliver: (prompt) => {
        h.prompts.push(prompt);
        calls += 1;
        if (calls > 1) return Promise.resolve(later);
        return new Promise((resolve) => {
          release = resolve;
          delivering();
        });
      },
      nowMs: () => h.clock.now,
      scheduler: h.scheduler,
      emit: (update) => h.emitted.push(update),
    });
    const held = { ...h, resume };
    failWithOutage(held, "a1", "toolu_spawn");
    const pending = resume.tick();
    await asked;
    h.clock.now += NETWORK_RESUME_WINDOW_MS;
    await resume.tick();
    failWithOutage(held, "a1", "toolu_send");
    release(answer);
    await pending;
    return held;
  }

  it("leaves the newer wait standing", async () => {
    // Arrange / Act
    const h = await lateWithNewerWait({ kind: "delivered" });

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual(["a1"]);
  });

  it("states no outcome for the newer wait", async () => {
    // Arrange / Act
    const h = await lateWithNewerWait({ kind: "delivered" });

    // Assert
    expect(statedOutcomes(h)).toEqual([{ work: "toolu_spawn", end: "gaveUp" }]);
  });

  it("keeps the newer wait in the stated set", async () => {
    // Arrange / Act
    const h = await lateWithNewerWait({ kind: "delivered" });

    // Assert
    expect(lastSet(h)?.map((wait) => wait.work)).toEqual(["toolu_send"]);
  });

  it("keeps the probe loop running for the newer wait", async () => {
    // Arrange / Act
    const h = await lateWithNewerWait({ kind: "delivered" });

    // Assert
    expect(h.resume.looping()).toBe(true);
  });

  it("the newer wait is later ended by its own resume", async () => {
    // Arrange
    const h = await lateWithNewerWait({ kind: "delivered" });

    // Act
    await h.resume.tick();

    // Assert
    expect(statedOutcomes(h)).toEqual([
      { work: "toolu_spawn", end: "gaveUp" },
      { work: "toolu_send", end: "resumed" },
    ]);
  });

  it("the newer wait is later ended by its own give-up", async () => {
    // Arrange
    const h = await lateWithNewerWait({ kind: "delivered" });
    h.clock.now += NETWORK_RESUME_WINDOW_MS;

    // Act
    await h.resume.tick();

    // Assert
    expect(statedOutcomes(h)).toEqual([
      { work: "toolu_spawn", end: "gaveUp" },
      { work: "toolu_send", end: "gaveUp" },
    ]);
  });

  it("an undeliverable late answer leaves the newer wait standing", async () => {
    // Arrange / Act
    const h = await lateWithNewerWait({ kind: "unavailable", detail: "no query" });

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual(["a1"]);
  });

  it("an undeliverable late answer records no second give-up", async () => {
    // Arrange
    const mark = logSinkMark();

    // Act
    await lateWithNewerWait({ kind: "unavailable", detail: "no query" });

    // Assert
    expect(logRecordsSince(mark).filter((r) => r.context.outcome === "gave_up")).toHaveLength(1);
  });

  it("records at INFO that the late delivery ended only its own wait, naming the newer one", async () => {
    // Arrange
    const mark = logSinkMark();

    // Act
    await lateWithNewerWait({ kind: "delivered" });

    // Assert
    const late = logRecordsSince(mark).find((r) => r.context.delivery !== undefined);
    expect([late?.level, late?.context]).toEqual([
      "info",
      expect.objectContaining({ task_id: "a1", wait_id: 1, ended: "gaveUp", delivery: "delivered", standing_wait_id: 2 }),
    ]);
  });

  it("logs the late resume as delivered and late", async () => {
    // Arrange
    const mark = logSinkMark();

    // Act
    await lateWithNewerWait({ kind: "delivered" });

    // Assert
    const resumedRecords = logRecordsSince(mark).filter((r) => r.context.outcome === "resumed");
    expect(resumedRecords.map((r) => [r.level, r.context.wait_id, r.context.late])).toEqual([["info", 1, true]]);
  });
});

describe("a newer failure of an agent whose wait still stands", () => {
  it("replaces the standing wait in the stated set", () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");

    // Act
    failWithOutage(h, "a1", "toolu_send");

    // Assert
    expect(lastSet(h)?.map((wait) => wait.work)).toEqual(["toolu_send"]);
  });

  it("records the replacement at INFO, naming both waits", () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    const mark = logSinkMark();

    // Act
    failWithOutage(h, "a1", "toolu_send");

    // Assert
    const replaced = logRecordsSince(mark).find((r) => r.context.next_wait_id !== undefined);
    expect([replaced?.level, replaced?.message, replaced?.context]).toEqual([
      "info",
      "the agent failed again while a wait for it stood; the new waiting edge for the same agent ends that wait, as the restated waiting set expresses, with no outcome of its own",
      expect.objectContaining({ task_id: "a1", wait_id: 1, work: "toolu_spawn", next_wait_id: 2 }),
    ]);
  });

  it("a delivery still out for the replaced wait leaves the newer wait standing", async () => {
    // Arrange
    const h = harness();
    let release: (delivery: ResumeDelivery) => void = () => undefined;
    let delivering: () => void = () => undefined;
    const asked = new Promise<void>((resolve) => {
      delivering = resolve;
    });
    const resume = new NetworkResume({
      probe: h.probe.probe,
      deliver: () =>
        new Promise((resolve) => {
          release = resolve;
          delivering();
        }),
      nowMs: () => h.clock.now,
      scheduler: h.scheduler,
      emit: (update) => h.emitted.push(update),
    });
    const held = { ...h, resume };
    failWithOutage(held, "a1", "toolu_spawn");
    const pending = resume.tick();
    await asked;
    failWithOutage(held, "a1", "toolu_send");

    // Act
    release({ kind: "delivered" });
    await pending;

    // Assert
    expect([resume.waitingTaskIds(), statedOutcomes(h)]).toEqual([["a1"], []]);
  });
});

describe("removing a wait that is not the one standing", () => {
  /** The private removal, reached directly: no public path hands it a stale wait. */
  function removeWaitOf(resume: NetworkResume): (entry: unknown) => void {
    const target = resume as unknown as { removeWait: (entry: unknown) => void };
    return (entry) => target.removeWait.call(resume, entry);
  }

  const stale = { id: 99, chain: { taskId: "a1" }, failedAtMs: 0, work: "toolu_old" };

  it("throws", () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");

    // Act
    const act = (): void => removeWaitOf(h.resume)(stale);

    // Assert
    expect(act).toThrow("wait 99 of agent a1 is not the standing wait (wait 1 stands)");
  });

  it("is recorded at ERROR, naming both waits", () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    const mark = logSinkMark();

    // Act
    try {
      removeWaitOf(h.resume)(stale);
    } catch {
      // the throw is the previous case's subject
    }

    // Assert
    const record = logRecordsSince(mark).find((r) => r.context.standing_wait_id !== undefined);
    expect([record?.level, record?.context]).toEqual([
      "error",
      expect.objectContaining({ task_id: "a1", wait_id: 99, standing_wait_id: 1 }),
    ]);
  });

  it("leaves the standing wait in place", () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");

    // Act
    try {
      removeWaitOf(h.resume)(stale);
    } catch {
      // the throw is the first case's subject
    }

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual(["a1"]);
  });
});

describe("an on-time delivery", () => {
  it("ends its own wait", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");

    // Act
    await h.resume.tick();

    // Assert
    expect([h.resume.waitingTaskIds(), statedOutcomes(h)]).toEqual([[], [{ work: "toolu_spawn", end: "resumed" }]]);
  });

  it("is logged as not late, naming its wait", async () => {
    // Arrange
    const h = harness();
    failWithOutage(h, "a1", "toolu_spawn");
    const mark = logSinkMark();

    // Act
    await h.resume.tick();

    // Assert
    const resumedRecords = logRecordsSince(mark).filter((r) => r.context.outcome === "resumed");
    expect(resumedRecords.map((r) => [r.level, r.context.wait_id, r.context.late])).toEqual([["info", 1, false]]);
  });
});

describe("a session stream that cannot take the statement", () => {
  /** A harness whose seam throws. */
  function throwing(): Harness {
    const h = harness();
    const resume = new NetworkResume({
      probe: h.probe.probe,
      deliver: () => Promise.resolve({ kind: "delivered" }),
      nowMs: () => h.clock.now,
      scheduler: h.scheduler,
      emit: () => {
        throw new Error("the fan-out is broken");
      },
    });
    return { ...h, resume };
  }

  it("is recorded at ERROR with the arm and the cause", () => {
    // Arrange
    const h = throwing();
    const mark = logSinkMark();

    // Act
    failWithOutage(h, "a1", "toolu_spawn");

    // Assert
    expect(logRecordsSince(mark).find((r) => r.level === "error")?.context).toMatchObject({
      arm: "networkResumeWaits",
      cause: "the fan-out is broken",
      task_ids: ["a1"],
    });
  });

  it("an outcome it cannot take is recorded at ERROR with the work", async () => {
    // Arrange
    const h = throwing();
    failWithOutage(h, "a1", "toolu_spawn");
    const mark = logSinkMark();

    // Act
    await h.resume.tick();

    // Assert
    expect(
      logRecordsSince(mark).find((r) => r.level === "error" && r.context.arm === "networkResumeOutcome")?.context,
    ).toMatchObject({ work: "toolu_spawn", end: "resumed", cause: "the fan-out is broken" });
  });

  it("leaves the wait standing", () => {
    // Arrange
    const h = throwing();

    // Act
    failWithOutage(h, "a1", "toolu_spawn");

    // Assert
    expect(h.resume.waitingTaskIds()).toEqual(["a1"]);
  });
});
