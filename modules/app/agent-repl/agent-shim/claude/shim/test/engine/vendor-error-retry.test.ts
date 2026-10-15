/**
 * test/engine/vendor-error-retry.test.ts — which failed turns are retried,
 * the vendor-error cadence and its window, the usage-limit wait for the
 * exhausted window's reset and its tail, and how a chain ends.
 *
 * No clock and no vendor: the scheduler is fired by hand, `nowMs` is a
 * variable, and delivery and the usage reading are scripted.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import {
  USAGE_LIMIT_RETRY_TAIL_MS,
  VENDOR_ERROR_RETRY_BUSY_MS,
  VENDOR_ERROR_RETRY_INTERVAL_MS,
  VENDOR_ERROR_RETRY_WINDOW_MS,
  VendorErrorRetry,
  exhaustedWindow,
  type UsageReading,
} from "../../src/engine/vendor-error-retry.js";
import { isVendorErrorRetryPrompt, vendorErrorRetryPrompt } from "../../src/engine/vendor-error-retry-prompt.js";
import type { ShimPromptDelivery } from "../../src/engine/shim-prompt.js";
import { conversationv1 } from "../../src/proto.js";
import type { AccountUsageLike, SdkMessage } from "../../src/sdk/types.js";
import { ManualScheduler } from "./fakes.js";
import { logRecordsSince, logSinkMark, type LogRecord } from "../log-records.js";

const MINUTE = 60_000;
const START = 1_000_000_000;

// ---------------------------------------------------------------------------
// Builders
// ---------------------------------------------------------------------------

const API_RETRY = {
  type: "system",
  subtype: "api_retry",
  attempt: 1,
  max_retries: 10,
  retry_delay_ms: 500,
  error_status: 529,
  error: "overloaded",
  uuid: "00000000-0000-4000-8000-000000000001",
  session_id: "s",
} as unknown as SdkMessage;

function frame(result: conversationv1.AgentFrame["result"]): conversationv1.AgentFrame {
  return create(conversationv1.AgentFrameSchema, { result });
}

function failed(failure: conversationv1.AgentFailure["failure"]): conversationv1.AgentFrame {
  return frame({ case: "failure", value: create(conversationv1.AgentFailureSchema, { failure }) });
}

function apiFailed(kind: conversationv1.ApiRequestFailed["kind"]): conversationv1.AgentFrame {
  return failed({
    case: "apiRequestFailed",
    value: create(conversationv1.ApiRequestFailedSchema, { message: "API Error", kind }),
  });
}

const OVERLOADED = apiFailed({ case: "overloaded", value: create(conversationv1.ApiOverloadedSchema, {}) });
const RATE_LIMITED = apiFailed({ case: "rateLimited", value: create(conversationv1.ApiRateLimitedSchema, {}) });
const BLOCKING_LIMIT = failed({ case: "blockingLimit", value: create(conversationv1.AgentStoppedAtBlockingLimitSchema, {}) });
const MODEL_ERROR = failed({ case: "modelError", value: create(conversationv1.AgentModelErrorSchema, {}) });
const COMPLETED = frame({
  case: "success",
  value: create(conversationv1.AgentSuccessSchema, {
    outcome: { case: "completed", value: create(conversationv1.AgentCompletedSchema, {}) },
  }),
});

interface WindowAt {
  readonly utilization: number;
  readonly resetsAtMs: number;
}

function usage(windows: Partial<Record<"five_hour" | "seven_day" | "seven_day_opus", WindowAt>>): AccountUsageLike {
  const rate: Record<string, { utilization: number; resets_at: string }> = {};
  for (const [name, at] of Object.entries(windows)) {
    rate[name] = { utilization: at.utilization, resets_at: new Date(at.resetsAtMs).toISOString() };
  }
  return { subscription_type: "max", rate_limits_available: true, rate_limits: rate } as unknown as AccountUsageLike;
}

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

interface Harness {
  readonly retry: VendorErrorRetry;
  readonly scheduler: ManualScheduler;
  readonly clock: { now: number };
  readonly prompts: string[];
  delivery: ShimPromptDelivery | Error;
  reading: UsageReading;
  readings: number;
}

function harness(): Harness {
  const scheduler = new ManualScheduler();
  const clock = { now: START };
  const prompts: string[] = [];
  const h: Harness = {
    scheduler,
    clock,
    prompts,
    delivery: { kind: "delivered", turnId: "retry-1" },
    reading: { kind: "sampled", usage: usage({}) },
    readings: 0,
    retry: new VendorErrorRetry({
      deliver: (prompt) => {
        prompts.push(prompt);
        const answer = h.delivery;
        return answer instanceof Error ? Promise.reject(answer) : Promise.resolve(answer);
      },
      readUsage: () => {
        h.readings += 1;
        return Promise.resolve(h.reading);
      },
      nowMs: () => clock.now,
      scheduler,
    }),
  };
  return h;
}

/** Let every queued promise continuation run. */
async function settle(): Promise<void> {
  for (let i = 0; i < 10; i++) await Promise.resolve();
}

/** A turn the vendor retried and then failed with TERMINAL. */
async function retriedTurnEnds(h: Harness, turnId: string | undefined, terminal = OVERLOADED): Promise<void> {
  h.retry.observe(API_RETRY);
  h.retry.turnEnded(turnId, terminal);
  await settle();
}

/** Fire the pending retry and let its delivery settle. */
async function fire(h: Harness): Promise<void> {
  h.scheduler.fireTimeout();
  await settle();
}

const recordsSaying = (records: LogRecord[], message: string): LogRecord[] =>
  records.filter((record) => record.message === message);

// ---------------------------------------------------------------------------
// The prompt
// ---------------------------------------------------------------------------

describe("the retry prompt", () => {
  it("is recognized as the shim's own", () => {
    // Act
    const prompt = vendorErrorRetryPrompt(3);

    // Assert
    expect(isVendorErrorRetryPrompt(prompt)).toBe(true);
  });

  it("names its attempt", () => {
    // Act
    const prompt = vendorErrorRetryPrompt(3);

    // Assert
    expect(prompt).toContain("automatic retry 3");
  });

  it("does not claim an ordinary prompt", () => {
    // Act
    const claimed = isVendorErrorRetryPrompt("continue please");

    // Assert
    expect(claimed).toBe(false);
  });
});

// ---------------------------------------------------------------------------
// The exhausted window
// ---------------------------------------------------------------------------

describe("exhaustedWindow", () => {
  const cases: { name: string; usage: AccountUsageLike; expected: ReturnType<typeof exhaustedWindow> }[] = [
    {
      name: "an exhausted session window answers its reset",
      usage: usage({ five_hour: { utilization: 100, resetsAtMs: START + 76 * MINUTE } }),
      expected: { window: "session", name: "five_hour", resetsAtMs: START + 76 * MINUTE },
    },
    {
      name: "an exhausted weekly window answers its reset",
      usage: usage({
        five_hour: { utilization: 20, resetsAtMs: START + 60 * MINUTE },
        seven_day: { utilization: 100, resetsAtMs: START + 3 * 24 * 60 * MINUTE },
      }),
      expected: { window: "weekly", name: "seven_day", resetsAtMs: START + 3 * 24 * 60 * MINUTE },
    },
    {
      name: "a model's weekly window counts as weekly",
      usage: usage({ seven_day_opus: { utilization: 100, resetsAtMs: START + 90 * MINUTE } }),
      expected: { window: "weekly", name: "seven_day_opus", resetsAtMs: START + 90 * MINUTE },
    },
    {
      name: "of two exhausted windows the later reset answers",
      usage: usage({
        five_hour: { utilization: 100, resetsAtMs: START + 60 * MINUTE },
        seven_day: { utilization: 100, resetsAtMs: START + 600 * MINUTE },
      }),
      expected: { window: "weekly", name: "seven_day", resetsAtMs: START + 600 * MINUTE },
    },
    {
      name: "a window below its limit is not exhausted",
      usage: usage({ five_hour: { utilization: 99.9, resetsAtMs: START + 60 * MINUTE } }),
      expected: undefined,
    },
    {
      name: "an exhausted window whose reset has passed is not waited for",
      usage: usage({ five_hour: { utilization: 100, resetsAtMs: START - MINUTE } }),
      expected: undefined,
    },
    {
      name: "an account with no plan limits has no exhausted window",
      usage: { subscription_type: null, rate_limits_available: false, rate_limits: null } as unknown as AccountUsageLike,
      expected: undefined,
    },
  ];

  it.each(cases)("$name", ({ usage: sampled, expected }) => {
    // Act
    const exhausted = exhaustedWindow(sampled, START);

    // Assert
    expect(exhausted).toEqual(expected);
  });
});

// ---------------------------------------------------------------------------
// Which failures open a chain
// ---------------------------------------------------------------------------

describe("a failed turn", () => {
  it("opens a chain when the vendor retried an API failure and gave up", async () => {
    // Arrange
    const h = harness();

    // Act
    await retriedTurnEnds(h, "turn-1");

    // Assert
    expect(h.scheduler.pendingTimeout()?.delayMs).toBe(VENDOR_ERROR_RETRY_INTERVAL_MS);
  });

  it("opens no chain for an API failure the vendor never retried", () => {
    // Arrange
    const h = harness();

    // Act
    h.retry.turnEnded("turn-1", OVERLOADED);

    // Assert
    expect(h.retry.standing()).toBe(false);
  });

  it("opens no chain for a failure that is not an API failure", () => {
    // Arrange
    const h = harness();
    h.retry.observe(API_RETRY);

    // Act
    h.retry.turnEnded("turn-1", MODEL_ERROR);

    // Assert
    expect(h.retry.standing()).toBe(false);
  });

  it("opens no chain for a completed turn", () => {
    // Arrange
    const h = harness();
    h.retry.observe(API_RETRY);

    // Act
    h.retry.turnEnded("turn-1", COMPLETED);

    // Assert
    expect(h.retry.standing()).toBe(false);
  });

  it("forgets an earlier turn's vendor retry", () => {
    // Arrange
    const h = harness();
    h.retry.observe(API_RETRY);
    h.retry.turnEnded("turn-1", COMPLETED);

    // Act
    h.retry.turnEnded("turn-2", OVERLOADED);

    // Assert
    expect(h.retry.standing()).toBe(false);
  });

  it("records the chain opening", async () => {
    // Arrange
    const h = harness();
    const before = logSinkMark();

    // Act
    await retriedTurnEnds(h, "turn-1");

    // Assert
    const opened = recordsSaying(logRecordsSince(before), "the turn ended on a failure agent-repl retries; a retry chain opened");
    expect(opened.map((record) => record.context["turn_id"])).toEqual(["turn-1"]);
  });
});

// ---------------------------------------------------------------------------
// The vendor-error cadence
// ---------------------------------------------------------------------------

describe("the vendor-error cadence", () => {
  it("delivers the first retry when the interval elapses", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");

    // Act
    await fire(h);

    // Assert
    expect(h.prompts).toEqual([vendorErrorRetryPrompt(1)]);
  });

  it("schedules the next retry when the retried turn fails the same way", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");
    await fire(h);
    h.clock.now += 10 * MINUTE;

    // Act
    await retriedTurnEnds(h, "retry-1");

    // Assert
    expect(h.scheduler.pendingTimeout()?.delayMs).toBe(VENDOR_ERROR_RETRY_INTERVAL_MS);
  });

  it("numbers each delivered retry", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");
    await fire(h);
    await retriedTurnEnds(h, "retry-1");
    h.delivery = { kind: "delivered", turnId: "retry-2" };

    // Act
    await fire(h);

    // Assert
    expect(h.prompts).toEqual([vendorErrorRetryPrompt(1), vendorErrorRetryPrompt(2)]);
  });

  it("ends the chain when the retried turn is answered", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");
    await fire(h);

    // Act
    h.retry.turnEnded("retry-1", COMPLETED);

    // Assert
    expect({ standing: h.retry.standing(), scheduled: h.retry.scheduled() }).toEqual({ standing: false, scheduled: false });
  });

  it("ends the chain when the retried turn fails without a vendor retry", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");
    await fire(h);

    // Act
    h.retry.turnEnded("retry-1", OVERLOADED);

    // Assert
    expect(h.retry.standing()).toBe(false);
  });

  it("is not moved by another turn's end while the retry runs", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");
    await fire(h);

    // Act
    h.retry.turnEnded("vendor-turn", COMPLETED);
    h.retry.turnEnded("retry-1", COMPLETED);

    // Assert: the retry's own end still ended it, as answered.
    expect(h.retry.standing()).toBe(false);
  });

  it("keeps waiting for the retry it delivered when another turn ends first", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");
    await fire(h);

    // Act
    h.retry.turnEnded("vendor-turn", COMPLETED);

    // Assert
    expect(h.retry.standing()).toBe(true);
  });

  it("is not moved by another turn's end while the retry waits to be delivered", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");

    // Act
    h.retry.turnEnded("vendor-turn", COMPLETED);

    // Assert
    expect(h.scheduler.pendingTimeout()?.delayMs).toBe(VENDOR_ERROR_RETRY_INTERVAL_MS);
  });

  it("measures the window from the chain's first vendor retry", async () => {
    // Arrange: the turn's first retry is two hours ago, less four minutes.
    const h = harness();
    h.retry.observe(API_RETRY);
    h.clock.now += VENDOR_ERROR_RETRY_WINDOW_MS - 4 * MINUTE;

    // Act
    h.retry.turnEnded("turn-1", OVERLOADED);

    // Assert
    expect(h.scheduler.pendingTimeout()).toBeUndefined();
  });

  it("makes the last retry that starts inside the window", async () => {
    // Arrange
    const h = harness();
    h.retry.observe(API_RETRY);
    h.clock.now += VENDOR_ERROR_RETRY_WINDOW_MS - VENDOR_ERROR_RETRY_INTERVAL_MS;

    // Act
    h.retry.turnEnded("turn-1", OVERLOADED);

    // Assert
    expect(h.scheduler.pendingTimeout()?.delayMs).toBe(VENDOR_ERROR_RETRY_INTERVAL_MS);
  });

  it("gives up at ERROR once the window is spent", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");
    await fire(h);
    h.clock.now += VENDOR_ERROR_RETRY_WINDOW_MS;
    const before = logSinkMark();

    // Act
    await retriedTurnEnds(h, "retry-1");

    // Assert
    const records = recordsSaying(
      logRecordsSince(before),
      "the vendor kept failing the turn for the whole retry window; agent-repl stopped retrying and the turn keeps its failure",
    );
    expect(records.map((record) => ({ level: record.level, retries: record.context["retries"] }))).toEqual([
      { level: "error", retries: 1 },
    ]);
  });

  it("leaves no chain once the window is spent", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");
    await fire(h);
    h.clock.now += VENDOR_ERROR_RETRY_WINDOW_MS;

    // Act
    await retriedTurnEnds(h, "retry-1");

    // Assert
    expect(h.retry.standing()).toBe(false);
  });
});

// ---------------------------------------------------------------------------
// Delivery
// ---------------------------------------------------------------------------

describe("delivering a retry", () => {
  it("asks again shortly while the main agent is busy", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");
    h.delivery = { kind: "busy", detail: "a keep-alive turn is open" };

    // Act
    await fire(h);

    // Assert
    expect(h.scheduler.pendingTimeout()?.delayMs).toBe(VENDOR_ERROR_RETRY_BUSY_MS);
  });

  it("ends the chain when the session can never take the prompt", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");
    h.delivery = { kind: "unavailable", detail: "the session is standing down" };

    // Act
    await fire(h);

    // Assert
    expect({ standing: h.retry.standing(), scheduled: h.retry.scheduled() }).toEqual({ standing: false, scheduled: false });
  });

  it("gives up at ERROR when the vendor refuses the prompt", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");
    h.delivery = new Error("the prompt queue is closed");
    const before = logSinkMark();

    // Act
    await fire(h);

    // Assert
    const records = recordsSaying(
      logRecordsSince(before),
      "the vendor refused the retry prompt; agent-repl stopped retrying and the turn keeps its failure",
    );
    expect(records.map((record) => ({ level: record.level, cause: record.context["cause"] }))).toEqual([
      { level: "error", cause: "the prompt queue is closed" },
    ]);
  });

  it("leaves no chain when the vendor refuses the prompt", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");
    h.delivery = new Error("the prompt queue is closed");

    // Act
    await fire(h);

    // Assert
    expect(h.retry.standing()).toBe(false);
  });

  it("is not revived by a delivery that lands after the chain ended", async () => {
    // Arrange
    const scheduler = new ManualScheduler();
    let release: (delivery: ShimPromptDelivery) => void = () => undefined;
    const pending = new Promise<ShimPromptDelivery>((resolve) => {
      release = resolve;
    });
    const retry = new VendorErrorRetry({
      deliver: () => pending,
      readUsage: () => Promise.resolve({ kind: "unavailable", detail: "unused" }),
      nowMs: () => START,
      scheduler,
    });
    retry.observe(API_RETRY);
    retry.turnEnded("turn-1", OVERLOADED);
    scheduler.fireTimeout();
    retry.promptSubmitted("turn-2");

    // Act
    release({ kind: "delivered", turnId: "retry-1" });
    await settle();

    // Assert
    expect(retry.standing()).toBe(false);
  });
});

// ---------------------------------------------------------------------------
// The usage-limit wait
// ---------------------------------------------------------------------------

describe("a usage-limit failure", () => {
  it("retries when the exhausted session window resets", async () => {
    // Arrange
    const h = harness();
    h.reading = { kind: "sampled", usage: usage({ five_hour: { utilization: 100, resetsAtMs: START + 76 * MINUTE } }) };

    // Act
    h.retry.turnEnded("turn-1", BLOCKING_LIMIT);
    await settle();

    // Assert
    expect(h.scheduler.pendingTimeout()?.delayMs).toBe(76 * MINUTE);
  });

  it("retries when the exhausted weekly window resets", async () => {
    // Arrange
    const h = harness();
    const weeklyReset = START + 2 * 24 * 60 * MINUTE;
    h.reading = {
      kind: "sampled",
      usage: usage({
        five_hour: { utilization: 30, resetsAtMs: START + 60 * MINUTE },
        seven_day: { utilization: 100, resetsAtMs: weeklyReset },
      }),
    };

    // Act
    h.retry.turnEnded("turn-1", RATE_LIMITED);
    await settle();

    // Assert
    expect(h.scheduler.pendingTimeout()?.delayMs).toBe(weeklyReset - START);
  });

  it("waits for the reset even when the vendor retried the 429", async () => {
    // Arrange
    const h = harness();
    h.reading = { kind: "sampled", usage: usage({ five_hour: { utilization: 100, resetsAtMs: START + 76 * MINUTE } }) };

    // Act
    await retriedTurnEnds(h, "turn-1", RATE_LIMITED);

    // Assert
    expect(h.scheduler.pendingTimeout()?.delayMs).toBe(76 * MINUTE);
  });

  it("reads the account's usage after the failure", async () => {
    // Arrange
    const h = harness();

    // Act
    h.retry.turnEnded("turn-1", BLOCKING_LIMIT);
    await settle();

    // Assert
    expect(h.readings).toBe(1);
  });

  it("does not read the usage for a failure that is not a usage limit", async () => {
    // Arrange
    const h = harness();

    // Act
    await retriedTurnEnds(h, "turn-1");

    // Assert
    expect(h.readings).toBe(0);
  });

  it("retries every interval after the reset while the vendor still refuses", async () => {
    // Arrange
    const h = harness();
    const reset = START + 76 * MINUTE;
    h.reading = { kind: "sampled", usage: usage({ five_hour: { utilization: 100, resetsAtMs: reset } }) };
    h.retry.turnEnded("turn-1", BLOCKING_LIMIT);
    await settle();
    h.clock.now = reset;
    await fire(h);

    // Act
    h.retry.turnEnded("retry-1", BLOCKING_LIMIT);
    await settle();

    // Assert
    expect(h.scheduler.pendingTimeout()?.delayMs).toBe(VENDOR_ERROR_RETRY_INTERVAL_MS);
  });

  it("gives up at ERROR once the tail after the reset is spent", async () => {
    // Arrange
    const h = harness();
    const reset = START + 76 * MINUTE;
    h.reading = { kind: "sampled", usage: usage({ five_hour: { utilization: 100, resetsAtMs: reset } }) };
    h.retry.turnEnded("turn-1", BLOCKING_LIMIT);
    await settle();
    h.clock.now = reset + USAGE_LIMIT_RETRY_TAIL_MS - 4 * MINUTE;
    await fire(h);
    const before = logSinkMark();

    // Act
    h.retry.turnEnded("retry-1", BLOCKING_LIMIT);
    await settle();

    // Assert
    const records = recordsSaying(
      logRecordsSince(before),
      "the vendor kept refusing the turn's usage after the window reset; agent-repl stopped retrying and the turn keeps its failure",
    );
    expect(records.map((record) => ({ level: record.level, window: record.context["window"] }))).toEqual([
      { level: "error", window: "session" },
    ]);
  });

  it("waits for a later reset the fresh reading names", async () => {
    // Arrange: the session window resets, and the weekly one is now the exhausted one.
    const h = harness();
    const reset = START + 76 * MINUTE;
    h.reading = { kind: "sampled", usage: usage({ five_hour: { utilization: 100, resetsAtMs: reset } }) };
    h.retry.turnEnded("turn-1", BLOCKING_LIMIT);
    await settle();
    h.clock.now = reset;
    await fire(h);
    const weeklyReset = reset + 24 * 60 * MINUTE;
    h.reading = { kind: "sampled", usage: usage({ seven_day: { utilization: 100, resetsAtMs: weeklyReset } }) };

    // Act
    h.retry.turnEnded("retry-1", BLOCKING_LIMIT);
    await settle();

    // Assert
    expect(h.scheduler.pendingTimeout()?.delayMs).toBe(weeklyReset - reset);
  });

  it("is retried as a vendor error when the vendor retried it and no window is exhausted", async () => {
    // Arrange
    const h = harness();
    h.reading = { kind: "sampled", usage: usage({ five_hour: { utilization: 40, resetsAtMs: START + 60 * MINUTE } }) };

    // Act
    await retriedTurnEnds(h, "turn-1", RATE_LIMITED);

    // Assert
    expect(h.scheduler.pendingTimeout()?.delayMs).toBe(VENDOR_ERROR_RETRY_INTERVAL_MS);
  });

  it("is not retried when the vendor never retried it and no window is exhausted", async () => {
    // Arrange
    const h = harness();
    h.reading = { kind: "sampled", usage: usage({ five_hour: { utilization: 40, resetsAtMs: START + 60 * MINUTE } }) };

    // Act
    h.retry.turnEnded("turn-1", BLOCKING_LIMIT);
    await settle();

    // Assert
    expect({ standing: h.retry.standing(), scheduled: h.retry.scheduled() }).toEqual({ standing: false, scheduled: false });
  });

  it("records why it is not retried when the usage could not be read", async () => {
    // Arrange
    const h = harness();
    h.reading = { kind: "unavailable", detail: "the usage service did not answer" };
    const before = logSinkMark();

    // Act
    h.retry.turnEnded("turn-1", BLOCKING_LIMIT);
    await settle();

    // Assert
    const records = recordsSaying(
      logRecordsSince(before),
      "the usage-limit failure names no exhausted allowance to wait for; the turn is not retried",
    );
    expect(records.map((record) => record.context["detail"])).toEqual(["the usage service did not answer"]);
  });

  it("is not scheduled when a prompt was submitted while the usage was read", async () => {
    // Arrange
    const h = harness();
    h.reading = { kind: "sampled", usage: usage({ five_hour: { utilization: 100, resetsAtMs: START + 76 * MINUTE } }) };
    h.retry.turnEnded("turn-1", BLOCKING_LIMIT);

    // Act
    h.retry.promptSubmitted("turn-2");
    await settle();

    // Assert
    expect(h.scheduler.pendingTimeout()).toBeUndefined();
  });
});

// ---------------------------------------------------------------------------
// How a chain ends
// ---------------------------------------------------------------------------

describe("ending a chain", () => {
  it("yields to a submitted prompt", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");

    // Act
    h.retry.promptSubmitted("turn-2");

    // Assert
    expect({ standing: h.retry.standing(), scheduled: h.retry.scheduled() }).toEqual({ standing: false, scheduled: false });
  });

  it("opens a fresh chain when the submitted prompt fails the same way", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");
    h.retry.promptSubmitted("turn-2");

    // Act
    await retriedTurnEnds(h, "turn-2");

    // Assert
    expect(h.scheduler.pendingTimeout()?.delayMs).toBe(VENDOR_ERROR_RETRY_INTERVAL_MS);
  });

  it("cancels the pending retry at stand-down", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");

    // Act
    h.retry.stop("the session stood down");

    // Assert
    expect(h.scheduler.pendingTimeout()).toBeUndefined();
  });

  it("records the abandoned chain at stand-down", async () => {
    // Arrange
    const h = harness();
    await retriedTurnEnds(h, "turn-1");
    const before = logSinkMark();

    // Act
    h.retry.stop("the session stood down");

    // Assert
    const records = recordsSaying(logRecordsSince(before), "the shim is standing down; the failed turn keeps its failure");
    expect(records.map((record) => record.context["reason"])).toEqual(["the session stood down"]);
  });

  it("opens nothing after stand-down", async () => {
    // Arrange
    const h = harness();
    h.retry.stop("the session stood down");

    // Act
    await retriedTurnEnds(h, "turn-1");

    // Assert
    expect(h.retry.standing()).toBe(false);
  });
});
