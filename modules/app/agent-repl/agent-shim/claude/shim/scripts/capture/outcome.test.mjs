/**
 * Tests for the capture outcome gate.
 *
 * The central fixtures are TRANSCRIBED FROM THE REAL POISONED RUN
 * (captures/prose-streamed/, 2026-08-29): the run that exited 0 and wrote an
 * authentication failure into the corpus as a golden. Every arm is tested
 * against that actual evidence rather than an invented shape, because the
 * thing that fooled the first version was a detail nobody would have invented
 * — `subtype: "success"` on a result whose `is_error` is `true`.
 */
import { describe, expect, it } from "vitest";

import {
  API_ERROR_SCENARIO,
  apiKeySource,
  classifyCapture,
  expectedErrorSubtypes,
  expectsApiError,
  hasTruthyKeyAtAnyDepth,
  initMessage,
  sdkMessages,
  verdictLine,
} from "./outcome.mjs";

/** The real result message from the poisoned run, field for field. */
const POISONED_RESULT = {
  is_error: true,
  duration_api_ms: 0,
  num_turns: 1,
  stop_reason: "stop_sequence",
  session_id: "7d72f2a3-d8be-46c8-bd7a-a7ee5850ae48",
  total_cost_usd: 0,
  modelUsage: {},
  permission_denials: [],
  terminal_reason: "api_error",
  fast_mode_state: "off",
  subtype: "success",
  api_error_status: null,
  result: "Not logged in · Please run /login",
  type: "result",
  duration_ms: 33,
};

/** The real assistant message from the poisoned run, trimmed to what matters. */
const POISONED_ASSISTANT = {
  type: "assistant",
  message: {
    id: "99d3a4a4-a161-4492-b55b-0c35a6cbecdf",
    model: "<synthetic>",
    role: "assistant",
    content: [{ type: "text", text: "Not logged in · Please run /login" }],
    is_api_error_message: true,
  },
  session_id: "7d72f2a3-d8be-46c8-bd7a-a7ee5850ae48",
};

const POISONED_INIT = {
  type: "system",
  subtype: "init",
  apiKeySource: "none",
  model: "claude-opus-5",
  permissionMode: "default",
  session_id: "7d72f2a3-d8be-46c8-bd7a-a7ee5850ae48",
};

/** The poisoned run's stream, in the shape the harness records. */
const POISONED_ENTRIES = [
  { t_ms: 9, dir: "control", msg: { kind: "query_started" } },
  { t_ms: 644, dir: "sdk", msg: POISONED_INIT },
  { t_ms: 644, dir: "sdk", msg: { type: "system", subtype: "status" } },
  { t_ms: 649, dir: "sdk", msg: POISONED_ASSISTANT },
  { t_ms: 650, dir: "sdk", msg: POISONED_RESULT },
];

/** A healthy stream: a real answer and a clean terminal. */
const HEALTHY_ENTRIES = [
  { t_ms: 1, dir: "control", msg: { kind: "query_started" } },
  { t_ms: 100, dir: "sdk", msg: POISONED_INIT },
  {
    t_ms: 900,
    dir: "sdk",
    msg: {
      type: "assistant",
      message: { model: "claude-opus-5", content: [{ type: "text", text: "A write-ahead log is…" }] },
    },
  },
  {
    t_ms: 1200,
    dir: "sdk",
    msg: {
      type: "result",
      subtype: "success",
      is_error: false,
      terminal_reason: "end_turn",
      api_error_status: null,
      duration_ms: 4200,
      total_cost_usd: 0.031,
    },
  },
];

const PROSE = { name: "prose-streamed" };

describe("the poisoned run is caught", () => {
  it("is classified as NOT ok", () => {
    expect(classifyCapture(POISONED_ENTRIES, PROSE).ok).toBe(false);
  });

  it("names result.is_error, the arm that actually moved", () => {
    expect(classifyCapture(POISONED_ENTRIES, PROSE).reasons).toContain("result.is_error is true");
  });

  it("names the api_error terminal_reason", () => {
    expect(classifyCapture(POISONED_ENTRIES, PROSE).reasons).toContain(
      'result.terminal_reason is "api_error"',
    );
  });

  it("names the is_api_error_message flag nested in the assistant envelope", () => {
    expect(classifyCapture(POISONED_ENTRIES, PROSE).reasons).toContain(
      "a message carries is_api_error_message: true",
    );
  });

  it("is caught DESPITE result.subtype being \"success\" — the detail that fooled the first run", () => {
    expect(POISONED_RESULT.subtype).toBe("success");
    expect(classifyCapture(POISONED_ENTRIES, PROSE).ok).toBe(false);
  });
});

describe("a healthy capture passes", () => {
  it("is classified as ok", () => {
    expect(classifyCapture(HEALTHY_ENTRIES, PROSE).ok).toBe(true);
  });

  it("collects no reasons", () => {
    expect(classifyCapture(HEALTHY_ENTRIES, PROSE).reasons).toEqual([]);
  });
});

describe("structural failures", () => {
  it("catches a stream with no SDK messages at all", () => {
    const out = classifyCapture([{ dir: "control", msg: {} }], PROSE);
    expect(out.reasons).toContain("the SDK produced no messages at all");
  });

  it("catches a turn that never reached a terminal", () => {
    const out = classifyCapture([{ dir: "sdk", msg: POISONED_INIT }], PROSE);
    expect(out.reasons).toContain("the turn never reached a result message (no terminal)");
  });

  it("catches an api_error_status even when is_error is false", () => {
    const entries = [
      { dir: "sdk", msg: POISONED_INIT },
      { dir: "sdk", msg: { type: "result", subtype: "success", is_error: false, api_error_status: 429 } },
    ];
    expect(classifyCapture(entries, PROSE).reasons).toContain("result.api_error_status is 429");
  });

  it("surfaces a throw the run recorded", () => {
    const out = classifyCapture(HEALTHY_ENTRIES, PROSE, {
      errors: [{ stage: "query", error: "boom" }],
    });
    expect(out.reasons).toContain("the run threw during query: boom");
  });

  it("treats a null api_error_status as clean", () => {
    expect(classifyCapture(HEALTHY_ENTRIES, PROSE).ok).toBe(true);
  });
});

describe("the one scenario allowed to carry an API error", () => {
  it("recognizes api-error-classes by name", () => {
    expect(expectsApiError({ name: API_ERROR_SCENARIO })).toBe(true);
  });

  it("recognizes an explicit expects_api_error flag", () => {
    expect(expectsApiError({ name: "other", expects_api_error: true })).toBe(true);
  });

  it("does not exempt an ordinary scenario", () => {
    expect(expectsApiError(PROSE)).toBe(false);
  });

  it("accepts the poisoned-shaped result for api-error-classes", () => {
    const out = classifyCapture(POISONED_ENTRIES, { name: API_ERROR_SCENARIO });
    expect(out.ok).toBe(true);
  });

  it("still requires api-error-classes to have produced a terminal", () => {
    const out = classifyCapture([{ dir: "sdk", msg: POISONED_INIT }], { name: API_ERROR_SCENARIO });
    expect(out.reasons).toContain("the turn never reached a result message (no terminal)");
  });
});

describe("hasTruthyKeyAtAnyDepth", () => {
  it("finds a flag at the top level", () => {
    expect(hasTruthyKeyAtAnyDepth({ is_api_error_message: true }, "is_api_error_message")).toBe(true);
  });

  it("finds a flag nested inside an envelope", () => {
    expect(hasTruthyKeyAtAnyDepth(POISONED_ASSISTANT, "is_api_error_message")).toBe(true);
  });

  it("finds a flag inside an array element", () => {
    expect(hasTruthyKeyAtAnyDepth({ a: [{ b: { flag: true } }] }, "flag")).toBe(true);
  });

  it("does not fire on a false value", () => {
    expect(hasTruthyKeyAtAnyDepth({ flag: false }, "flag")).toBe(false);
  });

  it("does not fire on a truthy non-true value", () => {
    expect(hasTruthyKeyAtAnyDepth({ flag: "yes" }, "flag")).toBe(false);
  });

  it("survives a null value without throwing", () => {
    expect(hasTruthyKeyAtAnyDepth({ a: null }, "flag")).toBe(false);
  });
});

describe("stream readers", () => {
  it("returns only the SDK messages", () => {
    expect(sdkMessages(POISONED_ENTRIES)).toHaveLength(4);
  });

  it("finds the init message", () => {
    expect(initMessage(POISONED_ENTRIES)).toBe(POISONED_INIT);
  });

  it("returns null when the session never initialized", () => {
    expect(initMessage([])).toBeNull();
  });

  it("reports the vendor's apiKeySource for the operator's report", () => {
    expect(apiKeySource(POISONED_ENTRIES)).toBe("none");
  });

  it("does NOT fail a capture on apiKeySource alone, since a subscription session reports none", () => {
    expect(classifyCapture(HEALTHY_ENTRIES, PROSE).ok).toBe(true);
    expect(apiKeySource(HEALTHY_ENTRIES)).toBe("none");
  });
});

describe("verdictLine", () => {
  it("reports an OK capture in one line", () => {
    expect(verdictLine("prose-streamed", { ok: true, reasons: [] })).toBe("capture: prose-streamed OK");
  });

  it("reports a failed capture with its reasons", () => {
    expect(verdictLine("prose-streamed", { ok: false, reasons: ["a", "b"] })).toBe(
      "capture: prose-streamed FAILED — a; b",
    );
  });
});

describe("expectedErrorSubtypes", () => {
  it("is empty when the scenario declares nothing", () => {
    expect(expectedErrorSubtypes({ name: "x" }).size).toBe(0);
  });

  it("refuses a non-string or empty declaration", () => {
    expect(() => expectedErrorSubtypes({ name: "x", expects_error_subtypes: [""] })).toThrow(/non-empty strings/);
  });

  it("tolerates is_error on exactly the declared subtype", () => {
    const entries = [
      { dir: "sdk", msg: { type: "system", subtype: "init", apiKeySource: "none" } },
      { dir: "sdk", msg: { type: "result", subtype: "error_max_turns", is_error: true, terminal_reason: "max_turns", api_error_status: null } },
    ];
    expect(classifyCapture(entries, { name: "t", expects_error_subtypes: ["error_max_turns"] }).ok).toBe(true);
  });

  it("still quarantines is_error on an undeclared subtype", () => {
    const entries = [
      { dir: "sdk", msg: { type: "system", subtype: "init", apiKeySource: "none" } },
      { dir: "sdk", msg: { type: "result", subtype: "error_during_execution", is_error: true, api_error_status: null } },
    ];
    expect(classifyCapture(entries, { name: "t", expects_error_subtypes: ["error_max_turns"] }).reasons).toContain("result.is_error is true");
  });

  it("never lets a declared subtype excuse an api_error terminal", () => {
    const entries = [
      { dir: "sdk", msg: { type: "system", subtype: "init", apiKeySource: "none" } },
      { dir: "sdk", msg: { type: "result", subtype: "success", is_error: true, terminal_reason: "api_error", api_error_status: null } },
    ];
    expect(classifyCapture(entries, { name: "t", expects_error_subtypes: ["success"] }).reasons).toContain('result.terminal_reason is "api_error"');
  });
});
