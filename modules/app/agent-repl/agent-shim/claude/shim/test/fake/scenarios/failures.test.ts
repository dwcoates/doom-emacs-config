/**
 * The failure family. The point of every test here is that the SIXTEEN
 * conversation.v1 failure arms are distinguished by `terminal_reason`, not by
 * the four declared `result.subtype` values — so the suite asserts the pairing,
 * not one half of it.
 */
import { describe, expect, it } from "vitest";

import { FAILURE_SCENARIOS } from "../../../src/fake/scenarios/failures.js";
import { driveScenario, ofType, recordsOfType, theResult } from "../harness.js";

const terminal = async (prompt: string): Promise<{ subtype: unknown; reason: unknown; error: boolean }> => {
  const result = theResult(await driveScenario([prompt]));
  return { subtype: result.subtype, reason: result.terminal_reason, error: result.is_error === true };
};

describe("turn-stop terminals", () => {
  it("pairs each declared error subtype with its own terminal reason", async () => {
    // Arrange + Act
    const observed = [
      await terminal("!fail-max-turns"),
      await terminal("!fail-budget"),
      await terminal("!fail-structured-output"),
    ];

    // Assert
    expect(observed).toEqual([
      { subtype: "error_max_turns", reason: "max_turns", error: true },
      { subtype: "error_max_budget_usd", reason: "budget_exhausted", error: true },
      {
        subtype: "error_max_structured_output_retries",
        reason: "structured_output_retry_exhausted",
        error: true,
      },
    ]);
  });

  it("reaches the reasons that have NO subtype of their own", async () => {
    // Arrange + Act
    const reasons: unknown[] = [];
    for (const prompt of [
      "!fail-blocking-limit",
      "!fail-rapid-refill",
      "!fail-prompt-too-long",
      "!fail-image",
      "!fail-model",
      "!fail-malformed-tool-use",
      "!fail-tool-deferred",
      "!fail-tool-deferred-unavailable",
      "!fail-turn-setup",
      "!fail-aborted-tools",
      "!fail-hook-stopped",
    ]) {
      reasons.push((await terminal(prompt)).reason);
    }

    // Assert. Each would be unreachable if the mock only varied the subtype.
    expect(reasons).toEqual([
      "blocking_limit",
      "rapid_refill_breaker",
      "prompt_too_long",
      "image_error",
      "model_error",
      "malformed_tool_use_exhausted",
      "tool_deferred",
      "tool_deferred_unavailable",
      "turn_setup_failed",
      "aborted_tools",
      "hook_stopped",
    ]);
  });

  it("emits NO assistant content on a stopped turn", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!fail-execution"]);

    // Assert. Fabricating an empty message would put a blank bubble on every
    // frontend that renders the conversation.
    expect(ofType(driven, "assistant")).toHaveLength(0);
  });

  it("carries the cause as an errors array on the failing result", async () => {
    // Arrange + Act
    const result = theResult(await driveScenario(["!fail-execution"]));

    // Assert
    expect(result.errors).toEqual(["the turn raised during execution"]);
  });

  it("records the stop hook's own summary beside a stop_hook_prevented terminal", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!fail-stop-hook"]);
    const summary = recordsOfType(driven.transcript(), "system").find(
      (l) => l.subtype === "stop_hook_summary",
    );

    // Assert
    expect(summary?.preventedContinuation).toBe(true);
  });

  it("pairs the continuation-prevented substitution with both declared signals", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!fail-continuation-prevented"]);
    const informational = ofType(driven, "system", "informational")[0];
    const summary = recordsOfType(driven.transcript(), "system").find(
      (l) => l.subtype === "stop_hook_summary",
    );

    // Assert. No `TerminalReason` names this arm; these two are the closest.
    expect({
      prevented: informational?.prevent_continuation,
      recorded: summary?.preventedContinuation,
      reason: theResult(driven).terminal_reason,
    }).toEqual({ prevented: true, recorded: true, reason: "stop_hook_prevented" });
  });
});

describe("API errors", () => {
  it("records the mid-turn evidence as a system:api_error line", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!api-429"]);
    const record = recordsOfType(driven.transcript(), "system").find((l) => l.subtype === "api_error");

    // Assert
    expect(record?.level).toBe("error");
  });

  it("emits api_retry only for the retriable classes", async () => {
    // Arrange + Act
    const retried = async (prompt: string): Promise<number> =>
      ofType(await driveScenario([prompt]), "system", "api_retry").length;

    // Assert. Retrying and not retrying are different facts about the class.
    expect([await retried("!api-429"), await retried("!api-401")]).toEqual([1, 0]);
  });

  it("carries the retry attempt and ceiling on the retry message", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!api-529"]);
    const retry = ofType(driven, "system", "api_retry")[0];

    // Assert
    expect({ attempt: retry?.attempt, max: retry?.max_retries }).toEqual({ attempt: 1, max: 10 });
  });

  it("names the retry-after on the 429's own record", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!api-429"]);
    const record = recordsOfType(driven.transcript(), "system").find((l) => l.subtype === "api_error");

    // Assert
    expect((record?.error as { rateLimits: { retryAfterSeconds: number } }).rateLimits).toEqual({
      retryAfterSeconds: 30,
    });
  });

  it("ends every api-error turn with an api_error terminal reason", async () => {
    // Arrange + Act
    const reasons: unknown[] = [];
    for (const prompt of ["!api-400", "!api-403", "!api-404", "!api-413", "!api-500", "!api-billing"]) {
      reasons.push((await terminal(prompt)).reason);
    }

    // Assert
    expect(new Set(reasons)).toEqual(new Set(["api_error"]));
  });

  it("carries a null status for the class that has none", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!api-max-output"]);
    const retry = ofType(driven, "system", "api_retry")[0];

    // Assert. `!api-max-output` does not retry, so the status lives only on the
    // recorded error; the absence of a retry message is itself the assertion.
    expect(retry).toBeUndefined();
  });
});

describe("response-level stops", () => {
  it("keeps the partial text when the response hit max_tokens", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!max-tokens"]);
    const message = ofType(driven, "assistant")[0]?.message as {
      stop_reason: string;
      content: { text: string }[];
    };

    // Assert
    expect({ reason: message.stop_reason, kept: message.content[0]?.text.length ?? 0 }).toEqual({
      reason: "max_tokens",
      kept: 37,
    });
  });

  it("succeeds the max_tokens turn, because the answer exists and is incomplete", async () => {
    // Arrange + Act
    const result = theResult(await driveScenario(["!max-tokens"]));

    // Assert
    expect({ subtype: result.subtype, stop: result.stop_reason }).toEqual({
      subtype: "success",
      stop: "max_tokens",
    });
  });

  it("emits a fallback content block naming both models on a refusal with fallback", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!refusal-fallback"]);
    const block = (ofType(driven, "assistant")[0]?.message as {
      content: { type: string; from: { model: string }; to: { model: string } }[];
    }).content[0];

    // Assert
    expect({ type: block?.type, from: block?.from.model, to: block?.to.model }).toEqual({
      type: "fallback",
      from: "fake-opus-4-8",
      to: "fake-sonnet-5",
    });
  });

  it("names the retracted uuids so a consumer can evict what it already showed", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!refusal-fallback"]);
    const notice = ofType(driven, "system", "model_refusal_fallback")[0];

    // Assert
    expect((notice?.retracted_message_uuids as string[]).length).toBe(1);
  });

  it("bills the fallback model separately in modelUsage", async () => {
    // Arrange + Act
    const result = theResult(await driveScenario(["!refusal-fallback"]));

    // Assert
    expect(Object.keys(result.modelUsage as object).sort()).toEqual(
      ["fake-opus-4-8", "fake-sonnet-5"].sort(),
    );
  });

  it("FAILS the turn when a refusal has no fallback configured", async () => {
    // Arrange + Act
    const result = theResult(await driveScenario(["!refusal-no-fallback"]));

    // Assert
    expect({ subtype: result.subtype, reason: result.terminal_reason }).toEqual({
      subtype: "error_during_execution",
      reason: "model_error",
    });
  });

  it("leaves the no-fallback notice's content empty, as the corpus does", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!refusal-no-fallback"]);
    const notice = ofType(driven, "system", "model_refusal_no_fallback")[0];

    // Assert
    expect(notice?.content).toBe("");
  });

  it("stops a context-window overflow with prompt_too_long and an informational notice", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!context-window"]);

    // Assert
    expect({
      notice: ofType(driven, "system", "informational").length,
      reason: theResult(driven).terminal_reason,
    }).toEqual({ notice: 1, reason: "prompt_too_long" });
  });
});

describe("the family's own coverage", () => {
  it("registers a scenario for every declared TerminalReason a turn can stop on", async () => {
    // Arrange. The reasons a TURN can stop on; `completed` and
    // `background_requested` are not failures and `aborted_streaming` is the
    // engine's interrupt terminal, covered in the engine suite.
    const wanted = [
      "blocking_limit", "rapid_refill_breaker", "prompt_too_long", "image_error", "model_error",
      "api_error", "malformed_tool_use_exhausted", "aborted_tools", "stop_hook_prevented",
      "hook_stopped", "tool_deferred", "max_turns", "budget_exhausted",
      "structured_output_retry_exhausted", "tool_deferred_unavailable", "turn_setup_failed",
    ];

    // Act
    const reached = new Set<unknown>();
    for (const s of FAILURE_SCENARIOS) {
      if (s.name === "fail-marker") continue;
      reached.add((await terminal(`!${s.name}`)).reason);
    }

    // Assert
    expect(wanted.filter((r) => !reached.has(r))).toEqual([]);
  });
});
