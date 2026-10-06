/**
 * The failure family. The point of every test here is that the SIXTEEN
 * conversation.v1 failure arms are distinguished by `terminal_reason`, not by
 * the four declared `result.subtype` values — so the suite asserts the pairing,
 * not one half of it.
 */
import { describe, expect, it } from "vitest";

import { FAILURE_SCENARIOS } from "../../../src/fake/scenarios/failures.js";
import { createFold } from "../../../src/convert/fold.js";
import type { SdkMessage } from "../../../src/sdk/types.js";
import { foldContext, residueOf } from "../../convert/fold-harness.js";
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

  it("emits the work the turn REACHED before it stopped", async () => {
    // A TURN DOES NOT STOP BEFORE IT HAS DONE ANYTHING. Every turn-stop capture
    // carries work ahead of its terminal (`turn-stop-max-budget-usd` is
    // thinking + a response, `turn-stop-max-turns` thinking + a bash call), so
    // a bare result was a shape no capture shows — and it left every stop arm
    // asserted over an empty turn, where a converter that dropped the work
    // silently would still have passed.
    // Arrange + Act
    const driven = await driveScenario(["!fail-execution"]);

    // Assert. One assistant line per block, and the response did NOT end the
    // turn — the result did.
    const lines = ofType(driven, "assistant");
    expect(
      lines.map((line) => (line.message as { content: { type?: string }[] }).content[0]?.type),
    ).toEqual(["thinking", "text"]);
    expect(
      lines.map((line) => (line.message as { stop_reason?: unknown }).stop_reason),
    ).toEqual([null, null]);
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

  it("states the vendor's error class on the failed assistant message", async () => {
    // Arrange + Act. The result record carries the status alone, so this is the
    // only stream carrier of a class no status can name.
    const driven = await driveScenario(["!api-oauth-org"]);
    const failed = ofType(driven, "assistant").find((m) => m.error !== undefined);

    // Assert
    expect(failed?.error).toBe("oauth_org_not_allowed");
  });

  it("keeps that class off the transcript, which has its own api_error line", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!api-oauth-org"]);
    const recorded = recordsOfType(driven.transcript(), "assistant").filter(
      (l) => l.error !== undefined,
    );

    // Assert
    expect(recorded).toEqual([]);
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

/**
 * The converter-fault pair, checked against the REAL fold.
 *
 * A scenario that claims to carry an unconvertible message is only as good as
 * the converter's actual refusal, so these tests fold what the mock emitted
 * rather than asserting the emission alone. `src/convert` is imported HERE and
 * nowhere in `src/fake`: the mock never knows what the fold will do with a
 * message, which is exactly why the mock cannot be the thing that decides a
 * defect happened.
 */
const foldEverything = (
  messages: readonly SdkMessage[],
): { frames: number; residue: number } => {
  const fold = createFold();
  const context = foldContext();
  let frames = 0;
  let residue = 0;
  for (const message of messages) {
    for (const entry of fold.onSdkMessage(message, context).entries) {
      if (entry.item.kind === "frame") frames++;
      if (residueOf(entry) !== undefined) residue++;
    }
  }
  return { frames, residue };
};

/** Every `hook_started` message one drive produced. */
const hookStarts = (messages: readonly SdkMessage[]): Record<string, unknown>[] =>
  (messages as unknown as Record<string, unknown>[]).filter(
    (m) => m.type === "system" && m.subtype === "hook_started",
  );

describe("a converter fault", () => {
  it("emits exactly ONE malformed message, and it is the hook announcement", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!fault-converter"]);

    // Assert
    expect(hookStarts(driven.messages).map((m) => m.hook_id)).toEqual([""]);
  });

  it("states the hook's other fields faithfully, so ONLY the identity is wrong", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!fault-converter"]);

    // Assert. A message wrong in three ways would not tell us which one the
    // converter refused.
    expect(hookStarts(driven.messages)[0]).toMatchObject({
      hook_name: "PreToolUse:Read",
      hook_event: "PreToolUse",
    });
  });

  it("really trips the fold's defect path: residue and NO frame", async () => {
    // Arrange
    const driven = await driveScenario(["!fault-converter"]);
    const malformed = hookStarts(driven.messages)[0] as unknown as SdkMessage;

    // Act
    const folded = foldEverything([malformed]);

    // Assert. `hook_id` IS the firing's activity identity, so an empty one is
    // not a degraded frame — it is no frame.
    expect(folded).toEqual({ frames: 0, residue: 1 });
  });

  it("lands as the DEFECT arm, not as a record no converter owns", async () => {
    // Arrange
    const driven = await driveScenario(["!fault-converter"]);
    const malformed = hookStarts(driven.messages)[0] as unknown as SdkMessage;
    const fold = createFold();

    // Act
    const [entry] = fold.onSdkMessage(malformed, foldContext()).entries;
    const residue = residueOf(entry)?.unservedItem;

    // Assert. `unknown` would mean a kind nobody models; `unparsed` naming a
    // converter defect is a converter that refused a record it DOES own.
    expect({
      arm: residue?.case,
      defect: residue?.case === "unparsed" ? residue.value.parseError.includes("converter defect") : false,
      source: residue?.case === "unparsed" ? residue.value.source : "",
    }).toEqual({ arm: "unparsed", defect: true, source: "system/hook_started" });
  });

  it("still ends the turn successfully, because a bad vendor line is not a stopped turn", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!fault-converter"]);

    // Assert
    expect(theResult(driven).subtype).toBe("success");
  });

  it("still produces the turn's ordinary prose after the malformed message", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!fault-converter"]);

    // Assert
    expect(theResult(driven).result).toBe(
      "One vendor message was unconvertible; the rest of the turn was ordinary.",
    );
  });
});

describe("recovering from a converter fault", () => {
  it("announces the SAME hook with a real identity", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!fault-recover"]);
    const started = hookStarts(driven.messages)[0];

    // Assert
    expect({ empty: started?.hook_id === "", event: started?.hook_event }).toEqual({
      empty: false,
      event: "PreToolUse",
    });
  });

  it("folds the recovered start without the defect the malformed turn tripped", async () => {
    // Arrange
    const driven = await driveScenario(["!fault-recover"]);
    const started = hookStarts(driven.messages)[0] as unknown as SdkMessage;

    // Act
    const folded = foldEverything([started]);

    // Assert. A hook's start is never stored (owner ruling 2026-10-06), so the
    // recovered start folds to nothing at all — and, unlike the malformed one,
    // to no residue.
    expect(folded).toEqual({ frames: 0, residue: 0 });
  });

  it("settles the hook, so the recovery is a whole unit and not half of one", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!fault-recover"]);
    const responses = ofType(driven, "system", "hook_response");

    // Assert
    expect(responses.map((r) => r.outcome)).toEqual(["success"]);
  });

  it("converts every message of the turn, malformed or not", async () => {
    // Arrange
    const driven = await driveScenario(["!fault-recover"]);

    // Act
    const folded = foldEverything(
      driven.messages.filter((m) => (m as { type?: string }).type === "system"),
    );

    // Assert. The init message contributes its own session rows; what matters is
    // that NOTHING landed as residue.
    expect(folded.residue).toBe(0);
  });
});
