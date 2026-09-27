/**
 * convert/terminals.ts's FAILURE_ARMS table, direct.
 *
 * `convertResult` is exercised end to end by the golden corpus and by
 * `fake/scenarios/failures.ts`, but the fake vendor's failure scenarios all
 * answer through the result SUBTYPE fallback (`raw.subtype`), never through
 * `terminal_reason` naming these particular arms directly — a REAL vendor can
 * set `terminal_reason` to any of them (that is what the table is FOR: "the
 * arm is the vendor's, not a classification"), so nothing here is dead, it is
 * just a shape no scenario happens to produce. This pins each arm the
 * coverage run found at zero hits by calling `convertResult` with a synthetic
 * `result` message naming it via `terminal_reason` directly.
 */
import { writeSync } from "node:fs";
import { describe, expect, it, vi } from "vitest";
import { containing } from "../expect-shapes.js";

import {
  classifyVendorApiFailure,
  convertResult,
  redactVendorMessage,
  VENDOR_MESSAGE_MAX,
} from "../../src/convert/terminals.js";
import type { VendorApiError } from "../../src/convert/terminals.js";
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../../src/proto.js";
import type { ContextOverrides } from "./fold-harness.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import { foldContext } from "./fold-harness.js";

const mockedWriteSync = vi.mocked(writeSync);

/** Every JSONL record the shim persisted to fd 3 since the last clear. */
function persistedRecords(): Array<{
  operation: string;
  message: string;
  context: Record<string, unknown>;
}> {
  return (mockedWriteSync.mock.calls as unknown as Array<[number, Buffer, number, number]>).map(
    ([, bytes, offset, length]) =>
      JSON.parse(bytes.subarray(offset, offset + length).toString("utf8")) as {
        operation: string;
        message: string;
        context: Record<string, unknown>;
      },
  );
}

/** Fold an api_error terminal and return the diagnostic record it emitted. */
function diagnosticRecord(
  apiErrorStatus: number | null,
  errors: readonly string[],
  vendor: VendorApiError,
  overrides: ContextOverrides,
): { operation: string; context: Record<string, unknown> } | undefined {
  mockedWriteSync.mockClear();
  const message = {
    type: "result",
    uuid: "result-uuid-diag",
    terminal_reason: "api_error",
    errors,
    api_error_status: apiErrorStatus,
  } as unknown as Extract<SdkMessage, { type: "result" }>;
  convertResult(message, foldContext(overrides), undefined, vendor);
  return persistedRecords().find((record) =>
    record.operation.startsWith("shim.vendor."),
  );
}

function resultMessage(terminalReason: string): Extract<SdkMessage, { type: "result" }> {
  return {
    type: "result",
    uuid: "result-uuid-1",
    terminal_reason: terminalReason,
  } as unknown as Extract<SdkMessage, { type: "result" }>;
}

function failureCaseOf(terminalReason: string): string | undefined {
  const output = convertResult(resultMessage(terminalReason), foldContext(), undefined);
  const frame = output.turnEnded?.frame;
  const result = frame?.result;
  if (result?.case !== "failure") return undefined;
  return result.value.failure.case;
}

describe.each([
  ["blocking_limit", "blockingLimit"],
  ["rapid_refill_breaker", "rapidRefillBreaker"],
  ["prompt_too_long", "promptTooLong"],
  ["image_error", "imageError"],
  ["model_error", "modelError"],
  ["malformed_tool_use_exhausted", "malformedToolUseExhausted"],
  ["hook_stopped", "hookStopped"],
  ["tool_deferred", "toolDeferred"],
  ["tool_deferred_unavailable", "toolDeferredUnavailable"],
  ["turn_setup_failed", "turnSetupFailed"],
])("terminal_reason %s", (reason, expectedCase) => {
  it(`converts to failure.${expectedCase}`, () => {
    expect(failureCaseOf(reason)).toBe(expectedCase);
  });
});

/**
 * The API failure taxonomy: which arm a status and a vendor error class name.
 *
 * The result record states the HTTP STATUS ALONE, so three classes have no
 * status that names them — `billing_error` shares 402, `oauth_org_not_allowed`
 * is an ordinary 403, `max_output_tokens` carries no status at all — and the
 * vendor's own error class, remembered by the fold from its error records, is
 * what puts each in its own arm.
 */
function apiResultMessage(
  apiErrorStatus: number | null,
): Extract<SdkMessage, { type: "result" }> {
  return {
    type: "result",
    uuid: "result-uuid-api",
    terminal_reason: "api_error",
    errors: ["the vendor said so"],
    api_error_status: apiErrorStatus,
  } as unknown as Extract<SdkMessage, { type: "result" }>;
}

function apiFailure(
  apiErrorStatus: number | null,
  vendor: { errorClass?: string; retryAfterMs?: number },
): conversationv1.ApiRequestFailed {
  const output = convertResult(apiResultMessage(apiErrorStatus), foldContext(), undefined, vendor);
  const result = output.turnEnded?.frame?.result;
  if (result?.case !== "failure") throw new Error("the api terminal must be a failure");
  const failure = result.value.failure;
  if (failure.case !== "apiRequestFailed") throw new Error("the api terminal must be api_request_failed");
  return failure.value;
}

describe.each([
  ["400", 400, "invalid_request", "invalidRequest"],
  ["401", 401, "authentication_failed", "authenticationFailed"],
  ["403", 403, "invalid_request", "permissionDenied"],
  ["404", 404, "model_not_found", "notFound"],
  ["413", 413, "invalid_request", "requestTooLarge"],
  ["429", 429, "rate_limit", "rateLimited"],
  ["500", 500, "server_error", "internal"],
  ["529", 529, "overloaded", "overloaded"],
])("an api_error terminal at status %s", (_name, status, errorClass, expectedKind) => {
  it(`lands on kind.${expectedKind}`, () => {
    // Arrange + Act
    const failed = apiFailure(status, { errorClass });

    // Assert
    expect(failed.kind.case).toBe(expectedKind);
  });
});

describe("an api_error terminal whose class no status can name", () => {
  it("lands billing_error on kind.billingError rather than a bare 402", () => {
    // Arrange + Act
    const failed = apiFailure(402, { errorClass: "billing_error" });

    // Assert
    expect(failed.kind.case).toBe("billingError");
  });

  it("lands oauth_org_not_allowed on its own arm, not the 403 it shares", () => {
    // Arrange + Act
    const failed = apiFailure(403, { errorClass: "oauth_org_not_allowed" });

    // Assert
    expect(failed.kind.case).toBe("oauthOrgNotAllowed");
  });

  it("lands max_output_tokens on its own arm though it carries no status", () => {
    // Arrange + Act
    const failed = apiFailure(null, { errorClass: "max_output_tokens" });

    // Assert
    expect(failed.kind.case).toBe("maxOutputTokens");
  });
});

describe("an api_error terminal the taxonomy does not model", () => {
  it("keeps an unmodelled class BY NAME", () => {
    // Arrange + Act
    const failed = apiFailure(418, { errorClass: "unknown" });

    // Assert
    expect(failed.kind).toEqual({
      case: "unmodeled",
      value: containing({ type: "unknown" }),
    });
  });

  it("names a status the taxonomy does not model when the vendor stated no class", () => {
    // Arrange + Act
    const failed = apiFailure(418, {});

    // Assert
    expect(failed.kind.value).toMatchObject({ type: "http_418" });
  });
});

describe("the vendor's stated wait", () => {
  it("rides the rate_limited arm the client counts down from", () => {
    // Arrange + Act
    const failed = apiFailure(429, { errorClass: "rate_limit", retryAfterMs: 549 });

    // Assert
    expect(failed.kind.value).toMatchObject({ retryAfterMs: 549n });
  });

  it("rides the overloaded arm too", () => {
    // Arrange + Act
    const failed = apiFailure(529, { errorClass: "overloaded", retryAfterMs: 1_144 });

    // Assert
    expect(failed.kind.value).toMatchObject({ retryAfterMs: 1_144n });
  });

  it("stays UNSET when the vendor stated no wait", () => {
    // Arrange + Act
    const failed = apiFailure(429, { errorClass: "rate_limit" });

    // Assert
    expect((failed.kind.value as conversationv1.ApiRateLimited).retryAfterMs).toBeUndefined();
  });
});

/**
 * The vendor's CLASS as the fallback for a failure that stated no status.
 *
 * A result record can carry `api_error_status: null` — the vendor failed before
 * it had a response to read a status off — and the only fact left is the class
 * its own error record stated. Without this fallback every one of those landed
 * on `unmodeled`, which says "a class we do not model" about a class that is
 * modelled.
 */
describe.each([
  ["authentication_failed", "authenticationFailed"],
  ["rate_limit", "rateLimited"],
  ["overloaded", "overloaded"],
  ["invalid_request", "invalidRequest"],
  ["model_not_found", "notFound"],
  ["server_error", "internal"],
])("an api_error terminal with NO status whose class is %s", (errorClass, expectedKind) => {
  it(`falls back to kind.${expectedKind}`, () => {
    // Arrange + Act
    const failed = apiFailure(null, { errorClass });

    // Assert
    expect(failed.kind.case).toBe(expectedKind);
  });
});

describe("an api_error terminal at status 402 with no class stated", () => {
  it("lands on kind.billingError from the status alone", () => {
    // Arrange + Act
    const failed = apiFailure(402, {});

    // Assert
    expect(failed.kind.case).toBe("billingError");
  });
});

describe("an api_error terminal with neither a status nor a class", () => {
  it("names the unmodelled failure `unknown`, since nothing else names it", () => {
    // Arrange + Act
    const failed = apiFailure(null, {});

    // Assert
    expect(failed.kind.value).toMatchObject({ type: "unknown" });
  });
});

describe("an api_error terminal the vendor accounted for with no errors", () => {
  it("states the shim's own message rather than an empty one", () => {
    // Arrange
    const message = {
      type: "result",
      uuid: "result-uuid-empty",
      terminal_reason: "api_error",
      errors: [],
      api_error_status: 500,
    } as unknown as SdkMessage;

    // Act
    const output = convertResult(message as never, foldContext(), undefined, {});

    // Assert
    const result = output.turnEnded?.frame?.result;
    const failure = result?.case === "failure" ? result.value.failure : undefined;
    expect(failure?.case === "apiRequestFailed" ? failure.value.message : undefined).toBe(
      "the vendor API failed the request",
    );
  });
});

/**
 * A user stop's `by_user` cause is the KillTurn caller's statement of HOW,
 * verbatim; a stop nobody stated a how for records no command.
 */
describe("a user stop's commanded HOW", () => {
  /** The `by_user` cause a user-stop terminal folded under `overrides` records. */
  function byUserOf(overrides: ContextOverrides): conversationv1.AgentInterruptedByUser | undefined {
    const output = convertResult(resultMessage("aborted_streaming"), foldContext(overrides), undefined);
    const result = output.turnEnded?.frame.result;
    if (result?.case !== "success" || result.value.outcome.case !== "interrupted") return undefined;
    const cause = result.value.outcome.value.cause;
    return cause.case === "byUser" ? cause.value : undefined;
  }

  it("records an interjection the caller stated", () => {
    // Arrange
    const stopCommand = create(conversationv1.AgentInterruptedByUserSchema, {
      command: { case: "interjection", value: create(conversationv1.AgentInterruptedByUserInterjectionSchema, {}) },
    });

    // Act
    const byUser = byUserOf({ stopCommand });

    // Assert
    expect(byUser?.command.case).toBe("interjection");
  });

  it("records a direct stop the caller stated", () => {
    // Arrange
    const stopCommand = create(conversationv1.AgentInterruptedByUserSchema, {
      command: { case: "direct", value: create(conversationv1.AgentInterruptedByUserDirectSchema, {}) },
    });

    // Act
    const byUser = byUserOf({ stopCommand });

    // Assert
    expect(byUser?.command.case).toBe("direct");
  });

  it("records no command when none was stated", () => {
    // Arrange + Act
    const byUser = byUserOf({});

    // Assert
    expect({ present: byUser !== undefined, command: byUser?.command.case }).toEqual({
      present: true,
      command: undefined,
    });
  });
});

/**
 * The two terminals nothing above spells: the background move, and the gap.
 */
describe("a turn that moved to the background", () => {
  it("is a SUCCESS whose outcome is backgrounded, because nothing ended", () => {
    // Arrange + Act
    const output = convertResult(resultMessage("background_requested"), foldContext(), undefined);

    // Assert
    const result = output.turnEnded?.frame?.result;
    const success = result?.case === "success" ? result.value.outcome.case : undefined;
    expect(success).toBe("backgrounded");
  });
});

describe("a terminal reason no arm spells", () => {
  it("is RELAYED as the unclassified execution failure rather than an invented arm", () => {
    // Arrange + Act
    const failureCase = failureCaseOf("swallowed_by_a_black_hole");

    // Assert
    expect(failureCase).toBe("executionError");
  });

  it("keeps the run's accumulated errors as the account of the gap", () => {
    // Arrange
    const message = {
      type: "result",
      uuid: "result-uuid-gap",
      terminal_reason: "swallowed_by_a_black_hole",
      errors: ["first attempt failed", "second attempt failed"],
    } as unknown as SdkMessage;

    // Act
    const output = convertResult(message as never, foldContext(), undefined);

    // Assert
    const result = output.turnEnded?.frame?.result;
    expect(result?.case === "failure" ? result.value.errors : undefined).toEqual([
      "first attempt failed",
      "second attempt failed",
    ]);
  });

  it("is the unclassified execution failure when the vendor stated no reason at all", () => {
    // Arrange
    const message = { type: "result", uuid: "result-uuid-bare" } as unknown as SdkMessage;

    // Act
    const output = convertResult(message as never, foldContext(), undefined);

    // Assert
    const result = output.turnEnded?.frame?.result;
    expect(result?.case === "failure" ? result.value.failure.case : undefined).toBe(
      "executionError",
    );
  });
});

/**
 * The result SUBTYPE, which is the arm for a vendor that stated no reason.
 */
describe.each([
  ["error_during_execution", "executionError"],
  ["error_max_turns", "maxTurns"],
  ["error_max_budget_usd", "budgetExhausted"],
  ["error_max_structured_output_retries", "structuredOutputRetryExhausted"],
])("a result whose subtype is %s and which stated no terminal reason", (subtype, expectedCase) => {
  it(`converts to failure.${expectedCase}`, () => {
    // Arrange
    const message = {
      type: "result",
      uuid: `result-uuid-${subtype}`,
      subtype,
      is_error: true,
    } as unknown as SdkMessage;

    // Act
    const output = convertResult(message as never, foldContext(), undefined);

    // Assert
    const result = output.turnEnded?.frame?.result;
    expect(result?.case === "failure" ? result.value.failure.case : undefined).toBe(expectedCase);
  });
});

/**
 * The vendor API-failure DIAGNOSTIC record.
 *
 * The owner intermittently sees a credential rejection or a "model or resource
 * does not exist" on a live workspace, and the cause is only recoverable after
 * the fact with the class, the status, the vendor's own sentence, the account
 * config-dir the failing token belonged to, and the model, all on one greppable
 * record. This pins that record for each of the two classes that matter.
 */
describe("the vendor API-failure diagnostic record", () => {
  it("fires shim.vendor.auth_rejected with class, status, message and config-dir for an authentication_failed result", () => {
    // Arrange, Act.
    const record = diagnosticRecord(
      401,
      ["the credential was rejected — sign in again"],
      { errorClass: "authentication_failed" },
      { claudeConfigDir: "/home/acct/.claude", model: "claude-opus-5" },
    );

    // Assert.
    expect(record?.operation).toBe("shim.vendor.auth_rejected");
    expect(record?.context.vendor_error).toBe("authentication_failed");
    expect(record?.context.http_status).toBe(401);
    expect(record?.context.vendor_message).toBe("the credential was rejected — sign in again");
    expect(record?.context.claude_config_dir).toBe("/home/acct/.claude");
    expect(record?.context.model).toBe("claude-opus-5");
  });

  it("fires shim.vendor.model_missing with class, status, message and config-dir for a model-not-found result", () => {
    // Arrange, Act.
    const record = diagnosticRecord(
      404,
      ["the model or resource does not exist"],
      { errorClass: "model_not_found" },
      { claudeConfigDir: "/home/acct/.claude-chesscom", model: "claude-opus-5" },
    );

    // Assert.
    expect(record?.operation).toBe("shim.vendor.model_missing");
    expect(record?.context.vendor_error).toBe("model_not_found");
    expect(record?.context.http_status).toBe(404);
    expect(record?.context.vendor_message).toBe("the model or resource does not exist");
    expect(record?.context.claude_config_dir).toBe("/home/acct/.claude-chesscom");
    expect(record?.context.model).toBe("claude-opus-5");
  });
});

/**
 * The classifier: which greppable operation a failure lands under.
 */
describe("classifyVendorApiFailure", () => {
  it("names a 401 an auth rejection", () => {
    expect(classifyVendorApiFailure(401, undefined, "")).toBe("shim.vendor.auth_rejected");
  });

  it("names a credential class an auth rejection even without a status", () => {
    expect(classifyVendorApiFailure(undefined, "authentication_failed", "")).toBe(
      "shim.vendor.auth_rejected",
    );
  });

  it("names a 404 a missing model", () => {
    expect(classifyVendorApiFailure(404, undefined, "")).toBe("shim.vendor.model_missing");
  });

  it("names a does-not-exist sentence a missing model", () => {
    expect(classifyVendorApiFailure(undefined, undefined, "the model or resource does not exist")).toBe(
      "shim.vendor.model_missing",
    );
  });

  it("names anything else the generic api_error", () => {
    expect(classifyVendorApiFailure(500, "server_error", "internal")).toBe("shim.vendor.api_error");
  });
});

/**
 * The redactor: a credential must never ride the vendor sentence into the log.
 */
describe("redactVendorMessage", () => {
  it("masks a bearer token embedded in the sentence", () => {
    const masked = redactVendorMessage("auth failed for Bearer abc123DEF456ghi789");
    expect(masked).toBe("auth failed for Bearer [redacted]");
  });

  it("masks an sk- api key embedded in the sentence", () => {
    const masked = redactVendorMessage("rejected key sk-ant-0123456789abcdef");
    expect(masked).toContain("[redacted-key]");
    expect(masked).not.toContain("sk-ant-0123456789abcdef");
  });

  it("masks a JWT embedded in the sentence", () => {
    const masked = redactVendorMessage("token eyJhbGciOi.eyJzdWIiOiJ1c2VyIn0 was rejected");
    expect(masked).toContain("[redacted-jwt]");
    expect(masked).not.toContain("eyJhbGciOi");
  });

  it("truncates a sentence longer than the bound", () => {
    const masked = redactVendorMessage("x".repeat(VENDOR_MESSAGE_MAX + 50));
    expect(masked).toHaveLength(VENDOR_MESSAGE_MAX + 1);
    expect(masked.endsWith("…")).toBe(true);
  });

  it("leaves an ordinary sentence untouched", () => {
    expect(redactVendorMessage("the credential was rejected — sign in again")).toBe(
      "the credential was rejected — sign in again",
    );
  });
});
