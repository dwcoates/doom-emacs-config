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
import { describe, expect, it } from "vitest";

import { convertResult } from "../../src/convert/terminals.js";
import type { conversationv1 } from "../../src/proto.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import { foldContext } from "./fold-harness.js";

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
      value: expect.objectContaining({ type: "unknown" }),
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
