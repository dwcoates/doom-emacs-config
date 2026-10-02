/**
 * The retry label a failed vendor start carries.
 *
 * WHAT THESE GUARD. The daemon retries a `vendor_start_failed` refusal only on
 * the shim's label and never re-reads `detail`, so a transient failure labeled
 * REJECTED is a session that never comes back, and a deterministic one labeled
 * RETRYABLE is a retry loop that cannot succeed.
 */
import { describe, expect, it } from "vitest";
import { logRecordsSince, logSinkMark } from "../log-records.js";
import {
  BoundExceededError,
  openingErrorVerdict,
  startFailureLabel,
  VendorStartError,
} from "../../src/engine/start-failure.js";

describe("openingErrorVerdict", () => {
  it.each([
    // [case, status, text, kind, retry]
    ["a network failure the errno names", undefined, "connect ECONNREFUSED 1.2.3.4:443", "shim.vendor.unreachable", "retryable"],
    ["a credential rejected by status", 401, "invalid x-api-key", "shim.vendor.auth_rejected", "rejected"],
    ["a credential rejected by its words", undefined, "OAuth token has expired", "shim.vendor.auth_rejected", "rejected"],
    ["a missing model by status", 404, "model: claude-nope", "shim.vendor.model_missing", "rejected"],
    ["a refused resume", undefined, "No conversation found with session ID: bf5fcae1", "shim.vendor.api_error", "rejected"],
    ["an overloaded API", 529, "Overloaded", "shim.vendor.api_error", "retryable"],
    ["a server error", 500, "Internal server error", "shim.vendor.api_error", "retryable"],
    ["an unavailable service", 503, "Service unavailable", "shim.vendor.api_error", "retryable"],
    ["a rate limit", 429, "Too many requests", "shim.vendor.api_error", "retryable"],
    ["a request timeout", 408, "Request timeout", "shim.vendor.api_error", "retryable"],
    ["a malformed request", 400, "messages: field required", "shim.vendor.api_error", "rejected"],
    ["an overloaded API that stated no status", undefined, "API Error: Overloaded", "shim.vendor.api_error", "retryable"],
    ["a network failure only the SDK's sentence names", undefined, "TypeError: fetch failed", "shim.vendor.api_error", "retryable"],
    ["an execution error with no transient words", undefined, "the budget is exhausted", "shim.vendor.api_error", "rejected"],
  ] as const)("labels %s", (_case, status, text, kind, retry) => {
    // Arrange, Act.
    const verdict = openingErrorVerdict(status, text);

    // Assert.
    expect(verdict).toEqual({ kind, retry });
  });

  it("lets a transient status win over text the classifier reads as a missing model", () => {
    // Arrange: "resource" alone sends the classifier to model_missing.
    const text = "the resource is temporarily unavailable";

    // Act.
    const verdict = openingErrorVerdict(503, text);

    // Assert.
    expect(verdict.retry).toBe("retryable");
  });
});

describe("BoundExceededError", () => {
  it("names the call and the bound it was given", () => {
    // Arrange, Act.
    const err = new BoundExceededError("supportedModels", 3_000);

    // Assert.
    expect(err.message).toBe("the vendor did not answer supportedModels within 3000ms");
  });
});

describe("startFailureLabel", () => {
  it("reads the label and bound a settle site set", () => {
    // Arrange.
    const err = new VendorStartError("silent", "retryable", { name: "live_signal", ms: 3_000 });

    // Act.
    const label = startFailureLabel(err);

    // Assert.
    expect(label).toEqual({ retry: "retryable", bound: { name: "live_signal", ms: 3_000 } });
  });

  it("answers an UNLABELED error as rejected, never as retryable", () => {
    // Arrange.
    const err = new Error("nobody classified this");

    // Act.
    const label = startFailureLabel(err);

    // Assert.
    expect(label).toEqual({ retry: "rejected", bound: undefined });
  });

  it("says an unlabeled error loudly, as the defect it is", () => {
    // Arrange.
    const before = logSinkMark();

    // Act.
    startFailureLabel("not even an Error");

    // Assert.
    const record = logRecordsSince(before).find((r) =>
      r.message.includes("a failed start reached the refusal with no retry label"),
    );
    expect({ level: record?.level, cause: record?.context["cause"] }).toEqual({
      level: "warn",
      cause: "not even an Error",
    });
  });
});
