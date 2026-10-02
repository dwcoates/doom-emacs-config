/**
 * Whether a vendor message says this machine can reach the network.
 */
import { describe, expect, it } from "vitest";
import { networkReach } from "../../src/engine/network-reach.js";
import type { SdkMessage } from "../../src/sdk/types.js";

const OFFLINE = "API Error: Can't reach the API server — check your internet or DNS (ENOTFOUND)";

function assistant(fields: Record<string, unknown>, text = "hello", model = "claude-x"): SdkMessage {
  return {
    type: "assistant",
    message: { model, content: [{ type: "text", text }] },
    parent_tool_use_id: null,
    uuid: "00000000-0000-4000-8000-000000000001",
    session_id: "s",
    ...fields,
  } as unknown as SdkMessage;
}

function result(fields: Record<string, unknown>): SdkMessage {
  return { type: "result", uuid: "00000000-0000-4000-8000-000000000002", session_id: "s", ...fields } as unknown as SdkMessage;
}

function apiRetry(errorStatus: number | null, error: string): SdkMessage {
  return {
    type: "system",
    subtype: "api_retry",
    attempt: 1,
    max_retries: 10,
    retry_delay_ms: 500,
    error_status: errorStatus,
    error,
    uuid: "00000000-0000-4000-8000-000000000003",
    session_id: "s",
  } as unknown as SdkMessage;
}

describe("networkReach", () => {
  it.each([
    ["a stream event", { type: "stream_event" } as unknown as SdkMessage, { kind: "reached" }],
    ["a model response", assistant({}), { kind: "reached" }],
    ["a synthetic notice with no error", assistant({}, "notice", "<synthetic>"), undefined],
    [
      "a failed request whose notice names an outage",
      assistant({ error: "unknown" }, OFFLINE, "<synthetic>"),
      { kind: "unreachable", detail: OFFLINE },
    ],
    ["a refusal the vendor classed", assistant({ error: "rate_limit" }, "rate limited", "<synthetic>"), { kind: "reached" }],
    ["a result the API answered with a status", result({ is_error: true, subtype: "success", result: "API Error: 500", api_error_status: 500 }), { kind: "reached" }],
    ["a successful result", result({ is_error: false, subtype: "success", result: "done" }), { kind: "reached" }],
    [
      "an error result with no status and outage words",
      result({ is_error: true, subtype: "success", result: "TypeError: fetch failed" }),
      { kind: "unreachable", detail: "TypeError: fetch failed" },
    ],
    ["an error result with no status and no outage words", result({ is_error: true, subtype: "error_during_execution", errors: ["boom"] }), undefined],
    ["a retry the API answered", apiRetry(529, "overloaded"), { kind: "reached" }],
    ["a retry with no status and no outage class", apiRetry(null, "server_error"), undefined],
    ["an unrelated system message", { type: "system", subtype: "status" } as unknown as SdkMessage, undefined],
  ] as const)("reads %s", (_name, message, want) => {
    // Act
    const reach = networkReach(message);

    // Assert
    expect(reach).toEqual(want);
  });
});
