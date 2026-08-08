import { afterEach, describe, expect, it, vi } from "vitest";
import { logAssistantApiResponseUsage } from "../src/usage-log.js";
import { normalizeApiUsage } from "../src/api-usage.js";

const cleanups: Array<() => void> = [];
afterEach(() => { for (const cleanup of cleanups.splice(0)) cleanup(); });

function records(): Array<{ level: string; message: string; context: Record<string, unknown> }> {
  const out: Array<{ level: string; message: string; context: Record<string, unknown> }> = [];
  const spy = vi.spyOn(process.stderr, "write").mockImplementation(((chunk: unknown): boolean => {
    out.push(JSON.parse(String(chunk)) as { level: string; message: string; context: Record<string, unknown> });
    return true;
  }) as typeof process.stderr.write);
  cleanups.push(() => spy.mockRestore());
  return out;
}

function assistant(usage: unknown, requestId?: string): Record<string, unknown> {
  return { type: "assistant", session_id: "claude-session", ...(requestId === undefined ? {} : { request_id: requestId }), message: { id: "msg_123", model: "claude-fable-5", usage } };
}

function log(usage: Record<string, unknown>, requestId?: string): void {
  const raw = assistant(usage, requestId);
  logAssistantApiResponseUsage(raw, normalizeApiUsage(usage), "agent-session");
}

describe("logAssistantApiResponseUsage", () => {
  it("logs complete response accounting with nested vendor details", () => {
    const out = records();
    log({
      input_tokens: 10, output_tokens: 20, cache_read_input_tokens: 50, cache_creation_input_tokens: 40,
      cache_creation: { ephemeral_5m_input_tokens: 30, ephemeral_1h_input_tokens: 10 },
      server_tool_use: { web_search_requests: 2 }, iterations: [{ input_tokens: 4 }], output_tokens_details: { reasoning_tokens: 3 },
      cache_diagnostic: { status: "verified" }, fallback_credit: { status: { type: "redeemed" } },
      unmodeled_usage: { beta_counter: 11 },
      service_tier: "standard", speed: "standard", inference_geo: "us-east-1",
    }, "req_123");
    expect(out).toHaveLength(1);
    expect(out[0]).toMatchObject({ agent_repl_session_id: "test-agent-session", claude_session_id: "claude-session", message: "completed assistant API response usage", context: {
      api_message_id: "msg_123", api_request_id: "req_123", model: "claude-fable-5",
      input_tokens: 10, output_tokens: 20, cache_read_input_tokens: 50, cache_creation_input_tokens: 40,
      cache_creation_5m_input_tokens: 30, cache_creation_1h_input_tokens: 10,
      service_tier: "standard", speed: "standard", inference_geo: "us-east-1",
      server_tool_use: { web_search_requests: 2 }, iterations: [{ input_tokens: 4 }], output_tokens_details: { reasoning_tokens: 3 },
      cache_diagnostic: { status: "verified" }, fallback_credit: { status: { type: "redeemed" } }, unmodeled_usage_fields: {},
      unmodeled_usage: { beta_counter: 11 },
    } });
  });

  /**
   * THE LOG IS RAW EVIDENCE, NOT A VERDICT. Every derived figure it used to
   * carry — the expensive-input sum, the total prompt input, the three rates,
   * and the low-reuse warning raised from them — was token judgment taken
   * outside the daemon. The daemon owns all of it now, over the canonical
   * TokenUsage shape (internal/tokenusage), and raises the expensive-turn alert
   * and the cold-ping alarm from it. These cases pin the shim on the faithful
   * side of that line: the buckets behind the daemon's verdict are still on
   * disk, and nothing computed from them is.
   */
  it("logs the vendor buckets of a cold re-ingest without summing them", () => {
    // Arrange — a cold re-ingest: input_tokens is a rounding error beside the
    // cache creation, and the sum is exactly the figure the daemon derives.
    const out = records();
    // Act
    log({ input_tokens: 12, output_tokens: 3, cache_read_input_tokens: 0, cache_creation_input_tokens: 500_000 });
    // Assert
    expect(out[0].context).toMatchObject({ input_tokens: 12, cache_creation_input_tokens: 500_000, cache_read_input_tokens: 0 });
    expect(out[0].context).not.toHaveProperty("uncached_input_tokens");
  });

  it("logs the cache read verbatim rather than a share of the prompt", () => {
    // Arrange — a large standing prefix presented again.
    const out = records();
    // Act
    log({ input_tokens: 10, output_tokens: 3, cache_read_input_tokens: 900_000, cache_creation_input_tokens: 20 });
    // Assert
    expect(out[0].context).toMatchObject({ cache_read_input_tokens: 900_000 });
    expect(out[0].context).not.toHaveProperty("total_prompt_input_tokens");
  });

  it("raises no cache-reuse warning of its own, however cold the response", () => {
    // Arrange — a response that would have tripped the retired 80% threshold.
    const out = records();
    // Act
    log({ input_tokens: 21, output_tokens: 1, cache_read_input_tokens: 79, cache_creation_input_tokens: 0 });
    // Assert
    expect(out).toHaveLength(1);
    expect(out.filter((record) => record.message.includes("below threshold"))).toHaveLength(0);
  });

  it("carries no rate partition, whatever the prompt-token denominator", () => {
    const out = records();
    log({ input_tokens: 0, output_tokens: 9, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 });
    expect(out).toHaveLength(1);
    expect(out[0].context).not.toHaveProperty("cache_hit_rate");
    expect(out[0].context).not.toHaveProperty("cache_write_rate");
    expect(out[0].context).not.toHaveProperty("uncached_input_rate");
    expect(out[0].context).not.toHaveProperty("api_request_id");
  });

  /**
   * THE ONE WARNING LEFT IS ABOUT TRANSLATION, NOT TOKENS. A usage key this
   * typed contract cannot express is a defect in the shim's own job, so the
   * shim is exactly the thing that should be shouting about it.
   */
  it("warns loudly while retaining unknown usage fields", () => {
    const out = records();
    log({ input_tokens: 1, output_tokens: 2, cache_read_input_tokens: 0, cache_creation_input_tokens: 0, future_counter: 7 });
    expect(out).toHaveLength(1);
    expect(out[0]).toMatchObject({ level: "warn", message: "completed assistant API response usage includes unmodeled fields", context: { unmodeled_usage_fields: { future_counter: 7 } } });
  });

});
