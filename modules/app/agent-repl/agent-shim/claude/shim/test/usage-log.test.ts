import { afterEach, describe, expect, it, vi } from "vitest";
import { logAssistantApiResponseUsage } from "../src/usage-log.js";

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

describe("logAssistantApiResponseUsage", () => {
  it("logs complete response accounting with nested vendor details", () => {
    const out = records();
    logAssistantApiResponseUsage(assistant({
      input_tokens: 10, output_tokens: 20, cache_read_input_tokens: 50, cache_creation_input_tokens: 40,
      cache_creation: { ephemeral_5m_input_tokens: 30, ephemeral_1h_input_tokens: 10 },
      server_tool_use: { web_search_requests: 2 }, iterations: [{ input_tokens: 4 }], output_tokens_details: { reasoning_tokens: 3 },
      cache_diagnostic: { status: "verified" }, fallback_credit: { status: { type: "redeemed" } },
      service_tier: "standard", speed: "standard", inference_geo: "us-east-1",
    }, "req_123"), "agent-session");
    expect(out).toHaveLength(2);
    expect(out[0]).toMatchObject({ agent_repl_session_id: "test-agent-session", claude_session_id: "claude-session", message: "completed assistant API response usage", context: {
      api_message_id: "msg_123", api_request_id: "req_123", model: "claude-fable-5",
      input_tokens: 10, output_tokens: 20, cache_read_input_tokens: 50, cache_creation_input_tokens: 40,
      cache_creation_5m_input_tokens: 30, cache_creation_1h_input_tokens: 10, total_prompt_input_tokens: 100,
      cache_hit_rate: 0.5, cache_write_rate: 0.4, uncached_input_rate: 0.1, service_tier: "standard", speed: "standard", inference_geo: "us-east-1",
      server_tool_use: { web_search_requests: 2 }, iterations: [{ input_tokens: 4 }], output_tokens_details: { reasoning_tokens: 3 },
      cache_diagnostic: { status: "verified" }, fallback_credit: { status: { type: "redeemed" } }, unmodeled_usage_fields: {},
    } });
    expect(out[1]).toMatchObject({ level: "warn", context: {
      api_request_id: "req_123", output_tokens_details: { reasoning_tokens: 3 },
      cache_diagnostic: { status: "verified" }, fallback_credit: { status: { type: "redeemed" } },
    } });
  });

  it("warns below but not at the eighty percent cache-hit boundary", () => {
    const out = records();
    logAssistantApiResponseUsage(assistant({ input_tokens: 21, output_tokens: 1, cache_read_input_tokens: 79 }), "agent-session");
    logAssistantApiResponseUsage(assistant({ input_tokens: 20, output_tokens: 1, cache_read_input_tokens: 80 }), "agent-session");
    expect(out.filter((record) => record.message.includes("below threshold"))).toHaveLength(1);
    expect(out[1].context).toMatchObject({ input_tokens: 21, cache_read_input_tokens: 79, total_prompt_input_tokens: 100, cache_hit_rate: 0.79 });
    expect(out[2].context).toMatchObject({ input_tokens: 20, cache_read_input_tokens: 80, total_prompt_input_tokens: 100, cache_hit_rate: 0.8 });
  });

  it("keeps rates absent when the prompt-token denominator is zero", () => {
    const out = records();
    logAssistantApiResponseUsage(assistant({ input_tokens: 0, output_tokens: 9, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 }), "agent-session");
    expect(out).toHaveLength(1);
    expect(out[0].context).toMatchObject({ total_prompt_input_tokens: 0 });
    expect(out[0].context).not.toHaveProperty("cache_hit_rate");
    expect(out[0].context).not.toHaveProperty("cache_write_rate");
    expect(out[0].context).not.toHaveProperty("uncached_input_rate");
    expect(out[0].context).not.toHaveProperty("api_request_id");
  });

  it("warns loudly while retaining unknown usage fields", () => {
    const out = records();
    logAssistantApiResponseUsage(assistant({ input_tokens: 1, output_tokens: 2, future_counter: 7 }), "agent-session");
    expect(out).toHaveLength(2);
    expect(out[0]).toMatchObject({ level: "warn", message: "completed assistant API response usage includes unmodeled fields", context: { unmodeled_usage_fields: { future_counter: 7 } } });
    expect(out[1]).toMatchObject({ level: "warn", context: { unmodeled_usage_fields: { future_counter: 7 } } });
  });

  it("logs a canonical error for an unreadable usage block", () => {
    const out = records();
    logAssistantApiResponseUsage(assistant(undefined), "agent-session");
    expect(out).toHaveLength(1);
    expect(out[0]).toMatchObject({ level: "error", agent_repl_session_id: "test-agent-session", message: "completed assistant API response has no readable usage block", context: { api_message_id: "msg_123", usage_present: false } });
  });
});
