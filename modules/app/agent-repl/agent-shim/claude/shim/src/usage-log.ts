/**
 * Raw accounting logs for completed Anthropic Messages API responses.
 *
 * THE VENDOR'S BUCKETS, AND NOTHING DERIVED FROM THEM. This log used to carry
 * an expensive-input sum and a cache-rate partition, and to raise its own
 * low-reuse warning. All three were token JUDGMENT taken outside the daemon.
 * The daemon owns every such figure now (its internal/tokenusage, over the
 * canonical agentshim.frontend.v1.TokenUsage), and it raises the expensive-turn
 * alert and the cold-ping alarm from it. What is kept here is the raw fidelity:
 * every counter the vendor reported, verbatim, so the evidence behind the
 * daemon's verdict is on disk beside it.
 */
import { bindLog, type LogFields } from "./uds/log.js";
import { type NormalizedApiUsage } from "./api-usage.js";

const LOGGER = bindLog({ component: "claude-shim-usage", operation: "shim.usage.assistant_api_response" });
function object(value: unknown): Record<string, unknown> | undefined {
  return typeof value === "object" && value !== null && !Array.isArray(value) ? value as Record<string, unknown> : undefined;
}
function string(value: unknown): string { return typeof value === "string" ? value : ""; }
function optionalString(value: unknown): string | undefined { return typeof value === "string" && value.length !== 0 ? value : undefined; }

/** Emit dense usage accounting and a self-contained warning for low cache reuse. */
export function logAssistantApiResponseUsage(
  sdkMessage: Record<string, unknown>,
  usage: NormalizedApiUsage,
  agentReplSessionId: string,
): void {
  const message = object(sdkMessage["message"]);
  if (message === undefined) throw new Error("validated assistant usage has no owning message object");
  const inputTokens = usage.inputTokens;
  const outputTokens = usage.outputTokens;
  const cacheReadInputTokens = usage.cacheReadInputTokens;
  const cacheCreationInputTokens = usage.cacheCreationInputTokens;
  const fields: LogFields = {
    agent_repl_session_id: agentReplSessionId, claude_session_id: string(sdkMessage["session_id"]), api_message_id: string(message["id"]), api_request_id: optionalString(sdkMessage["request_id"]), model: string(message["model"]),
    input_tokens: inputTokens, output_tokens: outputTokens, cache_read_input_tokens: cacheReadInputTokens, cache_creation_input_tokens: cacheCreationInputTokens,
    cache_creation_5m_input_tokens: usage.cacheCreation5mInputTokens, cache_creation_1h_input_tokens: usage.cacheCreation1hInputTokens,
    service_tier: usage.serviceTier, speed: usage.speed, inference_geo: usage.inferenceGeo,
    cache_creation: usage.cacheCreation, server_tool_use: usage.serverToolUse, iterations: usage.iterations, output_tokens_details: usage.outputTokensDetails, cache_diagnostic: usage.cacheDiagnostic, fallback_credit: usage.fallbackCredit, unmodeled_usage: usage.unmodeledUsage, unmodeled_usage_fields: usage.unknownUsageFields,
  };
  const hasUnmodeledUsage = Object.keys(usage.unknownUsageFields).length !== 0;
  // THE ONE WARNING LEFT IS NOT ABOUT TOKENS. An unmodeled usage field means
  // the vendor sent something this typed contract cannot express, which is a
  // TRANSLATION defect and therefore the shim's own business.
  LOGGER.log({ ...fields, ...(hasUnmodeledUsage ? { level: "warn" } : {}) }, hasUnmodeledUsage ? "completed assistant API response usage includes unmodeled fields" : "completed assistant API response usage");
}
