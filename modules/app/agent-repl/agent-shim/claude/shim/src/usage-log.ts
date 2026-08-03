/** Canonical accounting logs for completed Anthropic Messages API responses. */
import { bindLog, type LogFields } from "./uds/log.js";

const LOGGER = bindLog({ component: "claude-shim-usage", operation: "shim.usage.assistant_api_response" });
const CACHE_HIT_RATE_WARNING_THRESHOLD = 0.80;
// The complete `BetaUsage` key set exposed through `SDKAssistantMessage.message`.
const SDK_USAGE_FIELDS = [
  "input_tokens", "output_tokens", "cache_read_input_tokens", "cache_creation_input_tokens",
  "cache_creation", "server_tool_use", "service_tier", "speed", "inference_geo", "iterations",
  "output_tokens_details", "fallback_credit",
] as const;
// Agent-harness usage extensions represented by the vendor-agnostic frontend contract.
const USAGE_EXTENSION_FIELDS = ["cache_diagnostic"] as const;
const KNOWN_USAGE_FIELDS = new Set<string>([...SDK_USAGE_FIELDS, ...USAGE_EXTENSION_FIELDS]);

function object(value: unknown): Record<string, unknown> | undefined {
  return typeof value === "object" && value !== null && !Array.isArray(value) ? value as Record<string, unknown> : undefined;
}
function number(value: unknown): number | undefined { return typeof value === "number" && Number.isFinite(value) ? value : undefined; }
function counter(value: unknown): number { return number(value) ?? 0; }
function string(value: unknown): string { return typeof value === "string" ? value : ""; }
function optionalString(value: unknown): string | undefined { return typeof value === "string" && value.length !== 0 ? value : undefined; }

/** Emit dense usage accounting and a self-contained warning for low cache reuse. */
export function logAssistantApiResponseUsage(sdkMessage: Record<string, unknown>, agentReplSessionId: string): void {
  const message = object(sdkMessage["message"]);
  const usage = message === undefined ? undefined : object(message["usage"]);
  if (message === undefined || usage === undefined) {
    const apiMessageId = message === undefined ? "" : string(message["id"]);
    const model = message === undefined ? "" : string(message["model"]);
    LOGGER.log({ level: "error", agent_repl_session_id: agentReplSessionId, claude_session_id: string(sdkMessage["session_id"]), api_message_id: apiMessageId, model, sdk_type: string(sdkMessage["type"]), usage_present: false }, "completed assistant API response has no readable usage block");
    return;
  }
  const inputTokens = counter(usage["input_tokens"]);
  const outputTokens = counter(usage["output_tokens"]);
  const cacheReadInputTokens = counter(usage["cache_read_input_tokens"]);
  const cacheCreationInputTokens = counter(usage["cache_creation_input_tokens"]);
  const cacheCreation = object(usage["cache_creation"]);
  const cacheCreation5mInputTokens = counter(cacheCreation?.["ephemeral_5m_input_tokens"]);
  const cacheCreation1hInputTokens = counter(cacheCreation?.["ephemeral_1h_input_tokens"]);
  const totalPromptInputTokens = inputTokens + cacheReadInputTokens + cacheCreationInputTokens;
  const cacheHitRate = totalPromptInputTokens === 0 ? undefined : cacheReadInputTokens / totalPromptInputTokens;
  const cacheWriteRate = totalPromptInputTokens === 0 ? undefined : cacheCreationInputTokens / totalPromptInputTokens;
  const uncachedInputRate = totalPromptInputTokens === 0 ? undefined : inputTokens / totalPromptInputTokens;
  const unmodeledUsageFields = Object.fromEntries(Object.entries(usage).filter(([field]) => !KNOWN_USAGE_FIELDS.has(field)));
  const fields: LogFields = {
    agent_repl_session_id: agentReplSessionId, claude_session_id: string(sdkMessage["session_id"]), api_message_id: string(message["id"]), api_request_id: optionalString(sdkMessage["request_id"]), model: string(message["model"]),
    input_tokens: inputTokens, output_tokens: outputTokens, cache_read_input_tokens: cacheReadInputTokens, cache_creation_input_tokens: cacheCreationInputTokens,
    cache_creation_5m_input_tokens: cacheCreation5mInputTokens, cache_creation_1h_input_tokens: cacheCreation1hInputTokens, total_prompt_input_tokens: totalPromptInputTokens,
    cache_hit_rate: cacheHitRate, cache_write_rate: cacheWriteRate, uncached_input_rate: uncachedInputRate,
    service_tier: string(usage["service_tier"]), speed: string(usage["speed"]), inference_geo: string(usage["inference_geo"]),
    cache_creation: cacheCreation, server_tool_use: usage["server_tool_use"], iterations: usage["iterations"], output_tokens_details: usage["output_tokens_details"], cache_diagnostic: usage["cache_diagnostic"], fallback_credit: usage["fallback_credit"], unmodeled_usage_fields: unmodeledUsageFields,
  };
  const hasUnmodeledUsage = Object.keys(unmodeledUsageFields).length !== 0;
  LOGGER.log({ ...fields, ...(hasUnmodeledUsage ? { level: "warn" } : {}) }, hasUnmodeledUsage ? "completed assistant API response usage includes unmodeled fields" : "completed assistant API response usage");
  if (cacheHitRate !== undefined && cacheHitRate < CACHE_HIT_RATE_WARNING_THRESHOLD) {
    LOGGER.log({ ...fields, level: "warn", cache_hit_rate_threshold: CACHE_HIT_RATE_WARNING_THRESHOLD }, "completed assistant API response cache hit rate below threshold");
  }
}
