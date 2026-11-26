/**
 * The thinking tokens out of the vendor's output-token details, read once for
 * every site that carries a usage: the stream plane's assistant messages and a
 * subagent's tool-result totals share this one shape.
 */

/** A non-negative whole token count, or undefined when the value is not one. */
function tokenCount(value: unknown): bigint | undefined {
  return typeof value === "number" && Number.isFinite(value) && value >= 0
    ? BigInt(Math.trunc(value))
    : undefined;
}

/**
 * The thinking tokens out of the vendor's output-token details.
 *
 * Each plane writes its own name: the SDK's stream writes
 * `output_tokens_details.reasoning_tokens`, and the vendor's transcript on
 * disk writes `output_tokens_details.thinking_tokens`. No producer writes both
 * in one usage block, so `thinking_tokens` is read when present, else
 * `reasoning_tokens`. No details at all means no thinking: 0.
 */
export function thinkingTokens(details: Record<string, unknown> | undefined): bigint {
  if (details === undefined) return 0n;
  return tokenCount(details.thinking_tokens) ?? tokenCount(details.reasoning_tokens) ?? 0n;
}
