/**
 * The thinking tokens out of the vendor's output-token details, read once for
 * every site that carries a usage: the stream plane's assistant messages and a
 * subagent's tool-result totals share this one shape.
 */

import { bindLog } from "../log.js";

const LOGGER = bindLog("shim.convert.thinking-tokens", { component: "shim-convert-usage", operation: "shim.convert.usage" });

/** A non-negative whole token count, or undefined when the value is not one. */
function tokenCount(value: unknown): bigint | undefined {
  return typeof value === "number" && Number.isFinite(value) && value >= 0
    ? BigInt(Math.trunc(value))
    : undefined;
}

/**
 * The thinking tokens out of the vendor's output-token details.
 *
 * THE VENDOR WRITES `thinking_tokens` (a real record on 2026-11-26:
 * `output_tokens_details: { thinking_tokens: 209 }`); `reasoning_tokens` is the
 * name the earlier harvest modeled and is still read. Both present and
 * differing is a vendor contradiction: it is warned and `thinking_tokens`, the
 * vendor's current name, is taken. No details at all means no thinking: 0.
 */
export function thinkingTokens(details: Record<string, unknown> | undefined): bigint {
  if (details === undefined) return 0n;
  const thinking = tokenCount(details.thinking_tokens);
  const reasoning = tokenCount(details.reasoning_tokens);
  if (thinking !== undefined && reasoning !== undefined && thinking !== reasoning) {
    // warn: a defect because the vendor contradicted itself about one response's thinking tokens.
    LOGGER.warn(
      { thinking_tokens: Number(thinking), reasoning_tokens: Number(reasoning) },
      "the vendor's output-token details name two different thinking counts; taking thinking_tokens",
    );
  }
  return thinking ?? reasoning ?? 0n;
}
