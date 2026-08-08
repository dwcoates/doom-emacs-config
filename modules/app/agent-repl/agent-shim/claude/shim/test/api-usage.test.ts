import { describe, expect, it } from "vitest";
import { normalizeApiUsage } from "../src/api-usage.js";

/**
 * THE SHIM DERIVES NOTHING FROM THE COUNTERS IT VALIDATES.
 *
 * It used to export an expensive-input sum and a cache-rate partition, and both
 * were token JUDGMENT taken outside the daemon — a second owner of a cost
 * measure is a second thing that can disagree with the first about what one
 * turn spent. Those contracts did not disappear; they moved to their one owner,
 * the daemon's `internal/tokenusage` over the canonical
 * `agentshim.frontend.v1.TokenUsage` shape, and are proved by its tests (the
 * disjoint-sum cases, the cache-read exclusion, the threshold that only the sum
 * crosses, and the partition that sums to one).
 *
 * What is pinned HERE is the boundary itself: a normalized usage block carries
 * the vendor's counters and no figure computed from them, so the derivation
 * cannot quietly grow back on this side of the wire.
 */
describe("normalizeApiUsage carries vendor counters and derives nothing", () => {
  const usage = normalizeApiUsage({
    input_tokens: 10,
    output_tokens: 20,
    cache_creation_input_tokens: 40,
    cache_read_input_tokens: 50,
  });

  it("reports every vendor counter verbatim", () => {
    // Arrange + Act + Assert
    expect(usage.inputTokens).toBe(10);
    expect(usage.outputTokens).toBe(20);
    expect(usage.cacheCreationInputTokens).toBe(40);
    expect(usage.cacheReadInputTokens).toBe(50);
  });

  it("exposes no expensive-input sum", () => {
    // Arrange + Act + Assert — 10 + 40 is the daemon's answer to compute.
    expect(Object.keys(usage)).not.toContain("uncachedInputTokens");
  });

  it("exposes no cache-rate partition", () => {
    // Arrange + Act + Assert
    expect(Object.keys(usage)).not.toContain("promptCache");
  });

  it("exposes no total prompt input", () => {
    // Arrange + Act + Assert — summing the three buckets is a derivation too.
    expect(Object.keys(usage)).not.toContain("totalPromptInputTokens");
  });
});

