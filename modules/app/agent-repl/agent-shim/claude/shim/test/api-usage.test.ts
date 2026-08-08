import { describe, expect, it } from "vitest";
import { normalizeApiUsage, uncachedInputTokens } from "../src/api-usage.js";

/**
 * The whole value of this helper is that it is a SUM. The three SDK input
 * buckets are disjoint, only `cache_read_input_tokens` is cheap, and the field
 * NAMES invite reading `input_tokens` alone as "the uncached one" — which is
 * near zero on exactly the cold re-ingests that cost the most. One case per
 * way that reading fails.
 */
describe("uncachedInputTokens", () => {
  const cases: ReadonlyArray<{
    name: string;
    usage: { inputTokens: number; cacheCreationInputTokens: number };
    want: number;
  }> = [
    {
      name: "adds both expensive buckets when both are nonzero",
      usage: { inputTokens: 1_200, cacheCreationInputTokens: 48_000 },
      want: 49_200,
    },
    {
      name: "counts a cold re-ingest that landed entirely in cache creation",
      usage: { inputTokens: 12, cacheCreationInputTokens: 500_000 },
      want: 500_012,
    },
    {
      name: "counts fresh input that was never written to the cache",
      usage: { inputTokens: 700, cacheCreationInputTokens: 0 },
      want: 700,
    },
    {
      name: "a turn that paid for nothing new",
      usage: { inputTokens: 0, cacheCreationInputTokens: 0 },
      want: 0,
    },
  ];

  for (const testCase of cases) {
    it(testCase.name, () => {
      // Arrange + Act
      const got = uncachedInputTokens(testCase.usage);
      // Assert
      expect(got).toBe(testCase.want);
    });
  }

  it("never counts the cache read, however large it is", () => {
    // Arrange — a 900k standing prefix re-presented, which cost ~a tenth.
    const usage = normalizeApiUsage({
      input_tokens: 100,
      output_tokens: 40,
      cache_creation_input_tokens: 20,
      cache_read_input_tokens: 900_000,
    });
    // Act
    const got = uncachedInputTokens(usage);
    // Assert
    expect(got).toBe(120);
  });

  it("crosses a cost threshold only on the sum, never on either bucket alone", () => {
    // Arrange — neither bucket reaches 20k; together they pass it.
    const usage = normalizeApiUsage({
      input_tokens: 12_000,
      output_tokens: 5,
      cache_creation_input_tokens: 12_000,
      cache_read_input_tokens: 0,
    });
    // Act
    const got = uncachedInputTokens(usage);
    // Assert
    expect(usage.inputTokens).toBeLessThan(20_000);
    expect(usage.cacheCreationInputTokens).toBeLessThan(20_000);
    expect(got).toBeGreaterThan(20_000);
  });

  it("reads the sum off a normalized usage block without restating the arithmetic", () => {
    // Arrange
    const usage = normalizeApiUsage({
      input_tokens: 10,
      output_tokens: 20,
      cache_creation_input_tokens: 40,
      cache_read_input_tokens: 50,
    });
    // Act + Assert
    expect(uncachedInputTokens(usage)).toBe(50);
  });
});

/**
 * The three rates PARTITION the prompt input. `uncachedInputRate` is the
 * `input_tokens` bucket's share alone, so it is NOT the expensive share — that
 * is this rate plus the cache-write rate. The test pins the partition so a
 * later "fix" cannot fold cache creation into the uncached rate and silently
 * double-count it against the write rate.
 */
describe("normalizeApiUsage prompt-cache rates", () => {
  it("keeps the three bucket rates disjoint and summing to one", () => {
    // Arrange
    const usage = normalizeApiUsage({
      input_tokens: 10,
      output_tokens: 20,
      cache_creation_input_tokens: 40,
      cache_read_input_tokens: 50,
    });
    // Act
    const rates = usage.promptCache;
    // Assert
    if (rates.case !== "rates") throw new Error("expected prompt-cache rates");
    expect(rates.uncachedInputRate).toBe(0.1);
    expect(rates.cacheWriteRate).toBe(0.4);
    expect(rates.cacheHitRate).toBe(0.5);
    expect(rates.uncachedInputRate + rates.cacheWriteRate + rates.cacheHitRate).toBe(1);
  });

  it("leaves the expensive share as the sum of the two costed rates", () => {
    // Arrange — the fresh-input rate alone understates the cost by the writes.
    const usage = normalizeApiUsage({
      input_tokens: 10,
      output_tokens: 20,
      cache_creation_input_tokens: 40,
      cache_read_input_tokens: 50,
    });
    // Act
    const rates = usage.promptCache;
    // Assert
    if (rates.case !== "rates") throw new Error("expected prompt-cache rates");
    expect(rates.uncachedInputRate + rates.cacheWriteRate).toBeCloseTo(
      uncachedInputTokens(usage) / rates.totalPromptInputTokens,
      12,
    );
  });
});
