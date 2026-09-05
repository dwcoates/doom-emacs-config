import { describe, expect, it } from "vitest";
import { InvalidModeledUsageError, normalizeApiUsage } from "../src/api-usage.js";

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

describe("normalizeApiUsage refuses a shape it cannot carry faithfully", () => {
  it("rejects a non-object usage block, naming the field path", () => {
    // Arrange + Act
    let thrown: unknown;
    try {
      normalizeApiUsage("not an object");
    } catch (err) {
      thrown = err;
    }

    // Assert
    expect(thrown).toBeInstanceOf(InvalidModeledUsageError);
    expect((thrown as InvalidModeledUsageError).fieldPath).toBe("usage");
    expect((thrown as InvalidModeledUsageError).message).toContain("must be an object");
  });

  it("rejects a required counter that is missing", () => {
    // Arrange + Act
    let thrown: unknown;
    try {
      normalizeApiUsage({ output_tokens: 20 });
    } catch (err) {
      thrown = err;
    }

    // Assert
    expect(thrown).toBeInstanceOf(InvalidModeledUsageError);
    expect((thrown as InvalidModeledUsageError).fieldPath).toBe("usage.input_tokens");
  });
});


/**
 * The JSON-representability gate. `rawSdkUsage` is carried on a protobuf
 * Struct, so anything JSON cannot express faithfully has to be refused at the
 * boundary rather than silently coerced into a value the daemon would then
 * report as the vendor's own.
 */
describe("normalizeApiUsage refuses values a protobuf Struct cannot carry", () => {
  /** A minimal legal counter set, so only the value under test is at fault. */
  function withCounters(extra: Record<string, unknown>): Record<string, unknown> {
    return { input_tokens: 1, output_tokens: 2, ...extra };
  }

  it("rejects a circular object reference", () => {
    // Arrange
    const raw = withCounters({});
    raw["loop"] = raw;

    // Act + Assert
    expect(() => normalizeApiUsage(raw)).toThrow(/"usage\.loop" contains a circular reference/);
  });

  it("rejects a circular array reference", () => {
    // Arrange
    const cycle: unknown[] = [];
    cycle.push(cycle);

    // Act + Assert
    expect(() => normalizeApiUsage(withCounters({ trail: cycle }))).toThrow(
      /"usage\.trail\[0\]" contains a circular reference/,
    );
  });

  it("rejects symbol-keyed data", () => {
    // Arrange
    const nested: Record<string | symbol, unknown> = {};
    nested[Symbol("hidden")] = 1;

    // Act + Assert
    expect(() => normalizeApiUsage(withCounters({ detail: nested }))).toThrow(
      /"usage\.detail" contains symbol-keyed data/,
    );
  });

  it("rejects a non-finite number", () => {
    // Arrange + Act + Assert
    expect(() => normalizeApiUsage(withCounters({ ratio: Number.POSITIVE_INFINITY }))).toThrow(
      /"usage\.ratio" must be a finite JSON number/,
    );
  });

  it("rejects an array hole, which JSON cannot preserve", () => {
    // Arrange: a sparse array — index 0 is absent, not undefined.
    // The hole is the fixture: this test exists to prove the normalizer refuses one, and
    // there is no other way to write one.
    // eslint-disable-next-line no-sparse-arrays -- see above
    const sparse = [, 1] as unknown[];

    // Act + Assert
    expect(() => normalizeApiUsage(withCounters({ trail: sparse }))).toThrow(
      /"usage\.trail\[0\]" is an array hole/,
    );
  });

  it("rejects a value whose type JSON has no representation for", () => {
    // Arrange + Act + Assert
    expect(() => normalizeApiUsage(withCounters({ callback: (): void => {} }))).toThrow(
      /"usage\.callback" has non-JSON type function/,
    );
  });
});

describe("normalizeApiUsage refuses an ambiguous or malformed field", () => {
  it("rejects a field stated under both its snake_case and camelCase alias", () => {
    // Arrange + Act + Assert — two spellings of one fact can disagree.
    expect(() => normalizeApiUsage({ input_tokens: 1, inputTokens: 2, output_tokens: 3 })).toThrow(
      /"usage\.input_tokens" has conflicting aliases input_tokens, inputTokens/,
    );
  });

  it("rejects a counter that is not a non-negative safe integer", () => {
    // Arrange + Act + Assert
    expect(() => normalizeApiUsage({ input_tokens: -1, output_tokens: 2 })).toThrow(
      /"usage\.input_tokens" must be a non-negative safe integer/,
    );
  });

  it("rejects a nullable counter that is missing entirely", () => {
    // Arrange + Act + Assert
    expect(() => normalizeApiUsage({ input_tokens: 1, output_tokens: 2 })).toThrow(
      /"usage\.cache_read_input_tokens" is required/,
    );
  });

  it("rejects a modeled object field that is neither an object nor null", () => {
    // Arrange + Act + Assert
    expect(() =>
      normalizeApiUsage({
        input_tokens: 1,
        output_tokens: 2,
        cache_read_input_tokens: 0,
        cache_creation_input_tokens: 0,
        cache_creation: 7,
      }),
    ).toThrow(/"usage\.cache_creation" must be an object or null/);
  });

  it("rejects a modeled string field that is neither a string nor null", () => {
    // Arrange + Act + Assert
    expect(() =>
      normalizeApiUsage({
        input_tokens: 1,
        output_tokens: 2,
        cache_read_input_tokens: 0,
        cache_creation_input_tokens: 0,
        service_tier: 7,
      }),
    ).toThrow(/"usage\.service_tier" must be a string or null/);
  });

  it("rejects an iterations field that is neither an array nor null", () => {
    // Arrange + Act + Assert
    expect(() =>
      normalizeApiUsage({
        input_tokens: 1,
        output_tokens: 2,
        cache_read_input_tokens: 0,
        cache_creation_input_tokens: 0,
        iterations: { first: 1 },
      }),
    ).toThrow(/"usage\.iterations" must be an array or null/);
  });
});

describe("normalizeApiUsage normalizes the shapes it can carry", () => {
  /** Every required counter, so a test states only the field it is about. */
  const required = {
    input_tokens: 1,
    output_tokens: 2,
    cache_read_input_tokens: 0,
    cache_creation_input_tokens: 0,
  };

  it("reads an explicit null nullable counter as zero", () => {
    // Arrange + Act
    const usage = normalizeApiUsage({ ...required, cache_read_input_tokens: null });

    // Assert
    expect(usage.cacheReadInputTokens).toBe(0);
  });

  it("accepts the camelCase alias when it is the only spelling present", () => {
    // Arrange + Act
    const usage = normalizeApiUsage({
      inputTokens: 11,
      outputTokens: 22,
      cacheReadInputTokens: 33,
      cacheCreationInputTokens: 44,
    });

    // Assert
    expect(usage.inputTokens).toBe(11);
  });

  it("collects an unmodeled vendor field into unmodeledUsage", () => {
    // Arrange + Act — a field this build has never heard of must survive.
    const usage = normalizeApiUsage({ ...required, brand_new_counter: 5 });

    // Assert
    expect(usage.unmodeledUsage).toEqual({ brand_new_counter: 5 });
  });

  it("omits unmodeledUsage entirely when the vendor stated only modeled fields", () => {
    // Arrange + Act
    const usage = normalizeApiUsage(required);

    // Assert — presence, never an empty sentinel object.
    expect(usage.unmodeledUsage).toBeUndefined();
  });

  it("lifts the nested ephemeral cache-creation counters out of cache_creation", () => {
    // Arrange + Act
    const usage = normalizeApiUsage({
      ...required,
      cache_creation: { ephemeral_5m_input_tokens: 8, ephemeral_1h_input_tokens: 9 },
    });

    // Assert
    expect(usage.cacheCreation5mInputTokens).toBe(8);
    expect(usage.cacheCreation1hInputTokens).toBe(9);
  });

  it("reports zero for an ephemeral counter cache_creation does not declare", () => {
    // Arrange + Act
    const usage = normalizeApiUsage({ ...required, cache_creation: {} });

    // Assert
    expect(usage.cacheCreation5mInputTokens).toBe(0);
  });

  it("reads an explicit null modeled object as absence", () => {
    // Arrange + Act
    const usage = normalizeApiUsage({ ...required, fallback_credit: null });

    // Assert
    expect(usage.fallbackCredit).toBeUndefined();
  });

  it("reads an explicit null modeled string as the empty string", () => {
    // Arrange + Act
    const usage = normalizeApiUsage({ ...required, service_tier: null });

    // Assert
    expect(usage.serviceTier).toBe("");
  });

  it("reads an explicit null iterations as absence", () => {
    // Arrange + Act
    const usage = normalizeApiUsage({ ...required, iterations: null });

    // Assert
    expect(usage.iterations).toBeUndefined();
  });
});
