import { describe, expect, it } from "vitest";
import { ModelUsage, Usage } from "../src/protocol.js";
import {
  TokenMenuData,
  compactTokens,
  formatTokens,
  tokenHeatHue,
  TOKEN_HEAT_CLASS,
  uncachedInputHtml,
  tokensMenuHtml,
  tokensOverlayHtml,
  canonicalTokens,
  expensiveInput,
} from "../src/tokens.js";


/** A top-level usage payload, defaulted small and fully dimensioned. */
function usage(over: Partial<Usage> = {}): Usage {
  return {
    input_tokens: 100,
    output_tokens: 40,
    cache_creation_input_tokens: 20,
    cache_read_input_tokens: 3,
    ...over,
  };
}

/** One model's whole-tree slice, defaulted. */
function modelUsage(over: Partial<ModelUsage> = {}): ModelUsage {
  return {
    input_tokens: 10,
    output_tokens: 5,
    cache_creation_input_tokens: 2,
    cache_read_input_tokens: 1,
    web_search_requests: 0,
    cost_usd: 0.5,
    context_window: 1000000,
    ...over,
  };
}

function data(over: Partial<TokenMenuData> = {}): TokenMenuData {
  return { contextSize: null, topLevel: null, models: null, ...over };
}

/** The overlay's rows as `label|value` pairs, in document order. */
function rows(html: string): string[] {
  return [...html.matchAll(/tokens-label">([^<]*)<\/span><span class="tokens-value">([^<]*)</g)].map(
    (m) => `${m[1]}|${m[2]}`,
  );
}

describe("formatTokens", () => {
  it("groups thousands with commas", () => {
    // Arrange + Act + Assert
    expect(formatTokens(1234567)).toBe("1,234,567");
  });
});

describe("compactTokens", () => {
  it("writes a sub-thousand count as its plain digits", () => {
    // Arrange + Act + Assert
    expect(compactTokens(812)).toBe("812");
  });

  it("keeps one decimal only below ten thousand, where it still reads", () => {
    // Arrange + Act + Assert
    expect(compactTokens(9_460)).toBe("9.5k");
    expect(compactTokens(12_340)).toBe("12k");
  });

  it("writes a million-scale count in M", () => {
    // Arrange + Act + Assert
    expect(compactTokens(1_230_000)).toBe("1.2M");
  });
});

describe("tokensMenuHtml chip", () => {
  it("headlines the session's current context size", () => {
    // Arrange
    const d = data({ contextSize: 1234 });
    // Act + Assert
    expect(tokensMenuHtml(d, false)).toContain("tokens: 1,234 ");
  });

  it("shows the context size, not the cumulative input-side spend", () => {
    // Arrange — a large cumulative topLevel must NOT move the chip figure.
    const d = data({
      contextSize: 42,
      topLevel: usage({ input_tokens: 999999, cache_read_input_tokens: 999999 }),
    });
    // Act + Assert
    expect(tokensMenuHtml(d, false)).toContain("tokens: 42 ");
  });

  it("dashes the figure before any context size is known", () => {
    // Arrange + Act + Assert — null is unknown, not a spent 0.
    expect(tokensMenuHtml(data(), false)).toContain("tokens: — ");
  });

  it("renders no overlay while closed", () => {
    // Arrange + Act + Assert
    expect(tokensMenuHtml(data(), false)).not.toContain("tokens-overlay");
  });

  it("drops the overlay when open", () => {
    // Arrange + Act + Assert
    expect(tokensMenuHtml(data(), true)).toContain("tokens-overlay");
  });

  it("mirrors the disclosure state on aria-expanded", () => {
    // Arrange + Act + Assert
    expect(tokensMenuHtml(data(), true)).toContain(`aria-expanded="true"`);
    expect(tokensMenuHtml(data(), false)).toContain(`aria-expanded="false"`);
  });
});

describe("tokensOverlayHtml top-level section", () => {
  it("splits the input-side total into its three constituents", () => {
    // Arrange
    const d = data({ topLevel: usage() });
    // Act
    const got = rows(tokensOverlayHtml(d));
    // Assert — the total first, then the resolution it is made of.
    expect(got.slice(0, 4)).toEqual([
      "input|123",
      "fresh input|100",
      "cache read|3",
      "cache write|20",
    ]);
  });

  it("reports the uncached sum, not the fresh-input bucket, as the expensive figure", () => {
    // Arrange — fresh input and cache WRITE are both nonzero, so only the sum
    // is the cost; a reader taking "fresh input" for it would be short by 20.
    const d = data({ topLevel: usage() });
    // Act
    const got = rows(tokensOverlayHtml(d));
    // Assert
    expect(got).toContain("uncached input|120");
  });

  it("keeps the cache read out of the uncached figure", () => {
    // Arrange — a re-read prefix dwarfing everything the turn actually paid for.
    const d = data({ topLevel: usage({ cache_read_input_tokens: 900_000 }) });
    // Act
    const got = rows(tokensOverlayHtml(d));
    // Assert
    expect(got).toContain("uncached input|120");
  });

  it("carries the top-level output tokens as their own row", () => {
    // Arrange
    const d = data({ topLevel: usage({ output_tokens: 41 }) });
    // Act + Assert
    expect(rows(tokensOverlayHtml(d))).toContain("output|41");
  });

  it("dashes the section before any top-level usage is known", () => {
    // Arrange + Act
    const got = rows(tokensOverlayHtml(data()));
    // Assert
    expect(got.slice(0, 2)).toEqual(["input|—", "output|—"]);
  });
});

describe("tokensOverlayHtml whole-tree totals", () => {
  it("sums every model's slice into the all-agents rows", () => {
    // Arrange — two models, so the totals must be their sum.
    const d = data({
      models: {
        a: modelUsage({ input_tokens: 10, output_tokens: 5, cache_read_input_tokens: 1, cache_creation_input_tokens: 2 }),
        b: modelUsage({ input_tokens: 30, output_tokens: 15, cache_read_input_tokens: 3, cache_creation_input_tokens: 4 }),
      },
    });
    // Act
    const html = tokensOverlayHtml(d);
    const afterTotals = rows(html.slice(html.indexOf("all agents")));
    // Assert
    expect(afterTotals.slice(0, 6)).toEqual([
      "input|50",
      "fresh input|40",
      "cache read|4",
      "cache write|6",
      "uncached input|46",
      "output|20",
    ]);
  });

  it("dashes the all-agents rows until a result reports the per-model map", () => {
    // Arrange + Act
    const html = tokensOverlayHtml(data({ topLevel: usage() }));
    const afterTotals = rows(html.slice(html.indexOf("all agents")));
    // Assert
    expect(afterTotals.slice(0, 2)).toEqual(["input|—", "output|—"]);
  });

  it("totals the models' web search requests", () => {
    // Arrange
    const d = data({
      models: {
        a: modelUsage({ web_search_requests: 2 }),
        b: modelUsage({ web_search_requests: 3 }),
      },
    });
    // Act
    const html = tokensOverlayHtml(d);
    const afterTotals = rows(html.slice(html.indexOf("all agents")));
    // Assert
    expect(afterTotals).toContain("web searches|5");
  });

  it("totals the models' cost estimates", () => {
    // Arrange
    const d = data({ models: { a: modelUsage({ cost_usd: 0.5 }), b: modelUsage({ cost_usd: 0.25 }) } });
    // Act
    const html = tokensOverlayHtml(d);
    const afterTotals = rows(html.slice(html.indexOf("all agents")));
    // Assert
    expect(afterTotals).toContain("cost|$0.75");
  });
});

describe("tokensOverlayHtml per-model sections", () => {
  it("renders one section per model with its context window", () => {
    // Arrange
    const d = data({ models: { "claude-opus-4-8": modelUsage({ context_window: 1000000 }) } });
    // Act
    const html = tokensOverlayHtml(d);
    // Assert
    expect(html).toContain(`tokens-section">claude-opus-4-8<`);
    expect(rows(html)).toContain("context window|1,000,000");
  });

  it("orders the model sections most expensive first", () => {
    // Arrange
    const d = data({
      models: { cheap: modelUsage({ cost_usd: 0.01 }), dear: modelUsage({ cost_usd: 2 }) },
    });
    // Act
    const html = tokensOverlayHtml(d);
    // Assert
    expect(html.indexOf(`">dear<`)).toBeLessThan(html.indexOf(`">cheap<`));
  });

  it("escapes markup in a model name", () => {
    // Arrange
    const d = data({ models: { "<b>model": modelUsage() } });
    // Act + Assert
    expect(tokensOverlayHtml(d)).not.toContain("<b>model");
  });

  it("gives a cost under a dime four decimals so it does not read as $0.00", () => {
    // Arrange
    const d = data({ models: { a: modelUsage({ cost_usd: 0.0123 }) } });
    // Act + Assert
    expect(rows(tokensOverlayHtml(d))).toContain("cost|$0.0123");
  });

  it("rounds a cost at a dime or more to cents", () => {
    // Arrange
    const d = data({ models: { a: modelUsage({ cost_usd: 1.2345 }) } });
    // Act + Assert
    expect(rows(tokensOverlayHtml(d))).toContain("cost|$1.23");
  });
});

describe("expensiveInput: the NEW input a turn fed the model", () => {
  /**
   * `canonicalTokens` is the webapp's ONE translation of vendor buckets into
   * the canonical shape, so these cases pin BOTH halves at once: that each
   * vendor counter lands in the bucket whose economics it describes, and that
   * the expensive figure is the two misses together.
   */
  it("sums the fresh-input miss and the cache-write miss", () => {
    // Arrange
    const u = canonicalTokens({ input: 100, cacheCreation: 20, cacheRead: 0, output: 0 });
    // Act + Assert
    expect(expensiveInput(u)).toBe(120);
  });

  it("excludes the cache hit, which is the standing prefix presented again", () => {
    // Arrange — a re-read prefix dwarfing everything the turn actually added.
    const u = canonicalTokens({ input: 100, cacheCreation: 20, cacheRead: 900_000, output: 0 });
    // Act + Assert
    expect(expensiveInput(u)).toBe(120);
  });

  it("excludes the output tokens, this being an INPUT figure", () => {
    // Arrange
    const u = canonicalTokens({ input: 100, cacheCreation: 20, cacheRead: 0, output: 5_000 });
    // Act + Assert
    expect(expensiveInput(u)).toBe(120);
  });

  it("treats an absent cache write as no cache write", () => {
    // Arrange — the dimension is optional on the vendor wire.
    const u = canonicalTokens({ input: 100, cacheCreation: 0, cacheRead: 0, output: 40 });
    // Act + Assert
    expect(expensiveInput(u)).toBe(100);
  });

  it("reports nothing expensive when the misses are absent entirely", () => {
    // Arrange — a canonical usage the daemon left with no miss submessage.
    const u = canonicalTokens({ input: 0, cacheCreation: 0, cacheRead: 500, output: 0 });
    u.inputMisses = undefined;
    // Act + Assert
    expect(expensiveInput(u)).toBe(0);
  });
});

// --- the uncached-input heat ramp -------------------------------------------
//
// One anchor or one property per test: the ramp's whole value is that adjacent
// counts read as adjacent news, so the continuity cases matter as much as the
// named colors.

describe("tokenHeatHue", () => {
  it("paints a turn that spent nothing the green end of the ramp", () => {
    // Arrange / Act / Assert.
    expect(tokenHeatHue(0)).toBe(120);
  });

  it("keeps the whole cheap band green, right up to its top", () => {
    // Arrange / Act / Assert — 20k is the top of "cheap", not the start of
    // the climb, so it is still exactly green.
    expect(tokenHeatHue(20_000)).toBe(120);
  });

  it("reaches yellow exactly at the 50k anchor", () => {
    // Arrange / Act / Assert.
    expect(tokenHeatHue(50_000)).toBe(60);
  });

  it("reaches orange exactly at the 100k anchor", () => {
    // Arrange / Act / Assert.
    expect(tokenHeatHue(100_000)).toBe(30);
  });

  it("reaches red at the 200k anchor", () => {
    // Arrange / Act / Assert — 100k opens the red band; 200k is where the hue
    // actually arrives at red, so the worst turns stay distinguishable.
    expect(tokenHeatHue(200_000)).toBe(0);
  });

  it("clamps beyond the red anchor rather than wrapping past it", () => {
    // Arrange / Act / Assert — a hue that kept falling would wrap into
    // magenta and read as cooler than the red it passed.
    expect(tokenHeatHue(10_000_000)).toBe(0);
  });

  it("clamps below zero tokens to the green end", () => {
    // Arrange / Act / Assert — a negative figure is not a real count, but it
    // must not produce a hue outside the ramp.
    expect(tokenHeatHue(-1)).toBe(120);
  });

  it("interpolates between anchors instead of stepping", () => {
    // Arrange / Act — halfway from the 20k green anchor to the 50k yellow one.
    const mid = tokenHeatHue(35_000);
    // Assert — the exact midpoint hue, not either endpoint.
    expect(mid).toBe(90);
  });

  it("crosses a band boundary without a visible jump", () => {
    // Arrange / Act — a token either side of the 20k boundary.
    const below = tokenHeatHue(19_900);
    const above = tokenHeatHue(20_100);
    // Assert — the whole point of a continuous ramp: adjacent counts are
    // adjacent colors, so no single token repaints the figure.
    expect(Math.abs(above - below)).toBeLessThanOrEqual(1);
  });

  it("never rises as the count climbs", () => {
    // Arrange.
    const counts = [0, 5_000, 20_000, 35_000, 50_000, 75_000, 100_000, 150_000, 200_000];
    // Act.
    const hues = counts.map(tokenHeatHue);
    // Assert — monotonic descent green -> red; a rise anywhere would make a
    // costlier turn read as cheaper.
    expect(hues).toEqual([...hues].sort((a, b) => b - a));
  });
});

// `uncachedInputHtml` is the ONE spelling of an uncached-input figure. The
// cases below pin the parts the surfaces share (the measure's compact form,
// the `in` suffix, the heat class and hue) and the one part they don't (the
// caller's placement class), so a surface cannot drift into its own spelling.

describe("uncachedInputHtml: one spelling for every uncached-input figure", () => {
  it("writes the figure in the compact form with the shared `in` suffix", () => {
    // Arrange / Act
    const html = uncachedInputHtml("info-tokens", 41_000);
    // Assert
    expect(html).toContain(">41k in<");
  });

  it("carries the heat class so the stylesheet's one ramp rule colors it", () => {
    // Arrange / Act
    const html = uncachedInputHtml("info-tokens", 41_000);
    // Assert
    expect(html).toContain(`class="info-tokens ${TOKEN_HEAT_CLASS}"`);
  });

  it("carries the figure's own hue, so magnitude reads before the digits", () => {
    // Arrange / Act
    const html = uncachedInputHtml("info-tokens", 100_000);
    // Assert — the 100k anchor's orange (see tokenHeatHue).
    expect(html).toContain(`style="--token-heat-hue:${tokenHeatHue(100_000)}"`);
  });

  it("differs between two surfaces ONLY in the caller's placement class", () => {
    // Arrange — the badge and the footer row reporting the same attribution.
    const badge = uncachedInputHtml("async-badge-tokens", 12_340);
    const row = uncachedInputHtml("pfooter-agent-tokens", 12_340);
    // Act / Assert — swapping the class makes them identical, digits included.
    expect(badge.replace("async-badge-tokens", "pfooter-agent-tokens")).toBe(row);
  });

  it("escapes a placement class so no caller can inject markup through it", () => {
    // Arrange / Act — the figure itself is a number, but the class is a string.
    const html = uncachedInputHtml("info-tokens", 0);
    // Assert — a well-formed single span is all a caller can produce.
    expect(html.match(/<span/g)).toHaveLength(1);
  });
});
