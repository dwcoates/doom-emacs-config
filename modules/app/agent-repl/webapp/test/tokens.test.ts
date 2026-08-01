import { describe, expect, it } from "vitest";
import { ModelUsage, Usage } from "../src/protocol.js";
import {
  TokenMenuData,
  compactTokens,
  formatTokens,
  tokensMenuHtml,
  tokensOverlayHtml,
  turnInputTokens,
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
      "uncached|100",
      "cache read|3",
      "cache write|20",
    ]);
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
    expect(afterTotals.slice(0, 5)).toEqual([
      "input|50",
      "uncached|40",
      "cache read|4",
      "cache write|6",
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

describe("turnInputTokens: the NEW input a turn fed the model", () => {
  it("sums the uncached input and the cache write", () => {
    // Arrange
    const u = usage({ input_tokens: 100, cache_creation_input_tokens: 20 });
    // Act + Assert
    expect(turnInputTokens(u)).toBe(120);
  });

  it("excludes the cache read, which is the standing prefix presented again", () => {
    // Arrange — a re-read prefix dwarfing everything the turn actually added.
    const u = usage({ input_tokens: 100, cache_creation_input_tokens: 20, cache_read_input_tokens: 900_000 });
    // Act + Assert
    expect(turnInputTokens(u)).toBe(120);
  });

  it("excludes the output tokens, this being an INPUT figure", () => {
    // Arrange
    const u = usage({ input_tokens: 100, cache_creation_input_tokens: 20, output_tokens: 5_000 });
    // Act + Assert
    expect(turnInputTokens(u)).toBe(120);
  });

  it("treats an absent cache-write field as no cache write", () => {
    // Arrange — the dimension is optional on the wire.
    const u: Usage = { input_tokens: 100, output_tokens: 40 };
    // Act + Assert
    expect(turnInputTokens(u)).toBe(100);
  });
});
