import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { ModelUsage, TokenTimingTotals, Usage } from "../src/protocol.js";
import {
  TokenMenuData,
  compactTokens,
  averageTimeToFirstTokenMs,
  formatTokens,
  generationTokensPerSecond,
  timingRows,
  tokenHeatHue,
  tokensMenuHtml,
  tokensOverlayHtml,
  turnInputTokens,
} from "../src/tokens.js";
import { generatedSessionUtilization, generatedUngroupedResponse, ungroupedResponse } from "./token-utilization-fixture.js";
import {
  AgentTokenUtilizationSchema,
  ModelTokenUtilizationSchema,
  SessionTokenUtilizationSchema,
  TokenCacheCreationSchema,
  TokenCacheRatesSchema,
  TokenOutputDetailsSchema,
  TokenServerToolUseSchema,
  TokenTimingTotalsSchema,
  TokenUsageTotalsSchema,
  TokenUtilizationSubagentSchema,
} from "../../proto/gen/ts/agentshim/frontend/v1/frontend_pb";

function timing(over: Partial<TokenTimingTotals> = {}): TokenTimingTotals {
  return {
    output_tokens_with_generation_duration: 400,
    output_generation_duration_ms: 200,
    responses_with_generation_duration: 2,
    responses_without_generation_duration: 1,
    total_time_to_first_token_ms: 180,
    responses_with_time_to_first_token: 2,
    responses_without_time_to_first_token: 1,
    ...over,
  };
}

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

describe("timing derivations", () => {
  it("uses only timed output for generation throughput", () => {
    expect(generationTokensPerSecond(timing())).toBe(2000);
  });

  it("does not invent rates when timing is absent or has a zero denominator", () => {
    expect(generationTokensPerSecond(undefined)).toBeNull();
    expect(generationTokensPerSecond(timing({ output_generation_duration_ms: 0 }))).toBeNull();
    expect(averageTimeToFirstTokenMs(timing({ responses_with_time_to_first_token: 0 }))).toBeNull();
    expect(timingRows(undefined)).toEqual([["generation", "unavailable"], ["average TTFT", "unavailable"]]);
  });

  it("averages TTFT over only responses that reported it", () => {
    expect(averageTimeToFirstTokenMs(timing())).toBe(90);
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

describe("tokensOverlayHtml ungrouped subagent responses", () => {
  it("renders each response independently by API message ID and lineage", () => {
    const first = ungroupedResponse({
      apiMessageId: "message-one",
      usage: { ...ungroupedResponse().usage, inputTokens: 11 },
    });
    const second = ungroupedResponse({
      apiMessageId: "message-two",
      usage: { ...ungroupedResponse().usage, inputTokens: 22 },
    });

    const html = tokensOverlayHtml(data({ ungroupedSubagentResponses: [first, second] }));
    const firstStart = html.indexOf("ungrouped subagent response message-one");
    const secondStart = html.indexOf("ungrouped subagent response message-two");
    const firstSection = html.slice(firstStart, secondStart);
    const secondSection = html.slice(secondStart);

    expect(firstStart).toBeGreaterThanOrEqual(0);
    expect(secondStart).toBeGreaterThan(firstStart);
    expect(rows(firstSection)).toEqual(expect.arrayContaining([
      "API message ID|message-one",
      "parent agent|parent-agent",
      "subagent type|research",
      "task|inspect evidence",
      "uncached|11",
    ]));
    expect(rows(firstSection)).not.toContain("uncached|22");
    expect(rows(secondSection)).toEqual(expect.arrayContaining([
      "API message ID|message-two",
      "parent agent|parent-agent",
      "uncached|22",
    ]));
    expect(rows(secondSection)).not.toContain("uncached|11");
  });

  it("fails loudly if an ungrouped response lacks subagent lineage", () => {
    expect(() => tokensOverlayHtml(data({
      ungroupedSubagentResponses: [ungroupedResponse({ actor: "mainAgent", subagent: undefined })],
    }))).toThrow("lacks subagent lineage");
  });
});

describe("tokensOverlayHtml generated session accounting", () => {
  it("renders main, all-agent, grouped-subagent, model, timing, and ungrouped evidence", () => {
    const totals = create(TokenUsageTotalsSchema, {
      inputTokens: 10n,
      outputTokens: 20n,
      cacheReadInputTokens: 30n,
      cacheCreationInputTokens: 40n,
      cacheCreation: create(TokenCacheCreationSchema, { ephemeral5mInputTokens: 4n, ephemeral1hInputTokens: 36n }),
      serverToolUse: create(TokenServerToolUseSchema, { webSearchRequests: 2n, webFetchRequests: 3n }),
      outputDetails: create(TokenOutputDetailsSchema, { thinkingTokens: 5n }),
      cacheRates: create(TokenCacheRatesSchema, { totalPromptInputTokens: 80n, cacheHitRate: 0.375, cacheWriteRate: 0.5, uncachedInputRate: 0.125 }),
      timing: create(TokenTimingTotalsSchema, { outputTokensWithGenerationDuration: 20n, outputGenerationDurationMs: 100n, responsesWithGenerationDuration: 1n, responsesWithoutGenerationDuration: 2n, totalTimeToFirstTokenMs: 50n, responsesWithTimeToFirstToken: 1n, responsesWithoutTimeToFirstToken: 3n }),
    });
    const model = create(ModelTokenUtilizationSchema, { model: "opus", canonicalModel: "claude-opus", provider: "anthropic", totals, contextWindow: 200000n, maxOutputTokens: 32000n, costUsd: 1.25 });
    const subagent = create(AgentTokenUtilizationSchema, {
      agent: create(TokenUtilizationSubagentSchema, { agentId: "agent-7", parentToolUseId: "tool-parent", parentAgentId: "agent-parent", subagentType: "research", taskDescription: "inspect evidence" }),
      totals,
      models: [model],
    });
    const ungrouped = generatedUngroupedResponse({ apiMessageId: "message-ungrouped" });
    const session = create(SessionTokenUtilizationSchema, { allAgents: totals, mainAgent: totals, subagents: [subagent], models: [model], ungroupedSubagentResponses: [ungrouped] });

    const html = tokensOverlayHtml(data({ sessionUtilization: session }));
    expect(html).toContain("main agent");
    expect(html).toContain("all agents");
    expect(html).toContain("subagent agent-7");
    expect(html).toContain("all agents model opus");
    expect(html).toContain("ungrouped subagent response message-ungrouped");
    expect(rows(html)).toEqual(expect.arrayContaining([
      "cache write 5m|4",
      "cache write 1h|36",
      "total prompt input|80",
      "web searches|2",
      "web fetches|3",
      "thinking tokens|5",
      "generation|200.0 tok/s",
      "average TTFT|50 ms",
      "timed output tokens|20",
      "output generation duration|100 ms",
      "responses with generation duration|1",
      "responses without generation duration|2",
      "total TTFT|50 ms",
      "responses with TTFT|1",
      "responses without TTFT|3",
      "canonical model|claude-opus",
      "parent tool use ID|tool-parent",
    ]));
  });

  it("fails loudly when generated totals are structurally absent", () => {
    expect(() => tokensOverlayHtml(data({ sessionUtilization: create(SessionTokenUtilizationSchema) }))).toThrow(/lacks mainAgent or allAgents/);
  });

  it("renders unavailable model metadata without fabricating scalar defaults", () => {
    const totals = create(TokenUsageTotalsSchema);
    const model = create(ModelTokenUtilizationSchema, { model: "opus", totals });
    const session = create(SessionTokenUtilizationSchema, { allAgents: totals, mainAgent: totals, models: [model] });
    const html = tokensOverlayHtml(data({ sessionUtilization: session }));
    const start = html.indexOf("all agents model opus");
    const modelRows = rows(html.slice(start));

    expect(modelRows).toEqual(expect.arrayContaining([
      "canonical model|unavailable",
      "provider|unavailable",
      "cost|unavailable",
      "context window|unavailable",
      "max output|unavailable",
    ]));
  });

  it("renders generated ungrouped records from the shared fixture", () => {
    const response = generatedUngroupedResponse({ apiMessageId: "generated-message" });
    const session = generatedSessionUtilization([response]);
    expect(tokensOverlayHtml(data({ sessionUtilization: session }))).toContain("complete response JSON");
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
