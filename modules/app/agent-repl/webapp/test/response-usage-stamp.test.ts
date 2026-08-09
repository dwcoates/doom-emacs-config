/**
 * response-usage-stamp — the bubble-corner figures, drawn from the RESOLVED
 * `AgentResponse.usage_stamp`.
 *
 * The subject is verbatim rendering and, above all, that an ABSENT stamp
 * renders no figures rather than zeros. One edge per test.
 */
import { describe, expect, it } from "vitest";
import { responseUsageStampHtml, usageStampHeadline } from "../src/response-usage-stamp.js";
import type { ResponseUsageStamp } from "../src/frontend-proto.js";

function stamp(over: Partial<ResponseUsageStamp> = {}): ResponseUsageStamp {
  return {
    expensiveInputTokens: 4200,
    cacheReadTokens: 118000,
    outputTokens: 900,
    model: "claude-opus-5",
    ...over,
  };
}

describe("absence", () => {
  it("renders NO figures when the response carried no stamp", () => {
    // Arrange / Act — "0 in / 0 out" is a claim the response was free, which is
    // a different and false statement from "we were not told what it cost".
    // Assert
    expect(responseUsageStampHtml(undefined)).toBe("");
  });

  it("treats an explicit null as the same absence", () => {
    // Arrange / Act — so no call site has to normalize one into the other and
    // accidentally normalize it into a zero instead.
    // Assert
    expect(responseUsageStampHtml(null)).toBe("");
  });

  it("omits the model for a synthetic record", () => {
    // Arrange / Act — empty is never fabricated into a guess.
    const html = responseUsageStampHtml(stamp({ model: "" }));
    // Assert
    expect(html).not.toContain("usage-model");
  });
});

describe("the figures", () => {
  it("leads with the daemon's expensive-input total", () => {
    // Arrange / Act — the canonical input_misses total, resolved daemon-side.
    // Assert
    expect(usageStampHeadline(stamp())).toBe("4200 in");
  });

  it("renders the cache-served input verbatim", () => {
    // Arrange / Act
    const html = responseUsageStampHtml(stamp());
    // Assert
    expect(html).toContain("118000 cached");
  });

  it("renders the output as billed", () => {
    // Arrange / Act
    const html = responseUsageStampHtml(stamp());
    // Assert
    expect(html).toContain("900 out");
  });

  it("adds no total of its own across the three", () => {
    // Arrange / Act — this end computes nothing.
    const html = responseUsageStampHtml(stamp());
    // Assert
    expect(html).not.toContain("123100");
  });

  it("renders a real zero when the daemon reported one", () => {
    // Arrange / Act — a zero the daemon SENT is a figure; only an absent stamp
    // is an absence.
    const html = responseUsageStampHtml(stamp({ outputTokens: 0 }));
    // Assert
    expect(html).toContain("0 out");
  });

  it("renders the model verbatim", () => {
    // Arrange / Act
    const html = responseUsageStampHtml(stamp());
    // Assert
    expect(html).toContain("claude-opus-5");
  });

  it("escapes the model", () => {
    // Arrange / Act
    const html = responseUsageStampHtml(stamp({ model: "<img src=x>" }));
    // Assert
    expect(html).not.toContain("<img");
  });
});
