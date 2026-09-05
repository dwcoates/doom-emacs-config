import { describe, expect, it } from "vitest";
import { normalizeModel, normalizeOptionalModel } from "../src/model.js";

describe("model normalization", () => {
  it("gives the synthetic marker exactly the same value as empty", () => {
    expect(normalizeModel("<synthetic>")).toBe("");
    expect(normalizeModel(" \t<synthetic>\n")).toBe("");
    expect(normalizeModel("")).toBe("");
  });

  it("preserves real model ids byte-for-byte", () => {
    expect(normalizeModel("claude-opus-5")).toBe("claude-opus-5");
  });

  it("omits empty-equivalent SDK overrides", () => {
    expect(normalizeOptionalModel(undefined)).toBeUndefined();
    expect(normalizeOptionalModel("")).toBeUndefined();
    expect(normalizeOptionalModel("<synthetic>")).toBeUndefined();
  });
});

describe("normalizeOptionalModel keeps a real override", () => {
  it("returns the id itself when the model is not empty-equivalent", () => {
    // Arrange, Act.
    const normalized = normalizeOptionalModel("claude-opus-5");

    // Assert.
    expect(normalized).toBe("claude-opus-5");
  });
});
