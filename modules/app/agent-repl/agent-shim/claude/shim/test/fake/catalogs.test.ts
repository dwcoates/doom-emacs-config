/** `catalogs.ts`'s effort answers. */
import { describe, expect, it } from "vitest";
import {
  FAKE_DEFAULT_EFFORT,
  fakeAppliedEffort,
} from "../../src/fake/catalogs.js";

describe("fakeAppliedEffort", () => {
  it.each([
    ["the asked-for level the model accepts", "fake-sonnet-5", "low", "low"],
    ["the default for a level the model lacks", "fake-sonnet-5", "max", FAKE_DEFAULT_EFFORT],
    ["the default when nothing was asked", "fake-opus-4-8", undefined, FAKE_DEFAULT_EFFORT],
    ["no level for a model that takes none", "fake-haiku-4-5", "high", null],
    ["no level for a model the catalog does not carry", "not-a-model", "high", null],
  ] as const)("states %s", (_name, model, asked, want) => {
    // Arrange, Act, Assert
    expect(fakeAppliedEffort(model, asked)).toBe(want);
  });
});
