/** `catalogs.ts`'s effort and plan-usage answers. */
import { describe, expect, it } from "vitest";
import {
  FAKE_DEFAULT_EFFORT,
  fakeAppliedEffort,
  fakePlanUsage,
  isFakePlanUsageWindowName,
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

describe("fakePlanUsage", () => {
  it("answers a figured window in the vendor's units", () => {
    // Arrange + Act
    const usage = fakePlanUsage(new Map([["seven_day_opus", { utilizationPercent: 12, resetsAtMs: Date.UTC(2026, 10, 13) }]]));

    // Assert
    expect(usage.rate_limits?.seven_day_opus).toEqual({ utilization: 12, resets_at: "2026-11-13T00:00:00.000Z" });
  });

  it("answers null for a window it holds no figure for", () => {
    // Arrange + Act
    const usage = fakePlanUsage(new Map());

    // Assert
    expect([usage.rate_limits?.seven_day, usage.rate_limits?.seven_day_sonnet]).toEqual([null, null]);
  });

  it("answers as a max account the plan limits apply to", () => {
    // Arrange + Act
    const usage = fakePlanUsage(new Map());

    // Assert
    expect({ plan: usage.subscription_type, available: usage.rate_limits_available }).toEqual({ plan: "max", available: true });
  });
});

describe("isFakePlanUsageWindowName", () => {
  it.each([
    ["the account-wide weekly window", "seven_day", true],
    ["the Opus weekly window", "seven_day_opus", true],
    ["the Sonnet weekly window", "seven_day_sonnet", true],
    ["the five-hour window, which the verb's weekly figures do not follow", "five_hour", false],
    ["the overage window", "overage", false],
    ["a window named by no string", undefined, false],
  ])("%s", (_name, window, want) => {
    // Act + Assert
    expect(isFakePlanUsageWindowName(window)).toBe(want);
  });
});
