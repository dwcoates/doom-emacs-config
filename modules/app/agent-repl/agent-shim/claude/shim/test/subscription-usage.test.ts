import { describe, expect, it } from "vitest";
import { fiveHourUsageSample } from "../src/subscription-usage.js";

describe("fiveHourUsageSample", () => {
  it("preserves every five-hour field needed by turn-boundary logs", () => {
    // Act / Assert
    expect(fiveHourUsageSample({
      subscription_type: "max",
      rate_limits_available: true,
      rate_limits: {
        five_hour: {
          utilization: 37.5,
          resets_at: "2026-08-03T22:00:00Z",
        },
      },
    }, 12.25, 42)).toEqual({
      observedAtMs: 42,
      measurementAvailable: true,
      utilization: 37.5,
      resetsAt: "2026-08-03T22:00:00Z",
      subscriptionType: "max",
      rateLimitsAvailable: true,
      sampleLatencyMs: 12.25,
    });
  });

  it("reports account rate limits as explicitly unavailable", () => {
    // Act / Assert
    expect(fiveHourUsageSample({
      subscription_type: null,
      rate_limits_available: false,
      rate_limits: null,
    }, 4, 42)).toMatchObject({
      measurementAvailable: false,
      utilization: null,
      resetsAt: null,
      rateLimitsAvailable: false,
      unavailableReason: "rate_limits_unavailable",
    });
  });

  it("reports a missing five-hour window explicitly", () => {
    // Act / Assert
    expect(fiveHourUsageSample({
      subscription_type: "team",
      rate_limits_available: true,
      rate_limits: {},
    }, 2, 42)).toMatchObject({
      measurementAvailable: false,
      unavailableReason: "five_hour_window_unavailable",
    });
  });

  it("reports a null five-hour utilization explicitly", () => {
    // Act / Assert
    expect(fiveHourUsageSample({
      subscription_type: "team",
      rate_limits_available: true,
      rate_limits: { five_hour: { utilization: null, resets_at: null } },
    }, 3, 42)).toMatchObject({
      measurementAvailable: false,
      unavailableReason: "five_hour_utilization_unavailable",
    });
  });

  it.each([
    [
      "out-of-range utilization",
      { subscription_type: "max", rate_limits_available: true, rate_limits: { five_hour: { utilization: 101, resets_at: null } } },
      "outside 0..100",
    ],
    [
      "contradictory availability",
      { subscription_type: "max", rate_limits_available: false, rate_limits: {} },
      "has rate limits while rate_limits_available is false",
    ],
    [
      "non-string subscription type",
      { subscription_type: 7, rate_limits_available: true, rate_limits: {} },
      "subscription_type",
    ],
    [
      "non-string reset timestamp",
      { subscription_type: "max", rate_limits_available: true, rate_limits: { five_hour: { utilization: 50, resets_at: 7 } } },
      "resets_at",
    ],
  ] as const)("fails loudly for %s", (_case, response, expected) => {
    // Act / Assert
    expect(() => fiveHourUsageSample(response as never, 1, 42)).toThrow(expected);
  });
});
