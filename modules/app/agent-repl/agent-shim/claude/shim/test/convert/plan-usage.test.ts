/**
 * THE ACCOUNT'S PLAN USAGE, AS THE VENDOR'S USAGE VERB ANSWERS IT.
 *
 * The arm is the whole truth about the three weekly windows it carries, so the
 * load-bearing rule is that a window the vendor did not figure is UNSET and
 * never defaulted: a defaulted 0% would tell a user they have a whole week
 * left when the vendor said nothing.
 */
import { describe, expect, it } from "vitest";
import { planUsageUpdate } from "../../src/convert/plan-usage.js";
import type { PlanUsageLike } from "../../src/sdk/types.js";

type Limits = NonNullable<PlanUsageLike["rate_limits"]>;

/** A max account's answer whose rate limits are `limits`. */
function answer(limits: Partial<Limits>): PlanUsageLike {
  return { subscription_type: "max", rate_limits_available: true, rate_limits: limits as Limits };
}

const RESET_ISO = "2026-11-13T08:00:00.055542+00:00";
const RESET_MS = Date.parse(RESET_ISO);

describe("planUsageUpdate", () => {
  it.each([
    {
      name: "the account-wide weekly window carries its percent and its reset in millis",
      limits: { seven_day: { utilization: 8, resets_at: RESET_ISO } },
      want: { sevenDay: { utilizationPercent: 8, resetsAtMs: BigInt(RESET_MS) } },
    },
    {
      name: "each per-model weekly window is filed under its own field",
      limits: {
        seven_day: { utilization: 40, resets_at: RESET_ISO },
        seven_day_opus: { utilization: 12, resets_at: RESET_ISO },
        seven_day_sonnet: { utilization: 30, resets_at: RESET_ISO },
      },
      want: {
        sevenDay: { utilizationPercent: 40, resetsAtMs: BigInt(RESET_MS) },
        sevenDayOpus: { utilizationPercent: 12, resetsAtMs: BigInt(RESET_MS) },
        sevenDaySonnet: { utilizationPercent: 30, resetsAtMs: BigInt(RESET_MS) },
      },
    },
    {
      name: "a window the vendor stated null is unset",
      limits: { seven_day: null, seven_day_opus: null },
      want: {},
    },
    {
      name: "a window the vendor left out is unset",
      limits: {},
      want: {},
    },
    {
      name: "a window with a null utilization is unset",
      limits: { seven_day: { utilization: null, resets_at: RESET_ISO } },
      want: {},
    },
    {
      name: "a window with a null reset is unset",
      limits: { seven_day: { utilization: 8, resets_at: null } },
      want: {},
    },
    {
      name: "a window with an unreadable reset is unset",
      limits: { seven_day: { utilization: 8, resets_at: "next thursday" } },
      want: {},
    },
  ])("$name", ({ limits, want }) => {
    // Act.
    const update = planUsageUpdate(answer(limits));

    // Assert.
    if (update?.update.case !== "planUsage") throw new Error("expected a planUsage update");
    const usage = update.update.value;
    const window = (w: typeof usage.sevenDay) =>
      w === undefined ? undefined : { utilizationPercent: w.utilizationPercent, resetsAtMs: w.resetsAtMs };
    expect({
      sevenDay: window(usage.sevenDay),
      sevenDayOpus: window(usage.sevenDayOpus),
      sevenDaySonnet: window(usage.sevenDaySonnet),
    }).toEqual({ sevenDay: undefined, sevenDayOpus: undefined, sevenDaySonnet: undefined, ...want });
  });

  it("states nothing for an account the plan limits do not apply to", () => {
    // Act.
    const update = planUsageUpdate({ subscription_type: null, rate_limits_available: false, rate_limits: null });

    // Assert.
    expect(update).toBeUndefined();
  });

  it("states nothing when the vendor answered no rate limits", () => {
    // Act.
    const update = planUsageUpdate({ subscription_type: "max", rate_limits_available: true, rate_limits: null });

    // Assert.
    expect(update).toBeUndefined();
  });
});
