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
import { logRecordsDuring } from "../log-records.js";

type Limits = NonNullable<PlanUsageLike["rate_limits"]>;

/** A max account's answer whose rate limits are `limits`. */
function answer(limits: Partial<Limits>): PlanUsageLike {
  return { subscription_type: "max", rate_limits_available: true, rate_limits: limits };
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

/** The work account's extra usage as its vendor usage cache carries it. */
const EXTRA = { is_enabled: true, monthly_limit: 1_200_000, used_credits: 199_396, utilization: 16.6, currency: "USD" };

/** The monthly spend an enterprise answer with `extra` states, and the records the read wrote. */
function spendOf(extra: Limits["extra_usage"]) {
  let update: ReturnType<typeof planUsageUpdate>;
  const records = logRecordsDuring(() => {
    update = planUsageUpdate({
      subscription_type: "enterprise",
      rate_limits_available: true,
      rate_limits: { seven_day: null, extra_usage: extra },
    });
  });
  if (update?.update.case !== "planUsage") throw new Error("expected a planUsage update");
  return { spend: update.update.value.monthlySpend, records };
}

describe("planUsageUpdate's monthly spend", () => {
  it("a capped spend carries the spent amount and the limit in minor units", () => {
    // Act.
    const { spend } = spendOf(EXTRA);

    // Assert.
    expect(spend?.spent).toMatchObject({ amountMinor: 199_396n, currency: "USD" });
    expect(spend?.limit.case).toBe("capped");
    if (spend?.limit.case !== "capped") return;
    expect(spend.limit.value.limit).toMatchObject({ amountMinor: 1_200_000n, currency: "USD" });
  });

  it("a null monthly limit is an uncapped spend", () => {
    // Act.
    const { spend } = spendOf({ ...EXTRA, monthly_limit: null });

    // Assert.
    expect(spend?.spent?.amountMinor).toBe(199_396n);
    expect(spend?.limit.case).toBe("uncapped");
  });

  it("a stated spend is recorded at INFO with the vendor's figures", () => {
    // Act.
    const { records } = spendOf(EXTRA);

    // Assert.
    expect(records).toContainEqual(
      expect.objectContaining({
        level: "info",
        message: "the vendor stated the account's monthly spend",
        context: expect.objectContaining({ used_credits: "199396", monthly_limit: "1200000", currency: "USD" }),
      }),
    );
  });

  it.each([
    { name: "no extra usage stated", extra: undefined },
    { name: "a null extra usage", extra: null },
  ])("$name leaves the spend unset", ({ extra }) => {
    // Act.
    const { spend } = spendOf(extra);

    // Assert.
    expect(spend).toBeUndefined();
  });

  it.each([
    {
      name: "extra usage disabled",
      extra: { ...EXTRA, is_enabled: false },
      message: "the vendor stated extra usage disabled for this account; no monthly spend is stated",
    },
    {
      name: "a null spend",
      extra: { ...EXTRA, used_credits: null },
      message: "the vendor stated extra usage without a readable spend or currency; the monthly spend stays unset",
    },
    {
      name: "a fractional spend",
      extra: { ...EXTRA, used_credits: 1993.96 },
      message: "the vendor stated extra usage without a readable spend or currency; the monthly spend stays unset",
    },
    {
      name: "a negative limit",
      extra: { ...EXTRA, monthly_limit: -1 },
      message: "the vendor stated extra usage without a readable spend or currency; the monthly spend stays unset",
    },
    {
      name: "a null currency",
      extra: { ...EXTRA, currency: null },
      message: "the vendor stated extra usage without a readable spend or currency; the monthly spend stays unset",
    },
    {
      name: "no currency",
      extra: { is_enabled: true, monthly_limit: 1_200_000, used_credits: 199_396, utilization: 16.6 },
      message: "the vendor stated extra usage without a readable spend or currency; the monthly spend stays unset",
    },
  ])("$name leaves the spend unset and records why", ({ extra, message }) => {
    // Act.
    const { spend, records } = spendOf(extra);

    // Assert.
    expect(spend).toBeUndefined();
    expect(records).toContainEqual(expect.objectContaining({ level: "info", message }));
  });
});
