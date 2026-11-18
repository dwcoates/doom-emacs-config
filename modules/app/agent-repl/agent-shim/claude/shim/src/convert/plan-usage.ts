/**
 * convert/plan-usage.ts — the vendor's `get_usage` answer as
 * conversation.v1.SessionUpdate.plan_usage.
 *
 * The answer is read with the transcript scan skipped (`skipBehaviors`), so
 * only its `rate_limits` matter here. The arm is the whole truth about the
 * three weekly windows and the monthly spend it carries
 * (conversation.v1.SessionPlanUsage), so a figure the vendor did not state,
 * or stated null, is UNSET: nothing is defaulted.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";
import type { PlanUsageLike } from "../sdk/types.js";

const LOGGER = bindLog({ component: "shim-convert-plan-usage", operation: "shim.convert.plan_usage" });

/** One window as the vendor's answer carries it. */
type VendorWindow =
  | {
      readonly utilization: number | null;
      readonly resets_at: string | null;
    }
  | null
  | undefined;

/**
 * One window, typed. UNSET when the vendor stated no window, or a window
 * whose utilization or reset is null or unreadable.
 */
function planUsageWindow(name: string, window: VendorWindow): conversationv1.SessionPlanUsageWindow | undefined {
  if (window === null || window === undefined) return undefined;
  const { utilization, resets_at: resetsAt } = window;
  const resetsAtMs = resetsAt === null ? Number.NaN : Date.parse(resetsAt);
  if (utilization === null || !Number.isFinite(utilization) || !Number.isFinite(resetsAtMs)) {
    LOGGER.info(
      { window: name, utilization: String(utilization), resets_at: String(resetsAt) },
      "the vendor stated a plan-usage window without a readable figure; the window stays unset",
    );
    return undefined;
  }
  return create(conversationv1.SessionPlanUsageWindowSchema, {
    utilizationPercent: utilization,
    resetsAtMs: BigInt(resetsAtMs),
  });
}

/** The vendor's extra-usage figures as its answer carries them. */
type VendorExtraUsage = NonNullable<PlanUsageLike["rate_limits"]>["extra_usage"];

/**
 * The account's month-to-date spend, typed. UNSET when the vendor stated no
 * extra usage, stated it disabled, or stated it with a null spend or no
 * currency; a stated limit of null is an uncapped spend. The amounts are the
 * vendor's minor units, carried as they are.
 */
function monthlySpend(extra: VendorExtraUsage): conversationv1.SessionPlanUsageMonthlySpend | undefined {
  if (extra === null || extra === undefined) return undefined;
  const { is_enabled: enabled, used_credits: used, monthly_limit: limit, currency } = extra;
  const context = {
    is_enabled: enabled,
    used_credits: String(used),
    monthly_limit: String(limit),
    currency: String(currency),
  };
  if (!enabled) {
    LOGGER.info(context, "the vendor stated extra usage disabled for this account; no monthly spend is stated");
    return undefined;
  }
  if (!isMinorAmount(used) || (limit !== null && !isMinorAmount(limit)) || currency === null || currency === undefined || currency === "") {
    LOGGER.info(context, "the vendor stated extra usage without a readable spend or currency; the monthly spend stays unset");
    return undefined;
  }
  const money = (amount: number) =>
    create(conversationv1.SessionMoneySchema, { amountMinor: BigInt(amount), currency });
  LOGGER.info(context, "the vendor stated the account's monthly spend");
  return create(conversationv1.SessionPlanUsageMonthlySpendSchema, {
    spent: money(used),
    limit:
      limit === null
        ? { case: "uncapped", value: create(conversationv1.SessionPlanUsageMonthlySpendUncappedSchema, {}) }
        : { case: "capped", value: create(conversationv1.SessionPlanUsageMonthlySpendCappedSchema, { limit: money(limit) }) },
  });
}

/** Whether a vendor amount is a whole, non-negative count of minor units. */
function isMinorAmount(amount: number | null): amount is number {
  return amount !== null && Number.isSafeInteger(amount) && amount >= 0;
}

/**
 * The answer as a plan_usage update, or `undefined` when the plan limits do
 * not apply to this account (an API key or a third-party provider), which
 * states nothing.
 */
export function planUsageUpdate(answer: PlanUsageLike): conversationv1.SessionUpdate | undefined {
  const limits = answer.rate_limits;
  if (!answer.rate_limits_available || limits === null) {
    LOGGER.info(
      { subscription_type: answer.subscription_type ?? "none", rate_limits_available: answer.rate_limits_available },
      "the vendor answered no plan usage for this account; nothing is stated",
    );
    return undefined;
  }
  return create(conversationv1.SessionUpdateSchema, {
    update: {
      case: "planUsage",
      value: create(conversationv1.SessionPlanUsageSchema, {
        sevenDay: planUsageWindow("seven_day", limits.seven_day),
        sevenDayOpus: planUsageWindow("seven_day_opus", limits.seven_day_opus),
        sevenDaySonnet: planUsageWindow("seven_day_sonnet", limits.seven_day_sonnet),
        monthlySpend: monthlySpend(limits.extra_usage),
      }),
    },
  });
}
