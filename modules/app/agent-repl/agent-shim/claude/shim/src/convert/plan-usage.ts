/**
 * convert/plan-usage.ts — the vendor's `get_usage` answer as
 * conversation.v1.SessionUpdate.plan_usage.
 *
 * The answer is read with the transcript scan skipped (`skipBehaviors`), so
 * only its `rate_limits` matter here. The arm is the whole truth about the
 * three weekly windows it carries (conversation.v1.SessionPlanUsage), so a
 * window the vendor did not state, or stated with a null figure, is UNSET:
 * nothing is defaulted.
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
      }),
    },
  });
}
