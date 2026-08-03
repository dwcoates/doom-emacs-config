/** Structured Claude subscription usage returned by the Agent SDK control API. */
export interface SubscriptionUsageResponse {
  subscription_type: string | null;
  rate_limits_available: boolean;
  rate_limits: {
    five_hour?: {
      utilization: number | null;
      resets_at: string | null;
    } | null;
  } | null;
}

/** The experimental SDK method is contained here so the rest of the shim has a stable name. */
export interface SubscriptionUsageQuery {
  usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET(): Promise<SubscriptionUsageResponse>;
}

/** One observed five-hour quota window at a turn boundary. */
export interface FiveHourUsageSample {
  measurementAvailable: boolean;
  utilization: number | null;
  resetsAt: string | null;
  subscriptionType: string | null;
  rateLimitsAvailable: boolean;
  sampleLatencyMs: number;
  unavailableReason?: string;
}

/** Validate the experimental response instead of silently coercing a changed SDK shape. */
export function fiveHourUsageSample(
  response: SubscriptionUsageResponse,
  sampleLatencyMs: number,
): FiveHourUsageSample {
  if (typeof response.rate_limits_available !== "boolean") {
    throw new Error("Claude usage response rate_limits_available is not boolean");
  }
  if (response.subscription_type !== null && typeof response.subscription_type !== "string") {
    throw new Error("Claude usage response subscription_type is neither string nor null");
  }
  if (!response.rate_limits_available) {
    if (response.rate_limits !== null) {
      throw new Error("Claude usage response has rate limits while rate_limits_available is false");
    }
    return {
      measurementAvailable: false,
      utilization: null,
      resetsAt: null,
      subscriptionType: response.subscription_type,
      rateLimitsAvailable: false,
      sampleLatencyMs,
      unavailableReason: "rate_limits_unavailable",
    };
  }
  const window = response.rate_limits?.five_hour;
  if (window === undefined || window === null) {
    return {
      measurementAvailable: false,
      utilization: null,
      resetsAt: null,
      subscriptionType: response.subscription_type,
      rateLimitsAvailable: true,
      sampleLatencyMs,
      unavailableReason: "five_hour_window_unavailable",
    };
  }
  const utilization = window.utilization;
  if (utilization !== null && (!Number.isFinite(utilization) || utilization < 0 || utilization > 100)) {
    throw new Error(`Claude five-hour utilization is outside 0..100: ${String(utilization)}`);
  }
  if (window.resets_at !== null && typeof window.resets_at !== "string") {
    throw new Error("Claude five-hour resets_at is neither string nor null");
  }
  return {
    measurementAvailable: utilization !== null,
    utilization,
    resetsAt: window.resets_at,
    subscriptionType: response.subscription_type,
    rateLimitsAvailable: true,
    sampleLatencyMs,
    ...(utilization === null ? { unavailableReason: "five_hour_utilization_unavailable" } : {}),
  };
}
