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
  observedAtMs: number;
  measurementAvailable: boolean;
  utilization: number | null;
  resetsAt: string | null;
  /** Parsed once at the SDK boundary so downstream consumers never reinterpret provider text. */
  resetsAtMs: number | null;
  subscriptionType: string | null;
  rateLimitsAvailable: boolean;
  sampleLatencyMs: number;
  unavailableReason?: string;
  /** Complete diagnostic when sampling failed before a valid response existed. */
  unavailableCause?: string;
}

/** Anthropic's documented cadence is the only identity structure the SDK exposes. */
export const FIVE_HOUR_RESET_WINDOW_MS = 5 * 60 * 60 * 1000;
export const FIVE_HOUR_RESET_WINDOW_CONTRACT_VERSION = "anthropic-five-hour-cadence-v1";

export interface FiveHourWindowComparison {
  startResetsAtMs: number;
  endResetsAtMs: number;
  rawDeltaMs: number;
  canonicalCycleDisplacement: number;
  residualJitterMs: number;
  sameWindow: boolean;
}

/** Round to the nearest cycle with ties away from zero for signed symmetry. */
function roundNearestCycleDisplacement(value: number): number {
  return value < 0 ? -Math.round(-value) : Math.round(value);
}

/**
 * Compare reset instants relative to one another without inventing an epoch-aligned ID.
 * The provider's documented five-hour cadence makes the midpoint between adjacent
 * reset events the only contract-derived crossing boundary.
 */
export function compareFiveHourResetWindows(startResetsAtMs: number, endResetsAtMs: number): FiveHourWindowComparison {
  if (!Number.isFinite(startResetsAtMs) || !Number.isFinite(endResetsAtMs)) {
    throw new Error("five-hour reset comparison requires finite parsed timestamps");
  }
  const rawDeltaMs = endResetsAtMs - startResetsAtMs;
  const roundedDisplacement = roundNearestCycleDisplacement(rawDeltaMs / FIVE_HOUR_RESET_WINDOW_MS);
  const canonicalCycleDisplacement = Object.is(roundedDisplacement, -0) ? 0 : roundedDisplacement;
  return {
    startResetsAtMs,
    endResetsAtMs,
    rawDeltaMs,
    canonicalCycleDisplacement,
    residualJitterMs: rawDeltaMs - canonicalCycleDisplacement * FIVE_HOUR_RESET_WINDOW_MS,
    sameWindow: canonicalCycleDisplacement === 0,
  };
}

/** Validate the experimental response instead of silently coercing a changed SDK shape. */
export function fiveHourUsageSample(
  response: SubscriptionUsageResponse,
  sampleLatencyMs: number,
  observedAtMs: number,
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
      observedAtMs,
      measurementAvailable: false,
      utilization: null,
      resetsAt: null,
      resetsAtMs: null,
      subscriptionType: response.subscription_type,
      rateLimitsAvailable: false,
      sampleLatencyMs,
      unavailableReason: "rate_limits_unavailable",
    };
  }
  const window = response.rate_limits?.five_hour;
  if (window === undefined || window === null) {
    return {
      observedAtMs,
      measurementAvailable: false,
      utilization: null,
      resetsAt: null,
      resetsAtMs: null,
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
  const resetsAtMs = window.resets_at === null ? null : Date.parse(window.resets_at);
  if (resetsAtMs !== null && !Number.isFinite(resetsAtMs)) {
    throw new Error(`Claude five-hour resets_at is not a valid ISO 8601 timestamp: ${JSON.stringify(window.resets_at)}`);
  }
  return {
    observedAtMs,
    measurementAvailable: utilization !== null && resetsAtMs !== null,
    utilization,
    resetsAt: window.resets_at,
    resetsAtMs,
    subscriptionType: response.subscription_type,
    rateLimitsAvailable: true,
    sampleLatencyMs,
    ...(resetsAtMs === null
      ? { unavailableReason: "five_hour_window_unavailable" }
      : utilization === null
        ? { unavailableReason: "five_hour_utilization_unavailable" }
        : {}),
  };
}
