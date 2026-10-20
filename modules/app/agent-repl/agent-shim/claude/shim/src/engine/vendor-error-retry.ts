/**
 * engine/vendor-error-retry.ts — retrying a main-agent turn the vendor ended on
 * a retryable API failure, or on an exhausted usage allowance.
 *
 * AGENT-REPL OWNS THE RETRY CADENCE (owner ruling, 2026-10-20). The vendor
 * runs with its own in-process API retries OFF (sdk/real-query.ts sets
 * `CLAUDE_CODE_MAX_RETRIES=0`), so a failed API call ends the turn at once and
 * every retry is made here, where the footer can count down to it and the
 * user can ask for it now (shim.v1.RetryVendorErrorNow).
 *
 * THE RULE.
 *
 * - A RETRYABLE API FAILURE is retried every
 *   {@link VENDOR_ERROR_RETRY_INTERVAL_MS} for
 *   {@link VENDOR_ERROR_RETRY_WINDOW_MS}. It qualifies when the turn's
 *   terminal is `AgentFailure.api_request_failed` of a kind waiting can cure
 *   ({@link isRetryableApiFailure}): an overload, the API's own error, a
 *   failure with no status (the connection), or a status the vendor itself
 *   retries. The window is measured from the chain's first failure; a retry
 *   that would start past it is not made. A refusal of the request itself (a
 *   400, an auth, permission or billing refusal) is never retried.
 * - AN EXHAUSTED USAGE ALLOWANCE IS NEVER RETRIED ON THAT CADENCE: asking
 *   before the allowance resets cannot succeed. A usage-limit failure (a 429,
 *   `blocking_limit`, `rapid_refill_breaker`: the set the daemon's footer
 *   calls `usage_limit`) reads the account's usage windows afresh (the sample
 *   the footer's session and weekly figures are drawn from), and the retry is
 *   made at the moment the exhausted window resets: the five-hour session
 *   window, or a weekly one. Should the vendor still refuse once that moment
 *   has passed, the retry repeats every {@link VENDOR_ERROR_RETRY_INTERVAL_MS}
 *   for {@link USAGE_LIMIT_RETRY_TAIL_MS} after it. A 429 with no exhausted
 *   window on record is a passing rate limit, retried by the rule above.
 * - EVERY RETRY asks the main agent to continue, as an adopted turn of the
 *   shim's own (engine/session.ts `deliverShimPrompt`).
 * - THE SCHEDULE IS STATED, whole, every time it changes
 *   (conversation.v1.SessionVendorErrorRetry): the retry made next, when, and
 *   the failure it retries; or none, while no retry is due.
 * - A RETRY CAN BE MADE AT ONCE ({@link VendorErrorRetry.retryNow}): the
 *   pending one is delivered now instead of at its instant.
 * - THE CHAIN ENDS on the first retry turn that ends any other way (answered,
 *   interrupted, a failure neither rule retries), on a prompt someone else
 *   submits (the session is theirs again; a prompt of theirs that fails the
 *   same way opens a fresh chain), and at stand-down.
 * - ONE TIMER, and it exists only while a retry is due.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1, shimv1 } from "../proto.js";
import type { AccountUsageLike } from "../sdk/types.js";
import type { KeepaliveScheduler } from "./keepalive.js";
import type { ShimPromptDelivery } from "./shim-prompt.js";
import { vendorErrorRetryPrompt } from "./vendor-error-retry-prompt.js";

const LOGGER = bindLog({ component: "shim-engine-vendor-error-retry", operation: "shim.engine.vendor_error_retry" });

/** How long after a failed turn the next retry is delivered (owner ruling, 2026-10-20). */
export const VENDOR_ERROR_RETRY_INTERVAL_MS = 30 * 1_000;

/** How long after the chain's first failure the shim keeps retrying. */
export const VENDOR_ERROR_RETRY_WINDOW_MS = 2 * 60 * 60 * 1_000;

/** How long after an exhausted window's reset the shim keeps retrying. */
export const USAGE_LIMIT_RETRY_TAIL_MS = 30 * 60 * 1_000;

/**
 * How soon a retry that found the main agent busy (a keep-alive, a turn the
 * vendor started on its own, a rollback) asks again.
 */
export const VENDOR_ERROR_RETRY_BUSY_MS = 5_000;

/** The account's usage, as the retry reads it at a usage-limit failure. */
export type UsageReading =
  | { readonly kind: "sampled"; readonly usage: AccountUsageLike }
  | { readonly kind: "unavailable"; readonly detail: string };

/** The retry the chain makes next, as the session stream states it. */
export interface ScheduledRetry {
  /** Which retry of the chain it is, 1-based. */
  readonly attempt: number;
  /** When it is made, epoch ms. */
  readonly atMs: number;
  /** The failure it retries, in the vendor's words. */
  readonly status: string;
}

/** What asking for the retry now came to (shim.v1.RetryVendorErrorNow). */
export type RetryNowOutcome =
  /** The retry was delivered: the main agent continues the failed turn. */
  | { readonly kind: "retried" }
  /** No retry was scheduled. */
  | { readonly kind: "nothing_scheduled" }
  /** The main agent was busy; the retry asks again shortly. */
  | { readonly kind: "busy"; readonly detail: string }
  /** The retry could not be made and the chain ended. */
  | { readonly kind: "ended"; readonly detail: string };

/** Everything {@link VendorErrorRetry} does not own. */
export interface VendorErrorRetryDeps {
  /** Submits the retry prompt as the main agent's next turn. */
  readonly deliver: (prompt: string) => Promise<ShimPromptDelivery>;
  /** States the schedule on the session stream, whole: the next retry, or none. */
  readonly state: (scheduled: ScheduledRetry | undefined) => void;
  /** A usage sample taken AFTER it is asked for (engine/serial-probe.ts `run`). */
  readonly readUsage: () => Promise<UsageReading>;
  readonly nowMs: () => number;
  readonly scheduler: KeepaliveScheduler;
}

/** Which allowance an exhausted window is. */
export type AllowanceWindow = "session" | "weekly";

/** An exhausted usage window and when it resets. */
export interface ExhaustedWindow {
  readonly window: AllowanceWindow;
  /** The vendor's own name for the window (`five_hour`, `seven_day`, ...). */
  readonly name: string;
  readonly resetsAtMs: number;
}

/** The vendor's name for each account window a usage limit can exhaust. */
type WindowName = "five_hour" | "seven_day" | "seven_day_opus" | "seven_day_sonnet" | "seven_day_oauth_apps";

/** The account windows a usage limit can exhaust, by allowance. */
const ALLOWANCE_WINDOWS: readonly { readonly name: WindowName; readonly window: AllowanceWindow }[] = [
  { name: "five_hour", window: "session" },
  { name: "seven_day", window: "weekly" },
  { name: "seven_day_opus", window: "weekly" },
  { name: "seven_day_sonnet", window: "weekly" },
  { name: "seven_day_oauth_apps", window: "weekly" },
];

/**
 * The exhausted window that resets LAST, or absence when none is exhausted
 * and still to reset at NOW_MS. Every exhausted window must reset before a
 * request can be served, so the latest reset is the first moment worth asking.
 */
export function exhaustedWindow(usage: AccountUsageLike, nowMs: number): ExhaustedWindow | undefined {
  const limits = usage.rate_limits;
  if (limits === null) return undefined;
  let latest: ExhaustedWindow | undefined;
  for (const { name, window } of ALLOWANCE_WINDOWS) {
    const reading = limits[name];
    if (reading === null || reading === undefined) continue;
    if (reading.utilization === null || reading.utilization < 100 || reading.resets_at === null) continue;
    const resetsAtMs = Date.parse(reading.resets_at);
    if (Number.isNaN(resetsAtMs) || resetsAtMs <= nowMs) continue;
    if (latest === undefined || resetsAtMs > latest.resetsAtMs) latest = { window, name, resetsAtMs };
  }
  return latest;
}

/** How a chain is retrying. */
type Mode =
  /** Every interval, until the window from the chain's first failure closes. */
  | { readonly kind: "vendor_error"; readonly sinceMs: number }
  /** At the exhausted window's reset, then every interval until the tail closes. */
  | { readonly kind: "usage_limit"; readonly window: ExhaustedWindow; readonly untilMs: number };

/** One run of retries, from the first qualifying failure until it ends. */
interface Chain {
  /** Per-process generation, so every record names which chain. */
  readonly id: number;
  mode: Mode | undefined;
  /** How many retries this chain delivered. */
  retries: number;
  /** The latest failure the chain retries, in the vendor's words. */
  status: string;
  /** The shim turn the latest delivered retry opened, while it runs. */
  awaiting?: string;
}

/** What one failed turn's terminal says to the retry. */
interface Failure {
  readonly turnId: string | undefined;
  /** Whether the failure is an API failure waiting can cure. */
  readonly retryable: boolean;
  /** Whether the failure is a usage limit. */
  readonly usageLimit: boolean;
  /** The failure, in the vendor's words. */
  readonly status: string;
}

/** The shim's one vendor-error retry state. */
export class VendorErrorRetry {
  private chain: Chain | undefined;
  private nextChainId = 1;
  private handle: unknown;
  /** The retry the timer makes, while one is armed. */
  private pending: ScheduledRetry | undefined;
  private stopped = false;

  constructor(private readonly deps: VendorErrorRetryDeps) {}

  /** Whether a chain stands. */
  standing(): boolean {
    return this.chain !== undefined;
  }

  /** Whether a retry is scheduled. */
  scheduled(): boolean {
    return this.handle !== undefined;
  }

  /**
   * A non-keep-alive vendor turn ended. TURN_ID is the shim turn its result
   * answered (`undefined` for one that answered none of ours); FRAME is its
   * terminal.
   */
  turnEnded(turnId: string | undefined, frame: conversationv1.AgentFrame): void {
    if (this.stopped) return;
    const chain = this.chain;
    // ANOTHER TURN'S END says nothing about a standing chain: the retry it is
    // waiting to deliver, or the one it delivered, is still its own.
    if (chain !== undefined && (chain.awaiting === undefined || chain.awaiting !== turnId)) return;
    const failure: Failure = {
      turnId,
      retryable: isRetryableApiFailure(frame),
      usageLimit: isUsageLimit(frame),
      status: failureStatus(frame),
    };
    if (!failure.retryable && !failure.usageLimit) {
      if (chain === undefined) return;
      this.end(chain, "the retried turn ended without a failure agent-repl retries", {
        turn_id: turnId,
        outcome: frame.result.case === "failure" ? `failure.${String(frame.result.value.failure.case)}` : "success",
      });
      return;
    }
    const retrying = chain ?? this.open(failure);
    retrying.awaiting = undefined;
    retrying.status = failure.status;
    if (failure.usageLimit) {
      void this.readAllowance(retrying, failure);
      return;
    }
    this.retryVendorError(retrying, failure);
  }

  /** Someone else submitted a prompt: the session is theirs again. */
  promptSubmitted(turnId: string): void {
    const chain = this.chain;
    if (chain === undefined) return;
    this.end(chain, "a prompt was submitted; the retry chain yields to it", { turn_id: turnId });
  }

  /** Stand down: cancel the timer and abandon the chain. */
  stop(reason: string): void {
    if (this.stopped) return;
    this.stopped = true;
    const chain = this.chain;
    if (chain !== undefined) this.end(chain, "the shim is standing down; the failed turn keeps its failure", { reason });
  }

  /**
   * Make the pending retry NOW rather than at its instant (the user asked,
   * shim.v1.RetryVendorErrorNow). With none pending there is nothing to make:
   * no chain stands, its usage is still being read, or its latest retry is
   * already running as a turn.
   */
  async retryNow(): Promise<RetryNowOutcome> {
    const chain = this.chain;
    if (this.stopped || chain === undefined || this.handle === undefined) {
      LOGGER.info(
        { chain: chain?.id ?? 0, standing: chain !== undefined, awaiting: chain?.awaiting ?? "" },
        "a retry was asked for now, and none is scheduled",
      );
      return { kind: "nothing_scheduled" };
    }
    this.cancelTimer();
    LOGGER.info({ chain: chain.id, attempt: chain.retries + 1 }, "a retry was asked for now; agent-repl makes it at once");
    return this.retry(chain);
  }

  private open(failure: Failure): Chain {
    const opened: Chain = { id: this.nextChainId++, mode: undefined, retries: 0, status: failure.status };
    this.chain = opened;
    LOGGER.info(
      { chain: opened.id, turn_id: failure.turnId ?? "", retryable: failure.retryable, usage_limit: failure.usageLimit },
      "the turn ended on a failure agent-repl retries; a retry chain opened",
    );
    return opened;
  }

  /** The vendor-error cadence: every interval, inside the window. */
  private retryVendorError(chain: Chain, failure: Failure): void {
    if (chain.mode?.kind !== "vendor_error") {
      // A CHAIN THAT WAS WAITING ON AN ALLOWANCE and now fails some other way
      // starts the vendor-error window at this failure.
      chain.mode = { kind: "vendor_error", sinceMs: this.deps.nowMs() };
    }
    const now = this.deps.nowMs();
    const deadline = chain.mode.sinceMs + VENDOR_ERROR_RETRY_WINDOW_MS;
    if (now + VENDOR_ERROR_RETRY_INTERVAL_MS > deadline) {
      this.giveUp(
        chain,
        "the vendor kept failing the turn for the whole retry window; agent-repl stopped retrying and the turn keeps its failure",
        `the next retry would start past the ${String(VENDOR_ERROR_RETRY_WINDOW_MS)}ms window`,
        { retried_for_ms: now - chain.mode.sinceMs, window_ms: VENDOR_ERROR_RETRY_WINDOW_MS },
      );
      return;
    }
    LOGGER.info(
      { chain: chain.id, turn_id: failure.turnId ?? "", retries: chain.retries, delay_ms: VENDOR_ERROR_RETRY_INTERVAL_MS },
      "the vendor failed the turn's API call; agent-repl retries it after the interval",
    );
    this.arm(chain, VENDOR_ERROR_RETRY_INTERVAL_MS);
  }

  /**
   * The usage-limit schedule, read off a usage sample taken after the
   * failure. OFF THE MESSAGE LOOP: the sample is a vendor round trip, and the
   * loop must not wait on it.
   */
  private async readAllowance(chain: Chain, failure: Failure): Promise<void> {
    const reading = await this.deps.readUsage();
    if (this.chain !== chain) {
      LOGGER.debug({ chain: chain.id }, "the retry chain ended while the account's usage was read");
      return;
    }
    const now = this.deps.nowMs();
    const exhausted = reading.kind === "sampled" ? exhaustedWindow(reading.usage, now) : undefined;
    if (exhausted !== undefined) {
      chain.mode = { kind: "usage_limit", window: exhausted, untilMs: exhausted.resetsAtMs + USAGE_LIMIT_RETRY_TAIL_MS };
      LOGGER.info(
        {
          chain: chain.id,
          turn_id: failure.turnId ?? "",
          window: exhausted.window,
          vendor_window: exhausted.name,
          resets_at_ms: exhausted.resetsAtMs,
          delay_ms: exhausted.resetsAtMs - now,
        },
        "the usage allowance is exhausted; agent-repl retries the turn when the window resets",
      );
      this.arm(chain, exhausted.resetsAtMs - now);
      return;
    }
    // THE RESET HAS COME and the vendor still refuses: the tail's cadence.
    if (chain.mode?.kind === "usage_limit") {
      if (now + VENDOR_ERROR_RETRY_INTERVAL_MS > chain.mode.untilMs) {
        this.giveUp(
          chain,
          "the vendor kept refusing the turn's usage after the window reset; agent-repl stopped retrying and the turn keeps its failure",
          `the next retry would start past the ${String(USAGE_LIMIT_RETRY_TAIL_MS)}ms after the ${chain.mode.window.window} window reset`,
          { window: chain.mode.window.window, resets_at_ms: chain.mode.window.resetsAtMs },
        );
        return;
      }
      LOGGER.info(
        { chain: chain.id, turn_id: failure.turnId ?? "", window: chain.mode.window.window, delay_ms: VENDOR_ERROR_RETRY_INTERVAL_MS },
        "the usage window has reset but the vendor still refuses; agent-repl retries after the interval",
      );
      this.arm(chain, VENDOR_ERROR_RETRY_INTERVAL_MS);
      return;
    }
    const detail = reading.kind === "sampled" ? "no usage window is exhausted and still to reset" : reading.detail;
    if (failure.retryable) {
      LOGGER.info({ chain: chain.id, detail }, "the usage-limit failure names no exhausted allowance; it is retried as a vendor error");
      this.retryVendorError(chain, failure);
      return;
    }
    this.end(chain, "the usage-limit failure names no exhausted allowance to wait for; the turn is not retried", { detail });
  }

  private arm(chain: Chain, delayMs: number): void {
    this.cancelTimer();
    this.handle = this.deps.scheduler.setTimeout(() => {
      this.handle = undefined;
      void this.retry(chain);
    }, delayMs);
    this.pending = { attempt: chain.retries + 1, atMs: this.deps.nowMs() + delayMs, status: chain.status };
    this.deps.state(this.pending);
  }

  private async retry(chain: Chain): Promise<RetryNowOutcome> {
    if (this.chain !== chain) return { kind: "nothing_scheduled" };
    this.unschedule();
    const attempt = chain.retries + 1;
    let delivery: ShimPromptDelivery;
    try {
      delivery = await this.deps.deliver(vendorErrorRetryPrompt(attempt));
    } catch (err) {
      const cause = err instanceof Error ? err.message : String(err);
      if (this.chain !== chain) return { kind: "ended", detail: cause };
      this.giveUp(
        chain,
        "the vendor refused the retry prompt; agent-repl stopped retrying and the turn keeps its failure",
        cause,
        { attempt },
      );
      return { kind: "ended", detail: cause };
    }
    // A CHAIN ENDED WHILE THE DELIVERY WAS AWAITED (a prompt was submitted, the
    // shim stood down) is not revived by it.
    if (this.chain !== chain) {
      LOGGER.debug({ chain: chain.id, delivery: delivery.kind }, "the retry chain ended while its prompt was being delivered");
      return { kind: "ended", detail: "the retry chain ended while its prompt was being delivered" };
    }
    switch (delivery.kind) {
      case "delivered":
        chain.retries = attempt;
        chain.awaiting = delivery.turnId;
        LOGGER.info(
          { chain: chain.id, attempt, turn_id: delivery.turnId, mode: chain.mode?.kind ?? "" },
          "delivered the retry; the main agent continues the failed turn",
        );
        return { kind: "retried" };
      case "busy":
        LOGGER.debug({ chain: chain.id, attempt, detail: delivery.detail }, "the main agent is busy; the retry asks again shortly");
        this.arm(chain, VENDOR_ERROR_RETRY_BUSY_MS);
        return { kind: "busy", detail: delivery.detail };
      case "unavailable":
        this.end(chain, "the session cannot take the retry prompt; the failed turn keeps its failure", {
          attempt,
          detail: delivery.detail,
        });
        return { kind: "ended", detail: delivery.detail };
    }
  }

  /** The chain ran out: ERROR, because the turn's work is left undone. CAUSE says what ran out. */
  private giveUp(chain: Chain, why: string, cause: string, fields: Record<string, unknown>): void {
    this.cancelTimer();
    this.unschedule();
    this.chain = undefined;
    LOGGER.error({ chain: chain.id, retries: chain.retries, mode: chain.mode?.kind ?? "", cause, ...fields }, why);
  }

  private end(chain: Chain, why: string, fields: Record<string, unknown>): void {
    this.cancelTimer();
    this.unschedule();
    this.chain = undefined;
    LOGGER.info({ chain: chain.id, retries: chain.retries, mode: chain.mode?.kind ?? "", ...fields }, why);
  }

  private cancelTimer(): void {
    if (this.handle === undefined) return;
    this.deps.scheduler.clearTimeout(this.handle);
    this.handle = undefined;
  }

  /** State that no retry is due, once, when one was. */
  private unschedule(): void {
    if (this.pending === undefined) return;
    this.pending = undefined;
    this.deps.state(undefined);
  }
}

/**
 * The `unmodeled` types waiting can cure: a failure that stated no status
 * (the connection itself failed), a timeout or conflict, and every 5xx. The
 * set the vendor's own retry made, so turning its retries off retries nothing
 * less.
 */
function retryableUnmodeled(type: string): boolean {
  return type === "unknown" || type === "http_408" || type === "http_409" || /^http_5\d\d$/.test(type);
}

/**
 * Whether a terminal is an API failure waiting can cure. A refusal of the
 * request itself (malformed, too large, unauthorized, unpaid, a model that
 * does not exist, too much output asked for) fails the same way however
 * often it is sent, so it is never retried.
 */
export function isRetryableApiFailure(frame: conversationv1.AgentFrame): boolean {
  if (frame.result.case !== "failure") return false;
  const failure = frame.result.value.failure;
  if (failure.case !== "apiRequestFailed") return false;
  const kind = failure.value.kind;
  switch (kind.case) {
    case "overloaded":
    case "internal":
    case "rateLimited":
      return true;
    case "unmodeled":
      return retryableUnmodeled(kind.value.type);
    case "invalidRequest":
    case "authenticationFailed":
    case "permissionDenied":
    case "notFound":
    case "requestTooLarge":
    case "billingError":
    case "oauthOrgNotAllowed":
    case "maxOutputTokens":
      return false;
    case undefined:
      throw new Error("an api_request_failed terminal carries no kind");
  }
}

/** The failure a terminal ended on, in the vendor's words, for the schedule's line. */
function failureStatus(frame: conversationv1.AgentFrame): string {
  if (frame.result.case !== "failure") return "";
  const failure = frame.result.value;
  if (failure.failure.case === "apiRequestFailed") return failure.failure.value.message;
  return failure.errors[failure.errors.length - 1] ?? failure.failure.case ?? "";
}

/**
 * Whether a terminal is a usage limit: the set the daemon's footer resolves
 * to `vendor_fault · usage_limit` (daemon resolve/footer/chips.go).
 */
function isUsageLimit(frame: conversationv1.AgentFrame): boolean {
  if (frame.result.case !== "failure") return false;
  const failure = frame.result.value.failure;
  switch (failure.case) {
    case "apiRequestFailed":
      return failure.value.kind.case === "rateLimited";
    case "blockingLimit":
    case "rapidRefillBreaker":
      return true;
    default:
      return false;
  }
}

/** The schedule as the session stream states it: the next retry, or none. */
export function vendorErrorRetryUpdate(scheduled: ScheduledRetry | undefined): conversationv1.SessionUpdate {
  return create(conversationv1.SessionUpdateSchema, {
    update: {
      case: "vendorErrorRetry",
      value: create(conversationv1.SessionVendorErrorRetrySchema, {
        ...(scheduled === undefined
          ? {}
          : {
              scheduled: create(conversationv1.SessionVendorErrorRetryScheduledSchema, {
                attempt: scheduled.attempt,
                nextAttemptAtMs: BigInt(scheduled.atMs),
                status: scheduled.status,
              }),
            }),
      }),
    },
  });
}

/** shim.v1.RetryVendorErrorNow's answer for OUTCOME. */
export function retryVendorErrorNowResponse(outcome: RetryNowOutcome): shimv1.RetryVendorErrorNowResponse {
  const arm = ((): shimv1.RetryVendorErrorNowResponse["outcome"] => {
    switch (outcome.kind) {
      case "retried":
        return { case: "retried", value: create(shimv1.RetryVendorErrorNowRetriedSchema, {}) };
      case "nothing_scheduled":
        return { case: "nothingScheduled", value: create(shimv1.RetryVendorErrorNowNothingScheduledSchema, {}) };
      case "busy":
        return { case: "busy", value: create(shimv1.RetryVendorErrorNowBusySchema, { detail: outcome.detail }) };
      case "ended":
        return { case: "ended", value: create(shimv1.RetryVendorErrorNowEndedSchema, { detail: outcome.detail }) };
    }
  })();
  return create(shimv1.RetryVendorErrorNowResponseSchema, { outcome: arm });
}
