/**
 * expiry — the ONE re-render the footer schedules for itself: the instant the
 * transient it is drawing lapses.
 *
 * THE DAEMON DECIDES A TRANSIENT'S EXPIRY; THE CLIENT APPLIES IT (footer.proto,
 * "The transient tier"). A push carries the newest transient with its expiry
 * instant beside the enduring line beneath it, and the daemon pushes NOTHING
 * when the transient lapses. So without this timer the cell would keep drawing
 * a lapsed transient until some unrelated push happened to arrive. What is
 * drawn is a pure function of the last push and the client's clock, and this
 * is what makes the clock's side of that function take effect on time.
 *
 * EXACTLY ONE TIMER, NEVER A POLL. A draw that picks the transient schedules
 * its expiry; every draw first cancels whatever the previous one scheduled, so
 * a newer push (or a panel click, or a verdict redraw) replaces the pending
 * re-render rather than stacking a second one beside it. When the timer fires
 * it is forgotten before the re-render runs, and that re-render (the clock now
 * past the instant) draws the enduring line and schedules nothing.
 */
import type { Ticker } from "../clock.js";
import { log } from "../log.js";

/**
 * The longest delay `setTimeout` honours (2^31 - 1 ms, about 24.8 days). A
 * longer one overflows and fires at once, which would turn a far expiry into
 * a redraw loop; clamped, the timer fires early instead, and that redraw finds
 * the transient still live and schedules the remainder.
 */
export const MAX_TIMEOUT_MS = 2_147_483_647;

/** What a draw sees: the one call that asks for a re-render at an instant. */
export interface TransientExpirySchedule {
  /** Re-render at EXPIRESATMS (epoch ms), replacing any pending re-render. */
  schedule(expiresAtMs: number): void;
}

/** What the mount holds: the schedule, plus the cancel every draw starts with. */
export interface TransientExpiry extends TransientExpirySchedule {
  /** Drop the pending re-render, if any. */
  cancel(): void;
  /** Whether a re-render is pending; for the suite's assertions and logs. */
  pending(): boolean;
}

/**
 * Build the mount's expiry timer. ONEXPIRE is the re-render; TICKER is the
 * page's one clock, read for the delay so the instant and the comparison the
 * draw made share a source.
 */
export function createTransientExpiry(ticker: Ticker, onExpire: () => void): TransientExpiry {
  let handle: ReturnType<typeof setTimeout> | null = null;
  let scheduledAtMs: number | null = null;

  const cancel = (): void => {
    if (handle === null) return;
    clearTimeout(handle);
    log.debug("cancelled the pending transient expiry", {
      operation: "footer.transient-expiry-cancelled",
      context: { expires_at_ms: scheduledAtMs },
    });
    handle = null;
    scheduledAtMs = null;
  };

  return {
    schedule(expiresAtMs: number): void {
      cancel();
      const delayMs = Math.min(MAX_TIMEOUT_MS, Math.max(0, expiresAtMs - ticker.now()));
      log.debug("scheduled the transient expiry re-render", {
        operation: "footer.transient-expiry-scheduled",
        context: { expires_at_ms: expiresAtMs, delay_ms: delayMs },
      });
      scheduledAtMs = expiresAtMs;
      handle = setTimeout(() => {
        log.debug("the drawn transient lapsed; redrawing the activity cell", {
          operation: "footer.transient-expired",
          context: { expires_at_ms: expiresAtMs },
        });
        handle = null;
        scheduledAtMs = null;
        onExpire();
      }, delayMs);
    },
    cancel,
    pending(): boolean {
      return handle !== null;
    },
  };
}
