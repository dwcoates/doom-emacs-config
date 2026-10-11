/**
 * The response type-out's pacing: how many rendered characters of an
 * arriving bubble are on screen at each animation frame
 * (docs/protobuf-design/learned-rate-reveal.md).
 *
 * THE DAEMON STATES THE BOUNDS, THE CLIENT FOLLOWS THE ARRIVALS. Every live
 * draw carries `frontend.v1.FeedResponseRevealPace`: a minimum and maximum
 * chars/sec learned from how fast whole blocks arrive, and a bank limit. The
 * text that has arrived but is not yet shown is the BANK. The reveal types at
 * the speed the bank is filling, read off the bank itself (`bank /
 * FOLLOW_SECONDS`: a steady arrival speed keeps a steady bank, and the speed
 * that keeps it steady IS the arrival speed), held between the bounds:
 *
 *   - text arriving faster than the maximum is typed at the maximum and the
 *     rest waits in the bank;
 *   - while the bank holds text, the reveal never types slower than the
 *     minimum, so a slow arrival spends the bank down;
 *   - an empty bank waits for the next text;
 *   - only a bank holding more than the bank limit's worth of typing at the
 *     maximum types faster than the maximum: the excess past the limit is
 *     caught up on the same `FOLLOW_SECONDS` time constant the arrivals are
 *     followed on, so a whole block landing at once types out quickly rather
 *     than leaving the reveal seconds behind, and with no jump.
 *
 * Nothing here touches the DOM or a clock: the caller hands in how long a
 * frame took, which is what lets the pacing be tested without a browser.
 */
import type { FeedResponseRevealPace } from "../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "./rpc/malformed.js";

/**
 * How far behind the arrivals the followed speed sits, in seconds: a bank of
 * `speed × FOLLOW_SECONDS` characters is what holds the reveal at `speed`. A
 * shorter time follows the arrivals more tightly and so shows their clumps; a
 * longer one smooths more and finishes later.
 */
export const FOLLOW_SECONDS = 0.3;

/** The daemon's reveal bounds, in the units the pacing works in. */
export interface RevealPace {
  /** The slowest the reveal types while the bank holds text, chars/sec. */
  minCps: number;
  /** The fastest the reveal types while the bank is within its limit, chars/sec. */
  maxCps: number;
  /** The bank limit, as seconds of typing at `maxCps`. */
  bankLimitSeconds: number;
}

/**
 * Read the wire pace. Unset answers undefined: a replayed draw, whose prose
 * is drawn whole. A bound or limit of zero, or a minimum above the maximum,
 * breaks the contract and is refused as malformed.
 */
export function revealPaceOf(pace: FeedResponseRevealPace | undefined, path: string): RevealPace | undefined {
  if (pace === undefined) return undefined;
  if (pace.minCharsPerSecond === 0) {
    throw new MalformedView(`${path}.min_chars_per_second`, "a reveal pace's minimum is always above zero");
  }
  if (pace.maxCharsPerSecond === 0) {
    throw new MalformedView(`${path}.max_chars_per_second`, "a reveal pace's maximum is always above zero");
  }
  if (pace.minCharsPerSecond > pace.maxCharsPerSecond) {
    throw new MalformedView(`${path}.min_chars_per_second`, "a reveal pace's minimum is never above its maximum");
  }
  if (pace.bankLimitMs === 0) {
    throw new MalformedView(`${path}.bank_limit_ms`, "a reveal pace's bank limit is always above zero");
  }
  return {
    minCps: pace.minCharsPerSecond,
    maxCps: pace.maxCharsPerSecond,
    bankLimitSeconds: pace.bankLimitMs / 1000,
  };
}

/**
 * The speed, in chars/sec, the reveal types at while BANK characters are
 * arrived but not shown: the followed arrival speed held between the bounds,
 * raised past the maximum only by a bank over its limit, by that excess over
 * `FOLLOW_SECONDS`. Zero for an empty bank.
 */
export function revealSpeed(bank: number, pace: RevealPace): number {
  if (bank <= 0) return 0;
  const limit = pace.maxCps * pace.bankLimitSeconds;
  if (bank > limit) return pace.maxCps + (bank - limit) / FOLLOW_SECONDS;
  return Math.min(pace.maxCps, Math.max(pace.minCps, bank / FOLLOW_SECONDS));
}

/**
 * How many characters are shown ELAPSEDMS after SHOWN were, with TOTAL
 * arrived: one frame's step at the speed the current bank calls for, never
 * past what has arrived.
 */
export function advanceReveal(shown: number, total: number, elapsedMs: number, pace: RevealPace): number {
  if (shown >= total) return total;
  const step = (revealSpeed(total - shown, pace) * Math.max(0, elapsedMs)) / 1000;
  return Math.min(total, shown + step);
}

/**
 * Slice `text` to its first `count` characters without splitting a UTF-16
 * surrogate pair: a cut landing between a pair's high and low half would
 * leave a lone high surrogate that renders as a replacement glyph for the one
 * frame before the low half is revealed. When the last kept unit is a high
 * surrogate, its low half is the next (excluded) unit, so the high half is
 * dropped too and the pair reveals atomically on the following frame.
 */
export function revealSlice(text: string, count: number): string {
  const n = Math.floor(count);
  if (n <= 0) return "";
  if (n >= text.length) return text;
  const last = text.charCodeAt(n - 1);
  if (last >= 0xd800 && last <= 0xdbff) return text.slice(0, n - 1);
  return text.slice(0, n);
}
