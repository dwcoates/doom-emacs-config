/**
 * The footer's ticking figures: a countdown to a shipped instant, or the age
 * of one. Every such span is the same thing — an element marked with what it
 * counts, repainted on the shared clock (`tick`) — so it is built here once,
 * and each site says only what the reading reads.
 */
import type { Ticker } from "../clock.js";
import { tick } from "../feed/ticking.js";

/** What a clock span counts, stamped as `data-countdown` or `data-age`. */
export type FooterClock = "countdown" | "age";

/**
 * A span that counts CLOCK, wearing CLASSNAME when one is given, painted by
 * PAINT now and on every tick of the shared clock.
 */
export function footerClockSpan(
  ticker: Ticker,
  clock: FooterClock,
  className: string | undefined,
  paint: (span: HTMLElement, nowMs: number) => void,
): HTMLElement {
  const span = document.createElement("span");
  if (className !== undefined) span.className = className;
  span.setAttribute(`data-${clock}`, "");
  tick(span, ticker, (nowMs) => paint(span, nowMs));
  return span;
}
