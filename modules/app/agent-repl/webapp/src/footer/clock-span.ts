/**
 * The footer's ticking figures: a countdown to a shipped instant, or the age
 * of one. Every such span is the same thing — an element marked with what it
 * counts, repainted on the shared clock (`tick`) — so it is built here once,
 * and each site says only what the reading reads.
 */
import type { Ticker } from "../clock.js";
import { formatTickedAge } from "../duration.js";
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

/**
 * The age of AT_MS, " · 2m ago", ticking off TICKER: how every line in the
 * activity cell ends that says how long ago its instant was.
 */
export function footerAgeSpan(ticker: Ticker, atMs: number): HTMLElement {
  return footerClockSpan(ticker, "age", "footer-activity-age", (span, nowMs) => {
    span.textContent = ` · ${formatTickedAge(nowMs - atMs)} ago`;
  });
}
