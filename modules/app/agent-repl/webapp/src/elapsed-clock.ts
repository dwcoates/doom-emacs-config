/**
 * elapsed-clock — the ONE shape of a drawn elapsed time: a span that either
 * counts up from an instant on the wire, or shows a finished span and stops.
 *
 * Every elapsed clock the page draws is one of these two — a turn's, a
 * subagent's, a shell's, a merge's head, a merge tab, a queue entry's stage, an
 * expanded footer row's — and each used to build the span itself. The live one
 * repaints on the shared ticker through `tick` (so a discard finds it and
 * `stopTicking` ends it) and formats with `formatTickedElapsed`; the settled one
 * formats once with `formatElapsed` and holds no subscription, because A TIMER
 * STOPS WHEN ITS UNIT SETTLES. Building both here is what keeps every clock on
 * the page reading the same way.
 */
import type { Ticker } from "./clock.js";
import { formatElapsed, formatTickedElapsed } from "./duration.js";
import { tick } from "./feed/ticking.js";

/**
 * A span wearing CLASSNAME that counts up from STARTEDMS, painted now and on
 * every tick of the shared clock.
 */
export function liveElapsedClock(ticker: Ticker, className: string, startedMs: number): HTMLElement {
  const el = document.createElement("span");
  el.className = className;
  tick(el, ticker, (nowMs) => {
    el.textContent = formatTickedElapsed(nowMs - startedMs);
  });
  return el;
}

/**
 * A span wearing CLASSNAME that shows a finished span of SPANMS, drawn once
 * and never ticked.
 */
export function settledElapsedClock(className: string, spanMs: number): HTMLElement {
  const el = document.createElement("span");
  el.className = className;
  el.textContent = formatElapsed(spanMs);
  return el;
}
