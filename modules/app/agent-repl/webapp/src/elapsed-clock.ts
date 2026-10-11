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
import { log } from "./log.js";
import { fitsReserve, reserveLabels, reserveLabelsCss } from "./width-reserve.js";

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

/**
 * A CARD HEAD'S CLOCK HOLDS ITS WIDTH (owner ruling, 2026-10-11): a reserved
 * clock is drawn in a box as wide as the widest reading it can tick to, so a
 * tick never slides what follows it in the head, and never wraps the head onto
 * another line and changes the feed's height. The reserve is a `white-space:
 * pre` ghost (styles.css, `.reserved-clock`) reading `CLOCK_RESERVE_PROPERTY`.
 */
export const RESERVED_CLOCK_CLASS = "reserved-clock";

/** The custom property a reserved clock's ghost reads its labels from. */
export const CLOCK_RESERVE_PROPERTY = "--clock-reserve-labels";

/**
 * One span per shape the elapsed formatters write, each at its widest figures:
 * hours (to 999) with and without minutes, minutes with and without seconds,
 * and seconds alone.
 */
const ELAPSED_WIDEST_MS: readonly number[] = [
  (999 * 3_600 + 59 * 60) * 1000,
  999 * 3_600 * 1000,
  (59 * 60 + 59) * 1000,
  59 * 60 * 1000,
  59 * 1000,
];

/** The reserve of a clock whose reading is LABEL of an elapsed span. */
export function elapsedReserve(label: (elapsed: string) => string = (elapsed) => elapsed): readonly string[] {
  return reserveLabels(ELAPSED_WIDEST_MS, (ms) => label(formatElapsed(ms)));
}

/** The reserve of a bare elapsed reading: `5m 30s`. */
export const ELAPSED_RESERVE_LABELS = elapsedReserve();

/** Hold EL at the width of RESERVE's widest label. */
function reserveWidth(el: HTMLElement, reserve: readonly string[]): void {
  el.classList.add(RESERVED_CLOCK_CLASS);
  el.style.setProperty(CLOCK_RESERVE_PROPERTY, reserveLabelsCss(reserve));
}

/**
 * A span wearing CLASSNAME, held at RESERVE's width, painting READING of the
 * shared clock now and on every tick. A reading wider than the reserve is a
 * reserve that missed a shape: it is reported once, and drawn anyway.
 */
export function reservedTickingClock(
  ticker: Ticker,
  className: string,
  reserve: readonly string[],
  reading: (nowMs: number) => string,
): HTMLElement {
  const el = document.createElement("span");
  el.className = className;
  reserveWidth(el, reserve);
  let overflowReported = false;
  tick(el, ticker, (nowMs) => {
    const text = reading(nowMs);
    el.textContent = text;
    if (overflowReported || fitsReserve(text, reserve)) return;
    overflowReported = true;
    log.error("a reserved clock's reading outgrew its reserved width", {
      operation: "clock.reserve-exceeded",
      context: { class_name: className, reading: text, reserve },
    });
  });
  return el;
}

/** `liveElapsedClock`, held at the width of the widest elapsed reading. */
export function reservedLiveElapsedClock(ticker: Ticker, className: string, startedMs: number): HTMLElement {
  return reservedTickingClock(ticker, className, ELAPSED_RESERVE_LABELS, (nowMs) =>
    formatTickedElapsed(nowMs - startedMs),
  );
}

/** `settledElapsedClock`, held at the width its live reading was. */
export function reservedSettledElapsedClock(className: string, spanMs: number): HTMLElement {
  const el = settledElapsedClock(className, spanMs);
  reserveWidth(el, ELAPSED_RESERVE_LABELS);
  return el;
}

/** The reserve of a "quiet for" reading. */
const QUIET_RESERVE_LABELS = elapsedReserve((elapsed) => `quiet for ${elapsed}`);

/**
 * A head's "quiet for N s", ticking from the last beat the daemon observed at
 * ATMS, held at the width of its widest reading.
 */
export function reservedQuietClock(ticker: Ticker, className: string, atMs: number): HTMLElement {
  return reservedTickingClock(ticker, className, QUIET_RESERVE_LABELS, (nowMs) =>
    `quiet for ${formatTickedElapsed(nowMs - atMs)}`,
  );
}
