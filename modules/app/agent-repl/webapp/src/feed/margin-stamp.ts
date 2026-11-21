/**
 * margin-stamp — A HOVERED ROW'S TIMESTAMP AND AGE, IN THE FEED'S RIGHT MARGIN
 * (owner request, 2026-11-21). While the pointer is on a bubble, card or item
 * the daemon stamped (`hover-stamp.ts`), the margin right of the feed column
 * reads when it was sent or settled and how long ago that was, ticking:
 *
 *   "14:03:22 · 5m 30s ago"
 *
 * It FADES into place and out again: only its opacity moves, never its
 * position or size, so it never slides. It is set in the sans face at the
 * thinking bubble's text size, and it replaces the age the footer's hover line
 * used to end on. A row still under way has no instant, so it shows nothing.
 */
import type { Ticker } from "../clock.js";
import { formatTickedAge } from "../duration.js";
import type { HoverStamp } from "./hover-stamp.js";
import { stopTicking, tickWhileShown } from "./ticking.js";

/** The class of the margin stamp's element. */
export const MARGIN_STAMP_CLASS = "feed-margin-stamp";

/** The attribute the element wears while it is faded in. */
export const MARGIN_STAMP_SHOWN = "data-shown";

/** AT_MS as a wall-clock time of day, "14:03:22". */
export function marginClockTime(atMs: number): string {
  return new Date(atMs).toLocaleTimeString([], { hour: "2-digit", minute: "2-digit", second: "2-digit" });
}

/** The margin stamp's text for an instant AT_MS read at NOW_MS. */
export function marginStampText(atMs: number, nowMs: number): string {
  return `${marginClockTime(atMs)} · ${formatTickedAge(nowMs - atMs)} ago`;
}

/** The margin stamp: what the hover tells it, and its removal. */
export interface MarginStamp {
  /** Fade in STAMP beside ROW, or fade out when either is null or the stamp has no instant. */
  show(stamp: HoverStamp | null, row: Element | null): void;
  dispose(): void;
}

/**
 * Mount the margin stamp in PARENT, the positioned box the feed scrolls in,
 * its text counting off TICKER. A shown stamp stands level with the top of its
 * row.
 */
export function mountMarginStamp(parent: HTMLElement, ticker: Ticker): MarginStamp {
  const el = document.createElement("div");
  el.className = MARGIN_STAMP_CLASS;
  el.setAttribute("aria-hidden", "true");
  parent.append(el);
  const hide = (): void => {
    el.removeAttribute(MARGIN_STAMP_SHOWN);
    stopTicking(el);
  };
  return {
    show(stamp, row) {
      hide();
      if (stamp === null || row === null || stamp.atMs === null) return;
      const atMs = stamp.atMs;
      const box = parent.getBoundingClientRect();
      el.style.top = `${row.getBoundingClientRect().top - box.top + parent.scrollTop}px`;
      tickWhileShown(el, ticker, (nowMs) => {
        el.textContent = marginStampText(atMs, nowMs);
      });
      el.setAttribute(MARGIN_STAMP_SHOWN, "");
    },
    dispose() {
      hide();
      el.remove();
    },
  };
}
