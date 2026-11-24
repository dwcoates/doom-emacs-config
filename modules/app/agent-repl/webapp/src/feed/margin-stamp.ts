/**
 * margin-stamp — A HOVERED ROW'S TIMESTAMP AND AGE, IN THE FEED'S RIGHT MARGIN
 * (owner request, 2026-11-21). While the pointer is on a bubble, card or item
 * the daemon stamped (`hover-stamp.ts`), the margin right of the feed column
 * reads when it was sent or settled and how long ago that was, ticking:
 *
 *   14:03:22
 *   5m 30s ago
 *
 * It FADES into place and out again: only its opacity animates, never its
 * position or size, so it never slides. It is set on two lines in the response
 * bubbles' serif face, small and grey, and it replaces the age the footer's
 * hover line used to end on. A row still under way has no instant, so it shows
 * nothing.
 *
 * WHERE IT STANDS (owner request, 2026-11-23). Across, it is centered in the
 * margin between the column's right rail and the feed's right edge (the
 * stylesheet places it). Down, it is centered on the part of its row the feed
 * shows (`marginStampCenter`): a row wholly in view has it level with the
 * row's middle, and a row cut off by the feed's top or bottom edge has it at
 * the middle of what remains, so the stamp is always in view. A scroll, or a
 * resize of the row or the feed, places it again.
 */
import type { Ticker } from "../clock.js";
import { decorativeElement, pageResizeObserver } from "../dom.js";
import { formatTickedAge } from "../duration.js";
import { spanIntersection, type VerticalSpan } from "../scroll.js";
import type { HoverStamp } from "./hover-stamp.js";
import { stopTicking, tickWhileShown } from "./ticking.js";

/** The class of the margin stamp's element. */
export const MARGIN_STAMP_CLASS = "feed-margin-stamp";

/** The class of the element inside it that holds the two lines, centered in the margin. */
export const MARGIN_STAMP_LINES_CLASS = "feed-margin-stamp-lines";

/** The attribute the element wears while it is faded in. */
export const MARGIN_STAMP_SHOWN = "data-shown";

/** AT_MS as a wall-clock time of day, "14:03:22". */
export function marginClockTime(atMs: number): string {
  return new Date(atMs).toLocaleTimeString([], { hour: "2-digit", minute: "2-digit", second: "2-digit" });
}

/** The margin stamp's two lines for an instant AT_MS read at NOW_MS: the clock time, then the age. */
export function marginStampLines(atMs: number, nowMs: number): [string, string] {
  return [marginClockTime(atMs), `${formatTickedAge(nowMs - atMs)} ago`];
}

/**
 * Where the middle of a stamp HEIGHT tall stands for a row spanning ROW in a
 * feed showing VIEW: the middle of the part of the row the view shows, moved
 * just far enough that the whole stamp stays inside the view. Null when the
 * view shows none of the row.
 */
export function marginStampCenter(row: VerticalSpan, view: VerticalSpan, height: number): number | null {
  const shown = spanIntersection(row, view);
  if (shown.bottom <= shown.top) return null;
  const half = height / 2;
  return Math.min(Math.max((shown.top + shown.bottom) / 2, view.top + half), view.bottom - half);
}

/** The margin stamp: what the hover tells it, and its removal. */
export interface MarginStamp {
  /** Fade in STAMP beside ROW, or fade out when either is null or the stamp has no instant. */
  show(stamp: HoverStamp | null, row: Element | null): void;
  dispose(): void;
}

/**
 * Mount the margin stamp in PARENT, the positioned box the feed scrolls in,
 * its text counting off TICKER.
 */
export function mountMarginStamp(parent: HTMLElement, ticker: Ticker): MarginStamp {
  const Observer = pageResizeObserver(parent);
  if (Observer === null) throw new Error("invariant: the feed's page has a ResizeObserver");
  const el = decorativeElement("div", MARGIN_STAMP_CLASS);
  const lines = decorativeElement("div", MARGIN_STAMP_LINES_CLASS);
  el.append(lines);
  parent.append(el);
  let row: Element | null = null;
  const place = (): void => {
    if (row === null) return;
    const box = parent.getBoundingClientRect();
    const viewTop = box.top + parent.clientTop;
    const rect = row.getBoundingClientRect();
    const center = marginStampCenter(
      { top: rect.top, bottom: rect.bottom },
      { top: viewTop, bottom: viewTop + parent.clientHeight },
      el.offsetHeight,
    );
    if (center === null) {
      el.removeAttribute(MARGIN_STAMP_SHOWN);
      return;
    }
    el.style.top = `${center - viewTop + parent.scrollTop}px`;
    el.setAttribute(MARGIN_STAMP_SHOWN, "");
  };
  const observer = new Observer(place);
  observer.observe(parent);
  parent.addEventListener("scroll", place, { passive: true });
  const hide = (): void => {
    if (row !== null) observer.unobserve(row);
    row = null;
    el.removeAttribute(MARGIN_STAMP_SHOWN);
    stopTicking(lines);
  };
  return {
    show(stamp, shown) {
      hide();
      if (stamp === null || shown === null || stamp.atMs === null) return;
      const atMs = stamp.atMs;
      tickWhileShown(lines, ticker, (nowMs) => {
        const [clock, age] = marginStampLines(atMs, nowMs);
        const line = (text: string): HTMLElement => {
          const div = document.createElement("div");
          div.textContent = text;
          return div;
        };
        lines.replaceChildren(line(clock), line(age));
      });
      row = shown;
      observer.observe(row);
      place();
    },
    dispose() {
      hide();
      observer.disconnect();
      parent.removeEventListener("scroll", place);
      el.remove();
    },
  };
}
