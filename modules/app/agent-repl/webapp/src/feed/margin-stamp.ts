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
 * stylesheet places it). A response hangs on the LEFT rail and ends at the one
 * bubble width short of the right one, so beside a row whose own bubble is a
 * response the margin starts where that width ends instead (owner request,
 * 2026-11-24): the stamp wears its row's bubble role (`MARGIN_STAMP_ROW_ROLE`)
 * for the stylesheet to read. Down, it is centered on the part of its row the feed
 * shows (`marginStampCenter`): a row wholly in view has it level with the
 * row's middle, and a row cut off by the feed's top or bottom edge has it at
 * the middle of what remains, so the stamp is always in view. A scroll, or a
 * resize of the row or the feed, places it again.
 *
 * IT STANDS OUTSIDE THE SCROLL BOX (2026-11-24). It is mounted in its own
 * slot, a sibling of the feed's scroll box that does not scroll, and placed in
 * the viewport's terms rather than the scrolled content's. Inside the scroll
 * box, an absolutely positioned stamp counted toward the box's scrollable
 * height wherever it last stood: a faded stamp left low in a feed that then
 * shrank kept the box taller than its rows, and the tail follow parked the
 * view in the blank below the last row. Outside it, the stamp cannot add to
 * the feed's height at all, and a scroll never carries it along with the
 * content between two placings.
 */
import { BUBBLE_ROLE_ATTRIBUTE, BUBBLE_SELECTOR } from "../bubble/draw.js";
import type { Ticker } from "../clock.js";
import { decorativeElement, pageResizeObserver, queryAll } from "../dom.js";
import { formatTickedAge } from "../duration.js";
import { spanIntersection, type VerticalSpan } from "../scroll.js";
import type { HoverStamp } from "./hover-stamp.js";
import { feedRowOf } from "./row-id.js";
import { stopTicking, tickWhileShown } from "./ticking.js";

/** The class of the margin stamp's element. */
export const MARGIN_STAMP_CLASS = "feed-margin-stamp";

/** The class of the element inside it that holds the two lines, centered in the margin. */
export const MARGIN_STAMP_LINES_CLASS = "feed-margin-stamp-lines";

/** The attribute the element wears while it is faded in. */
export const MARGIN_STAMP_SHOWN = "data-shown";

/** The attribute naming the role of the stamped row's own bubble, absent for a row with none. */
export const MARGIN_STAMP_ROW_ROLE = "data-row-role";

/**
 * The role of ROW's own bubble: the first bubble drawn in ROW itself rather
 * than in a row nested inside it, or null when ROW draws none.
 */
export function rowBubbleRole(row: Element): string | null {
  for (const bubble of queryAll(row, BUBBLE_SELECTOR)) {
    if (feedRowOf(bubble) === row) return bubble.getAttribute(BUBBLE_ROLE_ATTRIBUTE);
  }
  return null;
}

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
 * Mount the margin stamp in SLOT, a positioned box beside the feed's scroll box
 * BOX that does not scroll, its text counting off TICKER. The stamp is placed
 * over BOX's visible area, read in viewport terms, so BOX's scrollable height
 * never includes it.
 */
export function mountMarginStamp(box: HTMLElement, slot: HTMLElement, ticker: Ticker): MarginStamp {
  const Observer = pageResizeObserver(box);
  if (Observer === null) throw new Error("invariant: the feed's page has a ResizeObserver");
  const el = decorativeElement("div", MARGIN_STAMP_CLASS);
  const lines = decorativeElement("div", MARGIN_STAMP_LINES_CLASS);
  el.append(lines);
  slot.append(el);
  let row: Element | null = null;
  const place = (): void => {
    if (row === null) return;
    const boxRect = box.getBoundingClientRect();
    const viewTop = boxRect.top + box.clientTop;
    const viewLeft = boxRect.left + box.clientLeft;
    const rect = row.getBoundingClientRect();
    const center = marginStampCenter(
      { top: rect.top, bottom: rect.bottom },
      { top: viewTop, bottom: viewTop + box.clientHeight },
      el.offsetHeight,
    );
    if (center === null) {
      el.removeAttribute(MARGIN_STAMP_SHOWN);
      return;
    }
    const slotRect = slot.getBoundingClientRect();
    const slotTop = slotRect.top + slot.clientTop;
    const slotLeft = slotRect.left + slot.clientLeft;
    // Across, the stamp spans the box's client area, its scrollbar excluded,
    // exactly as the feed's rows do.
    el.style.left = `${viewLeft - slotLeft}px`;
    el.style.width = `${box.clientWidth}px`;
    el.style.top = `${center - slotTop}px`;
    el.setAttribute(MARGIN_STAMP_SHOWN, "");
  };
  const observer = new Observer(place);
  observer.observe(box);
  box.addEventListener("scroll", place, { passive: true });
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
      const role = rowBubbleRole(shown);
      if (role === null) el.removeAttribute(MARGIN_STAMP_ROW_ROLE);
      else el.setAttribute(MARGIN_STAMP_ROW_ROLE, role);
      row = shown;
      observer.observe(row);
      place();
    },
    dispose() {
      hide();
      observer.disconnect();
      box.removeEventListener("scroll", place);
      el.remove();
    },
  };
}
