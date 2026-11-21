/**
 * hover-stamp — THE HOVERED BUBBLE'S STAMP, IN THE ACTIVITY CELL (owner
 * requests, 2026-10-25, 2026-10-26 and 2026-11-19). While the pointer is on a
 * bubble the daemon stamped (src/feed/hover-stamp.ts), the strip's activity
 * cell shows that stamp in place of the daemon's activity line:
 *
 *   "Response 2.1k tokens, 1m 30s"
 *   "Tool call Bash, 12s"
 *   "Prompt"
 *   "Testing the fix, 4m 10s"            (still running: the span ticks)
 *
 * Its label; its token figure, in its heat, when it has one; how long it took
 * once it settled (or how long it has run, ticking, while it is under way),
 * when its span's start is known. How long ago it was sent or settled is the
 * feed margin's (owner request, 2026-11-21), not the footer's. The
 * pointer leaving gives the cell back to the daemon's line on the next draw.
 *
 * It is the page's own line, not a daemon tier: the cell wears
 * `data-tier="hover"`, and nothing about it is ever pushed.
 */
import type { Ticker } from "../clock.js";
import type { HoverStamp } from "../feed/hover-stamp.js";
import { log } from "../log.js";
import { formatDurationCeil, formatTickedElapsed } from "../duration.js";
import { footerClockSpan } from "./clock-span.js";

/** The tier the hover line's cell wears, beside the daemon's three. */
export const HOVER_STAMP_TIER = "hover";

/** The kind the hover line's cell wears as its `data-arm`. */
export const HOVER_STAMP_ARM = "hover-stamp";

/** The class of the activity cell, which the hover line takes the place of. */
const ACTIVITY_CELL_CLASS = "footer-activity";

/**
 * STAMP's settled duration, "1m 30s", or null when it has none to state: it is
 * still under way, or no start for its span is known. A span that ends before
 * it began never gets here: its stamp is refused when it is read.
 */
export function hoverDuration(stamp: HoverStamp): string | null {
  const { startedAtMs, atMs } = stamp;
  if (startedAtMs === null || atMs === null) return null;
  return formatDurationCeil(atMs - startedAtMs);
}

/**
 * The activity cell drawing STAMP: the label, the figure in its heat when it
 * has one, its duration (settled, or ticking off TICKER while under way) when
 * its start is known. Its age is the feed margin's (feed/margin-stamp.ts).
 */
export function drawHoverStampActivity(stamp: HoverStamp, ticker: Ticker): HTMLElement {
  const { atMs, startedAtMs } = stamp;
  const duration = hoverDuration(stamp);
  const cell = document.createElement("div");
  cell.className = `pfooter-cell pfooter-grow ${ACTIVITY_CELL_CLASS}`;
  cell.setAttribute("data-tier", HOVER_STAMP_TIER);
  cell.setAttribute("data-arm", HOVER_STAMP_ARM);
  const line = document.createElement("span");
  line.className = "footer-hover-stamp";
  line.append(stamp.label);
  if (stamp.tokens !== null) {
    const tokens = document.createElement("span");
    tokens.className = "footer-hover-tokens";
    tokens.textContent = ` ${stamp.tokens.text} tokens`;
    tokens.style.color = stamp.tokens.color;
    line.append(tokens);
  }
  if (duration !== null) {
    const took = document.createElement("span");
    took.className = "footer-hover-duration";
    took.textContent = `, ${duration}`;
    line.append(took);
  } else if (atMs === null && startedAtMs !== null) {
    line.append(
      footerClockSpan(ticker, "elapsed", "footer-hover-duration", (span, nowMs) => {
        span.textContent = `, ${formatTickedElapsed(nowMs - startedAtMs)}`;
      }),
    );
  }
  cell.append(line);
  log.debug("drawing the hovered bubble's stamp in the activity cell", {
    operation: "footer.hover-stamp",
    context: {
      label: stamp.label,
      tokens: stamp.tokens?.text ?? null,
      settled: atMs !== null,
      running: atMs === null && startedAtMs !== null,
      duration,
    },
  });
  return cell;
}

/**
 * Put the hovered bubble's STAMP in STRIP's activity cell, which every strip
 * draws exactly once. A strip with none, or with more than one, is a defect.
 */
export function overlayHoverStamp(strip: HTMLElement, stamp: HoverStamp, ticker: Ticker): void {
  const cells = [...strip.children].filter((cell) => cell.classList.contains(ACTIVITY_CELL_CLASS));
  if (cells.length !== 1) {
    log.error("the hovered bubble's stamp found no single activity cell to stand in", {
      operation: "footer.hover-stamp.no-cell",
      context: { cells: cells.length, label: stamp.label },
    });
    throw new Error(`footer: a strip drew ${String(cells.length)} activity cells, not one`);
  }
  cells[0].replaceWith(drawHoverStampActivity(stamp, ticker));
}
