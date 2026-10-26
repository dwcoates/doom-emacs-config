/**
 * response-usage — THE HOVERED RESPONSE'S USAGE, IN THE ACTIVITY CELL (owner
 * requests, 2026-10-25 and 2026-10-26). While the pointer is on a response
 * bubble (src/feed/response-usage.ts), the strip's activity cell shows that
 * response's usage stamp in place of the daemon's activity line, as
 * "Response 2.1k tokens, 1m 30s · 5m ago": its token figure, in its heat;
 * once the response settled, how long it took (the span the figure covers,
 * when the daemon knows where that span began); and how long ago it settled,
 * as a live clock ending the line as the activity line's own age does. The
 * pointer leaving gives the cell back to the daemon's line on the next draw.
 *
 * It is the page's own line, not a daemon tier: the cell wears
 * `data-tier="hover"`, and nothing about it is ever pushed.
 */
import type { Ticker } from "../clock.js";
import type { ResponseUsage } from "../feed/response-usage.js";
import { log } from "../log.js";
import { formatDurationCeil } from "../duration.js";
import { footerAgeSpan } from "./clock-span.js";

/** The tier the hover line's cell wears, beside the daemon's three. */
export const RESPONSE_USAGE_TIER = "hover";

/** The kind the hover line's cell wears as its `data-arm`. */
export const RESPONSE_USAGE_ARM = "response-usage";

/** The class of the activity cell, which the hover line takes the place of. */
const ACTIVITY_CELL_CLASS = "footer-activity";

/** What the hover line opens with, naming what its figures are about. */
export const RESPONSE_USAGE_LABEL = "Response ";

/**
 * USAGE's duration, "1m 30s", or null when it has none to state: it is still
 * arriving, or the daemon knows no start for its span. A span that ends before
 * it began never gets here: its stamp is refused when it is read.
 */
export function responseDuration(usage: ResponseUsage): string | null {
  const { startedAtMs, settledAtMs } = usage;
  if (startedAtMs === null || settledAtMs === null) return null;
  return formatDurationCeil(settledAtMs - startedAtMs);
}

/**
 * The activity cell drawing USAGE: the label, the figure in its heat, then,
 * when the response settled, its duration (when known) and the live age of
 * its settle off TICKER.
 */
export function drawResponseUsageActivity(usage: ResponseUsage, ticker: Ticker): HTMLElement {
  const atMs = usage.settledAtMs;
  const duration = responseDuration(usage);
  const cell = document.createElement("div");
  cell.className = `pfooter-cell pfooter-grow ${ACTIVITY_CELL_CLASS}`;
  cell.setAttribute("data-tier", RESPONSE_USAGE_TIER);
  cell.setAttribute("data-arm", RESPONSE_USAGE_ARM);
  const line = document.createElement("span");
  line.className = "footer-response-usage";
  const tokens = document.createElement("span");
  tokens.className = "footer-response-tokens";
  tokens.textContent = `${usage.text} tokens`;
  tokens.style.color = usage.color;
  line.append(RESPONSE_USAGE_LABEL, tokens);
  if (duration !== null) {
    const took = document.createElement("span");
    took.className = "footer-response-duration";
    took.textContent = `, ${duration}`;
    line.append(took);
  }
  if (atMs !== null) line.append(footerAgeSpan(ticker, atMs));
  cell.append(line);
  log.debug("drawing the hovered response's usage in the activity cell", {
    operation: "footer.response-usage",
    context: { tokens: usage.text, settled: atMs !== null, duration },
  });
  return cell;
}

/**
 * Put the hovered response's USAGE in STRIP's activity cell, which every strip
 * draws exactly once. A strip with none, or with more than one, is a defect.
 */
export function overlayResponseUsage(strip: HTMLElement, usage: ResponseUsage, ticker: Ticker): void {
  const cells = [...strip.children].filter((cell) => cell.classList.contains(ACTIVITY_CELL_CLASS));
  if (cells.length !== 1) {
    log.error("the hovered response's usage found no single activity cell to stand in", {
      operation: "footer.response-usage.no-cell",
      context: { cells: cells.length, tokens: usage.text },
    });
    throw new Error(`footer: a strip drew ${String(cells.length)} activity cells, not one`);
  }
  cells[0].replaceWith(drawResponseUsageActivity(usage, ticker));
}
