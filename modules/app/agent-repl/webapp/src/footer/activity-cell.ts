/**
 * activity-cell — the strip's activity cell as an element: the one elastic
 * segment every strip draws exactly once, whichever line it holds (the
 * daemon's tiers, the client's own verdict, or a page-local line standing in
 * for the daemon's).
 *
 * ONE SHAPE FOR EVERY LINE. Each drawer builds its cell here, and each line
 * that stands in for the daemon's replaces the strip's one cell here, so a
 * cell drawn by hand with another class list cannot be missed by an overlay.
 */
import { createLogger } from "../log.js";

const log = createLogger("webapp.footer.activity-cell");

/** The class every activity cell wears, and the one an overlay looks for. */
export const ACTIVITY_CELL_CLASS = "footer-activity";

/** An empty activity cell: elastic, ellipsized, and findable by an overlay. */
export function activityCell(): HTMLElement {
  const cell = document.createElement("div");
  cell.className = `pfooter-cell pfooter-grow ${ACTIVITY_CELL_CLASS}`;
  return cell;
}

/**
 * Put CELL in place of STRIP's activity cell, which every strip draws exactly
 * once. A strip with none, or with more than one, is a defect: recorded at
 * error under OPERATION with CONTEXT and the count found, then thrown, with
 * STRIP left as it was.
 */
export function replaceActivityCell(
  strip: HTMLElement,
  cell: HTMLElement,
  operation: string,
  context: Record<string, unknown>,
): void {
  const cells = [...strip.children].filter((child) => child.classList.contains(ACTIVITY_CELL_CLASS));
  if (cells.length !== 1) {
    log.error("a line standing in for the activity found no single activity cell", {
      operation,
      context: { ...context, cells: cells.length },
    });
    throw new Error(`footer: a strip drew ${String(cells.length)} activity cells, not one`);
  }
  cells[0].replaceWith(cell);
}
