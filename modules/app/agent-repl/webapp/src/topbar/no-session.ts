/**
 * The no-session cell — what a session-scoped control draws when the daemon
 * states no session fact for it.
 *
 * THE STRIP HAS ONE SHAPE (topbar.proto, FIXED SCHEMA AND ORGANIZATION; owner
 * ruling 2026-09-13). A control the daemon left ABSENT is not a control the
 * strip omits: the slot stays, and it draws a dash. Omitting it would move
 * every cell to its right the moment a workspace hibernated and move them all
 * back when it woke, which is the strip rearranging itself under the reader —
 * the exact thing the fixed schema exists to forbid.
 *
 * ONE IMPLEMENTATION FOR FOUR CELLS. The model selector, the effort selector,
 * and the permission-mode picker all say the same thing in the same
 * way, and four copies of one dash is how they would come to say it
 * differently.
 *
 * IT IS NOT A CONTROL. There is nothing to pick and nothing to open, so it is
 * a span with no click and no reveal — a picker that opened an empty list
 * would only invite the click that proves it is empty.
 */
import { log } from "../log.js";

/** The character the empty slot draws. An em dash, not a hyphen. */
export const NO_SESSION_DASH = "—";

/** Which cell is stating that it has no session fact. */
export type NoSessionCell = "model" | "effort" | "mode";

/** The class and the tooltip each cell's empty slot carries. */
const NO_SESSION_CELLS: Record<NoSessionCell, { className: string; title: string }> = {
  model: { className: "topbar-model", title: "no session is running, so no model is in force" },
  effort: { className: "topbar-effort", title: "no effort level is known to be in force" },
  mode: { className: "topbar-mode", title: "no session is running, so no permission mode is in force" },
};

/** The dash in CELL's own slot. */
export function drawNoSessionCell(cell: NoSessionCell): HTMLElement {
  const drawn = NO_SESSION_CELLS[cell];
  log.debug("drawing a cell with no session fact behind it", {
    operation: "topbar.no-session",
    context: { cell },
  });

  const element = document.createElement("span");
  element.className = drawn.className;
  element.setAttribute("data-no-session", cell);
  element.title = drawn.title;
  element.textContent = NO_SESSION_DASH;
  return element;
}
