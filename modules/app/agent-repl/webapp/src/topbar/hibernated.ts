/**
 * The hibernated cell — the topbar's ONE whole-view state.
 *
 * A HIBERNATED WORKSPACE HAS NO SESSION, so the daemon can resolve none of the
 * session-scoped elements and sends none of them: `TopbarView.hibernated` is
 * set instead, and the strip draws the account, the connectivity glyph and the
 * title as ever, plus this cell. That is the whole difference — the strip is
 * not a shorter version of itself, it is the same strip stating one thing the
 * full one never has to.
 *
 * THE AGE TICKS CLIENT-SIDE, from the instant the wire carries. Clocks tick
 * client-side everywhere here (see feed/ticking): the topbar is republished on
 * facts, never on a clock, so a duration composed daemon-side would freeze at
 * whatever it read when the park was installed.
 */
import type { TopbarHibernated } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { formatTickedAge } from "../duration.js";
import { tick } from "../feed/ticking.js";
import { log } from "../log.js";
import type { TopbarContext } from "./context.js";

/** The literal word the strip draws for the state. */
export const HIBERNATED_LABEL = "hibernated";

/** The hibernated cell, with its age ticking from SINCE. */
export function drawTopbarHibernated(
  u: TopbarHibernated,
  tc: TopbarContext,
): HTMLElement {
  log.debug("drawing the hibernated cell", { operation: "topbar.hibernated" });

  const cell = document.createElement("span");
  cell.className = "topbar-hibernated";
  cell.setAttribute("data-hibernated", "");
  cell.title = "this workspace's session was stood down after being left idle";

  const since = Number(u.sinceMs);
  tick(cell, tc.ctx.ticker, (nowMs) => {
    cell.textContent = `${HIBERNATED_LABEL} ${formatTickedAge(nowMs - since)}`;
  });
  return cell;
}
