/**
 * The cold-gate cell — the topbar's second whole-view state, drawn exactly
 * where the hibernated one is and for the same reason.
 *
 * A COLD-GATED WORKSPACE HAS NO SESSION EITHER. The shim answered `cold` to
 * the session start, so nothing was created and the daemon can resolve none
 * of the session-scoped elements: `TopbarView.cold_gate` is set instead, and
 * the strip draws the account, the connectivity glyph and the title as ever,
 * plus this cell. Before the field existed the strip published NOTHING while
 * a gate stood, which is the blank topbar the owner saw.
 *
 * THE CHOICE IS NOT HERE. The feed's gate card is where the gate is answered
 * — pay, clear or compact — and this cell only names the state and its cost,
 * so one question has one place to be answered.
 *
 * THE AGE TICKS CLIENT-SIDE, from the instant the wire carries, exactly as the
 * hibernated cell's does.
 */
import type { TopbarColdGate } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { formatTickedAge } from "../duration.js";
import { tokenCountOf } from "../feed/asks/cold-gate.js";
import { tick } from "../feed/ticking.js";
import { formatTokens } from "../format.js";
import { log } from "../log.js";
import type { TopbarContext } from "./context.js";

/** The literal words the strip draws for the state. */
export const COLD_GATE_LABEL = "cold context";

/** The cold-gate cell: the label, the context figure, and the ticking age. */
export function drawTopbarColdGate(u: TopbarColdGate, tc: TopbarContext): HTMLElement {
  log.debug("drawing the cold-gate cell", { operation: "topbar.cold-gate" });

  const cell = document.createElement("span");
  cell.className = "topbar-cold-gate";
  cell.setAttribute("data-cold-gate", "");
  cell.title = "this session was refused cold; answer the gate in the feed to resume it";

  // THE SAME READER THE GATE CARD USES, so one count cannot be validated two
  // ways: a negative or unsafe token count is malformed wherever it lands.
  const tokens = formatTokens(tokenCountOf(u.contextTokens, "TopbarView.cold_gate.context_tokens"));
  const since = Number(u.sinceMs);
  tick(cell, tc.ctx.ticker, (nowMs) => {
    cell.textContent = `${COLD_GATE_LABEL} ${tokens} ${formatTickedAge(nowMs - since)}`;
  });
  return cell;
}
