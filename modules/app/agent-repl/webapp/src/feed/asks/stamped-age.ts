/**
 * stamped-age — a settled ask's instant as a ticking relative age ("3m ago").
 *
 * THE ONE DRAWING of an answered or resolved ask's "when": the permission, the
 * question and the cold gate each stamp the instant their ask settled, and a
 * shared helper keeps the three from drifting apart in wording or in which
 * clock they take. The age is a PRESENT clock (`tickWhileShown`): it stays true
 * after the ask's turn ends, so the finished turn's backstop leaves it counting.
 */
import { formatTickedAge } from "../../duration.js";
import { msOf } from "../../rpc/strict.js";
import type { RowContext } from "../renderers.js";
import { tickWhileShown } from "../ticking.js";

/** A span of CLASS_NAME reading `now - AT_MS` as "N ago", ticking while shown. */
export function stampedAge(
  atMs: bigint,
  path: string,
  rc: RowContext,
  className: string,
): HTMLElement {
  const at = msOf(atMs, path);
  const el = document.createElement("span");
  el.className = className;
  tickWhileShown(el, rc.ctx.ticker, (nowMs) => {
    el.textContent = `${formatTickedAge(nowMs - at)} ago`;
  });
  return el;
}
