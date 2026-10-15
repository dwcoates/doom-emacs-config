/**
 * draw-timing — how long one roster push takes the rail: the draw itself, and
 * the time to the next frame, which carries the layout and paint the draw
 * caused.
 *
 * WHY IT EXISTS (2026-10-14). Workspace switching lagged most in the sidebar,
 * and every push redraws the whole roster and re-measures its panels and
 * bands; nothing recorded what a draw cost or how big the rail was when it
 * did. A draw that costs a frame or more is recorded at INFO with the rail's
 * size; a cheaper one at DEBUG, so an ordinary push adds nothing at `info`.
 */
import { log } from "../log.js";
import { nextFrame } from "../frame.js";

/** A draw at or past one frame's budget is recorded at INFO. */
export const ROSTER_DRAW_SLOW_MS = 16;

/** What the timing needs from its environment, injectable for tests. */
export interface DrawTimingEnv {
  /** Monotonic milliseconds. */
  readonly now: () => number;
  /** Schedules a callback for the next animation frame. */
  readonly frame: (callback: () => void) => void;
}

/** The default environment: the page itself. */
export function drawTimingEnv(): DrawTimingEnv {
  return { now: () => performance.now(), frame: nextFrame };
}

/** Run DRAW, the whole of one roster push into RAIL, and record what it cost. */
export function timeRosterDraw(rail: ParentNode, placement: string, draw: () => void, env: DrawTimingEnv): void {
  const started = env.now();
  draw();
  const drawn = env.now();
  env.frame(() => {
    const settled = env.now();
    const drawMs = Math.round(drawn - started);
    const totalMs = Math.round(settled - started);
    const context = {
      placement,
      draw_ms: drawMs,
      frame_ms: Math.round(settled - drawn),
      total_ms: totalMs,
      rows: rail.querySelectorAll(".row").length,
      rail_nodes: rail.querySelectorAll("*").length,
    };
    const message = `drew the workspace roster in ${drawMs}ms, ${totalMs}ms to the next frame`;
    if (totalMs >= ROSTER_DRAW_SLOW_MS) log.info(message, { operation: "sidebar.roster-draw", context });
    else log.debug(message, { operation: "sidebar.roster-draw", context });
  });
}
