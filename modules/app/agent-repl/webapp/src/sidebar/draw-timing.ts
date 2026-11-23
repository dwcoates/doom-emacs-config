/**
 * draw-timing — how long one roster push takes the rail: the draw itself, and
 * the time to the next frame, which carries the layout and paint the draw
 * caused.
 *
 * WHY IT EXISTS (2026-10-14). Workspace switching lagged most in the sidebar,
 * and every push redrew the whole roster (it now redraws only the sections
 * it changed, `roster.ts`) and re-measures its panels and bands; nothing
 * recorded what a draw cost or how big the rail was when it did. A draw that costs a frame or more is recorded at INFO with the rail's
 * size; a cheaper one at DEBUG, so an ordinary push adds nothing at `info`.
 * Every record names its cause and what the draw changed.
 */
import { createLogger } from "../log.js";
import { nextFrame } from "../frame.js";
import { queryAll } from "../dom.js";

const log = createLogger("webapp.sidebar.draw-timing");

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

/**
 * The phases a draw marks, in order; each is recorded as `<phase>_ms`, the
 * time since the mark before it.
 */
export const ROSTER_DRAW_PHASES = ["build", "mount", "layout", "hooks"] as const;

/** One phase of a roster draw. */
export type RosterDrawPhase = (typeof ROSTER_DRAW_PHASES)[number];

/** Marks the end of one phase of a draw. */
export type MarkPhase = (phase: RosterDrawPhase) => void;

/**
 * WHY A FRAME DREW (2026-11-23). At a handover the sidebar drew 92 times in
 * three minutes and 57 of those draws had no cause anyone could name. Every
 * draw now says what asked for it: how many roster pushes it coalesced, and
 * which local events (`RosterDrawCause.local`) asked for it besides.
 */
export interface RosterDrawCause {
  /** Roster pushes this frame coalesced: each a stream push the page took. */
  readonly pushes: number;
  /** Local events that asked for the frame, such as `merged_pages`, in the order they first asked. */
  readonly local: readonly string[];
}

/**
 * What one draw changed, as the drawer counted it (`roster.ts`
 * `RosterDraw`), or null for a draw that changed nothing a reader sees, which
 * ran no layout and no hooks.
 */
export interface RosterDrawCounts {
  /** The roster streams the push replaced, by name. */
  readonly changed_streams: readonly string[];
  readonly sections_drawn: number;
  readonly rows_drawn: number;
  readonly rows_removed: number;
  readonly rows_moved: number;
  readonly repainted: number;
}

/** What a draw answers: its counts, or `nothing` when it changed nothing and drew nothing. */
export type RosterDrawOutcome = { readonly drew: RosterDrawCounts | "whole" } | "nothing";

/**
 * Run DRAW, the whole of one roster push into RAIL, and record what it cost,
 * phase by phase: DRAW calls MARK at the end of each phase it runs. CAUSE is
 * what asked for the draw; DRAW answers what it changed.
 *
 * A DRAW THAT CHANGED NOTHING is recorded at DEBUG as such
 * (`sidebar.roster-draw-skipped`), with its cause, and waits for no frame:
 * it ran no layout and caused no paint.
 */
export function timeRosterDraw(
  rail: ParentNode,
  placement: string,
  cause: RosterDrawCause,
  draw: (mark: MarkPhase) => RosterDrawOutcome,
  env: DrawTimingEnv,
): void {
  const started = env.now();
  const phases: Partial<Record<`${RosterDrawPhase}_ms`, number>> = {};
  let last = started;
  const outcome = draw((phase) => {
    const at = env.now();
    phases[`${phase}_ms`] = Math.round(at - last);
    last = at;
  });
  const drawn = env.now();
  const causeContext = { pushes: cause.pushes, local_causes: [...cause.local] };
  if (outcome === "nothing") {
    const skipped = log.message("the roster push changed nothing the rail draws; it drew nothing in {draw_ms}ms", { draw_ms: Math.round(drawn - started) });
    log.log("debug", skipped, { operation: "sidebar.roster-draw-skipped", context: { placement, ...causeContext } });
    return;
  }
  const counts = outcome.drew === "whole" ? { whole: true } : { ...outcome.drew };
  env.frame(() => {
    const settled = env.now();
    const drawMs = Math.round(drawn - started);
    const totalMs = Math.round(settled - started);
    // draw_ms and total_ms are the message's named values, recorded under those names.
    const context = {
      placement,
      ...causeContext,
      ...counts,
      ...phases,
      frame_ms: Math.round(settled - drawn),
      rows: queryAll(rail, ".row").length,
      rail_nodes: queryAll(rail, "*").length,
    };
    const message = log.message("drew the workspace roster in {draw_ms}ms, {total_ms}ms to the next frame", { draw_ms: drawMs, total_ms: totalMs });
    log.log(totalMs >= ROSTER_DRAW_SLOW_MS ? "info" : "debug", message, { operation: "sidebar.roster-draw", context });
  });
}
