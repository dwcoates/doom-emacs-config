/**
 * drawn-status — the record of which status a surface DREW.
 *
 * WHY IT EXISTS. The footer strip and the sidebar row each draw a workspace's
 * status from their own stream, and a disagreement between them (owner's
 * report, 2026-10-08: the footer read "agent repl fault" while the rail and
 * tab read working) could only be reconstructed from the DAEMON's records of
 * what it published -- never from what the page actually put on screen. A
 * client verdict, which overrides the daemon's view on the strip, left no
 * trace at all.
 *
 * So every surface states, at INFO, each CHANGE of the status it draws: one
 * record per transition, keyed per drawn object, with the previous status
 * beside the new one. A redraw that draws the same status says nothing.
 */
import { log } from "./log.js";

/** Where the drawn status came from. */
export type DrawnStatusSource = "daemon" | "client_verdict";

/** One drawn status: the arm, its step when it has one, and its source. */
export interface DrawnStatus {
  readonly arm: string;
  readonly substatus?: string;
  readonly source: DrawnStatusSource;
}

/** Records one surface's drawn-status changes, per drawn object. */
export interface DrawnStatusLog {
  /**
   * Note that the object KEY now draws DRAWN. Records at INFO only when it
   * differs from what KEY drew last, including the first draw.
   */
  note(key: string, drawn: DrawnStatus): void;
}

/** The comparable spelling of a drawn status. */
function signature(drawn: DrawnStatus): string {
  return `${drawn.source}/${drawn.arm}/${drawn.substatus ?? ""}`;
}

/**
 * A drawn-status log for one SURFACE, recording under OPERATION.
 *
 * The memory of what each key drew is this log's own, so one log belongs to
 * one mounted surface and goes with it.
 */
export function createDrawnStatusLog(surface: string, operation: string): DrawnStatusLog {
  const last = new Map<string, DrawnStatus>();
  return {
    note(key, drawn) {
      const previous = last.get(key);
      if (previous !== undefined && signature(previous) === signature(drawn)) return;
      last.set(key, drawn);
      log.info(`the ${surface} now draws ${drawn.arm}`, {
        operation,
        context: {
          key,
          arm: drawn.arm,
          substatus: drawn.substatus,
          source: drawn.source,
          previous_arm: previous?.arm,
          previous_substatus: previous?.substatus,
          previous_source: previous?.source,
        },
      });
    },
  };
}
