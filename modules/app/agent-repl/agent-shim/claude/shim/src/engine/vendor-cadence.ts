/**
 * engine/vendor-cadence.ts — when the vendor's message stream pauses, and
 * whether the shim was the one holding it.
 *
 * WHY THIS EXISTS. On 2026-10-08 (workspace 096682deb5314ad3, 15:31:25-28) the
 * shim enqueued the vendor's text deltas in clumps: ~11 rows within 1ms, then
 * nothing for ~450ms, on a steady ~530ms period. The rows-enqueued records
 * show WHEN the clumps reached the store writer, but not where they formed.
 * The SDK reads the vendor child's stdout line by line as it arrives
 * (readline over the pipe, no buffering of its own), and the shim's loop only
 * holds the next read while it folds a message or while the store writer's
 * backlog is past its high-water mark (which logs a WARN, and none was
 * logged). So a clump forms either upstream of the pipe (the vendor child or
 * the API) or in a shim whose event loop was busy and silent.
 *
 * This tells the two apart on the record. The loop says when it starts
 * waiting on the vendor for the next message and when one arrives; a wait of
 * VENDOR_PAUSE_MS or longer is logged with how busy the shim's event loop was
 * across it. A long wait at near-zero utilization is the vendor's silence: the
 * shim was idle and the next message simply had not been written.
 */
import { performance, type EventLoopUtilization } from "node:perf_hooks";
import { bindLog } from "../log.js";

const LOGGER = bindLog({ component: "shim-vendor-cadence", operation: "shim.engine.vendor_cadence" });

/**
 * The shortest wait on the vendor that is recorded. Inside a clump messages
 * arrive within a millisecond of each other, and the vendor's unclumped
 * cadence between deltas was 40-60ms on the 2026-10-08 logs; the clumps'
 * silences were 150-500ms. 100ms records every such silence and none of the
 * ordinary cadence.
 */
export const VENDOR_PAUSE_MS = 100;

/** How busy the shim's event loop was over a span (node:perf_hooks). */
export interface LoopMeter {
  /** A reading to measure a later span from. */
  mark(): EventLoopUtilization;
  /** The fraction (0 to 1) of the time since MARK the event loop spent running code. */
  utilizationSince(mark: EventLoopUtilization): number;
}

/** The process's own event loop meter. */
const NODE_LOOP_METER: LoopMeter = {
  mark: () => performance.eventLoopUtilization(),
  utilizationSince: (mark) => performance.eventLoopUtilization(mark).utilization,
};

/** One vendor message loop's record of its waits on the vendor. */
export class VendorCadence {
  private waitingSince: number | undefined;
  private waitMark: EventLoopUtilization | undefined;
  /** Messages received since the last recorded pause: the clump it ended. */
  private sincePause = 0;

  constructor(
    private readonly nowMs: () => number,
    private readonly meter: LoopMeter = NODE_LOOP_METER,
  ) {}

  /** The loop is done with a message and is asking the vendor for the next. */
  waiting(): void {
    this.waitingSince = this.nowMs();
    this.waitMark = this.meter.mark();
  }

  /**
   * A message arrived. A wait of VENDOR_PAUSE_MS or longer since `waiting()`
   * is recorded, with the messages that arrived before it since the previous
   * recorded pause. The first message of a loop had no measured wait.
   */
  received(): void {
    const since = this.waitingSince;
    const mark = this.waitMark;
    this.waitingSince = undefined;
    this.waitMark = undefined;
    if (since !== undefined && mark !== undefined) {
      const waitedMs = this.nowMs() - since;
      if (waitedMs >= VENDOR_PAUSE_MS) {
        LOGGER.debug(
          {
            waited_ms: waitedMs,
            event_loop_utilization: Math.round(this.meter.utilizationSince(mark) * 1000) / 1000,
            messages_before: this.sincePause,
          },
          "the vendor stream was silent while the shim waited on it",
        );
        this.sincePause = 0;
      }
    }
    this.sincePause += 1;
  }
}
