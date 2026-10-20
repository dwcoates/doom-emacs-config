/**
 * wall-latency — A LATENCY THAT STARTED IN ANOTHER PROCESS, ENDED HERE.
 *
 * Some latencies start where the page cannot see (Emacs takes a switch
 * keystroke, Emacs submits a prompt) and end in the page. The starting runtime
 * stamps the start on the host's wall clock (Emacs's `float-time`) and the
 * page reads the same clock (`Date.now`) at the end: the one clock the two
 * processes share.
 *
 * A duration that comes out negative means the wall clock stepped between the
 * start and the end. It measured nothing real, so it is recorded as a warning
 * and not reported (the daemon refuses negative durations).
 */
import { ReportedLatencyKind } from "../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import { log } from "./log.js";

/** Where a latency goes once measured (latency-report.ts `record`). */
export type RecordLatency = (kind: ReportedLatencyKind, seconds: number) => void;

/** Record KIND as taking from STARTEDATMS to ENDEDATMS, unless the clock stepped. */
export function recordWallLatency(
  kind: ReportedLatencyKind,
  startedAtMs: number,
  endedAtMs: number,
  record: RecordLatency,
): void {
  const seconds = (endedAtMs - startedAtMs) / 1000;
  if (seconds < 0) {
    log.warn("a latency ended before it began by the wall clock; the clock stepped, so it is not reported", {
      operation: "latency.clock-stepped",
      context: { kind: ReportedLatencyKind[kind], seconds },
    });
    return;
  }
  record(kind, seconds);
}
