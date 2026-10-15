/**
 * failure-sink — the one failure sink a test reads what was filed from.
 *
 * It keeps every filed kind, and beside it the arm each one carried, so a test
 * asserts either the whole report or just which cards were filed. It retracts
 * nothing and mutes nothing (a collecting sink has no row to hold back).
 */
import type { FailureKind } from "../../proto/gen/ts/frontend/v1/failure_pb";
import type { FailureSink } from "../src/failure/sink.js";

/** A sink that keeps what was filed. */
export interface RecordingFailures extends FailureSink {
  /** Every kind filed, in order. */
  readonly reports: FailureKind[];
  /** The arm of every kind filed, in order; "unset" for a kind with none. */
  readonly cases: string[];
}

/** A recording sink with nothing filed. */
export function recordingFailures(): RecordingFailures {
  const reports: FailureKind[] = [];
  const cases: string[] = [];
  return {
    reports,
    cases,
    report: (kind) => {
      reports.push(kind);
      cases.push(kind.kind.case ?? "unset");
    },
    retract: () => undefined,
  };
}
