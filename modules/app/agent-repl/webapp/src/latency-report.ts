/**
 * latency-report — THE PAGE REPORTS HOW LONG ITS INTERACTIONS TOOK.
 *
 * The page measures the latencies whose end it sees (switch-latency.ts) and
 * hands them here; this batches them to the daemon's always-on metrics
 * (ReportLatencies, endpoint_report_latencies.proto).
 *
 * A BATCH IS KEPT UNTIL THE DAEMON ACKNOWLEDGES IT. A report is sent a short
 * while after a latency is recorded, so a burst of switches is one call, and
 * again every time the page's stream re-attaches (a daemon that restarted).
 * A call that did not reach the daemon keeps its latencies for the next one;
 * nothing is retried on a timer of its own, so a daemon that is down costs no
 * calls until there is something new to say or the page re-attaches.
 *
 * Every answer the daemon gives ends the batch: `recorded`,
 * `partly_recorded` (a workspace closed meanwhile) and `metrics_off` are all
 * answers to a valid report, and a refusal (`workspace_ref_mismatch`) is a
 * defect that sending again would only repeat, so it is recorded loudly and
 * the batch dropped.
 */
import { create } from "@bufbuild/protobuf";
import {
  ReportedLatencySchema,
  ReportLatenciesRequestSchema,
  ReportLatenciesResponseSchema,
  type ReportedLatency,
  type ReportedLatencyKind,
  type ReportLatenciesResponse,
} from "../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import type { WorkspaceRef } from "../../proto/gen/ts/workspace/v1/workspace_pb";
import { log } from "./log.js";

/** How long after a latency is recorded the batch is sent. */
export const REPORT_DELAY_MS = 2_000;

/**
 * The most latencies held unsent. A daemon down for a long time while the user
 * keeps switching would otherwise grow the batch without bound; the oldest are
 * dropped, counted, and the count is recorded with the next send.
 */
export const MAX_PENDING_LATENCIES = 512;

/** What the reporter needs from the page. */
export interface LatencyReportDeps {
  /** The workspace every latency on this page happened in. */
  readonly workspace: WorkspaceRef;
  /** Send one batch; rejects when the call did not reach the daemon. */
  readonly send: (latencies: ReportedLatency[]) => Promise<ReportLatenciesResponse>;
  /** Run FN after MS; answers the cancel. */
  readonly after: (ms: number, fn: () => void) => () => void;
  /** Call FN every time the page's stream re-attaches; answers the stop. */
  readonly onAttached: (fn: () => void) => () => void;
}

/** The installed reporter. */
export interface LatencyReporter {
  /** Record one finished interaction of KIND that took SECONDS. */
  record(kind: ReportedLatencyKind, seconds: number): void;
  /** Stop: cancels a scheduled send and the re-attach watch. */
  dispose(): void;
}

/** Report the page's latencies through DEPS. */
export function installLatencyReport(deps: LatencyReportDeps): LatencyReporter {
  let pending: ReportedLatency[] = [];
  let dropped = 0;
  let inFlight = false;
  let cancelScheduled: (() => void) | undefined;

  const schedule = (): void => {
    cancelScheduled ??= deps.after(REPORT_DELAY_MS, () => {
      cancelScheduled = undefined;
      void flush("recorded");
    });
  };

  const flush = async (cause: "recorded" | "attached"): Promise<void> => {
    cancelScheduled?.();
    cancelScheduled = undefined;
    if (inFlight || pending.length === 0) return;
    inFlight = true;
    const batch = pending;
    pending = [];
    let answered = false;
    try {
      const response = await deps.send(batch);
      answered = true;
      const result = response.result;
      if (result.case === "success") {
        log.debug("the daemon took this page's latencies", {
          operation: "latency.reported",
          context: { cause, latencies: batch.length, outcome: String(result.value.outcome.case), dropped },
        });
        dropped = 0;
        return;
      }
      log.error(`the daemon refused this page's latencies: ${String(result.case === "error" ? result.value.cause.case : result.case)}`, {
        operation: "latency.report-refused",
        context: { cause, latencies: batch.length, refusal: String(result.case === "error" ? result.value.cause.case : result.case) },
      });
    } catch (err) {
      // The call did not reach the daemon (its transport failure is already
      // recorded by callUnary): the batch goes back, oldest first.
      pending = [...batch, ...pending];
      trim();
      log.info(`this page's latencies wait for the next report: ${String(err)}`, {
        operation: "latency.report-deferred",
        context: { cause, latencies: pending.length },
      });
    } finally {
      inFlight = false;
      // LATENCIES RECORDED WHILE THE CALL WAS OUT go next, once the daemon has
      // answered; after a failure they wait like the rest.
      if (answered && pending.length > 0) schedule();
    }
  };

  const trim = (): void => {
    const excess = pending.length - MAX_PENDING_LATENCIES;
    if (excess <= 0) return;
    pending = pending.slice(excess);
    dropped += excess;
    log.warn(`dropped the ${excess.toString()} oldest unsent latencies; the daemon has not taken a report for a while`, {
      operation: "latency.report-overflow",
      context: { dropped, held: pending.length },
    });
  };

  const stopAttached = deps.onAttached(() => {
    void flush("attached");
  });

  return {
    record(kind, seconds) {
      pending.push(create(ReportedLatencySchema, { kind, workspace: deps.workspace, seconds }));
      trim();
      schedule();
    },
    dispose() {
      cancelScheduled?.();
      cancelScheduled = undefined;
      stopAttached();
    },
  };
}

/** The request a batch is sent as. */
export function reportLatenciesRequest(latencies: ReportedLatency[]) {
  return create(ReportLatenciesRequestSchema, { latencies });
}

/** The response schema callUnary checks a report's answer against. */
export const REPORT_LATENCIES_RESPONSE = ReportLatenciesResponseSchema;
