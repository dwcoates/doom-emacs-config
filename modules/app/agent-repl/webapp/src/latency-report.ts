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
 *
 * THE REPORTER ENDS WHEN THE PAGE GOES QUIET. A quiet page (a workspace a
 * handover moved to a successor) sends nothing more on its client, ever, so a
 * report scheduled after it would only be refused by the page itself. The
 * latencies it still holds have no daemon left to take them: they are dropped
 * with a record of how many, and so is every latency recorded after.
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
import type { DaemonContext } from "./rpc/context.js";
import { callUnary } from "./rpc/unary.js";

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
  /**
   * The workspace every latency on this page happened in. Absent on the
   * sidebar page, which belongs to no workspace: each of its latencies names
   * its own (`record`'s WORKSPACE).
   */
  readonly workspace?: WorkspaceRef;
  /** Send one batch; rejects when the call did not reach the daemon. */
  readonly send: (latencies: ReportedLatency[]) => Promise<ReportLatenciesResponse>;
  /** Run FN after MS; answers the cancel. */
  readonly after: (ms: number, fn: () => void) => () => void;
  /** Call FN every time the page's stream re-attaches; answers the stop. */
  readonly onAttached: (fn: () => void) => () => void;
  /** Call FN once, when the page goes quiet; answers the stop. */
  readonly onQuiesced: (fn: () => void) => () => void;
}

/** The installed reporter. */
export interface LatencyReporter {
  /**
   * Record one finished interaction of KIND that took SECONDS, in WORKSPACE,
   * else the page's own. A latency that names neither is a defect, and
   * throws.
   */
  record(kind: ReportedLatencyKind, seconds: number, workspace?: WorkspaceRef): void;
  /** Stop: cancels a scheduled send and the re-attach watch. */
  dispose(): void;
}

/** Report the page's latencies through DEPS. */
export function installLatencyReport(deps: LatencyReportDeps): LatencyReporter {
  let pending: ReportedLatency[] = [];
  let dropped = 0;
  let inFlight = false;
  let ended = false;
  let cancelScheduled: (() => void) | undefined;

  /** Drop COUNT latencies the quiet page can no longer send, saying why. */
  const dropAtEnd = (count: number, why: string): void => {
    log.info(`dropped ${count.toString()} latencies ${why}: the page went quiet, so no daemon is left to take them`, {
      operation: "latency.report-ended",
      context: { latencies: count, dropped },
    });
  };

  const schedule = (): void => {
    cancelScheduled ??= deps.after(REPORT_DELAY_MS, () => {
      cancelScheduled = undefined;
      void flush("recorded");
    });
  };

  const flush = async (cause: "recorded" | "attached"): Promise<void> => {
    cancelScheduled?.();
    cancelScheduled = undefined;
    if (ended || inFlight || pending.length === 0) return;
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
      // A CALL OUT WHEN THE PAGE WENT QUIET has nowhere to wait for.
      if (ended) {
        dropAtEnd(batch.length, "whose report failed");
        return;
      }
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

  const end = (): void => {
    if (ended) return;
    ended = true;
    cancelScheduled?.();
    cancelScheduled = undefined;
    stopAttached();
    if (pending.length > 0) dropAtEnd(pending.length, "held unsent");
    pending = [];
  };
  const stopQuiesced = deps.onQuiesced(end);

  return {
    record(kind, seconds, workspace = deps.workspace) {
      if (workspace === undefined) {
        throw new Error(`latency-report: a ${String(kind)} latency names no workspace, and the page has none of its own`);
      }
      if (ended) {
        dropAtEnd(1, `of kind ${String(kind)} recorded after the end`);
        return;
      }
      pending.push(create(ReportedLatencySchema, { kind, workspace, seconds }));
      trim();
      schedule();
    },
    dispose() {
      cancelScheduled?.();
      cancelScheduled = undefined;
      stopAttached();
      stopQuiesced();
    },
  };
}

/**
 * Report this page's latencies to the daemon CTX talks to: as WORKSPACE's
 * when given, else each naming its own (the sidebar page's). Every page that
 * reports latencies installs its reporter here, so each sends its batch,
 * waits before sending, and resends on a re-attach alike.
 */
export function installDaemonLatencyReport(ctx: DaemonContext, workspace?: WorkspaceRef): LatencyReporter {
  return installLatencyReport({
    ...(workspace === undefined ? {} : { workspace }),
    send: (batch) =>
      callUnary(ctx, "ReportLatencies", (client) => client.reportLatencies(reportLatenciesRequest(batch)), REPORT_LATENCIES_RESPONSE),
    after: (ms, fn) => {
      const timer = window.setTimeout(fn, ms);
      return () => window.clearTimeout(timer);
    },
    onAttached: (fn) => ctx.streams.onAttached(fn),
    onQuiesced: (fn) => ctx.onQuiesced(fn),
  });
}

/** The request a batch is sent as. */
export function reportLatenciesRequest(latencies: ReportedLatency[]) {
  return create(ReportLatenciesRequestSchema, { latencies });
}

/** The response schema callUnary checks a report's answer against. */
export const REPORT_LATENCIES_RESPONSE = ReportLatenciesResponseSchema;
