/**
 * log-capture — read back the records a drawing path actually emitted, and at
 * which level.
 *
 * A record a person's action produces has to survive the deployment's `info`
 * threshold, and the only place that fact is observable is the forwarded
 * `ClientLogRecord`'s own level arm. The unit setup installs a forwarding
 * logger whose sink discards; this one keeps what it was handed.
 */
import type { ClientLogRecord } from "../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";
import type { ClientLogLevel } from "../src/log.js";
import * as logModule from "../src/log.js";

export interface LogCapture {
  logger: logModule.ForwardingLogger;
  sent: ClientLogRecord[];
}

/**
 * The log module a capture installs into. It is this file's own by default; a
 * test that imports a FRESH module graph (`vi.resetModules`) hands in the
 * fresh `log` module, which is the one the fresh graph logs through.
 */
export type LogModule = typeof logModule;

/**
 * Replace the suite's discarding logger with one whose records the test reads.
 *
 * MINIMUM is the deployment threshold the capture stands in for, and it
 * defaults to the contract's `info` because that is what the deployed page
 * runs at: a test that says nothing about the level is asking whether the
 * record survives the real threshold. A test whose subject IS a debug record
 * passes "debug", the level a page booted with `log_level=debug` carries — a
 * capture that admitted every level by default would report a debug record as
 * forwarded on a deployment that drops it.
 *
 * LOGGING is the log module to install into (see `LogModule`).
 */
export function captureLogRecords(minimum: ClientLogLevel = "info", logging: LogModule = logModule): LogCapture {
  const { ForwardingLogger, bindLogContext, resetLoggingForTests, setLogger } = logging;
  const sent: ClientLogRecord[] = [];
  const logger = new ForwardingLogger(
    async (record) => {
      sent.push(record);
      return "accepted";
    },
    () => undefined,
    {},
    minimum,
  );
  resetLoggingForTests();
  setLogger(logger);
  bindLogContext({ connection_id: "test-connection" });
  return { logger, sent };
}

/** Every record CAPTURE forwarded: the throttle's window released, the sink's promises settled. */
export async function forwardedRecords(capture: LogCapture): Promise<ClientLogRecord[]> {
  capture.logger.flush();
  await Promise.resolve();
  return capture.sent;
}

/**
 * The one record for OPERATION among RECORDS. Absence throws rather than
 * reading as an absent field: "the record never went" and "the record went
 * without it" are different facts and only one of them is under test.
 */
export function recordFor(records: readonly ClientLogRecord[], operation: string): ClientLogRecord {
  const found = records.find((record) => record.operation === operation);
  if (found === undefined) throw new Error(`no forwarded record for ${operation}`);
  return found;
}

/** The one forwarded record for OPERATION (`forwardedRecords`, `recordFor`). */
export async function forwardedRecord(capture: LogCapture, operation: string): Promise<ClientLogRecord> {
  return recordFor(await forwardedRecords(capture), operation);
}
