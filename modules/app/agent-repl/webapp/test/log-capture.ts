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
import { ForwardingLogger, bindLogContext, resetLoggingForTests, setLogger } from "../src/log.js";

export interface LogCapture {
  logger: ForwardingLogger;
  sent: ClientLogRecord[];
}

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
 */
export function captureLogRecords(minimum: ClientLogLevel = "info"): LogCapture {
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

/**
 * The one forwarded record for OPERATION. Absence throws rather than reading
 * as an absent level: "the record never went" and "the record went at the
 * wrong level" are different facts and only one of them is under test.
 */
export async function forwardedRecord(
  capture: LogCapture,
  operation: string,
): Promise<ClientLogRecord> {
  capture.logger.flush();
  await Promise.resolve();
  const found = capture.sent.find((record) => record.operation === operation);
  if (found === undefined) throw new Error(`no forwarded record for ${operation}`);
  return found;
}
