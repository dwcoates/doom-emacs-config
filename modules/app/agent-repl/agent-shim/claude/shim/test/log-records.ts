/**
 * The canonical logger's records, decoded back out of the suite's mocked sink.
 *
 * `test/log-setup.ts` replaces `node:fs`'s `writeSync` with a mock for the whole
 * suite, so every JSONL record the canonical logger persists lands in that
 * mock's call list as `(fd, bytes, offset, length)`. This is the ONE decoder of
 * those calls: a suite that asserts on what was logged reads it through here,
 * never through its own copy (`test/log-records.test.ts` guards that).
 */
import { writeSync } from "node:fs";
import { vi } from "vitest";

/** A persisted log record, as the canonical logger writes one. */
export interface LogRecord {
  readonly level: string;
  readonly message: string;
  readonly context: Record<string, unknown>;
  readonly [field: string]: unknown;
}

/** One call the logger made on the mocked durable sink. */
type SinkCall = [fd: number, bytes: Buffer, offset: number, length: number];

/** The mocked sink's current call count: a mark to read records since. */
export function logSinkMark(): number {
  return vi.mocked(writeSync).mock.calls.length;
}

/** Every record the canonical logger wrote since the sink stood at `before`. */
export function logRecordsSince(before: number): LogRecord[] {
  const calls = vi.mocked(writeSync).mock.calls.slice(before) as unknown as SinkCall[];
  return calls.map(
    ([, bytes, offset, length]) => JSON.parse(bytes.subarray(offset, offset + length).toString("utf8")) as LogRecord,
  );
}

/** Every record the canonical logger wrote while `act` ran. */
export function logRecordsDuring(act: () => unknown): LogRecord[] {
  const before = logSinkMark();
  act();
  return logRecordsSince(before);
}
