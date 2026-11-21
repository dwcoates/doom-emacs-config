/**
 * log-spy — a test's view of the log calls a subject made at one level,
 * whichever module's logger made them.
 *
 * Every module logs through its own scoped logger (src/log.ts
 * `createLogger`), so there is no one object to spy on. A spy here sees each
 * call as the call wrote it: its rendered message, and its options with the
 * template's named values in the context under their names. The page-state
 * reset (`resetLoggingForTests`) drops it before the next test.
 */
import { vi, type Mock } from "vitest";
import { observeLogCallsForTests, type ClientLogLevel, type LogOptions } from "../src/log.js";

/** The calls at LEVEL from now on, as a mock: `[message, options]` per call. */
export function spyLog(level: ClientLogLevel): Mock<(message: string, options: LogOptions) => void> {
  const spy = vi.fn<(message: string, options: LogOptions) => void>();
  observeLogCallsForTests(level, spy);
  return spy;
}
