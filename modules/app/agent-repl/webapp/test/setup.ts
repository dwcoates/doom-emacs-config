import { beforeEach, vi } from "vitest";
import {
  ForwardingLogger,
  bindLogContext,
  resetLoggingForTests,
  setLogger,
} from "../src/log.js";

/**
 * Production installs the ClientLog-forwarding logger before runtime work
 * begins. Reproduce that invariant for every unit test without emitting
 * diagnostics to the test process and without any rpc leaving the suite: the
 * sink resolves immediately and the console function is a no-op.
 * Logging-specific tests may reset or replace this instance to exercise the
 * initialization and routing contracts explicitly.
 */
beforeEach(() => {
  // Files share one worker environment when the suite runs un-isolated, so a
  // fake clock installed by one file would otherwise still be frozen when the
  // next file's first test awaits a real timer. Every test starts on the real
  // clock; a file that wants a fake one installs it in its own hook, which
  // runs after this one.
  vi.useRealTimers();
  // The same jsdom document serves every test in a worker, so a node one test
  // appended would still be found by the next one's query — and a leftover
  // `data-unit` or `data-component` is exactly what the routing code looks
  // for. Every test starts on an empty page. Files that ask for no dom
  // environment have no document to empty.
  if (typeof document !== "undefined") document.body.replaceChildren();
  resetLoggingForTests();
  setLogger(new ForwardingLogger(async () => {}, () => {}));
  bindLogContext({ connection_id: "test-connection" });
});
