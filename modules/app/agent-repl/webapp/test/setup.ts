import { beforeEach } from "vitest";
import {
  ForwardingLogger,
  bindLogContext,
  resetLoggingForTests,
  setLogger,
} from "../src/wslog.js";

/**
 * Production installs the daemon-forwarding logger before runtime work begins.
 * Reproduce that invariant for every unit test without emitting diagnostics to
 * the test process. Logging-specific tests may reset or replace this instance
 * to exercise the initialization and routing contracts explicitly.
 */
beforeEach(() => {
  resetLoggingForTests();
  setLogger(new ForwardingLogger(() => true, () => {}));
  bindLogContext({ connection_id: "test-connection" });
});
