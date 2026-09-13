import { beforeEach } from "vitest";
import {
  ForwardingLogger,
  bindLogContext,
  resetLoggingForTests,
  setLogger,
} from "../src/log.js";
import { clearClientFailures } from "../src/rpc/link.js";
import { installResizeObserver } from "./resize-observer.js";

/**
 * jsdom implements no `ResizeObserver` (it performs no layout), and the feed
 * mount subscribes one to its scroll box so a footer settling after a render
 * cannot leave the tail below the fold. Installed here, once per environment,
 * because it is a missing CAPABILITY of the environment rather than a seam in
 * the app -- the same standing the harness gives `Element.scrollIntoView`.
 */
installResizeObserver();

/**
 * Production installs the ClientLog-forwarding logger before runtime work
 * begins. Reproduce that invariant for every unit test without emitting
 * diagnostics to the test process and without any rpc leaving the suite: the
 * sink resolves immediately and the console function is a no-op.
 * Logging-specific tests may reset or replace this instance to exercise the
 * initialization and routing contracts explicitly.
 */
beforeEach(() => {
  resetLoggingForTests();
  setLogger(new ForwardingLogger(async () => "accepted", () => {}));
  bindLogContext({ connection_id: "test-connection" });
  // THE CLIENT'S LINK VERDICT IS PAGE-WIDE STATE, and the unit suite runs
  // un-isolated: a file that reported a transport failure would otherwise
  // leave the next file's footer drawing the client's disconnected strip.
  // Production has one page and one verdict; each test gets the same fresh
  // start. AFTER the logger, because clearing a standing verdict logs.
  clearClientFailures();
});
