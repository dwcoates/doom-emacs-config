import { beforeAll } from "vitest";
import { ForwardingLogger, bindLogContext, resetLoggingForTests, setLogger } from "../../src/log.js";

/**
 * THE LOGGER, INSTALLED BEFORE THE FIRST `beforeAll` MOUNTS.
 *
 * `test/setup.ts` reproduces production's "the forwarding logger is installed
 * before any runtime work" invariant in a `beforeEach`, which is early enough
 * for a suite that mounts inside `it`. This layer mounts ONCE per file in
 * `beforeAll` — one real daemon, one page, many scenarios — and `beforeAll`
 * runs before any `beforeEach`, so without this the very first `log()` on the
 * boot path throws "the webapp logger is not installed".
 *
 * The shared `beforeEach` still runs afterwards and re-installs an equivalent
 * logger for each test; this only makes the same guarantee hold one hook
 * earlier. The sink resolves immediately and the console function is a no-op,
 * so no diagnostic leaves the suite.
 */
beforeAll(() => {
  resetLoggingForTests();
  setLogger(new ForwardingLogger(async () => {}, () => {}));
  bindLogContext({ connection_id: "webapp-layer-connection" });
});
