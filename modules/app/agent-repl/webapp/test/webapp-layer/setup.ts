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
 * THIS PROJECT DOES NOT LOAD `test/setup.ts` (see
 * `vitest.webapp-layer.config.ts`): its `beforeEach` would land AFTER the
 * mount and replace the logger the mount installed, so a file that mounts with
 * production's real forwarding sink would forward nothing from its second test
 * onwards, with the page's bound session identity cleared besides.
 *
 * WHAT THIS INSTALLS IS A QUIET LOGGER, AND THE DEFAULT IS DELIBERATE. The
 * sink resolves immediately and the console function is a no-op, so a file
 * that says nothing about logging emits no `ClientLog` traffic at all. A file
 * that IS about logging opts in with `bootLayer({ clientLog: true })`, which
 * hands the mount production's own sink (`installClientLogSink`) for the whole
 * file.
 *
 * THE OPT-IN IS A MEASUREMENT, NOT A PRECAUTION. All eleven layer files were
 * timed against a real world both ways (vitest's own per-file `tests` time,
 * 2026-09-09, areas running as they normally do):
 *
 *   file                      quiet     forwarding
 *   merge-tabs                  90ms        186ms
 *   query-death                205ms        254ms
 *   proof-of-life              225ms        285ms
 *   roster                     274ms        340ms
 *   panels                     277ms        351ms
 *   refusals                   324ms        448ms
 *   surfaces                   337ms        487ms
 *   restart-handover           429ms        462ms
 *   subfeeds                   499ms        867ms
 *   cards                      594ms       1045ms
 *   feed-families             1521ms       2422ms
 *   ------------------------------------------------
 *   all eleven                4775ms       7147ms   (+50%)
 *   the Go areas, wall         14.1s        19.1s   (+35%)
 *
 * Every file got slower and the two heaviest gained ~0.5s and ~0.9s, against a
 * 900ms per-test bound this layer does not widen. It is the same term
 * `test/integration/client-log.integration.test.ts` measured against the fake
 * daemon (a boot alone emits ~56 records, each its own unary round trip) —
 * smaller here, because this layer is eleven serial files rather than 1600
 * tests, but not small enough to spend on every file for nothing. So:
 * forwarding is per-file and opt-in, and `client-log.layer.test.ts` — the file
 * that proves a browser record reaches the daemon's `webapp.log` carrying its
 * correlation identity — is the file that takes it.
 *
 * The right-hand column is what EVERY file forwarding would cost, which is the
 * thing that was rejected. As landed, only the client-log file pays it (267ms
 * for its six tests) and the other ten sit at their quiet times.
 */
beforeAll(() => {
  resetLoggingForTests();
  setLogger(new ForwardingLogger(async () => {}, () => {}));
  bindLogContext({ connection_id: "webapp-layer-connection" });
});
