import { beforeAll } from "vitest";
import { ForwardingLogger, bindLogContext, resetLoggingForTests, setLogger } from "../../src/log.js";
import { installResizeObserver } from "../resize-observer.js";
import { installIntersectionObserver } from "../intersection-observer.js";
import { installTreeLayout } from "../tree-layout.js";

/**
 * jsdom implements no `ResizeObserver` (it performs no layout), and the feed
 * mount subscribes one to its scroll box so a footer settling after a render
 * cannot leave the tail below the fold. This project does not load
 * `test/setup.ts`, so it installs the same environment substitution itself.
 */
installResizeObserver();

/**
 * jsdom implements no `IntersectionObserver` either, and the feed mount
 * subscribes one for the overscan pre-render band (overscan.ts). This project
 * does not load `test/setup.ts`, so it installs the same substitution itself.
 */
installIntersectionObserver();

/**
 * jsdom lays nothing out, and a response bubble that draws a metaprompt tree
 * measures its column budget off the bubble's real geometry — there is no
 * default width to wrap to. This stages that geometry (the stylesheet's 77% cap
 * of a 1000px column, an 8px column) for the whole layer, the same kind of
 * environment substitution as the two observers above.
 */
installTreeLayout();

/**
 * THE LOGGER, INSTALLED BEFORE THE FIRST `beforeAll` MOUNTS.
 *
 * `test/setup.ts` reproduces production's "the forwarding logger is installed
 * before any runtime work" invariant in a `beforeEach`, which is early enough
 * for a suite that mounts inside `it`. This layer mounts ONCE per file in
 * `beforeAll` — one real daemon, one page, many scenarios — and `beforeAll`
 * runs before any `beforeEach`, so without this the first canonical log call on the
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
 * Every file got slower and the two heaviest gained ~0.5s and ~0.9s. The cost
 * comes from each admitted record being its own unary round trip. So:
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
  setLogger(new ForwardingLogger(async () => "accepted", () => {}));
  bindLogContext({ connection_id: "webapp-layer-connection" });
});
