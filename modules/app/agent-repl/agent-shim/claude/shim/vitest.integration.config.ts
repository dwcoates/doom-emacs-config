/**
 * vitest.integration.config.ts — the INTEGRATION suite, run on its own.
 *
 * A separate config rather than a second `include` on the unit config, for two
 * reasons that both matter to whoever runs these:
 *
 *   - `npm test` must stay the fast, hermetic unit suite. The integration
 *     suite spawns a real process per test and depends on `npm run build`
 *     having produced `dist/main.js`, so folding it into `npm test` would make
 *     a fresh checkout's unit run fail for want of a bundle.
 *   - the timeouts are different in kind. A unit test that takes five seconds
 *     is broken; an integration test that spawns a node process, binds a
 *     socket, drives a mocked vendor through a turn and stands the process
 *     down legitimately takes longer, and a shared budget would either mask
 *     unit-suite regressions or flake here.
 *
 * `test/setup.ts` is deliberately NOT loaded: it configures the IN-PROCESS
 * vendor guard, and the vendor here runs in the CHILD, where the harness sets
 * `AGENT_REPL_FORBID_VENDOR_CALLS=1` in the spawn environment itself.
 *
 * NO FAIL-FAST. `bail` stays unset so one run reports every failure it found;
 * fixing them one process-restart at a time is what this suite exists to avoid.
 */
// Refuses a run that did not come through bin/background.sh: tests only ever
// run at background priority (see that script and require-background.mjs).
import "../../../bin/require-background.mjs";
import { defineConfig } from "vitest/config";
import { fileURLToPath } from "node:url";

// Same reason as the unit config: the generated protobuf stubs live outside
// this package, and vite's fs guard blocks them without this allowance.
const agentReplRoot = fileURLToPath(new URL("../../../", import.meta.url));

export default defineConfig({
  server: { fs: { allow: [agentReplRoot] } },
  test: {
    include: ["test/integration/**/*.test.ts"],
    // Each test spawns and tears down its own shim, so they must not share a
    // process-global; threads are fine, but the per-test budget has to cover a
    // real spawn.
    //
    // Tight on purpose. The three production windows these tests used to RIDE
    // are now `--fake`-only overrides the harness scales (see
    // `test/integration-support/harness.ts`), so the observed healthy max
    // across 310 tests fell from ~5.14s to ~645ms on a quiet machine
    // (test/integration/session.test.ts, a forced kill spending the whole
    // scaled watcher-conclusion budget).
    //
    // The budget is sized from the CONTENDED max, ~1.55s, and not from that
    // quiet-machine 645ms: this suite always runs its seven files in parallel,
    // each spawning a real node process, so contention is its normal condition
    // rather than an anomaly to size below and flake on. 5s is ~3x that
    // contended max. A test hitting this timeout is hung, not merely slow —
    // raise it only with a new measured reason, never to paper over a hang.
    //
    // Hooks share the test budget: the only one is `afterEach(cleanupShims)`
    // (SIGKILL + temp-dir removal), far cheaper than any test body.
    testTimeout: 5_000,
    hookTimeout: 5_000,
    teardownTimeout: 10_000,
  },
});
